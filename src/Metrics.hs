{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Metrics where

import Control.Exception (IOException, handle)
import Control.Monad (join, void, when)
import Data.Foldable (for_)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (isSuffixOf)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Data.Time.Clock (diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import System.Directory
import Text.Printf (printf)

data AppMetrics = AppMetrics
  { successCount :: Int,
    failureCount :: Int,
    callDuration :: Map.Map String Int
  }
  deriving (Show)

newtype Metrics = Metrics {apMetricsStore :: IORef AppMetrics}

newMetrics :: IO Metrics
newMetrics = let emptyMetrics = AppMetrics 0 0 Map.empty in Metrics <$> newIORef emptyMetrics

tickSuccess :: Metrics -> IO ()
tickSuccess (Metrics ref) = modifyIORef' ref (\m -> m {successCount = successCount m + 1})

tickFailure :: Metrics -> IO ()
tickFailure (Metrics ref) = modifyIORef' ref (\m -> m {failureCount = failureCount m + 1})

timeFunction :: Metrics -> String -> IO a -> IO a
timeFunction (Metrics ref) name action = do
  start <- getCurrentTime
  result <- action
  end <- getCurrentTime
  modifyIORef' ref $ \oldMetrics ->
    let oldDuration = fromMaybe 0 $ Map.lookup name (callDuration oldMetrics)
        runDuration = floor . nominalDiffTimeToSeconds $ diffUTCTime end start
        newDuration = oldDuration + runDuration
     in oldMetrics {callDuration = Map.insert name newDuration (callDuration oldMetrics)}
  pure result

displayMetrics :: Metrics -> IO ()
displayMetrics (Metrics ref) = do
  AppMetrics {..} <- readIORef ref
  putStrLn $ "Success count: " ++ show successCount
  putStrLn $ "Failure count: " ++ show failureCount
  mapM_ (\(name, duration) -> do 
    putStrLn $ printf "Time spent in \"%s\": %d" name duration) (Map.toList callDuration)

--------------------------------------------------------------------------------

dropSuffix :: (Eq a) => [a] -> [a] -> [a]
dropSuffix suffix s
  | suffix `isSuffixOf` s = take (length s - length suffix) s
  | otherwise = s

data FileType
  = FileTypeDirectory
  | FileTypeRegularFile
  | FileTypeOther
  deriving (Show)

classifyFile :: FilePath -> IO FileType
classifyFile path = do
  isDir <- doesDirectoryExist path
  isFile <- doesFileExist path
  pure $ case (isDir, isFile) of
    (True, _) -> FileTypeDirectory
    (_, True) -> FileTypeRegularFile
    _ -> FileTypeOther

--------------------------------------------------------------------------------

traverseDirectory :: Metrics -> FilePath -> (FilePath -> IO ()) -> IO ()
traverseDirectory metrics rootPath action = do
  seen <- newIORef Set.empty :: IO (IORef (Set.Set FilePath))
  let haveSeenDirectory canonicalPath = do
        Set.member canonicalPath <$> readIORef seen

      addDirectoryToSeen canonicalPath =
        modifyIORef' seen (Set.insert canonicalPath)

      handler ex = print ex >> tickFailure metrics

      traverseSubdirectory subdirPath = do
        timeFunction metrics "traverseSubdirectory" $ do
          contents <- listDirectory subdirPath
          for_ contents $ \file' ->
            handle @IOException handler $ do
              let file = subdirPath <> "/" <> file'
              canonicalPath <- canonicalizePath file
              classified <- classifyFile canonicalPath
              res <- case classified of
                FileTypeOther -> pure ()
                FileTypeRegularFile -> action file
                FileTypeDirectory -> do
                  alreadySeen <- haveSeenDirectory canonicalPath
                  when (not alreadySeen) $ do
                    addDirectoryToSeen file
                    traverseSubdirectory file
              tickSuccess metrics
              pure res

  traverseSubdirectory (dropSuffix "/" rootPath)

directorySummary :: FilePath -> IO ()
directorySummary rootPath = do
  metrics <- newMetrics
  histogramRef <- newIORef (Map.empty :: Map.Map Char Int)
  traverseDirectory metrics rootPath $ \file -> do
    putStrLn $ file <> ":"
    contents <- timeFunction metrics "TextIO.readFile" $ TextIO.readFile file

    timeFunction metrics "wordCount" $ do
      let wordCount = length $ Text.words contents
      putStrLn $ "    Word count: " <> show wordCount

    timeFunction metrics "histogram" $ do
      oldHistogram <- readIORef histogramRef
      let addCharToHistogram histogram c = Map.insertWith (+) c 1 histogram
          newHistogram = Text.foldl' addCharToHistogram oldHistogram contents
      writeIORef histogramRef newHistogram

  histogram <- readIORef histogramRef
  putStrLn "Histogram:"
  for_ (Map.toList histogram) $ \(c, count) ->
    putStrLn $ printf "    %c: %d" c count

  displayMetrics metrics
