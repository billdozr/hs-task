module Text.Task.Finder.Find
       (
         Info(..), Iterate(..)
       , Iterator, Predicate
       , find, isDirectory, getInfo
       ) where

import System.Directory (Permissions(..), 
                         getModificationTime, 
                         getPermissions, 
                         getDirectoryContents)
import System.Time (ClockTime(..))
import Control.Exception
import System.IO (IOMode(..), hClose, hFileSize, openFile)
import System.FilePath ((</>))
import Control.Monad
import Data.Char (toLower)

data Info = Info {
      infoPath :: FilePath
    , infoPerms :: Maybe Permissions
    , infoSize :: Maybe Integer
    , infoModTime :: Maybe ClockTime
    } deriving (Eq, Ord, Show)

data Iterate seed = Done     { unwrap :: seed }
                  | Skip     { unwrap :: seed }
                  | Continue { unwrap :: seed }
                    deriving (Show)

type Iterator seed = seed -> Info -> Iterate seed
type Predicate = [FilePath] -> Info -> Iterate [FilePath]

foldTree :: Iterator a -> a -> FilePath -> IO a
foldTree iter initSeed path = do
    endSeed <- fold initSeed path
    return (unwrap endSeed)
  where
    fold seed subpath = getUsefulContents subpath >>= walk seed subpath
    walk seed subpath (name:names) = do
      let path' = subpath </> name
      info <- getInfo path'
      case iter seed info of
        done@(Done _) -> return done
        Skip seed'    -> walk seed' subpath names
        Continue seed'
          | isDirectory info -> do
              next <- fold seed' path'
              case next of
                done@(Done _) -> return done
                seed''        -> walk (unwrap seed'') subpath names
          | otherwise -> walk seed' subpath names
    walk seed _ _ = return (Continue seed)

getInfo :: FilePath -> IO Info
getInfo path = do
  perms <- maybeIO (getPermissions path)
  size <- maybeIO (bracket (openFile path ReadMode) hClose hFileSize)
  modified <- maybeIO (getModificationTime path)
  return (Info path perms size modified)

find :: Predicate -> FilePath -> IO [FilePath]
find pred path = foldTree pred [] path

getUsefulContents :: FilePath -> IO [String]
getUsefulContents path = do
    names <- getDirectoryContents path
    return (filter (`notElem` [".", ".."]) names)

isDirectory :: Info -> Bool
isDirectory = maybe False searchable . infoPerms

maybeIO :: IO a -> IO (Maybe a)
maybeIO act = handleAny (\_ -> return Nothing) (Just `liftM` act)

handleAny :: (SomeException -> IO a) -> IO a -> IO a
handleAny = handle
