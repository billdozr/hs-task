module Main where

import System.Environment
import Data.List (intercalate)
import Data.List.Split (splitOn)
import System.FilePath (takeFileName, takeExtension)
import Data.Char (toLower)
import Text.Task.Parser.Parse (parseCmdLns)
import Text.Task.Parser.Types (todoTagStr)
import Text.Task.Finder.Find

main :: IO ()
main = do 
   args <- getArgs
   let (isRecur, fileExts, topDirPath) = loadArgs args
   pathInfo <- getInfo topDirPath
   filteredNames <- case isDirectory pathInfo of
                      True      -> find (withExtNoSCMFilter isRecur fileExts) 
                                        topDirPath
                      otherwise -> if isRecur || (not . null) fileExts
                                     then error usage
                                     else return [topDirPath]
   rawTodos <- mapM (\p -> parseCmdLns p todoTagStr) filteredNames
   putStrLn $ intercalate "\n" $ concat rawTodos
 where withExtNoSCMFilter rec fileExts paths info
          | isDirectory info 
            && (takeFileName path == ".svn" || takeFileName path == ".git")         
            = Skip paths
          | not rec && isDirectory info 
            = Skip paths
          | extension `elem` fileExts
            = Continue (path : paths)
          | otherwise
            = Continue paths
         where extension = map toLower (takeExtension path)
               path = infoPath info

loadArgs :: [String] -> (Bool, [String], String)
loadArgs ["-h"] = error usage
loadArgs [('-':xs)] = error usage
loadArgs ["-R", "-f", fxs, topLevelDir] = (True, fileXts fxs, topLevelDir)
loadArgs ["-f", fxs, "-R", topLevelDir] = (True, fileXts fxs, topLevelDir)
loadArgs ["-f", fxs, topLevelDir] = (False, fileXts fxs, topLevelDir)
loadArgs ["-R", topLevelDir] = (True, [], topLevelDir)
loadArgs [topLevelDir] = (False, [], topLevelDir)
loadArgs badArgs = error $ "Invalid argument list: " 
                         ++ intercalate ", " badArgs ++ "\n\n" ++ usage
fileXts = splitOn ","

header = "\nUsage: task-find [OPTION...] toplevel_dir|file_path"
options = "\t-R\tRecurse sub-directories\n" 
            ++ "\n\t-f\tFilter by file extention\n" 
            ++ "\n\t-h\tThis usage info\n"
usage = header ++ "\n\n" ++ options
