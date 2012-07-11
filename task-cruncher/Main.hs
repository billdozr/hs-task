module Main where

import System.Environment (getArgs)
import Data.List (lookup, intercalate)
import Data.Maybe (fromJust)
import Text.Task.Parser.Parse (parseTodoEntry, parseMeta)
import qualified Text.Task.Parser.Types as T
import qualified Text.Task.Cruncher.Plain as P
import qualified Text.Task.Cruncher.Html as H
import qualified Text.Task.Cruncher.Xml as X

main :: IO ()
main = do
    stdin <- getContents
    args  <- getArgs
    let (renderF, rawTodos) = loadArgs args (lines stdin)
        result = map (\r -> (r, parseTodoEntry r)) rawTodos
    putStrLn $ renderF (map sEntry result)
  where
    sEntry (rawTodo, (Left err))    = 
        Left $ T.TagEntryError (show err) (parseMeta rawTodo)
    sEntry (_, (Right eitherEntry)) = eitherEntry

dispatch :: [(String, [Either T.TagEntryError T.TodoEntry] -> String)]  
dispatch =  [ ("plain", P.renderEntries)
            , ("html", H.renderEntries) 
            , ("xml", X.renderEntries)
            ]

loadArgs :: [String] -> 
            [String] ->
            ([Either T.TagEntryError T.TodoEntry] -> String, [T.RawEntry])
loadArgs ["-h"] _ = error usage
loadArgs [('-':xs)] _ = error usage
loadArgs [rawTodo] _ = loadArgs ["-o", "plain", rawTodo] []
loadArgs ["-o", outType, rawTodo] _ = loadArgs ["-o", outType] [rawTodo]
loadArgs [] rawTodos@(x:xs) = loadArgs ["-o", "plain"] rawTodos
loadArgs ["-o", outType] rawTodos = 
    if outType == "plain" || outType == "html" || outType == "xml"
    then (fromJust $ lookup outType dispatch
         , rawTodos)
    else loadArgs [] []
loadArgs badArgs _ = 
    error $ "Invalid argument list: " 
              ++ intercalate ", " badArgs ++ "\n\n" ++ usage

header = "\nUsage: task-crunch [OPTION...] raw_task"
options = "\t-o\tOutput format (plain | html | xml)\n" 
            ++ "\n\t-h\tThis usage info\n"
usage = header ++ "\n\n" ++ options
