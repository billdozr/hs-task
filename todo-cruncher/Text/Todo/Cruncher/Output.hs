module Text.Todo.Cruncher.Output
       (
         showPri
       ) where

showPri :: Char -> String
showPri p = case p of
              'L' -> "Low"
              'N' -> "Normal"
              'H' -> "High"
