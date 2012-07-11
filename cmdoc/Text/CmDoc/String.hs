module Text.CmDoc.String where

import Data.Char (isSpace)

stripr :: String -> String
stripr = reverse . p . reverse -- . p
  where
    p = dropWhile isSpace
