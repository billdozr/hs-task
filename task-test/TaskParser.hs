module TaskParser where

import Text.CmDoc
import Text.Task.Parser.Parse
import SetGen

prop_idempotent (RawTag rt) = parse (parse rt) == parse rt
    where 
      parse rawTodo = either show (\t -> either (\e -> show e) show t)
                      $ parseTodoEntry rawTodo

prop_minmaxAttrSize (RawTag rt) = 
    case parseCmd rt of
      (Right (Command _ _ attrs)) -> 0 < attrDataLen && attrDataLen < 4
           where
             attrDataLen = length $ init attrs
             meta        = last attrs -- last attr is always meta-data
      otherwise                   -> False
