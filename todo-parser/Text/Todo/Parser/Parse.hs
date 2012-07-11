module Text.Todo.Parser.Parse
    (
      parseCmdLns
    , parseMeta
    , parseTodoEntry
    ) where 

import Control.Applicative
import Control.Monad (liftM)
import Data.Maybe (isJust)
import Text.Parsec hiding (many, optional, (<|>))
import qualified Text.Parsec.Token as T
import qualified Text.Parsec.Language as L
import Text.CmDoc
import Text.Todo.Parser.Types

-- Bind all the used lexical parsers at toplevel
lexer = T.makeTokenParser L.emptyDef
parens = T.parens lexer
comma = T.comma lexer
naturalOrFloat = T.naturalOrFloat lexer

-- Parsers
attrParser = (,) 
             <$> optional 
                 (oneOf "LNH" <* (comma <|> (eof *> return "")))
             <*> optional ((try (manyTill (noneOf ",") comma) 
                                    *> (naturalOrFloat <* eof)) 
                           <|> (naturalOrFloat <* eof))

cmd2todo :: Command -> Either TagEntryError TodoEntry
cmd2todo (Command _ lu attrs)  
    | 0 < len && len < 4 = 
        either (\err -> Left  $ TagEntryError (show err) meta)
               (\_   -> Right $ TodoEntry sub act lb ul pri ts meta) attr
    | otherwise          = 
        Left $ TagEntryError "Required: 1 to 3 data attributes" meta
  where
    aParser = parse attrParser "attr"
    attrData = init attrs
    metaRawEntry = last attrs
    len = length attrData
    userP s = length s > 0 && head s == todoHashTag
    lb = jcon $ filter (not . userP) `liftM` lu
    ul = jcon $ map (\s-> if length s > 1 then tail s else s) 
                    `liftM` (filter userP `liftM` lu)
    sub = case (len, isJust pri || isJust ts) of
            (1, _)    -> Nothing
            (2, True) -> Nothing
            _         -> Just $ head attrData
    act = case (len, isJust pri || isJust ts) of
            (1, _)    -> head attrData
            (2, True) -> head attrData
            _         -> head $ tail attrData
    attr = case len of
             2 -> aParser (head $ tail attrData)
             3 -> aParser (head $ tail $ tail attrData)
             _ -> Right (Nothing, Nothing)
    (pri, ts) = either (\_ -> (Nothing, Nothing)) (\x -> x) attr
    meta = parseMeta metaRawEntry

instance Parsable TodoEntry where
    cmd2entry = cmd2todo

parseTodoEntry :: RawEntry 
                  -> Either ParseError (Either TagEntryError TodoEntry)
parseTodoEntry = parseTagEntry
