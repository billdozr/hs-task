module Text.CmDoc.Lexer 
    (
      Parsable(..)
    , parseCmdLns
    , parseCmd
    , parseMeta
    ) where

import System.Directory (getModificationTime)
import Control.Monad (liftM)
import Control.Exception (handle, SomeException)
import Control.Applicative
import Data.Time (parseTime)
import System.Locale (TimeLocale(..), defaultTimeLocale)
import Text.Parsec hiding (many, optional, (<|>), Line, Column)
import Text.Parsec.ByteString (parseFromFile)
import qualified Text.Parsec.Token as T
import qualified Text.Parsec.Language as L
import Text.CmDoc.Types
import Text.CmDoc.String (stripr)

-- Bind all the used lexical parsers at toplevel
lexer = T.makeTokenParser L.emptyDef
parens = T.parens lexer
comma = T.comma lexer
colon = T.colon lexer
symbol = T.symbol lexer
whiteSpace = T.whiteSpace lexer

-- Parsers
-- @TODO: Opt | cmdLnsParser is very slow; should parse in comments only | N
cmdLnsParser ctype modified = 
    (ctype++)
    <$> (manyTill anyChar 
           (try (string ctype *> notFollowedBy alphaNum))
        *> (do e <- manyTill (noneOf "\n\r") 
                     (try eol <|> (eof *> return ""))
               p <- getPosition
               return $ e ++ " | \"" 
                          ++ (sourceName p)
                          ++ "\", ("
                          ++ (show $ (sourceLine p) - 1)
                          ++ ","
                          ++ (show $ sourceColumn p)
                          ++ ")"
                          ++ (maybe "" (\m-> ", " ++ show m) modified)))
cmdParser = Command 
            <$> (char '@' *> many alphaNum) 
            <*> optional (parens $ try ((++) <$> string "#" <*> many1 alphaNum
                                    <|> many alphaNum) `sepBy` comma)
            <*> (colon *> cmdAttrsParser)
cmdAttrsParser = do xs <- many (noneOf "|\n") `sepBy` symbol "|" 
                    return $ map stripr xs
metaParser = MetaInfo
             <$> (between dblQuote dblQuote (many (noneOf "\"")) <* comma)
             <*> parens (do xs <- many digit `sepBy` comma
                            return (read $ head xs :: Line, 
                                    read (head $ tail xs) :: Column))
             <*> (try (comma *> 
                        (do rd <- manyTill (noneOf "\n\r") eof
                            return $ parseTime 
                                     defaultTimeLocale 
                                     (dateTimeFmt defaultTimeLocale) 
                                     rd))
                      <|> return Nothing)
  where dblQuote = (string "\"")

eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"

-- Parse functions
parseCmdLns :: FilePath -> CmdType -> IO [RawEntry]
parseCmdLns path ctype = do 
    modified <- maybeIO (getModificationTime path)
    result   <- parseFromFile (many (try $ cmdLnsParser ctype modified)) path
    case result of
      Left err -> return [] -- ignore errors at this level
      Right xs -> return xs
  where
    maybeIO act = handleAny (\_ -> return Nothing) (Just `liftM` act)
    handleAny :: (SomeException -> IO a) -> IO a -> IO a
    handleAny = handle

parseCmd :: RawEntry -> Either ParseError Command
parseCmd rt = parse (do {whiteSpace;cmdParser}) "cmd" rt

parseMeta :: RawEntry -> MetaInfo
parseMeta rt = either (\_ -> MetaInfo "(unknow)" (0,0) Nothing) (\x -> x)
               $ mParser metaRawEntry
  where dataAttrs = parse cmdAttrsParser "cmdAttrs" rt
        mParser   = parse metaParser "meta"
        metaRawEntry = either (\_->"") (last) dataAttrs

-- Type classes
class Parsable a where
    parseTagEntry :: RawEntry 
                     -> Either ParseError (Either TagEntryError a)
    parseTagEntry rt = cmd2entry <$> parseCmd rt
    
    cmd2entry :: Command -> Either TagEntryError a
