module Text.CmDoc.Types where

import Data.Time (ZonedTime(..), formatTime)
import System.Locale (TimeLocale(..), defaultTimeLocale)

type CmdType   = String
type CmdLabels = (Maybe [String])
type CmdAttrs  = [String]

type RawEntry  = String

type Line      = Int
type Column    = Int

data TagEntryError = TagEntryError String MetaInfo

data Command = Command CmdType CmdLabels CmdAttrs
               deriving (Eq, Ord, Show)

data MetaInfo = MetaInfo { srcName :: FilePath
                         , srcPos :: (Line, Column)
                         , modTime :: Maybe ZonedTime
                         } deriving (Eq, Ord)

instance Show MetaInfo where
    show inf = "\"" ++ (srcName inf) ++ "\"" ++
               ", (" ++ (show l) ++ "," ++ (show c) ++ ")" ++
               modified
      where (l, c)   = (srcPos inf)
            modified = maybe "" 
                             (\m -> ", " ++ 
                               formatTime defaultTimeLocale 
                               (dateTimeFmt defaultTimeLocale) m) 
                             (modTime inf)

instance Show TagEntryError where
    show (TagEntryError errMsg srcInf) = 
        errMsg ++ "\n***" ++ show srcInf ++ "***"

instance Eq ZonedTime where
    (ZonedTime alt azt) == (ZonedTime blt bzt) = (alt == blt) && (azt == bzt) 

instance Ord ZonedTime where
    compare (ZonedTime alt azt) (ZonedTime blt bzt) = 
            case (compare alt blt) of
                 EQ -> compare azt bzt
                 cmp -> cmp
