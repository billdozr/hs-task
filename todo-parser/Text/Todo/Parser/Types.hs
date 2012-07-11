module Text.Todo.Parser.Types 
       (
         TodoEntry(..)
       , MetaInfo(..)
       , TagEntryError(..)
       , RawEntry
       , todoTagStr, todoHashTag
       , jcon, ncon
       ) where

import Control.Applicative
import Control.Monad (liftM)
import Data.Maybe (isJust)
import Data.List (intercalate)
import Text.CmDoc.Types

todoTagStr  = "@TODO"
todoHashTag = '#'

data TodoEntry = TodoEntry { subject   :: (Maybe String)
                           , action    :: String
                           , labels    :: (Maybe [String])
                           , users     :: (Maybe [String])
                           , priority  :: (Maybe Char)
                           , timeSpent :: (Maybe (Either Integer Double))
                           , srcInfo   :: MetaInfo
                           } deriving (Eq, Ord)

instance Show TodoEntry where
    show entry = todoTagStr ++ (uStr $ 
                               (++) <$> (ncon $ labels entry)
                                    <*> (map (\u -> todoHashTag:u) 
                                        `liftM` (ncon $ users entry)))
                            ++ ": " 
                            ++ (mStr $ subject entry) 
                            ++ (action entry)
                            ++ (pStr $ priority entry) 
                            ++ (tStr $ timeSpent entry) 
                            ++ atSep ++ (show $ srcInfo entry)
      where 
        atSep   = " | "
        mStr m  = maybe "" (\s -> s ++ atSep) m
        uStr ul = maybe "" (\l -> if null l then ""
                                    else "(" ++ (intercalate ", " l) ++ ")") ul
        pStr p  = maybe "" (\s -> atSep ++ [s]) p
        tStr t  = maybe "" (\n -> case isJust (priority entry) of
                                    True      -> ", " ++ either show show n
                                    otherwise -> atSep ++ either show show n) t

-- Maybe helper functions
jcon :: Maybe [t] -> Maybe [t]
jcon m = case m of 
            Just [] -> Nothing
            result  -> result

ncon :: Maybe [a] -> Maybe [a]
ncon m = case m of
            Nothing -> Just []
            result  -> result
