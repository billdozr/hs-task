{-# LANGUAGE OverloadedStrings #-}
module Text.Todo.Cruncher.Html 
    (
      renderEntries
    ) where

import Prelude
import qualified Prelude as P
import Control.Monad (forM_)
import Data.Maybe (maybe)
import Data.List (intercalate)
import Data.Time (formatTime)
import System.Locale (TimeLocale(..), defaultTimeLocale)
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Text.Todo.Parser.Types as T
import Text.Todo.Cruncher.Output (showPri)

allEntries :: [Either T.TagEntryError T.TodoEntry] -> Html a 
allEntries ts = html $ do
        H.head $ do
          H.title "Todo's"
        body $ do
          p "A list of Todo's:"
          ul $ forM_ ts ((li ! A.style "padding-bottom:10px" ) . pprint)
  where
    pprint (Left err)    = showHtml err
    pprint (Right entry) = do 
        b $ string "Subject: " 
        string (maybe "None" (\r->r) (T.subject entry))
        br
        b $ string "Action: "
        string (T.action entry)
        br
        b $ string "Label(s): "
        string (maybe "No labels" (\r -> intercalate ", " r) 
                                 (T.labels entry))
        br
        b $ string "User(s): "
        string (maybe "No users" (\r -> intercalate ", " r) 
                                 (T.users entry))
        br
        b $ string "Priority: "
        string (maybe "None" (\p-> showPri p) (T.priority entry))
        br
        b $ string "Time spent: "
        string (maybe "None" (\t-> either show show t) (T.timeSpent entry))
        br
        b $ string "Source file: "
        string (T.srcName $ T.srcInfo entry)
        br
        b $ string "Line / Column: "
        showHtml (T.srcPos $ T.srcInfo entry)
        br
        b $ string "File modified: "
        string (maybe "Unknown" (\m -> formatTime defaultTimeLocale 
                                    (dateTimeFmt defaultTimeLocale) m) 
                (T.modTime $ T.srcInfo entry))

renderEntries :: [Either T.TagEntryError T.TodoEntry] -> String
renderEntries ts = C.unpack . renderHtml $ allEntries ts
