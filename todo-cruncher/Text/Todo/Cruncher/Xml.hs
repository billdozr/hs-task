module Text.Todo.Cruncher.Xml 
    (
      renderEntries
    ) where

import Data.List
import Text.XML.Light
import Data.Time (formatTime)
import System.Locale (TimeLocale(..), defaultTimeLocale)
import Text.Todo.Parser.Parse
import Text.Todo.Parser.Types as T
import Text.Todo.Cruncher.Output (showPri)

xmlTodo :: T.TodoEntry -> Content
xmlTodo (TodoEntry s a l u p t si) = Elem $ Element
    (unqual "todo")
    []
    ( sub ++ act ++ lbls ++ usrs ++ 
      attrs ++ sinfo )
    Nothing
  where 
    sub      = el "subject" s
    act      = el "action" $ Just a
    lbls     = lu "label" l 
    usrs     = lu "user" u
    attrs    = case (null pr, null ts) of 
                 (True, True) -> []
                 otherwise    -> [ Elem $ Element
                                   (unqual "attributes")
                                   []
                                   (pr ++ ts)
                                   Nothing ]
    (pr, ts) = (el "priority" (fmap (\pv -> showPri pv) p), 
                el "time-spent" (fmap (either show show) t))
    sinfo    = [Elem $ xmlMetaInfo si]
    lu l xs  = case xs of
                 Just lab  -> concat $ map (\s -> el l $ Just s) lab
                 otherwise -> []
    el k v   = 
        case v of
          Just s    -> [ Elem $ Element 
                         (unqual k)
                         []
                         [Text $ CData CDataText s Nothing]
                         Nothing ]
          otherwise -> []

xmlMetaInfo :: T.MetaInfo -> Element
xmlMetaInfo (MetaInfo sname (lno, col) mdate) = addOptMDate $ Element
    (unqual "meta-info")
    [
      Attr (unqual "line") (show lno)
    , Attr (unqual "column") (show col)
    ]
    [Text $ CData CDataText sname Nothing]
    Nothing
  where addOptMDate el = el { elAttribs =
            case mdate of
              Just d    -> (elAttribs el) 
                           ++ [ Attr (unqual "modtime") 
                                (formatTime defaultTimeLocale 
                                (dateTimeFmt defaultTimeLocale) d) ]
              otherwise -> (elAttribs el) }

xmlError :: T.TagEntryError -> Content
xmlError (T.TagEntryError errMsg srcInf) = 
    Elem $ Element 
    (unqual "error")
    []
    [ Elem $ Element (unqual "message") [] 
      [Text $ CData CDataText errMsg Nothing] Nothing
    , Elem $ xmlMetaInfo srcInf ]
    Nothing

allEntries :: [Either T.TagEntryError T.TodoEntry] -> String
allEntries ts = showTopElement $ Element 
    (unqual "todos")
    []
    (map (getXml') ts)
    Nothing
  where
    getXml' (Left err)    = xmlError err
    getXml' (Right entry) = xmlTodo entry

renderEntries :: [Either T.TagEntryError T.TodoEntry] -> String
renderEntries ts = allEntries ts
--renderEntries ts = concat $ map (ppContent) $ parseXML $ allEntries ts
