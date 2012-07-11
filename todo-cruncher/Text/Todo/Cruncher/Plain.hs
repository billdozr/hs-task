module Text.Todo.Cruncher.Plain
    (
      renderEntries
    ) where

import Data.List (intercalate)
import qualified Text.Todo.Parser.Types as T

renderEntries :: [Either T.TagEntryError T.TodoEntry] -> String
renderEntries ts = intercalate "\n\n" $ map (either show show) ts
