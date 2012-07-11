import Control.Monad
import Text.Printf
import Test.QuickCheck

-- Importing of test properties
import qualified TodoParser as T

main = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests

testOptions = stdArgs { maxSize = 50, maxSuccess = 200 }
tests = [("todo-parser/idempotent", 
          quickCheckWith testOptions T.prop_idempotent),
         ("todo-parser/minmaxAttrSize",
          quickCheckWith testOptions T.prop_minmaxAttrSize)]
