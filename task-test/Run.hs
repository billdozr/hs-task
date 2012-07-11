import Control.Monad
import Text.Printf
import Test.QuickCheck

-- Importing of test properties
import qualified TaskParser as T

main = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests

testOptions = stdArgs { maxSize = 50, maxSuccess = 200 }
tests = [("task-parser/idempotent", 
          quickCheckWith testOptions T.prop_idempotent),
         ("task-parser/minmaxAttrSize",
          quickCheckWith testOptions T.prop_minmaxAttrSize)]
