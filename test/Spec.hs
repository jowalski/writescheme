import qualified PrimTests as PT
import System.Exit

main :: IO ()
main =
  do
     -- add test runners into the array for each module
     good <- and <$> sequence [PT.runTests]
     if good
        then exitSuccess
        else exitFailure