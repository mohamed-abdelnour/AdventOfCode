import qualified Modular                       as M
import qualified Naive                         as N
import           System.Environment
import           Text.Printf


output :: String -> IO ()
output path = do
  input <- lines <$> readFile path
  printf "File: %s\n" path
  printf "  Naive:   %d\n" . N.solve . parse $ input
  printf "  Modular: %d\n" . M.solve . parse $ input
 where
  parse (k1 : k2 : _) = (read k1, read k2)
  parse _             = error "Invalid input format"

main :: IO ()
main = getArgs >>= mapM_ output
