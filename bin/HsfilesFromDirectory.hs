import qualified Data.Text.IO       as Text
import           Hscaffold
import           System.Environment

main :: IO ()
main = do
    as <- getArgs
    case as of
        (fp : _) -> go fp
        [] -> error "Usage: hsfiles-from-directory <DIRECTORY>"
  where
    go fp = do
        ws <- hscaffoldFromDirectory fp
        Text.putStr $ toHsfiles (tell ws)
