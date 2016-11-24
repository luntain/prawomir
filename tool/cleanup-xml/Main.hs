import MyPrelude
import Text.XML.HXT.Core
import System.Environment

main = do
  filename <- head <$> getArgs
  runX (readDocument [] filename)
