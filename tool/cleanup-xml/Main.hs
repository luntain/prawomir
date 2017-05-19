-- Get rid of white space inside TOKEN tags. This is a helper tool for preparing tests.
-- used to process "tests/pit1.xml"
import MyPrelude hiding (when)
import Text.XML.HXT.Core
import System.Environment
import System.Directory
import Data.String.Utils

main = do
  filename <- head <$> getArgs
  (tmpFile, _handle) <- openTempFile "/tmp" "clean-xml-tool"
  runX (readDocument [withInputEncoding "UTF-8", withValidate no] filename
       >>> processBottomUp (processChildren (changeText strip `when` isText) `when` hasName "TOKEN")
       >>> writeDocument [withOutputEncoding "UTF-8", withValidate yes] tmpFile)
  renameFile tmpFile filename
