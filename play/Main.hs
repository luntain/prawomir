import MyPrelude
import Scratch
import ParseXml
import System.Environment

--main =
--  writeFile "/tmp/parsed-pages" . show =<< parsePages "cache/xml/pit.xml"

main = do
 [filename] <- getArgs
 fromXml filename
