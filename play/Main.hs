import MyPrelude
import Scratch
import ParseXml

--main =
--  writeFile "/tmp/parsed-pages" . show =<< parsePages "cache/xml/pit.xml"

main = fromXml "cache/xml/pit.xml"
