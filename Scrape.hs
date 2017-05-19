{-# LANGUAGE Arrows, OverloadedStrings #-}
module Scrape where
import MyPrelude hiding (utf8)
import Data.Time
import Text.XML.HXT.Core
import Text.XML.HXT.HTTP
import Text.XML.HXT.TagSoup
import Text.XML.HXT.Cache
import qualified Data.Text as T
import Codec.Compression.GZip (compress, decompress)
import Data.Attoparsec.Text

data Pozycja =
  Pozycja { _pDataOgloszenia, _pDataWydania :: Day
          , _pDataWejsciaWZycie, _pDataObowiazywania :: Maybe Day -- what is the diff?
          , _pStatus :: T.Text -- obowiazujacy
          , _pOrganWydajacy :: T.Text
          , _pRok :: Int
          , _pPoz :: Int
          , _pOpis :: T.Text
          , _pPdf :: T.Text -- Tekst ogloszony, z dziennika
          , _pTekstAktu :: Maybe T.Text -- tresc ta sama jak "tekst ogloszony", ale ostylowanie ISAPu
          , _pTekstUjednolicony :: Maybe T.Text -- URI
          , _pUwagi :: Maybe T.Text
          , _pOrganZob :: Maybe T.Text
          } deriving (Show, Read)

dziennikUstawUri :: Int -> String
dziennikUstawUri year = printf "%sVolumeServlet?type=wdu&rok=%d&numer=000" uriRoot year

-- extract links to detail servlets
volumeServlet :: Int -> IOSArrow a Uri
volumeServlet year =
  readDocument
    [withTagSoup, withInputEncoding utf8, withHTTP []
    , withCache "cache/hxt" (60*60*24) False, withCompression (compress, decompress)]
    (dziennikUstawUri year)
  >>>
  deep (hasName "a" >>> hasAttrValue "href" ("DetailsServlet" `isPrefixOf`))
  >>>
  getAttrValue "href" >>^ (uriRoot ++)

uriRoot :: String
uriRoot = "http://isap.sejm.gov.pl/"
type Uri = String

(.<) a b = b >. a

-- I should make a wrapper around this arrow to assert it returns exactly 1 Pozycja (use listA) and arrIO
detailsServlet :: IOSArrow Uri Pozycja
detailsServlet =
  readFromDocument
    [withTagSoup, withInputEncoding utf8, withHTTP []
    , withCache "cache/hxt" maxBound False, withCompression (compress, decompress)]
  >>>
  deep (hasName "tbody" </ (hasName "tr" </ (hasName "th" </ hasText (=="Data wydania:"))))
  >>> proc tbody -> do
    header <- allText <<< deep (hasName "span" >>> hasAttrValue "class" (=="h1")) -< tbody
    let (_pRok, _pPoz) =
          forceEitherMsg ("Parsing: " ++ header)
          . parseOnly ((,) <$> (space *> string "Dz.U. " *> decimal) <*> (string " poz. " *> decimal))
          $ T.pack header
    _pOpis              <- T.pack ^<< allText <<< deep (hasName "span" >>> hasAttrValue "class" (=="h2")) -< tbody
    _pPdf               <- T.pack . (init uriRoot ++) ^<< getAttrValue "href" <<< deep (hasName "a")
                            <<< rowValue "Tekst ogłoszony:" -< tbody
    _pStatus            <- T.pack ^<< rowValueText "Status aktu prawnego:" -< tbody
    _pDataOgloszenia    <- read ^<< rowValueText "Data ogłoszenia:" -< tbody
    _pDataWydania       <- rowValueText "Data wydania:" -< tbody
    _pDataWejsciaWZycie <- listToMaybe .< rowValueText "Data wejścia w życie:" -< tbody
    _pDataObowiazywania <- listToMaybe .< rowValueText "Data obowiązywania:" -< tbody
    _pOrganWydajacy     <- rowValueText "Organ wydający:" -< tbody
    _pUwagi             <- listToMaybe .< (T.pack ^<< rowValueText "Uwagi:") -< tbody
    _pTekstAktu         <- listToMaybe .< (T.pack . (init uriRoot ++) ^<< getAttrValue "href" <<< deep (hasName "a")
                            <<< rowValue "Tekst aktu:") -< tbody
    _pTekstUjednolicony <- listToMaybe .< (T.pack . (init uriRoot ++) ^<< getAttrValue "href" <<< deep (hasName "a")
                            <<< rowValue "Tekst ujednolicony:") -< tbody
    _pOrganZob          <- listToMaybe .< (T.pack ^<< rowValueText "Organ zobowiązany:") -< tbody
    returnA -< Pozycja {_pDataOgloszenia,
                        _pDataWydania=read _pDataWydania,
                        _pDataWejsciaWZycie = fmap read _pDataWejsciaWZycie,
                        _pDataObowiazywania = fmap read _pDataObowiazywania,
                        _pStatus,
                        _pOrganWydajacy = T.pack _pOrganWydajacy,
                        _pRok, _pPoz, _pOpis, _pPdf,
                        _pUwagi, _pTekstAktu, _pOrganZob, _pTekstUjednolicony
                        }
  where rowValue header = getChildren
                            >>> hasName "tr"
                            >>> filterA (deep (hasName "th" >>> deep (hasText (==header)))) /> hasName "td"
        rowValueText ::  String -> IOSArrow XmlTree String
        rowValueText header = rowValue header >>> allText

allText :: ArrowXml a => a XmlTree String
allText = deep isText >>> getText >. mconcat
