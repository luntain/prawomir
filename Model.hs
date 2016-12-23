module Model where

import MyPrelude
import Data.Time
import qualified Data.Text as T
import qualified Data.Map as M

data PozId =
  PozId {_pidYear, _pidNr, _pidSeq :: Int}
  deriving (Show, Read, Eq)

-- data DUPozycja =
--   DUPozycja {
--       _dupId :: PozId
--     --, _dupTitle :: T.Text in ustawa
--     --, _dupZDnia :: Day already in Ustawa
--     --, _dupWSprawie :: T.Text
--     , _dupAkt :: Akt} deriving (Show, Read)

data Akt =
  Ustawa { _upId :: PozId
         , _uzDnia :: Day
         , _uTytul :: String
         , _uspisTresci :: TableOfContents
         , _uarticles :: M.Map String Article
         } deriving (Show, Read, Eq)

-- data DivType =
--   Dzial | Rozdzial | Artykul | Ustep | Punkt
--   deriving (Show, Read)
-- hdrPriority divTyp =
--   case divtyp of
--     Dzial -> 1
--     Rozdzial -> 2
--     Artykul -> 3
--     Ustep -> 4
--     Punkt -> 5


data TableOfContents = -- [Dzial, [rozdzial]]
  Partitions String [(String, String)] (M.Map String TableOfContents) -- partition has a number and a title (perhaps no title, but not sure)
  | Articles [String]
  deriving (Show, Read, Eq)

type TextWithReferences = [TextOrReference]
data TextOrReference = Text T.Text | Table Table -- just test for now
  deriving (Show, Read, Eq)

data Article =
  Article { _aprefix :: ZWyliczeniem
          , _aindex :: [String]
          , _apoints :: M.Map String ZWyliczeniem -- ustepy
          }
  deriving (Show, Read, Eq)

-- reprezents ustep, punkt, i nawet podpunkt (ten trzeci bez dzieci)
data ZWyliczeniem =
  ZWyliczeniem { _zwprefix :: TextWithReferences
               , _zwindex :: [String]
               , _zwpoints :: M.Map String ZWyliczeniem
               , _zsuffix :: TextWithReferences
               }
  deriving (Show, Read, Eq)

emptyZWyliczeniem = ZWyliczeniem [] [] M.empty []

type Table = [[TableCell]]
data TableCell =
  TableCell {
    _tcwidth, _tcheight :: Float, _tccolSpan, _tcrowSpan :: Int, _tctext :: TextWithReferences
   , _tcborderTop, _tcborderLeft, _tcborderRight, _tcborderBottom :: Bool
  } deriving (Show, Read, Eq)
