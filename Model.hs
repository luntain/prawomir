module Model where

import MyPrelude
import Control.Lens hiding (elements)
import Data.Time
import qualified Data.Text as T

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
         , _uTytul :: T.Text
         , _uspisTresci :: TableOfContents
         , _uarticles :: [(T.Text, ZWyliczeniem)]
         , _uannexes :: [Annex]
         } deriving (Show, Read, Eq)

type PartNumber = T.Text
type PartTitle = T.Text

data TableOfContents =
  Partitions T.Text [(PartNumber, PartTitle)] [(PartNumber, TableOfContents)] -- partition has a number and a title (perhaps no title, but not sure)
  | Articles [PartNumber]
  deriving (Show, Read, Eq)

type TextWithReferences = [TextOrReference]
data TextOrReference =
  Text T.Text | Table [[TableCell]]
  deriving (Show, Read, Eq, Ord)

data Annex =
  Annex { _anum :: T.Text, _astartPage, _apages :: Int
        }
  deriving (Show, Read, Eq)

-- reprezentuje artykuł, ustęp, punkt, literę z opcjonalnymi tiretami
data ZWyliczeniem =
  ZWyliczeniem { _zwprefix :: TextWithReferences
               , _zwpoints :: [(T.Text, ZWyliczeniem)]
               }
  deriving (Show, Read, Eq)

emptyZWyliczeniem = ZWyliczeniem [] []

--type Table = [[TableCell]]

data TableCell =
  TableCell {
    _tcwidth, _tcheight :: Float, _tccolSpan, _tcrowSpan :: Int, _tctext :: TextWithReferences
   , _tcborderTop, _tcborderLeft, _tcborderRight, _tcborderBottom :: Bool
  } deriving (Show, Read, Eq, Ord)

fullyBordered tc = tc {_tcborderTop = True, _tcborderBottom = True, _tcborderLeft = True, _tcborderRight = True}

makeLenses ''TableCell
makePrisms ''TextOrReference
