module Model where

import MyPrelude
import Control.Lens hiding (elements)
import Data.Time
import qualified Data.Text as T
import Test.Tasty.QuickCheck

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

-- crazy: this generator is more complicated than the code it is intended to test
-- it generates various rowspan colspan configurations
arbitraryTable :: Int -> Int -> Gen [[TableCell]]
arbitraryTable rows cols
  | rows + cols <= 5 =
      elements [singleCellSpanningWholeTable, cellsOfSize1]
      where singleCellSpanningWholeTable = [[tc rows cols]] ++ replicate (rows - 1) []
            cellsOfSize1 = replicate rows (replicate cols (tc 1 1))
arbitraryTable rows cols =
  frequency -- `choose (1, 0)` returns `1`, so I set freq params to prevent such branch
    [(rows-1, do rows1 <- choose (1, (rows-1))
                 (++) <$> arbitraryTable rows1 cols <*> arbitraryTable (rows - rows1) cols)
    ,(cols-1, do cols1 <- choose (1, (cols-1))
                 zipWith (++) <$> arbitraryTable rows cols1 <*> arbitraryTable rows (cols - cols1))]
tc :: Int -> Int -> TableCell
tc rowSpan colSpan =
  TableCell (fromIntegral colSpan) (fromIntegral rowSpan) colSpan rowSpan [] False False False False


fullyBordered tc = tc {_tcborderTop = True, _tcborderBottom = True, _tcborderLeft = True, _tcborderRight = True}

makeLenses ''TableCell
makePrisms ''TextOrReference
