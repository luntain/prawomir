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
         , _uroot :: Node
         , _uannexes :: [Annex]
         } deriving (Show, Read, Eq)

type Content = [TextOrReference]
data TextOrReference =
  Text T.Text | Table [[TableCell]]
  deriving (Show, Read, Eq, Ord)

data Annex =
  Annex { _anum :: T.Text, _astartPage, _apages :: Int
        }
  deriving (Show, Read, Eq)

data Node =
  Node { _nvalue :: Value, _nchildren :: [Node] }
  deriving (Show, Read, Eq)

data Value =
  Addressable { _vordinal :: T.Text, _vcontent :: Content }
  -- ^ artykul, ustep, punkt, litera ...
  | CommonPart { _vcontent :: Content }
  | Header { _vheaderLevel :: HeaderLevel, _vheaderOrdinal :: T.Text, _vheaderText :: T.Text }
  | Root
  deriving (Show, Read, Eq)

data HeaderLevel =
  Rozdział
  | Oddział -- may appear under `Rozdział`, can have articles on the same level
  deriving (Show, Read, Eq)

--type Table = [[TableCell]]

data TableCell =
  TableCell {
    _tcwidth, _tcheight :: Float, _tccolSpan, _tcrowSpan :: Int, _tctext :: Content
   , _tcborderTop, _tcborderLeft, _tcborderRight, _tcborderBottom :: Bool
  } deriving (Show, Read, Eq, Ord)

fullyBordered tc = tc {_tcborderTop = True, _tcborderBottom = True, _tcborderLeft = True, _tcborderRight = True}

makeLenses ''TableCell
makePrisms ''TextOrReference
