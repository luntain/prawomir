module Model where

import MyPrelude
import Data.Time
import qualified Data.Text as T
import Control.Lens

data PozId =
  PozId {_pidYear, _pidNr, _pidSeq :: Int}
  deriving (Show, Read)

data DUPozycja =
  DUPozycja {
      _dupId :: PozId
    --, _dupTitle :: T.Text in ustawa
    --, _dupZDnia :: Day already in Ustawa
    --, _dupWSprawie :: T.Text
    , _dupAkt :: Akt} deriving (Show, Read)

data Akt =
  Ustawa {_uzDnia :: Day, _uTytul :: T.Text, _utresc :: Div } deriving (Show, Read)

data Div = -- [Dzial, [rozdzial]]
  Div String String [Div]
  | Articles [Art]
  deriving (Show, Read)

data DivType =
  Dzial | Rozdzial | Artykul | Ustep | Punkt
  deriving (Show, Read)

data Node = Div {_dtype:: DivType, _dtitle:: T.Text, _dsubtitle:: Maybe T.Text }
 deriving (Show, Read)

data Art = Art String [Content] [Ustep] deriving (Show, Read)
data Ustep = Ustep String [Content] [Punkt] deriving (Show, Read)
data Punkt = Punk String [Content] deriving (Show, Read)

data Content =
  Text T.Text
  -- | Table
  deriving (Show, Read)

makeLenses ''DUPozycja
makeLenses ''Akt
