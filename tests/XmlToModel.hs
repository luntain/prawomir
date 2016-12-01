{-# LANGUAGE OverloadedStrings #-}
import MyPrelude
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Map as M
import Data.Time
import qualified Data.Text as T
import Model
import Parse
import Text.Nicify
import Data.Algorithm.DiffOutput (ppDiff)
import Data.Algorithm.Diff (getGroupedDiff)


main = defaultMain tests

tests =
  testGroup "HUnit tests"
    [ testCase "parse pit1.xml" $ do
      ustawa <- parseUstawa "tests/pit1.xml"
      diffAssertEqual expectedUstawa ustawa
    ]

expectedUstawa :: Akt
expectedUstawa =
  Ustawa {
    _upId = PozId { _pidYear = 1991, _pidNr = 80, _pidSeq = 350 }
    , _uzDnia = fromGregorian 1991 7 26
    , _uTytul = "o podatku dochodowym od osób fizycznych"
    , _uspisTresci =
      Partitions "Rozdział" [("1", "Podmiot i przedmiot opodatkowania")] (M.fromList [("1", Articles ["1", "2"])])
    , _uarticles =
      M.fromList [("1", mkLeaf "Ustawa reguluje opodatkowanie podatkiem dochodowym dochodow osob fizycznych.")
                 ,("2", mkPoint "" [
                    ("1", mkPoint "Przepisow ustawy nie stosuje się do:" [
                      ("1", mkLeaf "Przychodów z działalności rolniczej, z wyjątkiem przychodow z działów specjalnej produkcji rolnej;")
                     ,("2", mkLeaf "przychodów z gospodarki leśnej w rozumieniu ustawy o lasach.")])
                   ,("2", mkLeaf "Działalnością rolniczą, w rozumieniu ust. 1 pkt 1, jest działalność polegająca na wytwarzaniu produktów roślinnych lub zwierzęcych w stanie nieprzetworzonym.")])]

         }



-- sa jeszcze wyliczenia w wyliczeniach, stroana 18 w pit, art 10 ust 1 pkt 8 lit. b???

class Point a where
  mkLeaf :: String -> a
  mkPoint :: String -> [(String, ZWyliczeniem)] -> a

instance Point Article where
  mkLeaf text = Article (mkLeaf text) [] M.empty
  mkPoint text children = Article (mkLeaf text) (map fst children) (M.fromList children)

instance Point ZWyliczeniem where
  mkLeaf text = ZWyliczeniem [Text $ T.pack text] [] M.empty []
  mkPoint text children = ZWyliczeniem [Text $ T.pack text] (map fst children) (M.fromList children) []


diffAssertEqual :: (Show a, Eq a) => a -> a -> Assertion
diffAssertEqual expected actual =
  assertEqual (ppDiff (getGroupedDiff (lines . nicify . show $ expected) (lines . nicify . show $ actual))) expected actual
