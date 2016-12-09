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
      Partitions "Rozdział" [("1", "Podmiot i przedmiot opodatkowania")
                            ,("1a", "Opodatkowanie przychodów nieznajdujących pokrycia w ujawnionych\
                                    \ źródłach lub pochodzących ze źródeł nieujawnionych")]
         (M.fromList [("1", Articles ["1", "2"]), ("1a", Articles ["25b"])])
    , _uarticles =
      M.fromList [("1", mkLeaf "Ustawa reguluje opodatkowanie podatkiem dochodowym dochodów osób fizycznych.")
                 ,("2", mkPoint "" [
                    ("1", mkPoint "Przepisów ustawy nie stosuje się do:" [
                      ("1", mkLeaf "przychodów z działalności rolniczej, z wyjątkiem przychodów z \
                                   \działów specjalnych produkcji rolnej;")
                     ,("2", mkLeaf "przychodów z gospodarki leśnej w rozumieniu ustawy o lasach.")
                     ,("3", mkPoint "odpłatne zbycie, z zastrzeżeniem ust. 2:" [
                          ("a", mkLeaf "nieruchomości lub ich części oraz udziału w nieruchomości,")
                          , ("b", mkLeaf "spółdzielczego własnościowego prawa do lokalu mieszkalnego lub,")
                          ] [Text "– jeżeli odpłatne zbycie nie następuje w wykonaniu działalności gospodarczej"])
                     ] [Text "– w ramach stosunku pracy lub spółdzielczego stosunku pracy, podatnik ten traci w"])
                   ,("2", mkLeaf "Działalnością rolniczą, w rozumieniu ust. 1 pkt 1, jest działalność polegająca \
                                 \na wytwarzaniu produktów roślinnych lub zwierzęcych w stanie nieprzetworzonym.")] [])
                 ,("25b", mkPoint "" [
                    ("1", mkPoint "Za przychody, o których mowa w art. 20 ust. 1b, uważa się przychody:" [
                       ("1", mkLeaf "nieznajdujące pokrycia w ujawnionych źródłach obejmujące przychody ze\
                                    \ źródeł wskazanych przez podatnika, ujawnione w nieprawidłowej wysokości,")
                      ,("2", mkLeaf "ze źródeł nieujawnionych obejmujące przychody ze źródeł niewskazanych przez\
                                    \ podatnika i nieustalonych przez organ podatkowy lub organ kontroli skarbowej")]
                       [Text "– w kwocie odpowiadającej nadwyżce wydatku nad przychodami (dochodami) opodatkowanymi lub\
                             \ przychodami (dochodami) nieopodatkowanymi, uzyskanymi przed poniesieniem tego wydatku."])
                    ,("2", mkLeaf "Za wydatek uznaje się wartość zgromadzonego w roku podatkowym mienia lub wysokość \
                                  \wydatkowanych w roku podatkowym środków, w przypadku gdy nie jest możliwe ustalenie \
                                  \roku podatkowego, w którym zgromadzono te środki.")] [])]
         }

class Point a where
  mkLeaf :: String -> a
  mkPoint :: String -> [(String, ZWyliczeniem)] -> TextWithReferences -> a

instance Point Article where
  mkLeaf text = Article (mkLeaf text) [] M.empty
  mkPoint text children _suffix = Article (mkLeaf text) (map fst children) (M.fromList children)

instance Point ZWyliczeniem where
  mkLeaf text = ZWyliczeniem [Text $ T.pack text] [] M.empty []
  mkPoint text children suffix = ZWyliczeniem [Text $ T.pack text] (map fst children) (M.fromList children) suffix


diffAssertEqual :: (Show a, Eq a) => a -> a -> Assertion
diffAssertEqual expected actual =
  if expected == actual
    then return ()
    else do
      let expected' = nice expected
          actual'   = nice actual
          diff = ppDiff (getGroupedDiff (lines expected') (lines actual'))
      assertFailure (printf "expected:\n%s\nactual:\n%s\ndiff:%s\n" expected' actual' diff)
  where
    nice = nicify . show

-- TODO:
-- changes (additions deletions)
-- zalacznik
-- tabelki
-- bez podzialu na rozdzialy
-- rozdzialy i inne dzialy
-- dodany rozdzial s146
-- przypisy s 159 art 26d
-- tabelk w tekscie, art 27 s 159
-- recognize links (relative and absoulte)
