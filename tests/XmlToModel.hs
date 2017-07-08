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
import System.IO.Unsafe (unsafePerformIO)
import Text.Show.Unicode


main = defaultMain (testGroup "all" [tableTests, tests])

tests =
  testGroup "HUnit tests"
    [ testCase "Parse pit1.xml" $ do
      ustawa <- parseUstawa "tests/pit1.xml" ["tests/pit1-157.vec", "tests/pit1-158.vec", "tests/pit1-159.vec"]
      diffAssertEqual expectedUstawa ustawa
    , testCase "Część wspólna w ciągu punktów (nie na końcu, wbrew zasadom techniki prawodawczej)" $ do
      ustawa <- parseUstawa "tests/pit2.xml" []
      diffAssertEqual expectedUstawa2 ustawa
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
         ([("1", Articles ["1", "2"]), ("1a", Articles ["25b", "27"])])
    , _uarticles =
      -- this is a Frankenstein of fragments from "Ustawa o podatku doch. od osob fizycznych"
      [("1", mkPoint "" [
         ("", mkLeaf "Ustawa reguluje opodatkowanie podatkiem dochodowym dochodów osób fizycznych.")])
      ,("2", mkPoint "" [
         ("1", mkPoint "Przepisów ustawy nie stosuje się do:" [
           ("1", mkLeaf "przychodów z działalności rolniczej, z wyjątkiem przychodów z \
                        \działów specjalnych produkcji rolnej;")
          ,("2", mkLeaf "przychodów z gospodarki leśnej w rozumieniu ustawy o lasach.")
          ,("3", mkPoint "odpłatne zbycie, z zastrzeżeniem ust. 2:" [
              ("a", mkLeaf "nieruchomości lub ich części oraz udziału w nieruchomości,")
            , ("b", mkLeaf "spółdzielczego własnościowego prawa do lokalu mieszkalnego lub,")
            , ("-", mkLeaf  "– jeżeli odpłatne zbycie nie następuje w wykonaniu działalności gospodarczej")
              ])
          ,("-", mkLeaf "– w ramach stosunku pracy lub spółdzielczego stosunku pracy, podatnik ten traci w")])
        ,("2", mkPoint "Działalnością rolniczą, w rozumieniu ust. 1 pkt 1, jest działalność polegająca \
                      \na wytwarzaniu produktów roślinnych lub zwierzęcych w stanie nieprzetworzonym." [
          -- tirety
           ("19a", mkPoint "samochodzie osobowym – oznacza to pojazd samochodowy w rozumieniu\
                        \ przepisów o ruchu drogowym o dopuszczalnej masie całkowitej\
                        \ nieprzekraczającej 3,5 tony, konstrukcyjnie przeznaczony do przewozu nie\
                        \ więcej niż 9 osób łącznie z kierowcą, z wyjątkiem:" [
              ("a", mkPoint "pojazdu samochodowego mającego jeden rząd siedzeń, który oddzielony jest\
                          \ od części przeznaczonej do przewozu ładunków ścianą lub trwałą przegrodą:" [
                  ("-", mkLeaf "klasyfikowanego na podstawie przepisów o ruchu drogowym do podrodzaju:\
                        \ wielozadaniowy, van lub")
                , ("-", mkLeaf "z otwartą częścią przeznaczoną do przewozu ładunków.")])])])])
      ,("25b", mkPoint "" [
         ("1", mkPoint "Za przychody, o których mowa w art. 20 ust. 1b, uważa się przychody:" [
           ("1", mkLeaf "nieznajdujące pokrycia w ujawnionych źródłach obejmujące przychody ze\
                        \ źródeł wskazanych przez podatnika, ujawnione w nieprawidłowej wysokości,")
          ,("2", mkLeaf "ze źródeł nieujawnionych obejmujące przychody ze źródeł niewskazanych przez\
                        \ podatnika i nieustalonych przez organ podatkowy lub organ kontroli skarbowej")
          ,("-", mkLeaf "– w kwocie odpowiadającej nadwyżce wydatku nad przychodami (dochodami) opodatkowanymi lub\
                  \ przychodami (dochodami) nieopodatkowanymi, uzyskanymi przed poniesieniem tego wydatku.")])
        ,("2", mkLeaf "Za wydatek uznaje się wartość zgromadzonego w roku podatkowym mienia lub wysokość \
                      \wydatkowanych w roku podatkowym środków, w przypadku gdy nie jest możliwe ustalenie \
                      \roku podatkowego, w którym zgromadzono te środki.")])
      ,("27", mkPoint "" [
         ("1", ZWyliczeniem [Text "Podatek dochodowy, z zastrzeżeniem art. 29–30f, pobiera się od\
                                    \ podstawy jego obliczenia według następującej skali:", Table podstawaTable][])
       , ("2", mkLeaf "Jeżeli u podatników, którzy osiągają wyłącznie przychody z tytułu emerytur.")])]
    , _uannexes = []
    }

expectedUstawa2 :: Akt
expectedUstawa2 =
  Ustawa {
    _upId = PozId { _pidYear = 1991, _pidNr = 80, _pidSeq = 350 }
    , _uzDnia = fromGregorian 1991 7 26
    , _uTytul = "o podatku dochodowym od osób fizycznych"
    , _uspisTresci =
      Partitions "Rozdział" [("1", "Podmiot i przedmiot opodatkowania")]
         ([("1", Articles ["22a", "52"])])
    , _uarticles =
      -- this is a Frankenstein of fragments from "Ustawa o podatku doch. od osob fizycznych"
      [("22a", mkPoint "" [
         ("2", mkPoint "Amortyzacji podlegają również:" [
           ("1", mkLeaf "przyjęte do używania inwestycje,")
          ,("2", mkLeaf "budynki i budowle wybudowane na cudzym gruncie,")
          ,("3", mkLeaf "składniki majątku,")
          ,("-", mkLeaf "– zwane także środkami trwałymi;")
          ,("4", mkLeaf "tabor transportu morskiego.")])])
      ,("52", mkPoint "" [
         ("", mkPoint "Zwalnia się od podatku dochodowego:" [
           ("1", mkPoint "w okresie od dnia 1 stycznia 2001 r. do dnia 31 grudnia 2003 r. dochody:" [
             ("a", mkLeaf "z odpłatnego zbycia nabytych przed dniem 1 stycznia 2003 r. obligacji,")
            ,("b", mkLeaf "z odpłatnego zbycia papierów,")
            ,("c", mkLeaf "(uchylona)")
            ,("-", mkLeaf "– przy czym zwolnienie nie ma zastosowania, jeżeli sprzedaż tych\
                 \ papierów wartościowych jest przedmiotem działalności gospodarczej,")
            ,("d", mkLeaf "uzyskane z realizacji praw wynikających z papierów wartościowych.")])])])]
    , _uannexes = []
    }

podstawaTable = unsafePerformIO (read <$> readFile "tests/podstawaObliczeniaPodatku.table")

class Point a where
  mkLeaf :: T.Text -> a
  mkPoint :: T.Text -> [(T.Text, ZWyliczeniem)] -> a

instance Point ZWyliczeniem where
  mkLeaf text = ZWyliczeniem (if T.null text then [] else [Text text]) []
  mkPoint text children =
     ZWyliczeniem (if T.null text then [] else [Text text]) children


diffAssertEqual :: (Show a, Eq a) => a -> a -> Assertion
diffAssertEqual expected actual =
  if expected == actual
    then return ()
    else do
      let expected' = nice expected
          actual'   = nice actual
          diff = ppDiff (getGroupedDiff (lines expected') (lines actual'))
      -- take first 60 chars from the first char that differes
      let firstDiff = unzip . take 60 . dropWhile (uncurry (==)) . zip actual' $ expected'
      assertFailure (printf "expected:\n%s\nactual:\n%s\ndiff:%s\nFirst diff:\nact: %s\nexp: %s\n"
                             expected' actual' diff (fst firstDiff) (snd firstDiff))
  where
    nice = nicify . ushow

-- TODO:
-- annex test
-- bez podzialu na rozdzialy
-- rozdzialy i inne dzialy
-- przypisy s 159 art 26d
-- recognize links (relative and absoulte)
