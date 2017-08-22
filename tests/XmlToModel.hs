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
    , _uroot =
      -- this is a Frankenstein of fragments from "Ustawa o podatku doch. od osob fizycznych"
      Node Root [
        Node (Header Rozdział "1" "Podmiot i przedmiot opodatkowania") [
          Node (Addressable "1" []) [
            Node (Addressable "" [Text "Ustawa reguluje opodatkowanie podatkiem dochodowym dochodów osób fizycznych."]) []]
          , Node (Addressable "2" []) [
            Node (Addressable "1" [Text "Przepisów ustawy nie stosuje się do:"]) [
              Node (Addressable "1" [Text "przychodów z działalności rolniczej, z wyjątkiem przychodów\
                                          \ z działów specjalnych produkcji rolnej;"]) []
              , Node (Addressable "2" [Text "przychodów z gospodarki leśnej w rozumieniu ustawy o lasach."]) []
              , Node (Addressable "3" [Text "odpłatne zbycie, z zastrzeżeniem ust. 2:"]) [
                Node (Addressable "a" [Text "nieruchomości lub ich części oraz udziału w nieruchomości,"]) []
                , Node (Addressable "b" [Text "spółdzielczego własnościowego prawa do lokalu mieszkalnego lub,"]) []
                , Node (Addressable "-" [Text "– jeżeli odpłatne zbycie nie następuje w wykonaniu działalności\
                                              \ gospodarczej"]) []]
              , Node (Addressable "-" [Text "– w ramach stosunku pracy lub spółdzielczego stosunku pracy,\
                                          \ podatnik ten traci w"]) []]
            , Node (Addressable "2" [Text "Działalnością rolniczą, w rozumieniu ust. 1 pkt 1, jest działalność\
                                          \ polegająca na wytwarzaniu produktów roślinnych lub zwierzęcych w\
                                          \ stanie nieprzetworzonym."]) [
              Node (Addressable "19a" [Text "samochodzie osobowym – oznacza to pojazd samochodowy w rozumieniu\
                                          \ przepisów o ruchu drogowym o dopuszczalnej masie całkowitej\
                                          \ nieprzekraczającej 3,5 tony, konstrukcyjnie przeznaczony do przewozu\
                                          \ nie więcej niż 9 osób łącznie z kierowcą, z wyjątkiem:"]) [
                Node (Addressable "a" [Text "pojazdu samochodowego mającego jeden rząd siedzeń, który oddzielony\
                                          \ jest od części przeznaczonej do przewozu ładunków ścianą lub trwałą\
                                          \ przegrodą:"]) [
                  Node (Addressable "-" [Text "klasyfikowanego na podstawie przepisów o ruchu drogowym do\
                                          \ podrodzaju: wielozadaniowy, van lub"]) []
                  , Node (Addressable "-" [Text "z otwartą częścią przeznaczoną do przewozu ładunków."]) []]]]]]

          , Node (Header Rozdział "1a" "Opodatkowanie przychodów nieznajdujących pokrycia w ujawnionych\
                                     \ źródłach lub pochodzących ze źródeł nieujawnionych") [
            Node (Addressable "25b" []) [
            Node (Addressable "1" [Text "Za przychody, o których mowa w art. 20 ust. 1b, uważa się przychody:"]) [
              Node (Addressable "1" [Text "nieznajdujące pokrycia w ujawnionych źródłach obejmujące przychody\
                                          \ ze źródeł wskazanych przez podatnika, ujawnione w nieprawidłowej\
                                          \ wysokości,"]) []
              , Node (Addressable "2" [Text "ze źródeł nieujawnionych obejmujące przychody ze źródeł\
                                          \ niewskazanych przez podatnika i nieustalonych przez organ podatkowy\
                                          \ lub organ kontroli skarbowej"]) []
              , Node (Addressable "-" [Text "– w kwocie odpowiadającej nadwyżce wydatku nad przychodami (dochodami)\
                                            \ opodatkowanymi lub przychodami (dochodami) nieopodatkowanymi, uzyskanymi\
                                            \ przed poniesieniem tego wydatku."]) []]
            , Node (Addressable "2" [Text "Za wydatek uznaje się wartość zgromadzonego w roku podatkowym mienia lub\
                                          \ wysokość wydatkowanych w roku podatkowym środków, w przypadku gdy nie jest\
                                          \ możliwe ustalenie roku podatkowego, w którym zgromadzono te środki."]) []]
          , Node (Addressable "27" []) [
            Node (Addressable "1" [Text "Podatek dochodowy, z zastrzeżeniem art. 29–30f, pobiera się od podstawy jego\
                                        \ obliczenia według następującej skali:", Table podstawaTable]) []
            , Node (Addressable "2" [Text "Jeżeli u podatników, którzy osiągają wyłącznie przychody z tytułu emerytur."]) []]]]
    , _uannexes = []
    }

expectedUstawa2 :: Akt
expectedUstawa2 =
  Ustawa {
    _upId = PozId { _pidYear = 1991, _pidNr = 80, _pidSeq = 350 }
    , _uzDnia = fromGregorian 1991 7 26
    , _uTytul = "o podatku dochodowym od osób fizycznych"
    , _uroot = Node Root [
      Node (Header Rozdział "1" "Podmiot i przedmiot opodatkowania") [
      -- this is a Frankenstein of fragments from "Ustawa o podatku doch. od osob fizycznych"
        Node (Addressable "22a" []) [
          Node (Addressable "2" [Text "Amortyzacji podlegają również:"]) [
            Node (Addressable "1" [Text "przyjęte do używania inwestycje,"]) []
            , Node (Addressable "2" [Text "budynki i budowle wybudowane na cudzym gruncie,"]) []
            , Node (Addressable "3" [Text "składniki majątku,"]) []
            , Node (Addressable "-" [Text "– zwane także środkami trwałymi;"]) []
            , Node (Addressable "4" [Text "tabor transportu morskiego."]) []]]
        , Node (Addressable "52" []) [
          Node (Addressable "" [Text "Zwalnia się od podatku dochodowego:"]) [
            Node (Addressable "1" [Text "w okresie od dnia 1 stycznia 2001 r. do dnia 31 grudnia 2003 r. dochody:"]) [
              Node (Addressable "a" [Text "z odpłatnego zbycia nabytych przed dniem 1 stycznia 2003 r. obligacji,"]) []
              , Node (Addressable "b" [Text "z odpłatnego zbycia papierów,"]) []
              , Node (Addressable "c" [Text "(uchylona)"]) []
              , Node (Addressable "-" [Text "– przy czym zwolnienie nie ma zastosowania, jeżeli sprzedaż tych papierów wartościowych jest przedmiotem działalności gospodarczej,"]) []
              , Node (Addressable "d" [Text "uzyskane z realizacji praw wynikających z papierów wartościowych."]) []]]]]]

    , _uannexes = []
    }

podstawaTable = unsafePerformIO (read <$> readFile "tests/podstawaObliczeniaPodatku.table")

diffAssertEqual :: (Show a, Eq a) => a -> a -> Assertion
diffAssertEqual expected actual =
  unless (expected == actual) $ do
    let expected' = pretty expected
        actual'   = pretty actual
        diff = ppDiff (getGroupedDiff (lines expected') (lines actual'))
    -- take first 60 chars from the first char that differs
    let firstDiff = unzip . take 60 . dropWhile (uncurry (==)) . zip actual' $ expected'
    assertFailure (printf "expected:\n%s\nactual:\n%s\ndiff:%s\nFirst diff:\nact: %s\nexp: %s\n"
                            expected' actual' diff (fst firstDiff) (snd firstDiff))
  where
    pretty = nicify . ushow

-- TODO:
-- annex test
-- bez podzialu na rozdzialy
-- recognize links (relative and absoulte)
