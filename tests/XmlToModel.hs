{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
import MyPrelude
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Map as M
import Data.Time
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Model
import Parse
import Text.Nicify
import System.IO
import Data.Algorithm.DiffOutput (ppDiff)
import Data.Algorithm.Diff (getGroupedDiff)
import System.IO.Unsafe (unsafePerformIO)
import Text.Show.Unicode
import NeatInterpolation


main = defaultMain (testGroup "all" [tableTests, tests])

tests =
  testGroup "HUnit tests"
    [ testCase "Parse pit1.xml" $ do
      ustawa <- parseUstawa "tests/pit1.xml" ["tests/pit1-157.vec", "tests/pit1-158.vec", "tests/pit1-159.vec"]
      diffAssertEqual expectedUstawa ustawa
    , testCase "Część wspólna w ciągu punktów (nie na końcu, wbrew zasadom techniki prawodawczej)" $ do
      testParsingArticles
        [Node (Addressable "22a" []) [
          Node (Addressable "2" [Text "Amortyzacji podlegają również:"]) [
            Node (Addressable "1" [Text "przyjęte do używania inwestycje,"]) []
            , Node (Addressable "2" [Text "budynki i budowle wybudowane na cudzym gruncie,"]) []
            , Node (Addressable "3" [Text "składniki majątku,"]) []
            , Node (Addressable "-" [Text "– zwane także środkami trwałymi;"]) []
            , Node (Addressable "4" [Text "tabor transportu morskiego."]) []]]]
        [text|
        <TEXT width="385.488" height="11.88" x="82.2" y="123.756">
          <TOKEN bold="yes" italic="no" font-size="12" x="82.2" y="127.644" width="20.88" height="10.8">Art.</TOKEN>
          <TOKEN bold="yes" italic="no" font-size="12" x="110.04" y="127.644" width="21" height="10.8">22a.</TOKEN>
        </TEXT>
        <TEXT width="385.32" height="11.88" x="82.2" y="330.876">
          <TOKEN bold="no" italic="no" font-size="12" x="82.2" y="334.764" width="9" height="10.8">2.</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="97.08" y="334.764" width="59.856" height="10.8">Amortyzacji</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="162.84" y="330.876" width="46.728" height="15.768">podlegają</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="215.4" y="330.876" width="41.64" height="15.768">również:</TOKEN>
        </TEXT>
        <TEXT width="410.976" height="11.88" x="56.64" y="372.276">
          <TOKEN bold="no" italic="no" font-size="12" x="56.64" y="376.164" width="9.996" height="10.8">1)</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="82.2" y="372.276" width="38.568" height="15.768">przyjęte</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="126.48" y="376.164" width="12" height="10.8">do</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="144.24" y="372.276" width="46.128" height="15.768">używania</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="196.08" y="376.164" width="51.288" height="10.8">inwestycje,</TOKEN>
        </TEXT>
        <TEXT width="279.72" height="10.8" x="56.64" y="417.564">
          <TOKEN bold="no" italic="no" font-size="12" x="56.64" y="417.564" width="9.996" height="10.8">2)</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="82.2" y="417.564" width="39.336" height="10.8">budynki</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="124.56" y="417.564" width="3.336" height="10.8">i</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="130.92" y="417.564" width="41.328" height="10.8">budowle</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="175.2" y="417.564" width="63.888" height="10.8">wybudowane</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="242.04" y="417.564" width="11.328" height="10.8">na</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="256.44" y="417.564" width="37.896" height="10.8">cudzym</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="297.36" y="417.564" width="39" height="10.8">gruncie,</TOKEN>
        </TEXT>
        <TEXT width="411.12" height="11.88" x="56.64" y="434.436">
          <TOKEN bold="no" italic="no" font-size="12" x="56.64" y="438.324" width="9.996" height="10.8">3)</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="82.2" y="434.436" width="44.016" height="15.768">składniki</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="135.6" y="434.436" width="41.64" height="15.768">majątku,</TOKEN>
        </TEXT><TEXT width="164.856" height="11.88" x="56.64" y="558.636">
          <TOKEN bold="no" italic="no" font-size="12" x="56.64" y="562.524" width="6" height="10.8">–</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="65.16" y="562.524" width="30.528" height="10.8">zwane</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="98.64" y="558.636" width="25.368" height="15.768">także</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="126.96" y="558.636" width="44.736" height="15.768">środkami</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="174.72" y="558.636" width="46.776" height="15.768">trwałymi;</TOKEN>
        </TEXT>
        <TEXT width="294.12" height="10.8" x="56.64" y="583.164">
          <TOKEN bold="no" italic="no" font-size="12" x="56.64" y="583.164" width="9.996" height="10.8">4)</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="82.2" y="583.164" width="24.636" height="10.8">tabor</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="109.8" y="583.164" width="48.6" height="10.8">transportu</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="161.4" y="583.164" width="50.76" height="10.8">morskiego.</TOKEN>
        </TEXT>|]
    , testCase "Część wspólna w ciągu liter (nie na końcu, wbrew zasadom techniki prawodawczej)" $ do
      testParsingArticles
        [Node (Addressable "52" []) [
          Node (Addressable "" [Text "Zwalnia się od podatku dochodowego:"]) [
            Node (Addressable "1" [Text "w okresie od dnia 1 stycznia 2001 r. do dnia 31 grudnia 2003 r. dochody:"]) [
              Node (Addressable "a" [Text "z odpłatnego zbycia nabytych przed dniem 1 stycznia 2003 r. obligacji,"]) []
              , Node (Addressable "b" [Text "z odpłatnego zbycia papierów,"]) []
              , Node (Addressable "c" [Text "(uchylona)"]) []
              , Node (Addressable "-" [Text "– przy czym zwolnienie nie ma zastosowania, jeżeli sprzedaż tych papierów wartościowych jest przedmiotem działalności gospodarczej,"]) []
              , Node (Addressable "d" [Text "uzyskane z realizacji praw wynikających z papierów wartościowych."]) []]]]]
        [text|
        <TEXT width="227.856" height="11.88" x="82.2" y="278.076">
          <TOKEN bold="yes" italic="no" font-size="12" x="82.2" y="281.964" width="20.88" height="10.8">Art.</TOKEN>
          <TOKEN bold="yes" italic="no" font-size="12" x="106.08" y="281.964" width="15" height="10.8">52.</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="124.08" y="281.964" width="39.408" height="10.8">Zwalnia</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="166.44" y="278.076" width="13.368" height="15.768">się</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="182.76" y="281.964" width="12" height="10.8">od</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="197.76" y="281.964" width="38.76" height="10.8">podatku</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="239.52" y="281.964" width="70.536" height="10.8">dochodowego:</TOKEN>
        </TEXT>
        <TEXT width="376.776" height="10.8" x="56.64" y="302.844">
          <TOKEN bold="no" italic="no" font-size="12" x="56.64" y="302.844" width="9.996" height="10.8">1)</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="82.2" y="302.844" width="8.664" height="10.8">w</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="93.84" y="302.844" width="34.608" height="10.8">okresie</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="131.4" y="302.844" width="12" height="10.8">od</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="146.4" y="302.844" width="20.688" height="10.8">dnia</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="170.04" y="302.844" width="6" height="10.8">1</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="179.04" y="302.844" width="39.528" height="10.8">stycznia</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="221.52" y="302.844" width="24" height="10.8">2001</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="248.52" y="302.844" width="6.96" height="10.8">r.</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="258.48" y="302.844" width="12" height="10.8">do</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="273.48" y="302.844" width="20.688" height="10.8">dnia</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="297.12" y="302.844" width="12" height="10.8">31</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="312.12" y="302.844" width="36.768" height="10.8">grudnia</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="351.84" y="302.844" width="24" height="10.8">2003</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="378.84" y="302.844" width="6.96" height="10.8">r.</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="388.8" y="302.844" width="44.616" height="10.8">dochody:</TOKEN>
        </TEXT>
        <TEXT width="385.416" height="11.88" x="82.2" y="319.716">
          <TOKEN bold="no" italic="no" font-size="12" x="82.2" y="323.604" width="9.276" height="10.8">a)</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="105.96" y="323.604" width="5.328" height="10.8">z</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="116.52" y="319.716" width="53.28" height="15.768">odpłatnego</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="175.08" y="323.604" width="31.368" height="10.8">zbycia</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="211.68" y="323.604" width="44.04" height="10.8">nabytych</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="261" y="323.604" width="26.52" height="10.8">przed</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="292.8" y="323.604" width="29.976" height="10.8">dniem</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="328.08" y="323.604" width="6" height="10.8">1</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="339.36" y="323.604" width="39.408" height="10.8">stycznia</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="384" y="323.604" width="24" height="10.8">2003</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="413.28" y="323.604" width="6.96" height="10.8">r.</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="425.52" y="323.604" width="42.096" height="10.8">obligacji,</TOKEN>
        </TEXT><TEXT width="385.44" height="11.88" x="82.2" y="402.516">
          <TOKEN bold="no" italic="no" font-size="12" x="82.2" y="406.404" width="9.996" height="10.8">b)</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="105.96" y="406.404" width="5.328" height="10.8">z</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="116.64" y="402.516" width="53.28" height="15.768">odpłatnego</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="175.32" y="406.404" width="31.248" height="10.8">zbycia</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="211.92" y="406.404" width="44.664" height="10.8">papierów,</TOKEN>
        </TEXT><TEXT width="75.636" height="10.8" x="82.2" y="592.644">
          <TOKEN bold="no" italic="no" font-size="12" x="82.2" y="592.644" width="9.276" height="10.8">c)</TOKEN>
        <TOKEN bold="no" italic="no" font-size="12" x="105.96" y="592.644" width="51.876" height="10.8">(uchylona)</TOKEN></TEXT><TEXT width="385.464" height="11.88" x="82.2" y="609.516">
          <TOKEN bold="no" italic="no" font-size="12" x="82.2" y="613.404" width="6" height="10.8">–</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="90.6" y="613.404" width="21.24" height="10.8">przy</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="117.12" y="613.404" width="25.896" height="10.8">czym</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="148.44" y="613.404" width="52.608" height="10.8">zwolnienie</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="206.28" y="613.404" width="14.808" height="10.8">nie</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="226.32" y="613.404" width="14.688" height="10.8">ma</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="246.24" y="613.404" width="66.84" height="10.8">zastosowania,</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="318.36" y="609.516" width="26.136" height="15.768">jeżeli</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="349.8" y="609.516" width="41.928" height="15.768">sprzedaż</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="396.96" y="613.404" width="20.64" height="10.8">tych</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="422.88" y="613.404" width="44.784" height="10.8">papierów</TOKEN>
        </TEXT><TEXT width="288.12" height="15.768" x="82.2" y="630.156">
          <TOKEN bold="no" italic="no" font-size="12" x="82.2" y="630.156" width="72.48" height="15.768">wartościowych</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="157.68" y="634.044" width="16.776" height="10.8">jest</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="177.48" y="634.044" width="63.336" height="10.8">przedmiotem</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="243.84" y="630.156" width="57.216" height="15.768">działalności</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="304.08" y="634.044" width="66.24" height="10.8">gospodarczej,</TOKEN>
        </TEXT><TEXT width="385.32" height="11.88" x="82.2" y="650.916">
          <TOKEN bold="no" italic="no" font-size="12" x="82.2" y="654.804" width="9.996" height="10.8">d)</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="105.96" y="654.804" width="44.568" height="10.8">uzyskane</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="156.24" y="654.804" width="5.328" height="10.8">z</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="167.28" y="654.804" width="43.896" height="10.8">realizacji</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="216.84" y="654.804" width="24.024" height="10.8">praw</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="246.48" y="650.916" width="66.6" height="15.768">wynikających</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="318.84" y="654.804" width="5.328" height="10.8">z</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="329.88" y="654.804" width="44.664" height="10.8">papierów</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="380.16" y="650.916" width="75.72" height="15.768">wartościowych.</TOKEN>
          </TEXT>|]
    , testCase "Oddział can appear on the same level as articles" $ do
      testParsingArticles
        [Node (Addressable "19" []) [
          Node (Addressable "" [Text "Statut bankowi państwowemu nadaje, w drodze rozporządzenia, Prezes"]) []]
        ,Node (Header Oddział "B" "Banki spółdzielcze") [
          Node (Addressable "20" []) [
            Node (Addressable "1" [Text "Bankiem spółdzielczym jest bank w rozumieniu art. 2 pkt 1 ustawy"]) []]]]
        [text|
        <TEXT width="385.544" height="13.284" x="82.22" y="374.108">
          <TOKEN bold="yes" italic="no" font-size="12" x="82.22" y="374.108" width="20.988" height="13.284">Art.</TOKEN>
          <TOKEN bold="yes" italic="no" font-size="12" x="106.22" y="374.108" width="15" height="13.284">19.</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="124.22" y="374.108" width="28.008" height="13.284">Statut</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="155.348" y="374.108" width="41.328" height="13.284">bankowi</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="199.736" y="374.108" width="69.312" height="13.284">państwowemu</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="272.132" y="374.108" width="34.356" height="13.284">nadaje,</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="309.548" y="374.108" width="8.664" height="13.284">w</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="321.28" y="374.108" width="32.676" height="13.284">drodze</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="357.076" y="374.108" width="76.248" height="13.284">rozporządzenia,</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="436.444" y="374.108" width="31.32" height="13.284">Prezes</TOKEN>
        </TEXT>
        <TEXT width="108.24" height="13.284" x="208.04" y="442.348">
          <TOKEN bold="yes" italic="no" font-size="12" x="208.04" y="442.348" width="11.004" height="13.284">B.</TOKEN>
          <TOKEN bold="yes" italic="no" font-size="12" x="222.044" y="442.348" width="30.66" height="13.284">Banki</TOKEN>
          <TOKEN bold="yes" italic="no" font-size="12" x="255.704" y="442.348" width="60.576" height="13.284">spółdzielcze</TOKEN>
        </TEXT>
        <TEXT width="385.48" height="13.284" x="82.22" y="468.928">
          <TOKEN bold="yes" italic="no" font-size="12" x="82.22" y="468.928" width="20.988" height="13.284">Art.</TOKEN>
          <TOKEN bold="yes" italic="no" font-size="12" x="106.22" y="468.928" width="15" height="13.284">20.</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="124.22" y="468.928" width="9" height="13.284">1.</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="136.22" y="468.928" width="43.332" height="13.284">Bankiem</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="183.668" y="468.928" width="69.42" height="13.284">spółdzielczym</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="257.18" y="468.928" width="16.704" height="13.284">jest</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="278.096" y="468.928" width="23.328" height="13.284">bank</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="305.624" y="468.928" width="8.664" height="13.284">w</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="317.38" y="468.928" width="54.636" height="13.284">rozumieniu</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="376.204" y="468.928" width="15.708" height="13.284">art.</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="394.96" y="468.928" width="6" height="13.284">2</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="405.148" y="468.928" width="15.336" height="13.284">pkt</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="423.54" y="468.928" width="6" height="13.284">1</TOKEN>
          <TOKEN bold="no" italic="no" font-size="12" x="433.668" y="468.928" width="34.032" height="13.284">ustawy</TOKEN>
        </TEXT>|]

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


podstawaTable = unsafePerformIO (read <$> readFile "tests/podstawaObliczeniaPodatku.table")


testParsingArticles :: [Node] -> T.Text -> Assertion
testParsingArticles expectedArticles articlesText = do
  testParsing expectedAct input
  where
    expectedAct =
      Ustawa {
        _upId = PozId { _pidYear = 1991, _pidNr = 80, _pidSeq = 350 }
        , _uzDnia = fromGregorian 1991 7 26
        , _uTytul = "o podatku dochodowym od osób fizycznych"
        , _uroot = Node Root [
          Node (Header Rozdział "1" "Podmiot i przedmiot opodatkowania") expectedArticles]
        , _uannexes = [] }
    input = [text|
<?xml version="1.0" encoding="UTF-8"?>
<DOCUMENT>
  <METADATA>
    <PDFFILENAME>pdf/D19910350Lj.pdf</PDFFILENAME>
    <PROCESS name="pdftoxml" cmd="-fullFontName">
      <VERSION value="1.2">
        <COMMENT/>
      </VERSION>
      <CREATIONDATE>Sat Nov 14 13:02:27 2015</CREATIONDATE>
    </PROCESS>
  </METADATA>
  <PAGE width="595.32" height="841.92" number="1"><TEXT width="70.974" height="8.1" x="56.64" y="37.413">
      <TOKEN bold="no" italic="no" font-size="9" x="56.64" y="37.413" width="45.756" height="8.1">©Kancelaria</TOKEN>
      <TOKEN bold="no" italic="no" font-size="9" x="104.637" y="37.413" width="22.977" height="8.1">Sejmu</TOKEN>
    </TEXT><TEXT width="28.62" height="8.1" x="481.683" y="37.413">
      <TOKEN bold="no" italic="no" font-size="9" x="481.683" y="37.413" width="5.733" height="8.1">s.</TOKEN>
      <TOKEN bold="no" italic="no" font-size="9" x="489.72" y="37.413" width="20.583" height="8.1">1/253</TOKEN>
    </TEXT><TEXT width="42.057" height="8.1" x="425.64" y="793.173">
      <TOKEN bold="no" italic="no" font-size="9" x="425.64" y="793.173" width="42.057" height="8.1">2015-11-02</TOKEN>
    </TEXT><TEXT width="132.6" height="10.8" x="185.64" y="59.604">
      <TOKEN bold="yes" italic="no" font-size="12" x="185.64" y="59.604" width="28.56" height="10.8">Dz.U.</TOKEN>
      <TOKEN bold="yes" italic="no" font-size="12" x="217.2" y="59.604" width="24" height="10.8">1991</TOKEN>
      <TOKEN bold="yes" italic="no" font-size="12" x="244.2" y="59.604" width="13.968" height="10.8">Nr</TOKEN>
      <TOKEN bold="yes" italic="no" font-size="12" x="261.12" y="59.604" width="12" height="10.8">80</TOKEN>
      <TOKEN bold="yes" italic="no" font-size="12" x="276.12" y="59.604" width="21" height="10.8">poz.</TOKEN>
      <TOKEN bold="yes" italic="no" font-size="12" x="300.24" y="59.604" width="18" height="10.8">350</TOKEN>
    </TEXT><TEXT width="66.264" height="10.8" x="227.64" y="100.884">
      <TOKEN bold="yes" italic="no" font-size="12" x="227.64" y="100.884" width="8.664" height="10.8">U</TOKEN>
      <TOKEN bold="yes" italic="no" font-size="12" x="239.04" y="100.884" width="6.672" height="10.8">S</TOKEN>
      <TOKEN bold="yes" italic="no" font-size="12" x="248.4" y="100.884" width="8.004" height="10.8">T</TOKEN>
      <TOKEN bold="yes" italic="no" font-size="12" x="259.2" y="100.884" width="8.664" height="10.8">A</TOKEN>
      <TOKEN bold="yes" italic="no" font-size="12" x="270.48" y="100.884" width="12" height="10.8">W</TOKEN>
      <TOKEN bold="yes" italic="no" font-size="12" x="285.24" y="100.884" width="8.664" height="10.8">A</TOKEN>
    </TEXT><TEXT width="107.16" height="10.8" x="208.44" y="127.764">
      <TOKEN bold="no" italic="no" font-size="12" x="208.44" y="127.764" width="5.328" height="10.8">z</TOKEN>
      <TOKEN bold="no" italic="no" font-size="12" x="216.72" y="127.764" width="20.688" height="10.8">dnia</TOKEN>
      <TOKEN bold="no" italic="no" font-size="12" x="240.36" y="127.764" width="12" height="10.8">26</TOKEN>
      <TOKEN bold="no" italic="no" font-size="12" x="255.36" y="127.764" width="23.328" height="10.8">lipca</TOKEN>
      <TOKEN bold="no" italic="no" font-size="12" x="281.64" y="127.764" width="24" height="10.8">1991</TOKEN>
      <TOKEN bold="no" italic="no" font-size="12" x="308.64" y="127.764" width="6.96" height="10.8">r.</TOKEN>
    </TEXT><TEXT width="221.712" height="10.8" x="151.2" y="154.404">
      <TOKEN bold="yes" italic="no" font-size="12" x="151.2" y="154.404" width="6" height="10.8">o</TOKEN>
      <TOKEN bold="yes" italic="no" font-size="12" x="160.2" y="154.404" width="42.792" height="10.8">podatku</TOKEN>
      <TOKEN bold="yes" italic="no" font-size="12" x="205.92" y="154.404" width="68.076" height="10.8">dochodowym</TOKEN>
      <TOKEN bold="yes" italic="no" font-size="12" x="276.96" y="154.404" width="12.672" height="10.8">od</TOKEN>
      <TOKEN bold="yes" italic="no" font-size="12" x="292.68" y="154.404" width="23.352" height="10.8">osób</TOKEN>
      <TOKEN bold="yes" italic="no" font-size="12" x="319.08" y="154.404" width="53.832" height="10.8">fizycznych</TOKEN>
      <TOKEN bold="yes" italic="no" font-size="7.98" x="380.08" y="154.404" width="12.832" height="8">1)</TOKEN>
    </TEXT><TEXT width="51.6" height="15.768" x="236.28" y="189.396">
      <TOKEN bold="no" italic="no" font-size="12" x="236.28" y="189.396" width="42.576" height="15.768">Rozdział</TOKEN>
      <TOKEN bold="no" italic="no" font-size="12" x="281.88" y="193.284" width="6" height="10.8">1</TOKEN>
    </TEXT><TEXT width="187.08" height="10.8" x="168.6" y="219.804">
      <TOKEN bold="yes" italic="no" font-size="12" x="168.6" y="219.804" width="43.356" height="10.8">Podmiot</TOKEN>
      <TOKEN bold="yes" italic="no" font-size="12" x="214.92" y="219.804" width="3.336" height="10.8">i</TOKEN>
      <TOKEN bold="yes" italic="no" font-size="12" x="221.28" y="219.804" width="52.596" height="10.8">przedmiot</TOKEN>
      <TOKEN bold="yes" italic="no" font-size="12" x="276.84" y="219.804" width="78.84" height="10.8">opodatkowania</TOKEN>
    </TEXT>
    $articlesText
  </PAGE>
</DOCUMENT>|]

testParsing :: Akt -> T.Text -> Assertion
testParsing expected input = do
  (filePath, handle) <- openTempFile "/tmp" "parsing-test.xml"
  T.hPutStr handle input
  hClose handle
  ustawa <- parseUstawa filePath []
  diffAssertEqual expected ustawa


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
