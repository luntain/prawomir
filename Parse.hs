{-# LANGUAGE OverloadedStrings #-}
module Parse where

import Data.Monoid
import MyPrelude hiding (many, (<|>))
import Control.Lens
import Data.Time
import qualified Data.Text as T
import Model
import ParseXml
import Text.Parsec
import Text.Parsec.Pos

-- Tok not to clash with ParseXml.Token
data Tok = Tok {_ttok :: Token, _tline:: TEXT, _tpage::Page} deriving (Show, Read)

makeLenses ''Tok

type Parser a = Parsec [Tok] () a

data Partitioned =
  Partitioned {_mainContent :: [Tok], _sidenotes :: [Tok], _footnotes::[Tok]}
  deriving (Show, Read)

instance Monoid Partitioned where
  mempty = Partitioned [] [] []
  mappend (Partitioned m s f) (Partitioned m2 s2 f2) = Partitioned (m <>m2) (s<>s2) (f<>f2)

makeLenses ''Partitioned

partitionPage :: (Int, Page) -> Partitioned
partitionPage (seq, page) = Partitioned {_mainContent=toToks mainCont, _sidenotes=toToks sidenotsLines, _footnotes=toToks footnots}
  where
    (sidenotsLines, otherLines) = partition isSidenote (_ptexts page)
    (mainCont, footnots) = forceRight . runParser parseOtherLines () (show seq) $ otherLines
    parseOtherLines =
      parseLine mempty (parsec $ string "©Kancelaria Sejmu")
        *> parseLine mempty (parsec $ string "s." *> space *> many1 digit *> string "/" *> many1 digit)
        *> parseLine mempty (parsec $ count 4 digit *> string "-" *> count 2 digit *> string "-" *> count 2 digit)
        *> manyTill' (parseLine mempty Just) parseFootnotes
    parseFootnotes =
      many (parseLine ((All.(==6.48) . view (ltokens.to head.tfontsize)) <> (All.(==1).length.view ltokens)) (parsec $ many1 digit <* string ")")
            *> parseLine (All .(==9.96). view (ltokens.to head.tfontsize)) Just) <* eof
    isSidenote :: TEXT -> Bool
    isSidenote line = line^.lx > 470 && firstTok^.tfontsize == 9.96 && firstTok^.tbold
      where
        firstTok :: Token
        firstTok = head $ line^.ltokens
    toToks lines = [Tok tk ln page | ln <- lines, tk <- _ltokens ln]
    parseLine :: (TEXT->All) -> (TEXT->Maybe a) -> Parsec [TEXT] () a
    parseLine fp parser =
      ftoken
      (\line -> newPos ("Page " ++ show seq) 0 0) -- TODO put in a line and col number maybe?
      fp
      parser

parseDoc :: [Page] -> Partitioned
parseDoc = mconcat . map partitionPage . zip [1..]


duPozycja :: Parser DUPozycja
duPozycja = do
  pid <- duPozId
  consumeWords "U S T A W A"
  zDnia <- consumeWords "z dnia" *> dlugaData
  tytul <- T.unwords <$> many1 bold
  div <- parseDivs
  return (DUPozycja {
            _dupId=pid
          , _dupAkt=Ustawa {
              _uzDnia=zDnia
              , _uTytul=tytul
              , _utresc=Articles []} })

duPozId :: Parser PozId
duPozId = do
  consumeWords "Dz.U."
  rok <- int
  consumeWords "Nr"
  nr <- int
  consumeWords "poz."
  seq <- int
  return $ PozId {_pidYear=rok, _pidNr=nr, _pidSeq=seq}

--parseDivs = many1 parseRozdzial
--parseRozdzial = do
  --consumeWords "Rozdział"
  --rozdzialId <- anyWord
  --tytul <- manyTill (ftok boldF anyT) (ftok boldF (constT "Art."))
  --articles <- 

data DivType =
  Dzial | Rozdzial | Artykul | Ustep | Punkt
  deriving (Show, Read)

hdrPriority divTyp =
  case divtyp of
    Dzial -> 1
    Rozdzial -> 2
    Artykul -> 3
    Ustep -> 4
    Punkt -> 5

data Div = Div { _dtext :: T.Text, _dtype :: DivType}

consumeWords :: T.Text -> Parser ()
consumeWords ws = forM_ (words . T.unpack $ ws) (\w -> parsecTok mempty (string w))

tekstJednolity :: Parser Div
tekstJednolity = undefined

dlugaData :: Parser Day
dlugaData = do
  components <- sequence . take 4 . repeat $ ftoken tokPos mempty (Just . view (ttok.ttext))
  case parseTimeM True polishTimeLocaleLc "%d %B %Y r." (unwords . map T.unpack $ components) of
    Just r -> return r
    Nothing -> mzero

boldF = All . view (ttok.tbold)
anyT = Just . view (ttok . ttext)
constT expected tok = if tok^.ttok.ttext == expected then Just () else Nothing

int :: Parser Int
int = read <$> parsecTok mempty (many1 digit)

-- this generalized the manyTill operator from Parsec lib
manyTill' :: Stream s m t => ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m ([a], end)
manyTill' m end = do
  res <- fmap Left end <|> fmap Right m
  case res of
    Left end -> return ([], end)
    Right part -> first (part:) <$> manyTill' m end

-- specialise ftoken for Tok
ftok :: (t -> All) -> (t -> Maybe a) -> Parsec [t] () a
ftok = ftoken tokPos
-- formatted token
ftoken :: Show t => (t->SourcePos) -> (t -> All) -> (t -> Maybe a) -> Parsec [t] () a
ftoken srcPos fp match = token show srcPos (\t -> if getAll (fp t) then match t else Nothing)

parsecTok :: (Tok->All) -> (Parsec T.Text () a) -> Parser a
parsecTok fp parser =
  ftoken tokPos fp
    (\t -> case runParser parser () (show (tokPos t)) (t^.ttok.ttext) of
            Left err -> Nothing
            Right res -> Just res)
tokPos t = newPos ("Page " ++ (show $ t^.tpage.pnumber)) (round $ t^.tline.ly) (round $ t^.ttok.tx)

parsec :: ToText t => Parsec T.Text () a -> (t->Maybe a)
parsec parser = either (const Nothing) Just . runParser parser () "TODO" . toText


class ToText t where
  toText :: t -> T.Text

instance ToText TEXT where
  toText t = T.unwords $ t^..ltokens.traverse.ttext
instance ToText Tok where
  toText t = t^.ttok.ttext
