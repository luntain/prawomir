{-# LANGUAGE OverloadedStrings #-}
module Parse where

import Data.Monoid
import MyPrelude hiding (many, (<|>))
import Control.Lens
import Data.Time
import qualified Data.Map as M
import qualified Data.Text as T
import Model
import ParseXml
import Text.Parsec
import Text.Parsec.Pos

data NonTerminal =
    RozdziałToken
  | ArticleToken
  deriving (Show, Read, Eq)

-- Tok not to clash with ParseXml.Token
data Tok =
  RawToken {_ttok :: Token, _tline:: TEXT, _tpage::Page}
  -- following greatly simplify parsing
  | NonTerminal NonTerminal SourcePos
  deriving (Show)
makeLenses ''Tok

type Parser a = Parsec [Tok] () a

data Partitioned =
  Partitioned {_mainContent :: [Tok], _sidenotes :: [Tok], _footnotes::[Tok]}
  deriving (Show)

instance Monoid Partitioned where
  mempty = Partitioned [] [] []
  mappend (Partitioned m s f) (Partitioned m2 s2 f2) = Partitioned (m <>m2) (s<>s2) (f<>f2)

makeLenses ''Partitioned

partitionPages :: [Page] -> Partitioned
partitionPages = mconcat . map partitionPage . zip [1..]

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
    toToks lines = [RawToken tk ln page | ln <- lines, tk <- _ltokens ln]
    parseLine :: (TEXT->All) -> (TEXT->Maybe a) -> Parsec [TEXT] () a
    parseLine fp parser =
      ftoken
      (\_line -> newPos ("Page " ++ show seq) 0 0) -- TODO put in a line and col number maybe?
      fp
      parser

detectNonTerminals :: [Tok] -> [Tok]
detectNonTerminals = concat . map detect . groupBy ((==) `on` lineId)
  where
    lineId :: Tok -> Float
    lineId (RawToken {_tline=tline}) = _ly tline
    lineId (NonTerminal _ _) = error "There should't be any NonTerminal's at this point"
    detect :: [Tok] -> [Tok]
    detect [] = error "There should not be an empty line"
    detect (first:rest) = detect' first : rest
    detect' token@(RawToken {_ttok=tok})
      | _ttext tok == "Art." && _tbold tok = NonTerminal ArticleToken (tokPos token)
      -- I saw one Rozdział with x 236, arts seem to have it at 82.2
      | _ttext tok == "Rozdział" && _tx tok > 150.0 = NonTerminal RozdziałToken (tokPos token)
      | otherwise = token
    detect' tok = tok

parseUstawa :: FilePath -> IO Akt
parseUstawa path = do
  pages <- parsePages path
  let partitioned = partitionPages pages
  forceResult path
    $ toResult $ runParser tekstJednolity () path (detectNonTerminals $ _mainContent partitioned)

tekstJednolity :: Parser Akt
tekstJednolity = do
  pid <- duPozId
  consumeWords "U S T A W A"
  zDnia <- consumeWords "z dnia" *> dlugaData
  tytul <- (T.unpack . T.unwords) <$> many1 (ftok boldF (Just . view (ttok.ttext)))
  return (Ustawa {
            _upId=pid
            , _uzDnia=zDnia
            , _uTytul=tytul
            , _uspisTresci=Articles []
            , _uarticles = M.empty })

rozdział :: Parser (String, String, M.Map String TableOfContents)

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

consumeWords :: T.Text -> Parser ()
consumeWords ws = forM_ (words . T.unpack $ ws) (\w -> parsecTok mempty (string w))

dlugaData :: Parser Day
dlugaData = do
  components <- sequence . take 4 . repeat $ ftok mempty (Just . view (ttok.ttext))
  let date_text = unwords . map T.unpack $ components
  case parseTimeM True polishTimeLocaleLc "%d %B %Y r." date_text of
    Just r -> return r
    Nothing -> fail ("Nie mozna sparsowac daty: " ++ date_text)

boldF :: Tok -> All
boldF (NonTerminal _ _) = All False
boldF (RawToken {_ttok=ttok}) = All . _tbold $ ttok

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
ftok :: (Tok -> All) -> (Tok -> Maybe a) -> Parsec [Tok] () a
ftok = ftoken tokPos
-- formatted token
ftoken :: Show t => (t->SourcePos) -> (t -> All) -> (t -> Maybe a) -> Parsec [t] () a
ftoken srcPos fp match = token show srcPos (\t -> if getAll (fp t) then match t else Nothing)

parsecTok :: (Tok->All) -> Parsec T.Text () a -> Parser a
parsecTok fp parser =
  ftok fp
    (\t -> case runParser parser () (show (tokPos t)) (t^.ttok.ttext) of
            Left _err -> Nothing
            Right res -> Just res)

tokPos (RawToken {_ttok=ttok, _tline=tline, _tpage=tpage}) =
  newPos ("Page " ++ (show $ tpage^.pnumber)) (round $ tline^.ly) (round $ ttok^.tx)
tokPos (NonTerminal _ sp) = sp

-- try a parsec text parser on a text like thing, yielding a Maybe if it is successful
parsec :: ToText t => Parsec T.Text () a -> (t->Maybe a)
parsec parser = either (const Nothing) Just . runParser parser () "TODO" . toText


class ToText t where
  toText :: t -> T.Text

instance ToText TEXT where
  toText t = T.unwords $ t^..ltokens.traverse.ttext
instance ToText Tok where
  toText t = t^.ttok.ttext

-- data Stream = Stream { _scurrentLine :: [Token], _scurrentPage :: [Text], _sotherPages :: [Page], _siLineBeginning :: Bool }
--               deriving (Show, Read)
-- makeLenses ''Stream

-- instance Monad m => Stream Stream m Stream where
--   uncons stream =
--     case _scurrentLine stream of
--       _:(rest@(_:_)) -> Just $ stream {_scurrentLine = rest, _siLineBeginning = False}
--       _ -> case _scurrentPage of
--         x:rest -> Just $ stream {_scurrentLine=_ltokens x, _siLineBeginning=True, _scurrentPage=rest}
--         [] -> case _sotherPages stream of
--           [] -> Nothing
--           p:pages ->

--   feedLine =
