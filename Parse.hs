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
import Data.Char (isDigit, isLower)

data NonTerminal =
    RozdziałToken
  | ArticleToken
  | UstępToken String
  | PunktToken String
  | PodpunktToken String
  deriving (Show, Read, Eq)
makePrisms ''NonTerminal

-- Tok not to clash with ParseXml.Token
data Tok =
  RawToken {_ttok :: Token, _tline:: TEXT, _tpage::Page}
  -- following greatly simplify parsing
  | NonTerminal NonTerminal SourcePos
  deriving (Show)
makeLenses ''Tok
makePrisms ''Tok

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

processAdditionsAndRemovals :: [Tok] -> [Tok]
processAdditionsAndRemovals [] = []
processAdditionsAndRemovals (x@(NonTerminal _ _) : rest) =
  x : processAdditionsAndRemovals rest
processAdditionsAndRemovals (x@(RawToken {_ttok=ttok'}) : rest)
  | T.head (_ttext ttok') == '<' && _tbold ttok' =
    over (ttok.ttext) T.tail x : consumeAddition rest
  | T.head (_ttext ttok') == '[' && _titalic ttok' =
    consumeDeletion rest
  | otherwise = x : processAdditionsAndRemovals rest
  where
    consumeAddition (x@(RawToken {_ttok=ttok'}):rest)
      | T.isSuffixOf ">" (ttok' ^.ttext) && ttok' ^.tbold =
        over (ttok.ttext) T.init x : processAdditionsAndRemovals rest
    consumeAddition (x : rest) = x : consumeAddition rest
    consumeAddition [] = []
    consumeDeletion (RawToken {_ttok=ttok'}:rest)
      | T.isSuffixOf "]" (ttok' ^.ttext) && ttok' ^.titalic =
        processAdditionsAndRemovals rest
    consumeDeletion (_ : rest) = consumeDeletion rest
    consumeDeletion [] = []


-- default Art x pos seems to be 82.2, shifted one for added rozdzial is 107.64

-- lexer? :)
-- mostly looks at the first token in a line and judges if it is any of the NonTerminals
detectNonTerminals :: [Tok] -> [Tok]
detectNonTerminals = concat . snd . mapAccumL detect 82.2 . groupBy ((==) `on` lineId)
  where
    lineId :: Tok -> Float
    lineId (RawToken {_tline=tline}) = _ly tline
    lineId (NonTerminal _ _) = error "There should't be any NonTerminal's at this point"
    detect :: Float -> [Tok] -> (Float, [Tok])
    detect _ [] = error "There should not be an empty line"
    detect artXPos orig@(first:rest) =
      let replacement = case first of
            RawToken {_ttok=tok}
              | _ttext tok == "Art.", _tbold tok -> Just ArticleToken
              -- I saw one Rozdział with x 236, arts seem to have it at 82.2
              | _ttext tok == "Rozdział", _tx tok > 150.0 -> Just RozdziałToken
              | _tx tok == artXPos, text <- _ttext tok, likeUstepNumber text -> Just $ mkUstepToken text
               -- default punkt posx seems to be 56.64, 82.2 in shifted
              | _tx tok < artXPos - 24, text <- _ttext tok
                 , likePunktNumber text
                 , nextTok : _ <- rest
                 -- TODO I should assert that the is greater thatn 82.2 because s 65 i pit
                 , Just nextTokPos <- nextTok^?ttok.tx, nextTokPos >= artXPos ->
                     Just . PunktToken . T.unpack . T.init $ text
              | _tx tok == artXPos, text <- _ttext tok, likePodpunktNumber text ->
                 Just . PodpunktToken . T.unpack . T.init $ text
            _ -> Nothing
      in
      case replacement of
        Nothing -> (artXPos, orig)
        Just x@ArticleToken -- we should detect a possible ustep token following
          | artNum : maybeUstep : newRest <- rest
               , Just text <- maybeUstep^?ttok.ttext
               , likeUstepNumber text ->
                 let newArtXPos = fromJust $ first^?ttok.tx in
                 (newArtXPos, NonTerminal x (tokPos first)
                    : artNum -- leave that as is, gets parsed later
                    : NonTerminal (mkUstepToken text) (tokPos maybeUstep)
                    : newRest)
        Just otherNonTerminal ->
          (artXPos, NonTerminal otherNonTerminal (tokPos first) : rest)
    likeUstepNumber :: T.Text -> Bool
    likeUstepNumber text    = isDigit (T.head text) && T.last text == '.'
    likePunktNumber text    = isDigit (T.head text) && T.last text == ')'
    likePodpunktNumber text = isLower (T.head text) && T.last text == ')'
    mkUstepToken :: T.Text -> NonTerminal
    mkUstepToken = UstępToken . T.unpack . T.init -- chop off the '.' at the end


parseUstawa :: FilePath -> IO Akt
parseUstawa path = do
  pages <- parsePages path
  let partitioned = partitionPages pages
  forceResult path
    $ toResult $ runParser tekstJednolity () path (detectNonTerminals . processAdditionsAndRemovals $ _mainContent partitioned)

tekstJednolity :: Parser Akt
tekstJednolity = do
  pid <- duPozId
  consumeWords "U S T A W A"
  zDnia <- consumeWords "z dnia" *> dlugaData
  tytul <- (T.unpack . T.unwords) <$> many1 (ftok boldF (preview (ttok.ttext)))
  rozdzialy <- many rozdział -- TODO what about the case of no rozdzialy?
  let rozdzialIndex = map (view _1 &&& view _2) rozdzialy
  let spisTresci = M.fromList . map (view _1 &&& view _3) $ rozdzialy
  return (Ustawa {
            _upId=pid
            , _uzDnia=zDnia
            , _uTytul=tytul
            , _uspisTresci = Partitions "Rozdział" rozdzialIndex spisTresci
            , _uarticles = M.fromList . concat . map (view _4) $ rozdzialy })

rozdział :: Parser (String, String, TableOfContents, [(String, Article)])
rozdział = do
  _ <- ftok mempty (preview $ _NonTerminal . _1 . _RozdziałToken)
  number <- T.unpack <$> ftok mempty (preview $ ttok.ttext)
  podtytul <- (T.unpack . T.unwords) <$> many1 (ftok boldF (preview (ttok.ttext)))
  articles <- many1 article
  return (number, podtytul, Articles (map fst articles), articles)

article :: Parser (String, Article)
article = do
  ftok mempty (preview (_NonTerminal . _1 . _ArticleToken))
  number <- (T.unpack . T.init) <$> ftok boldF anyT
  prefix <- option emptyZWyliczeniem ustępBody
  ustępy <- many ustęp
  return $ (number, Article {_aprefix = prefix, _aindex = map fst ustępy, _apoints = M.fromList ustępy})


ustęp :: Parser (String, ZWyliczeniem)
ustęp = do
  ustępNum <- ftok mempty (preview $ _NonTerminal . _1 . _UstępToken)
  ustęp <- ustępBody
  return (ustępNum, ustęp)


ustępBody :: Parser ZWyliczeniem
ustępBody = do
  text <- T.unwords <$> many (ftok mempty anyT)
  punkty <- many punkt
  suffix <- many $ ftok mempty anyT
  return (ZWyliczeniem [Text text] (map fst punkty) (M.fromList punkty) (processText suffix))

punkt :: Parser (String, ZWyliczeniem)
punkt = do
  num <- ftok mempty (preview $ _NonTerminal . _1 . _PunktToken)
  body <- punktBody
  return (num, body)

punktBody :: Parser ZWyliczeniem
punktBody = do
  startX <- peekNextTokXPos
  texts <- many $ ftok (indentedByAtLeast startX) anyT
  podpunkty <- many podpunkt
  suffix <- many $ ftok (indentedByAtLeast startX) anyT
  return (ZWyliczeniem (processText texts) (map fst podpunkty) (M.fromList podpunkty) (processText suffix))

processText :: [T.Text] -> TextWithReferences
processText [] = []
processText texts = [Text . T.unwords $ texts]

podpunkt :: Parser (String, ZWyliczeniem)
podpunkt = do
  num <- ftok mempty (preview $ _NonTerminal . _1 . _PodpunktToken)
  startX <- peekNextTokXPos
  text <- many $ ftok (indentedByAtLeast startX) anyT
  return (num, ZWyliczeniem [Text . T.unwords $ text] [] M.empty [])

peekNextTokXPos :: Parser Float
peekNextTokXPos = do
  input <- getInput
  let maybeNextPos = input^?ix 0 .ttok.tx
  maybe (fail "Expected a token with an x position") return maybeNextPos

duPozId :: Parser PozId
duPozId = do
  consumeWords "Dz.U."
  rok <- int
  consumeWords "Nr"
  nr <- int
  consumeWords "poz."
  seq <- int
  return $ PozId {_pidYear=rok, _pidNr=nr, _pidSeq=seq}

consumeWords :: T.Text -> Parser ()
consumeWords ws = forM_ (words . T.unpack $ ws) (\w -> parsecTok mempty (string w))

dlugaData :: Parser Day
dlugaData = do
  components <- sequence . take 4 . repeat $ ftok mempty (preview (ttok.ttext))
  let date_text = unwords . map T.unpack $ components
  case parseTimeM True polishTimeLocaleLc "%d %B %Y r." date_text of
    Just r -> return r
    Nothing -> fail ("Nie mozna sparsowac daty: " ++ date_text)

boldF :: Tok -> All
boldF (NonTerminal _ _) = All False
boldF (RawToken {_ttok=ttok}) = All . _tbold $ ttok

anyT = preview (ttok . ttext)
constT expected tok = if tok^.ttok.ttext == expected then Just () else Nothing

indentedByAtLeast :: Float -> Tok -> All
indentedByAtLeast ind = view $ ttok.tx.to (All.(>=ind))

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
