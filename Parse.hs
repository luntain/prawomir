{-# LANGUAGE OverloadedStrings #-}
module Parse where

import Data.Monoid
import MyPrelude hiding (many, (<|>))
import Control.Lens
import Data.Time
--import Text.Nicify
--import Text.Show.Unicode
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.IntervalMap as IM
import qualified Data.IntervalSet as IS
import Model
import ParseXml
import Text.Megaparsec
import Text.Megaparsec.Prim -- MonadParsec is not available from Text.Megaparsec
import Data.Char (isDigit, isLower)
import Test.Tasty
import Test.Tasty.HUnit
import Control.Monad.State.Lazy

data NonTerminal =
    RozdziałToken T.Text
  | ArticleToken T.Text
  | UstępToken T.Text
  | PunktToken T.Text
  | PodpunktToken T.Text
  | TiretToken
  | TableToken TableInfo
  | AnnexToken Int T.Text -- PageNo in the source document, AppendixNumber
  deriving (Show, Read, Eq, Ord)

data Tok =
  RawToken {_ttok :: WORD, _tline:: TEXT, _tpage::Page}
  -- following greatly simplify parsing
  | NonTerminal TokPosition NonTerminal
  deriving (Show, Eq, Ord) -- Megaparsec's Stream requires Tokens to have Ord

data TokPosition =
  TokPosition { _tpPageNum :: Int, _tpLineY, _tpTokX :: Float }
  deriving (Show, Read, Eq, Ord)

type Parser a = Parsec Dec [Tok] a

data Partitioned =
  Partitioned {_mainContent :: [Tok], _sidenotes :: [Tok], _footnotes::[Tok]}
  deriving (Show)

instance Monoid Partitioned where
  mempty = Partitioned [] [] []
  mappend (Partitioned m s f) (Partitioned m2 s2 f2) = Partitioned (m <>m2) (s<>s2) (f<>f2)


-- has to be here because of the makeLenses TH
data TableBuilder =
  TableBuilder {
    _tbxEdges :: (Float, Float)
    , _tbrowEnds :: [Float]
    , _tbcolEnds :: [Float]
    , _tbrows :: [[VClip]] -- rows have cells in reverse order, rows are in reverse order too
    }
  deriving (Show, Eq)


data TableInfo =
  TableInfo { _tix, _tiy, _tiwidth, _tiheight :: Float
            , _tirowEnds, _ticolEnds :: [Float], _titable :: [[TableCell]] }
  deriving (Show, Eq, Read, Ord)

makeLenses ''Tok
makePrisms ''Tok
makeLenses ''Partitioned
makePrisms ''NonTerminal
makeLenses ''TableInfo
makeLenses ''TableBuilder
makeLenses ''TokPosition

instance (Ord t, HasTokPosition t) => Stream [t] where
  type Token [t] = t
  uncons [] = Nothing
  uncons (a:as) = Just (a, as)
  updatePos _ _ _ tok = (tokPos tok, tokPos tok)

class HasTokPosition t where
  tokPos :: t -> SourcePos


partitionPages :: [Page] -> Partitioned
partitionPages = mconcat . map partitionPage

partitionPage :: Page -> Partitioned
partitionPage page =
    Partitioned {_mainContent=toToks mainCont, _sidenotes=toToks sidenotsLines, _footnotes=toToks footnots}
  where
    (sidenotsLines, otherLines) = partition isSidenote (_ptexts page)
    (mainCont, footnots) = forceRight . runParser parseOtherLines ("other lines: " ++ show (_pnumber page)) $ otherLines
    parseOtherLines :: Parsec Dec [TEXT] ([TEXT],[TEXT])
    parseOtherLines =
      parseLine "kancelaria" mempty (parsec $ string "©Kancelaria Sejmu")
        *> parseLine "page header" mempty (parsec $ string "s." *> space *> some digitChar *> string "/" *> some digitChar)
        *> parseLine "strona" mempty (parsec $ count 4 digitChar *> string "-" *> count 2 digitChar *> string "-" *> count 2 digitChar)
        *> manyTill' (parseLine "footnotes" mempty Just) (parseFootnotes)
    parseFootnotes = do
      maybeContinuationFromPreviousPage <- option [] footnoteBody
      footnotes <- many footnote
      eof
      return $ maybeContinuationFromPreviousPage ++ concat footnotes
    footnote :: Parsec Dec [TEXT] [TEXT]
    footnote =
        -- looks like we are droping the footnote number
       parseLine "footnote" (All.(==6.48) . view (ltokens.to head.tfontsize)) (parsec $ some digitChar <* string ")") *> footnoteBody
    footnoteBody = some (parseLine "footnote body" (All .(==9.96). view (ltokens.to head.tfontsize)) Just)
    isSidenote :: TEXT -> Bool
    isSidenote line = line^.lx > 470 && firstTok^.tfontsize == 9.96 && firstTok^.tbold
      where
        firstTok :: WORD
        firstTok = head $ line^.ltokens
    toToks lines = [RawToken tk ln page | ln <- lines, tk <- _ltokens ln]
    parseLine :: String -> (TEXT->All) -> (TEXT->Maybe a) -> Parsec Dec [TEXT] a
    parseLine _what fp parser = -- TODO set the page in the SourcePos somehow?
      ftoken fp parser

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


-- recognize small structures that make the subsequnt main parse easier
-- for example `Art. 1.` is replaced by `NonTerminal SourcePosition (ArticleToken "1")`
recognizeNonTerminals :: [Tok] -> [Tok]
recognizeNonTerminals = concat . snd . mapAccumL go 82.2 . groupBy ((==) `on` lineId)
 where
    lineId :: Tok -> Float
    lineId (RawToken {_tline=tline}) = _ly tline
    lineId (NonTerminal _ (TableToken _)) = -1 -- tables are the only allowed non terminal, we ban it into -1 class
    lineId (NonTerminal _ _) = error "There should't be a NonTerminal's at this point yet"
    go :: Float -> [Tok] -> (Float, [Tok])
    go offset toks =
      case (runParser (runStateT parseLine offset) "pre-parsing line" toks) of
        Left err -> error $ "impossible, because nonTerminalsInLine always succeeds: " ++ show err
        Right (toks, offset) -> (offset, toks)
      where
        parseLine :: StateT Float (Parsec Dec [Tok]) [Tok]
        parseLine = do
          detectedNonTeminals <- nonTerminalsInALine
          unconsumed <- getInput
          return $ (detectedNonTeminals ++ unconsumed)

        -- we can have an `UstępToken` following an `ArticleToken`,
        -- otherwise there is at most 1 NonTerminal in a line
        nonTerminalsInALine :: StateT Float (Parsec Dec [Tok]) [Tok]
        nonTerminalsInALine =  artykuł
                           <|> (offset (==  0) >> ustęp)    -- 1.
                           <|> (offset (< -24) >> punkt)    -- 1)
                           <|> (offset (== 0)  >> podpunkt) -- a)
                           <|> (offset (> 0)   >> tiret)    -- -
                           <|> rozdział
                           <|> annex
                           <|> pure [] -- make sure it allways succeeds
          where
            artykuł = do
              tokPos <- position
              newArtXPos <- ftoken boldF (\t -> constT "Art." t >> preview (ttok . tx) t)
              put newArtXPos
              num <- T.init <$> ftoken boldF (anyT >=> guarded (('.'==) . T.last))
              -- handle the case where ustęp starts in the same line as the article
              mUstępTokens <- optional ustęp
              return $ (NonTerminal tokPos . ArticleToken $ num) : (join . toList $ mUstępTokens)
            ustęp = do
              tokPos <- position
              ustępNum <- ftoken mempty (anyT >=> guarded likeUstepNumber)
              return [NonTerminal tokPos . UstępToken . T.init $ ustępNum]
            punkt = do
              tokPos <- position
              punktNum <- ftoken mempty (anyT >=> guarded likePunktNumber)
              return [NonTerminal tokPos . PunktToken . T.init $ punktNum]
            podpunkt :: StateT Float (Parsec Dec [Tok]) [Tok]
            podpunkt = do
              tokPos <- position
              podpunktNum <- ftoken mempty (anyT >=> guarded likePodpunktNumber)
              return [NonTerminal tokPos . PodpunktToken . T.init $ podpunktNum]
            tiret = do
              tokPos <- position
              void $ ftoken mempty (constT "–")
              return [NonTerminal tokPos TiretToken]
            rozdział = do
              tokPos <- position
              ftoken mempty (constT "Rozdział")
              num <- ftoken mempty anyT
              eof
              return [NonTerminal tokPos (RozdziałToken num)]
            annex = do
              tokPos <- position
              ftoken mempty (constT "Załącznik")
              (pageNum, num) <- ftoken mempty $ \t -> (,) <$> preview (tpage.pnumber) t <*> anyT t
              eof
              return [NonTerminal tokPos $ AnnexToken pageNum num]

            likeUstepNumber text    = isDigit (T.head text) && T.last text == '.'
            likePunktNumber text    = isDigit (T.head text) && T.last text == ')'
            likePodpunktNumber text = isLower (T.head text) && T.last text == ')'

            -- run a predicate on the x offset from the enclosing Art
            offset :: (Float -> Bool) -> StateT Float (Parsec Dec [Tok]) ()
            offset predicateOnOffset = do
              artX <- get
              remainingInput <- getInput
              case remainingInput of
                RawToken {_ttok=tok}:_ ->
                  guard (predicateOnOffset $ tok^.tx - artX)
                _ -> mzero

position :: MonadParsec e [Tok] m => m TokPosition
position = do
  tokens <- getInput
  case tokens of
    [] -> mzero
    tok:_ -> return $ tokPosition tok

parseUstawa :: FilePath -> [FilePath] -> IO Akt
parseUstawa path vecFiles = do
  pages <- parsePages path
  tablesPerPage <- mapM processVecFilesPage vecFiles
  --putStrLn (nicify . show . filter ((==234). fst) $ tablesPerPage)
  let partitioned = partitionPages (take 80 pages)
      tokenStream = recognizeNonTerminals . insertTables tablesPerPage . processAdditionsAndRemovals $ _mainContent partitioned
  --writeFile "/tmp/foo" (nicify. ushow . mapMaybe (preview (_NonTerminal . _2 . _TableToken)) $ tokenStream)
  forceResult path $ toResult $ runParser tekstJednolity path tokenStream

tekstJednolity :: Parser Akt
tekstJednolity = do
  pid <- duPozId
  consumeWords "U S T A W A"
  zDnia <- consumeWords "z dnia" *> dlugaData
  tytul <- T.unwords <$> some (ftoken boldF (preview (ttok.ttext)))
  rozdzialy <- many rozdział -- TODO what about the case of no rozdzialy?
  annexes <- many annex
  let rozdzialIndex = map (view _1 &&& view _2) rozdzialy
  let spisTresci = map (view _1 &&& view _3) $ rozdzialy
  return Ustawa {
            _upId=pid
            , _uzDnia=zDnia
            , _uTytul=tytul
            , _uspisTresci = Partitions "Rozdział" rozdzialIndex spisTresci
            , _uarticles = M.fromList . concatMap (view _4) $ rozdzialy
            , _uannexes = annexes}

rozdział :: Parser (T.Text, T.Text, TableOfContents, [(T.Text, Article)])
rozdział = do
  number <- ftoken mempty (preview $ _NonTerminal . _2 . _RozdziałToken)
  podtytul <- T.unwords <$> some (ftoken boldF (preview (ttok.ttext)))
  articles <- some article
  return (number, podtytul, Articles (map fst articles), articles)

article :: Parser (T.Text, Article)
article = do
  number <- ftoken mempty (preview (_NonTerminal . _2 . _ArticleToken))
  prefix <- option emptyZWyliczeniem ustępBody
  ustępy <- many ustęp
  return $ (number, Article {_aprefix = prefix, _aindex = map fst ustępy, _apoints = M.fromList ustępy})


ustęp :: Parser (T.Text, ZWyliczeniem)
ustęp = do
  ustępNum <- ftoken mempty (preview $ _NonTerminal . _2 . _UstępToken)
  ustęp <- ustępBody
  return (ustępNum, ustęp)


ustępBody :: Parser ZWyliczeniem
ustępBody = do
  text <- textWithReferencesAndTables 0
  punkty <- many punkt
  suffix <- textWithReferencesAndTables 0
  return (ZWyliczeniem text (map fst punkty) (punkty) suffix)

punkt :: Parser (T.Text, ZWyliczeniem)
punkt = do
  num <- ftoken mempty (preview $ _NonTerminal . _2 . _PunktToken)
  body <- punktBody
  return (num, body)

punktBody :: Parser ZWyliczeniem
punktBody = do
  startX <- peekNextTokXPos
  wprowadzenie <- textWithReferencesAndTables startX
  podpunkty <- many podpunkt
  suffix <- textWithReferencesAndTables startX
  return (ZWyliczeniem wprowadzenie (map fst podpunkty) (podpunkty) suffix)

textWithReferencesAndTables :: Float -> Parser TextWithReferences
textWithReferencesAndTables indent =
  fmap (mergeTexts []) $ many $
    choice [Text  <$> ftoken (indentedByAtLeast indent) anyT
           ,Table <$> ftoken (view $ _NonTerminal . _2 . _TableToken . tix . to (All.(>=indent)))
                           (preview $ _NonTerminal . _2 . _TableToken . titable)
           ]
 where
    mergeTexts acc (Text t1:rest) = mergeTexts (t1:acc) rest
    mergeTexts [] [] = []
    mergeTexts acc [] = [Text (T.unwords (reverse acc))]
    mergeTexts acc (Table t:rest) = Text (T.unwords (reverse acc)) : Table t : mergeTexts [] rest


podpunkt :: Parser (T.Text, ZWyliczeniem)
podpunkt = do
  num <- ftoken mempty (preview $ _NonTerminal . _2 . _PodpunktToken)
  startX <- peekNextTokXPos
  text <- many $ ftoken (indentedByAtLeast startX) anyT
  tirety <- many (tiret startX)
  return (num, ZWyliczeniem
                  [Text . T.unwords $ text]
                  (map (const "-") tirety)
                  (map (const "-" &&& (\t -> ZWyliczeniem t [] mempty [])) $ tirety)
                  []) -- TODO, test podsumowujący "-"

tiret :: Float -> Parser TextWithReferences
tiret indent = do
  ftoken mempty (preview $ _NonTerminal . _2 .  _TiretToken)
  -- TODO, probably should detect tables even here
  texts <- many $ ftoken (indentedByAtLeast indent) anyT
  return [Text . T.unwords $ texts]

-- Don't atttempt to parse annexes boyond recognizing their location in the
-- pdf document.
annex :: Parser Annex
annex = do
  (pageNum, annexNum) <- annexToken
  -- skip to the last token in the annex, I am assuming here that there is
  -- at least one token in the annex
  skipMany (anyToken >> notFollowedBy (void annexToken <|> eof))
  lastPageOfAnnex <- _tpPageNum <$> position
  void anyToken
  return (Annex {_anum = annexNum, _astartPage = pageNum, _apages = lastPageOfAnnex - pageNum + 1})
 where annexToken = ftoken mempty (preview (_NonTerminal . _2 . _AnnexToken))

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
  return PozId {_pidYear=rok, _pidNr=nr, _pidSeq=seq}

consumeWords :: T.Text -> Parser ()
consumeWords ws = forM_ (words . T.unpack $ ws) (\w -> parsecTok mempty (string w))

dlugaData :: Parser Day
dlugaData = do
  components <- sequence . take 4 . repeat $ ftoken mempty (preview (ttok.ttext))
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
int = read <$> parsecTok mempty (some digitChar)

-- TODO can this be a performance problem?
-- this generalized the manyTill operator from Parsec lib
manyTill' :: MonadParsec e s m => m a -> m end -> m ([a], end)
manyTill' m end =
  ([],) <$> end
  <|> (m >>= \part -> first (part:) <$> manyTill' m end)


anyToken = ftoken (const mempty) Just

-- formatted token
ftoken :: MonadParsec e [t] m => (t -> All) -> (t -> Maybe a) -> m a
ftoken fp match =
  token (\t -> let mres = if getAll (fp t) then match t else Nothing in
               maybe (Left (mempty, mempty, mempty)) Right mres) Nothing

parsecTok :: (Tok->All) -> Parsec Dec T.Text a -> Parser a
parsecTok fp parser =
  ftoken fp
    (\t -> case runParser parser (show (tokPos t)) (t^.ttok.ttext) of
            Left _err -> Nothing
            Right res -> Just res)

instance HasTokPosition TEXT where
  tokPos TEXT {_ly=ly} = initialPos ("Line y " ++ show ly)

instance HasTokPosition Tok where
  tokPos (RawToken {_ttok=ttok, _tline=tline, _tpage=tpage}) =
    SourcePos ("Page " ++ (show $ tpage^.pnumber))
        (forcePos . round $ tline^.ly)
        (forcePos . round $ ttok^.tx)
  tokPos (NonTerminal tp _) =
    SourcePos ("Page " ++ (show $ tp^.tpPageNum))
        (forcePos . round $ tp^.tpLineY)
        (forcePos . round $ tp^.tpTokX)

forcePos :: Int -> Pos
forcePos x = forceEitherMsg ("forcePos: " ++ show x) . mkPos $ succ x

tokPosition (NonTerminal pos _) = pos
tokPosition (RawToken {_ttok=ttok, _tline=tline, _tpage=tpage}) =
  TokPosition (tpage^.pnumber) (tline^.ly) (ttok^.tx)

-- try a parsec text parser on a text like thing, yielding a Maybe if it is successful
parsec :: ToText t => Parsec Dec T.Text a -> t -> Maybe a
parsec parser = either (const Nothing) Just . runParser parser "TODO" . toText


class ToText t where
  toText :: t -> T.Text

instance ToText TEXT where
  toText t = T.unwords $ t^..ltokens.traverse.ttext
instance ToText Tok where
  toText t = t^.ttok.ttext

-- puts text from token stream into cells, puts tables into the token stream
insertTables :: [(Int, [TableInfo])] -> [Tok] -> [Tok]
insertTables tis = insertTables' (sortWith (second _tiy) (tis >>= \(p, tis) -> map (p,) tis))

insertTables' :: [(Int, TableInfo)] -> [Tok] -> [Tok]
insertTables' [] toks = toks
insertTables' ((page, ti):_) [] = [NonTerminal (TokPosition page 0 0) (TableToken (over titable reverseCellTexts ti))]
insertTables' allTis@((page, ti) : tis) allToks@(tok:toks)
  | (Just tokPage, Just toky) <- (tok^?tpage.pnumber, tok^?ttok.tmidY)
     , (page, _tiy ti + _tiheight ti) <= (tokPage, toky)
    = NonTerminal (tokPosition tok) (TableToken (over titable reverseCellTexts ti))  : insertTables' tis allToks
  | (Just tokPage, Just toky) <- (tok^?tpage.pnumber, tok^?ttok.tmidY), (page, _tiy ti) > (tokPage, toky)
    = tok : insertTables' allTis toks
  | otherwise = insertTables' ((page, insertIntoCell ti tok):tis) toks
 where
    insertIntoCell _ (NonTerminal _ _) = error "unexpected non terminal, expected a raw token for table cell"
    insertIntoCell ti RawToken {_ttok=ttok} =
        fromMaybe (error $ "no such cell " ++ show (row, col))
                  (failover (titable.tcell (row, col)._Just.tctext) ((Text $ ttok^.ttext):) ti)
      where x = _tmidX ttok
            y = _tmidY ttok
            -- 0 based coords in the table
            row = cellNo y (ti^.tirowEnds)
            col = cellNo x (ti^.ticolEnds)
            cellNo pos ends = length (takeWhile (<pos) ends)

-- when collecting the text into cells, I put them in reverse order, reorder them when putting the
-- complete table into the token stream
reverseCellTexts :: [[TableCell]] -> [[TableCell]] -- TODO Table
reverseCellTexts = map (map $ over tctext reverseAndProcess)
reverseAndProcess :: TextWithReferences -> TextWithReferences
reverseAndProcess = return . Text . T.unwords . reverse . mapMaybe (preview _Text)

-- let it be 0 based
tcell :: (Int, Int) -> Traversal' [[TableCell]] (Maybe TableCell)
tcell (row, col) = ix row . tcell col
  where tcell :: Int -> Lens' [TableCell] (Maybe TableCell)
        tcell col = lens (getCell (col+1)) (changeCell (col+1))
          where getCell _ [] = Nothing
                getCell col (c:cs)
                  | col <= _tccolSpan c = Just c
                  | otherwise = getCell (col - _tccolSpan c) cs
                changeCell :: Int -> [TableCell] -> Maybe TableCell -> [TableCell]
                changeCell _ [] _ = []
                changeCell col (c:cs) mNewCell
                  | col <= _tccolSpan c = maybeToList mNewCell ++ cs
                  | otherwise = c : changeCell (col - _tccolSpan c) cs mNewCell


buildTables :: [VGroup] -> [VClip] -> [TableInfo]
buildTables groups clips' =
  let (horizontal, vertical) =
       (tableBordersMap***tableBordersMap) . partitionEithers . map horizontalOrVertical $ groups
      clips = filter (\c -> _vcheight c < 840 || _vcwidth c < 590) clips'
      hasBorderAt intervalMap a b =
        any (not . null . IS.elems . flip IS.containing b) (IM.elems $ IM.containing intervalMap a)
      cellFrom tb clip =
        TableCell { _tcwidth = _vcwidth clip, _tcheight = _vcheight clip, _tctext=[]
                  , _tccolSpan = span (tb^.tbcolEnds) (_vcx clip) (_vcx clip + _vcwidth clip)
                  , _tcrowSpan = span (tb^.tbrowEnds) (_vcy clip) (_vcy clip + _vcheight clip)
                  , _tcborderTop    = hasBorderAt horizontal (_vcy clip)                   (_vcx clip + _vcwidth clip / 2)
                  , _tcborderBottom = hasBorderAt horizontal (_vcy clip + _vcheight clip)  (_vcx clip + _vcwidth clip / 2)
                  , _tcborderLeft   = hasBorderAt vertical   (_vcx clip)                   (_vcy clip + _vcheight clip / 2)
                  , _tcborderRight  = hasBorderAt vertical   (_vcx clip + _vcwidth clip)   (_vcy clip + _vcheight clip / 2)
                  }
      span ends from to =
        length . takeWhile (<= (to + 3)) . dropWhile (<= (from + 3)) $ ends
      visitClip [] clip = [TableBuilder {_tbxEdges = (_vcx clip, _vcx clip + _vcwidth clip)
                                        ,_tbrowEnds = [_vcy clip + _vcheight clip]
                                        ,_tbcolEnds = [_vcx clip + _vcwidth clip]
                                        ,_tbrows = [[clip]]}]
      visitClip (tb:tbs) clip = if adjecent tb clip then addClip tb clip : tbs else tb : visitClip tbs clip
      adjecent tb c = tb^.tbxEdges._1 <= _vcx c && _vcx c <= tb^.tbxEdges._2 + 5.2
                      && (clipExtendsLastRow c tb || any (isAround 2 (_vcy c)) (tb^.tbrowEnds))
      clipExtendsLastRow c tb
         | Just ylastrow <- tb^?tbrows._head._head.vcy = isAround 2 (_vcy c) ylastrow
         | otherwise = False
      addClip tb c = TableBuilder {_tbxEdges = (min (_vcx c) *** max (_vcx c + _vcwidth c)) (tb^.tbxEdges)
                                  ,_tbrowEnds = insertWithTolerance 2.0 (_vcy c + _vcheight c) (tb^.tbrowEnds)
                                  ,_tbcolEnds = insertWithTolerance 2.0 (_vcx c + _vcwidth c) (tb^.tbcolEnds)
                                  ,_tbrows = if clipExtendsLastRow c tb then over _head (c:) (tb^.tbrows)
                                                                        else [c] : tb^.tbrows }
      builders = foldl' visitClip [] (sortWith (_vcy &&& _vcx) clips)
      toTableInfo tb
        | tb^.tbrows.to length < 2 = Nothing
        | otherwise =
          let rs = reverse $ map reverse (tb^.tbrows)
              topLeftCell = head . head $ rs
              x = _vcx topLeftCell
              y = _vcy topLeftCell
          in
          Just TableInfo {
              _tix = x
            , _tiy = y
            , _tiwidth = (last . _tbcolEnds) tb - x
            , _tiheight = (last . _tbrowEnds) tb - y
            , _tirowEnds = _tbrowEnds tb
            , _ticolEnds = _tbcolEnds tb
            , _titable = map (map $ cellFrom tb) rs

          }
  in mapMaybe toTableInfo builders

isAround :: Float -> Float -> Float -> Bool
isAround tolerance val ref = val <= ref + tolerance && val >= ref - tolerance

insertWithTolerance :: Float -> Float -> [Float] -> [Float]
insertWithTolerance tolerance val list =
  if any (isAround tolerance val) list then list else L.insert val list

processVecFilesPage :: FilePath -> IO (Int, [TableInfo])
processVecFilesPage file = do
  (groups, clips) <- (partitionEithers . nub) <$> parseVectorialImagesFile file
  let pageNo = fromMaybe 0 $ preview (ix 1.vcpage) clips
  --  putStrLn (show $ map (\x -> map ( $ x) [_vcy, _vcx, _vcwidth, _vcheight]) $ sortWith (_vcy &&& _vcx) clips)
  return (pageNo, buildTables groups clips)


tableBordersMap :: [(IM.Interval Float, IM.Interval Float)]
                        -> IM.IntervalMap Float (IS.IntervalSet (IM.Interval Float))
tableBordersMap = IM.mapKeys expandInterval . foldr (\(k,v) -> IM.insertWith' IS.union k (IS.singleton v)) IM.empty
  where
    expandInterval i = IM.ClosedInterval (IS.lowerBound i - 1) (IS.upperBound i + 1)

-- VGroups are really rectangles filled with black reprezenting lines (that could be borders)
-- this decides if it is a vertical or a hirozontala line (Right - vertical, Left - horizontal)
-- the first interval is the span of the short end of the rectangle (edge xs in case of vertical)
horizontalOrVertical :: VGroup -> Either (IM.Interval Float, IM.Interval Float) (IM.Interval Float, IM.Interval Float)
horizontalOrVertical (VGroup {_vgpoints=points}) =
  if h > w
     then Right vertical
     else Left $ swap vertical
  where
    vertical = (IM.ClosedInterval (minimum xs) (maximum xs), IM.ClosedInterval (minimum ys) (maximum ys))
    w = maximum xs - minimum xs
    h = maximum ys - minimum ys
    xs = map fst points
    ys = map snd points


tableTests =
  testGroup "table building tests"
    [ testCase "no tables" $ do
        assertEqual "empty" [] (buildTables [] [])
        assertEqual "whole page clip" [] (buildTables [] [
          VClip {_vcx=0,_vcy=0,_vcwidth=595.32,_vcheight=841.92,_vcpage=12,_vcgroup=undefined}])
        assertEqual "whole page + side note" [] (buildTables [] [
          VClip {_vcx=0,_vcy=0,_vcwidth=595.32,_vcheight=841.92,_vcpage=12,_vcgroup=undefined},
          VClip {_vcx=485.28,_vcy=577.68,_vcwidth=89.4,_vcheight=92.282,_vcpage=13,_vcgroup=undefined}])
    , testCase "some tables" $ do
        assertEqual "table with one double row cell + side note "
          [TableInfo 37 577.68 102 80 [627.68, 657.68] [87, 139]
          [[nakedBox {_tcwidth=50,_tcheight=80,_tccolSpan=1,_tcrowSpan=2,_tctext=[]}
           ,nakedBox {_tcwidth=50,_tcheight=50,_tccolSpan=1,_tcrowSpan=1,_tctext=[]}]
          ,[nakedBox {_tcwidth=50,_tcheight=28,_tccolSpan=1,_tcrowSpan=1,_tctext=[]}]]]
          (buildTables [] [
             VClip {_vcx=37,_vcy=577.68,_vcwidth=50,_vcheight=80,_vcpage=13,_vcgroup=undefined}
            ,VClip {_vcx=89,_vcy=577.68,_vcwidth=50,_vcheight=50,_vcpage=13,_vcgroup=undefined}
            ,VClip {_vcx=89,_vcy=629.68,_vcwidth=50,_vcheight=28,_vcpage=13,_vcgroup=undefined}

            ,VClip {_vcx=485.28,_vcy=577.68,_vcwidth=89.4,_vcheight=92.282,_vcpage=13,_vcgroup=undefined}])
        assertEqual "col span"
          [TableInfo 0 0 100 100 [50, 100] [50, 100]
          [[nakedBox {_tcwidth=50,_tcheight=50,_tccolSpan=1,_tcrowSpan=1,_tctext=[]}
           ,nakedBox {_tcwidth=50,_tcheight=50,_tccolSpan=1,_tcrowSpan=1,_tctext=[]}]
          ,[nakedBox {_tcwidth=100,_tcheight=50,_tccolSpan=2,_tcrowSpan=1,_tctext=[]}]]]
          (buildTables [] [
             VClip {_vcx=0,_vcy=0,_vcwidth=50,_vcheight=50,_vcpage=13,_vcgroup=undefined}
            ,VClip {_vcx=50,_vcy=0,_vcwidth=50,_vcheight=50,_vcpage=13,_vcgroup=undefined}
            ,VClip {_vcx=0,_vcy=50,_vcwidth=100,_vcheight=50,_vcpage=13,_vcgroup=undefined}])
    ]

nakedBox = TableCell {_tcwidth= -1,_tcheight= -1,_tccolSpan=0,_tcrowSpan=0,_tctext=[],
                      _tcborderTop = False, _tcborderBottom = False, _tcborderLeft = False, _tcborderRight = False}
