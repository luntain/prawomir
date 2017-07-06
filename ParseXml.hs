module ParseXml where
-- Parse the xml obtaind from pdftoxml

import MyPrelude
import Data.Char (isDigit)
import qualified Data.Text as T
import Control.Lens hiding (deep)
import Text.XML.HXT.Core

data Page =
  Page { _pnumber :: Int, _pwidth, _pheight :: Float, _ptexts:: [TEXT]} deriving (Show, Read, Eq, Ord)

data TEXT =
  TEXT { _lx, _ly, _lwidht, _lheight:: Float, _ltokens :: [WORD]} deriving (Show, Read, Eq, Ord)

data WORD =
  WORD { _tx, _tmidX, _tmidY :: Float, _tbold:: Bool, _titalic::Bool, _tfontsize :: Float, _ttext :: T.Text} deriving (Show, Read, Eq, Ord)

makeLenses ''Page
makeLenses ''TEXT
makeLenses ''WORD

parsePages :: FilePath -> IO [Page]
parsePages path =
  runX (parseDoc path)

parseDoc :: FilePath -> IOSArrow a Page
parseDoc file =
  readDocument [withInputEncoding "UTF-8", withValidate no] file
  >>>
  deep (hasName "PAGE")
  >>>
  xunpickleVal xpPage

-- PU stands for pickler unpicker. We only use the unpickler (read) part since
-- we don't need to write the xml.

prunePage :: ArrowXml a => a XmlTree XmlTree
prunePage = removeAttr "id" >>> processChildren (hasName "TEXT")

xpPage :: PU Page
xpPage =
  xpFilterCont (prunePage >>> processChildren (neg isWhiteSpace)) $
  xpElem "PAGE" $
  xpWrap (uncurry4 Page, undefined) $ -- xpWrap is like fmap in two directions
  xp4Tuple (xpAttr "number" xpPrim)
  (xpAttr "width" xpPrim)
  (xpAttr "height" xpPrim)
  (xpList xpTEXT)

pruneText :: ArrowXml a => a XmlTree XmlTree
pruneText = removeAttr "id"

xpTEXT :: PU TEXT
xpTEXT =
  xpFilterCont (pruneText >>> processChildren (neg isWhiteSpace)) $
  xpElem "TEXT" $
  xpWrap (uncurry5 TEXT, undefined) $
  xp5Tuple (xpAttr "x" xpPrim)
  (xpAttr "y" xpPrim)
  (xpAttr "width" xpPrim)
  (xpAttr "height" xpPrim)
  (xpList xpToken)

pruneToken :: ArrowXml a => a XmlTree XmlTree
pruneToken =
  removeAttr "sid" >>> removeAttr "id" >>> removeAttr "font-name" >>> removeAttr "symbolic"
  >>> removeAttr "font-color" >>> removeAttr "rotation" >>> removeAttr "angle"
  >>> removeAttr "base" >>> removeAttr "serif" >>> removeAttr "fixed-width"

xpToken :: PU WORD
xpToken =
  xpFilterCont pruneToken $
  xpElem "TOKEN" $
  xpWrap (\(x, y, w, h, bold, italic, fs, text) -> WORD x (x+w/2) (y+h/2) bold italic fs text, undefined) $
  xp8Tuple (xpAttr "x" xpPrim)
  (xpAttr "y" xpPrim)
  (xpAttr "width" xpPrim)
  (xpAttr "height" xpPrim)
               (xpAttr "bold" xpYesNo)
               (xpAttr "italic" xpYesNo)
               (xpAttr "font-size" xpPrim)
               (xpWrap (T.pack, T.unpack) xpText)


xpYesNo :: PU Bool
xpYesNo = xpWrap (\case
                     "yes" -> True
                     "no" -> False
                     other -> error $"unrecognized yes/no value: " ++ other,
                  \case
                    True -> "yes"
                    False -> "no") xpText

uncurry5 f (a1, a2, a3, a4, a5) = f a1 a2 a3 a4 a5
uncurry6 f (a1, a2, a3, a4, a5, a6) = f a1 a2 a3 a4 a5 a6

prune :: ArrowXml a => a XmlTree XmlTree
prune =
  processTopDown (choiceA [
      hasName "TOKEN" :-> pruneToken
    , hasName "TEXT"  :-> pruneText
    , hasName "PAGE"  :-> prunePage
    , this :-> arr id])


-- this, when outside of clip, seems to represent lines
data VGroup = VGroup { _vgstyle :: Maybe String
                     , _vgclosed :: String
                     , _vgpoints :: [(Float, Float)] }
  deriving (Show, Read, Eq)

data Rectangle = Rectangle { _rx, _ry, _rw, rh :: Float }
  deriving (Show, Read)

xpVGroup :: PU VGroup
xpVGroup =
  xpFilterCont (removeAttr "sid" >>> removeAttr "clipZone") $
  xpElem "GROUP" $
  xpWrap (uncurry3 VGroup, undefined) $
    xpTriple (xpAttrImplied "style" xpText)
             (xpAttr "closed" xpText)
             (xpList xpVGroupPoint)

xpVGroupPoint :: PU (Float, Float)
xpVGroupPoint = xpElem "L" $ xpPair (xpAttr "x" xpPrim) (xpAttr "y" xpPrim)

data VClip = VClip { _vcx, _vcy, _vcwidth, _vcheight :: Float, _vcpage :: Int, _vcgroup :: VGroup}
  deriving (Show, Read, Eq)

makeLenses ''VClip

xpVClip :: PU VClip
xpVClip =
  xpFilterCont (removeAttr "idClipZone") $
  xpElem "CLIP" $
  xpWrap (uncurry6 VClip, undefined) $
    xp6Tuple (xpAttr "x" xpPrim)
             (xpAttr "y" xpPrim)
             (xpAttr "width" xpPrim)
             (xpAttr "height" xpPrim)
             (xpWrap (readPageNumberFromSid, undefined) (xpAttr "sid" xpText))
             xpVGroup

readPageNumberFromSid :: String -> Int
readPageNumberFromSid ('p' : rest) =
  read . takeWhile isDigit $ rest
readPageNumberFromSid rest = error $ "unexpected sid format, expected it to start with p: " ++ rest

parseVectorialImages :: FilePath -> IOSArrow a (Either VGroup VClip)
parseVectorialImages file =
  readDocument [withInputEncoding "UTF-8", withValidate no] file
  >>>
  deep (hasName "VECTORIALIMAGES")
  >>>
  getChildren
  >>>
  isElem
  >>>
  processTopDown ((hasName "M" >>> setElemName (mkName "L")) `orElse` arr id) -- normalize
  >>>
  ((hasName "GROUP" >>> xunpickleVal xpVGroup >>> arr Left) `orElse` (xunpickleVal xpVClip >>> arr Right))

parseVectorialImagesFile :: FilePath -> IO [Either VGroup VClip]
parseVectorialImagesFile file = runX (parseVectorialImages file)
