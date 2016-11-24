{-# LANGUAGE Arrows #-}
module ParseXml where
-- Parse the xml obtaind from pdftoxml

import MyPrelude
import qualified Data.Text as T
import Control.Lens hiding (deep)
import Text.XML.HXT.Core

data Page =
  Page { _pnumber :: Int, _pwidth, _pheight :: Float, _ptexts:: [TEXT]} deriving (Show, Read)

data TEXT =
  TEXT { _lx, _ly, _lwidht, _lheight:: Float, _ltokens :: [Token]} deriving (Show, Read)

data Token =
  Token { _tx :: Float, _tbold:: Bool, _titalic::Bool, _tfontsize :: Float, _ttext :: T.Text} deriving (Show, Read)

makeLenses ''Page
makeLenses ''TEXT
makeLenses ''Token


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

xpPage :: PU Page
xpPage =
  xpFilterCont (removeAttr "id" >>> processChildren (hasName "TEXT")) $
  xpElem "PAGE" $
    xpWrap (uncurry4 Page, undefined) $ -- xpWrap is like fmap in two directions
    xp4Tuple (xpAttr "number" xpPrim)
             (xpAttr "width" xpPrim)
             (xpAttr "height" xpPrim)
             (xpList xpTEXT)

xpTEXT :: PU TEXT
xpTEXT =
  xpFilterCont (removeAttr "id" >>> processChildren (hasName "TOKEN")) $ -- remove text elements from formatting
  xpElem "TEXT" $
    xpWrap (uncurry5 TEXT, undefined) $
    xp5Tuple (xpAttr "x" xpPrim)
             (xpAttr "y" xpPrim)
             (xpAttr "width" xpPrim)
             (xpAttr "height" xpPrim)
             (xpList xpToken)

xpToken :: PU Token
xpToken =
  xpFilterCont (removeAttr "sid" >>> removeAttr "id" >>> removeAttr "font-name" >>> removeAttr "symbolic"
                >>> removeAttr "font-color" >>> removeAttr "rotation" >>> removeAttr "angle"
                >>> removeAttr "base" >>> removeAttr "serif" >>> removeAttr "fixed-width" >>> removeAttr "y"
                >>> removeAttr "width" >>> removeAttr "height") $
  xpElem "TOKEN" $
    xpWrap (uncurry5 Token, undefined) $
      xp5Tuple (xpAttr "x" xpPrim)
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
