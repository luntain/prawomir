module Scratch where

import MyPrelude
import System.Directory
import Text.StringTemplate
import Data.Aeson
import ParseXml
import Parse
import Text.Nicify
import Text.Show.Unicode

main :: FilePath -> IO ()
main vectorImagesFile = do
  rectangles <- (map prepareForDisplay . nub) <$> parseVectorialImagesFile vectorImagesFile
  template <- newSTMP <$> readFile "tool/visualize-vectorimages/template.html"
  let rectangles' = encode rectangles
  writeFile "/tmp/scratch.html" (toString (setAttribute "rectangles" rectangles' template))

prepareForDisplay :: Either VGroup VClip -> (String, [Float])
prepareForDisplay (Left vgroup) =
  ("group", [minimum xs, minimum ys, maximum xs - minimum xs, maximum ys - minimum ys])
  where xs = map fst (_vgpoints vgroup)
        ys = map snd (_vgpoints vgroup)
prepareForDisplay (Right vclip) =
  ("clip", snd . prepareForDisplay . Left . _vcgroup $ vclip)


fromXml :: FilePath -> IO ()
fromXml path = do
  let dataDir = path ++ "_data"
  dataFiles <- listDirectory dataDir
  let vecFiles = map (dataDir </>) . filter (".vec" `isSuffixOf`) $ dataFiles
  akt <- parseUstawa path vecFiles
  putStrLn (nicify . ushow $ akt)
