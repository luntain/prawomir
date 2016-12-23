module Scratch where

import MyPrelude
import Text.StringTemplate
import Data.Aeson
import ParseXml

main :: FilePath -> IO ()
main vectorImagesFile = do
  rectangles <- (map prepareForDisplay . nub) <$> parseVectorialImagesFile vectorImagesFile
  template <- newSTMP <$> readFile "tool/visualize-vectorimages/template.html"
  let rectangles' = encode rectangles
  writeFile "/tmp/scratch.html" (toString (setAttribute "rectangles" rectangles' template))

--  rectangles :: [(String, [Float])]

prepareForDisplay :: Either VGroup VClip -> (String, [Float])
prepareForDisplay (Left vgroup) =
  ("group", [minimum xs, minimum ys, maximum xs - minimum xs, maximum ys - minimum ys])
  where xs = map fst (_vgpoints vgroup)
        ys = map snd (_vgpoints vgroup)
prepareForDisplay (Right vclip) =
  ("clip", snd . prepareForDisplay . Left . _vcgroup $ vclip)
