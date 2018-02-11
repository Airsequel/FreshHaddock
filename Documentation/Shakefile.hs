#! /usr/bin/env stack
{- stack
  script
  --resolver lts-10.4
  --package clay
  --package shake
  --package text
-}

{- |
Description: -
  Generate Feram's documentation living at
  <feram.io/docs/[secret]>
-}

module Documentation.Shakefile where

import Clay
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import Data.Text.Lazy (unpack)

import Documentation.Style.Code (stylesheet)


basePath = "docs/j8PrKBwA"
styleCodePath = basePath ++ "/src/style.css"


main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="shakecache"} $ do

  want [styleCodePath]

  styleCodePath %> \out -> do
    let src = "Documentation/Style/Code.hs"
    let css = renderWith pretty [] stylesheet
    need [src]
    writeFileChanged out (unpack css)

  phony "clean" $ do
    putNormal "Cleaning files in docs and cache directory"
    removeFilesAfter "docs" ["//*"]
    removeFilesAfter "shakecache" ["//*"]
