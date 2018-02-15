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
  <feram.io/documentation/[secret]>
-}

module Documentation.Shakefile where

import Clay (renderWith, compact)
import Control.Monad (void, liftM2)
import Data.Function ((&))
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import Data.Text.Lazy (unpack)
import Data.List (isSuffixOf)

import Documentation.Style.Code (stylesheet)


docsPath = "docs/j8PrKBwA"
docsIndex = docsPath ++ "/index.html"
styleCodePath = docsPath ++ "/src/style.css"
velvetReadmePath = "https://github.com/sourrust/velvet"
-- TODO: Automatically load from GitHub
repos =
  "api-server" :
  "base-image" :
  "cli" :
  "config" :
  "core" :
  "dashboard" :
  "design" :
  "documentation" :
  "feram-io" :
  "finances" :
  "fix-indefinite-article" :
  "fix-package-file" :
  "fix-target-blank" :
  "fix-typos" :
  "mirror-repos" :
  "modules" :
  "perspectra" :
  "repodash" :
  "tools" :
  "webapp" :
  "website" :
  "worker" :
  []


customShakeOptions :: ShakeOptions
customShakeOptions = shakeOptions
  { shakeColor = True  -- Print colors to terminal
  , shakeThreads = 0  -- Match number of processors
  , shakeTimings = True  -- Print runtime statistics
  , shakeFiles = "shakecache" -- Save cache files to `shakecache`
  -- Enable following line if you need to check out the report
  -- , shakeReport = ["shake-report.html"] -- Enable this
  }


{-
Build the documentation with Haddock.
Hide it in a secret directory to be able to host it publicly
without making it discoverable.
-}
buildDocsIndex :: FilePath -> Action ()
buildDocsIndex out = do
  hsFiles <- getDirectoryFiles "" ["repos//*.hs"]
  lhsFiles <- getDirectoryFiles "" ["repos//*.lhs"]

  let
    haskellFiles = (hsFiles ++ lhsFiles)
      & filter (not . (isSuffixOf "Setup.hs"))
      -- TODO: Remove after website/postgrest was improved
      & filter (not . (isSuffixOf "migrate.hs"))

  need haskellFiles

  cmd_ "stack exec -- haddock"
    "--html"
    "--hyperlinked-source"
    "--odir" [takeDirectory out]
    "--theme" ["./velvet/build"]
    "--title" ["Feram's Documentation"]
    "--prologue" ["./prologue.md"]
    haskellFiles

  -- Delete default syntax highlighting theme
  cmd_ "rm -f" [styleCodePath]

  need [styleCodePath]


-- | Clone and build Haddock theme "velvet"
getTheme :: FilePath -> Action ()
getTheme out = do
    cmd_ "rm -rf velvet"

    -- Get the "Velvet" Haddock theme
    cmd_ "git clone"
      "https://github.com/sourrust/velvet"
      "velvet"

    -- Install missing node.js dependencies for velvet
    cmd_ "cp package.json velvet/package.json"
    cmd_ (Cwd "velvet") "npm install"

    -- Build velvet theme
    cmd_ (Cwd "velvet") "npx grunt"


{- |
Build `docs` directory containing all documentation as static HTML
with Haddock.
-}
main :: IO ()
main = shakeArgs customShakeOptions $ do
  let gitHeads = fmap (("repos/" ++) . (++ "/.git/HEAD")) repos

  want ["all"]

  phony "all" $ do
    need gitHeads
    need ["velvet/package-lock.json"]
    need [docsIndex]


  "velvet/package-lock.json" %> getTheme


  -- Overwrite the default syntax highlighting theme
  styleCodePath %> \out -> do
    let src = "Documentation/Style/Code.hs"
    need [src]
    let css = renderWith compact [] stylesheet
    writeFileChanged out (unpack css)


  docsIndex %> buildDocsIndex


  phony "clean" $ do
    let
      dirsToDelete =
        ("docs", "//*") :
        ("node_modules", "//*") :
        ("repos", "//*") :
        ("shakecache", "//*") :
        ("velvet", "//*") :
        []
      deleteDir (dir, pattern) = do
        putNormal $ "Remove directory './" ++ dir ++ "'"
        removeFilesAfter dir [pattern]

    void $ forP dirsToDelete deleteDir


  "repos/*/.git/HEAD" %> \out -> do
    githubToken <- getEnv "GITHUB_TOKEN"

    let
      slug = takeDirectory $ takeDirectory out
      repoName = takeBaseName slug
      feramUrl = "github.com/feramhq/"
      url = case githubToken of
        Nothing -> "https://" ++ feramUrl
        Just token -> "https://" ++ token ++ ":x-oauth-basic@" ++ feramUrl

    cmd_ "rm -rf" [slug ++ "/.git"] -- Delete automatically generated stub
    cmd_ "git clone" [url ++ repoName] slug
