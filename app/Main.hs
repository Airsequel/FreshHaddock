module Main where

import Clay (compact, renderWith)
import Control.Arrow ((<<<), (>>>))
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Bool (Bool (..), not)
import Data.Char (Char)
import Data.Function (($), (&))
import Data.Functor ((<&>))
import Data.List (filter, isSuffixOf, nub)
import Data.List.Extra (replace)
import Data.Monoid ((<>))
import Data.Text.Lazy qualified as TL
import Development.Shake (
  Action,
  CmdOption (Cwd),
  ShakeOptions (shakeColor, shakeFiles, shakeThreads, shakeTimings),
  cmd_,
  copyFile',
  forP,
  getDirectoryFiles,
  getEnv,
  need,
  phony,
  putNormal,
  readFile',
  removeFilesAfter,
  shakeArgs,
  shakeOptions,
  want,
  writeFileChanged,
  (%>),
 )
import Development.Shake.Command ()
import Development.Shake.FilePath (takeBaseName, takeDirectory)
import Development.Shake.Util ()
import System.Directory (getCurrentDirectory)
import System.Exit (die)

import Debug.Trace (traceShowId)
import Style.Code (stylesheet)


-- TODO: Fix broken packages
-- TODO: Automatically load from GitHub
packages :: [String]
packages =
  [ -- - Airsequel/AirGQL/
    -- - Airsequel/Airput/
    "Airsequel/SQLiteDAV/"
  , "Airsequel/graphql/"
  , "Airsequel/haskell-template/"
  , "Airsequel/servant-docs-blaze/"
  , "ad-si/Brillo/brillo"
  , "ad-si/Brillo/brillo-algorithms"
  , -- - ad-si/Brillo/brillo-examples
    "ad-si/Brillo/brillo-juicy"
  , "ad-si/Brillo/brillo-rendering"
  , "ad-si/Fuzzily/"
  , "ad-si/Haskram/"
  , "ad-si/Hikchr/"
  , -- - ad-si/Perspec/
    -- - ad-si/TaskLite/tasklite
    -- - ad-si/TaskLite/tasklite-core
    "ad-si/dear-imgui-test/"
  , "ad-si/uku/"
  ]


-- TODO: Automatically load from repos
extraDeps :: [String]
extraDeps =
  [ "dear-imgui-2.3.1"
  , "docopt-0.7.0.8"
  , "double-x-encoding-1.2.1"
  , "graphql-spice-1.0.6.0"
  , "hip-1.5.6.0"
  , "iso8601-duration-0.1.2.0"
  , "servant-options-0.1.0.0"
  , -- " , simple-sql-parser-0.7.1"
    "repa-3.4.2.0"
  ]


{-| Extract the base repositories from packages list.
| E.g. "ad-si/Brillo/brillo" -> "ad-si/Brillo"
-}
repos :: [String]
repos =
  packages <&> takeDirectory & nub


stackYamlTempPath :: String
stackYamlTempPath = "stack-temp.yaml"


docsPath :: String
docsPath = "docs"


docsIndex :: String
docsIndex = docsPath <> "/index.html"


styleCodePath :: String
styleCodePath = docsPath <> "/src/style.css"


velvetReadmePath :: String
velvetReadmePath = "https://github.com/sourrust/velvet"


customShakeOptions :: ShakeOptions
customShakeOptions =
  shakeOptions
    { shakeColor = True -- Print colors to terminal
    , shakeThreads = 0 -- Match number of processors
    , shakeTimings = True -- Print runtime statistics
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
    haskellFiles =
      (hsFiles <> lhsFiles)
        & filter (isSuffixOf "Setup.hs" >>> not)

  need haskellFiles
  need [stackYamlTempPath]

  cwd <- liftIO getCurrentDirectory

  cmd_
    ("stack haddock" :: String)
    ("--stack-yaml" :: String)
    [stackYamlTempPath :: String]
    ("--haddock-arguments" :: String)
    [ traceShowId $
        unwords
          [ "--title=Documentation"
          , "--theme=" <> cwd <> "/velvet/build"
          , "--odir=" <> cwd <> "/" <> takeDirectory out
          -- TODO: Figure out why this doesn't work
          --      https://github.com/commercialhaskell/stack/issues/6718
          -- , "--prologue=" <> cwd <> "/prologue.md"
          ]
    ]
    -- ("--open" :: String)
    (packages <&> ("./repos/" <>))

  -- Delete default syntax highlighting theme
  cmd_ ("rm -f" :: String) [styleCodePath]

  need [styleCodePath]


-- | Clone and build Haddock theme "velvet"
getTheme :: FilePath -> Action ()
getTheme out = do
  cmd_ ("rm -rf velvet" :: String)

  -- Get the "Velvet" Haddock theme
  cmd_
    ("git clone" :: String)
    ("https://github.com/sourrust/velvet" :: String)
    ("velvet" :: String)

  -- Install missing node.js dependencies for velvet
  copyFile' "package.json" "velvet/package.json"
  cmd_ (Cwd "velvet") ("npm install" :: String)

  -- Build velvet theme
  cmd_ (Cwd "velvet") ("npx grunt" :: String)


{-|
Build `docs` directory containing all documentation as static HTML
with Haddock.
-}
main :: IO ()
main = shakeArgs customShakeOptions $ do
  let gitHeads = repos <&> (("repos/" <>) <<< (<> "/.git/HEAD"))

  want ["all"]

  phony "all" $ do
    need gitHeads
    need ["velvet/package-lock.json"]
    need [docsIndex]

  "velvet/package-lock.json" %> getTheme

  -- Overwrite the default syntax highlighting theme
  styleCodePath %> \out -> do
    let src = "app/Style/Code.hs"
    need [src]
    let css = renderWith compact [] stylesheet
    writeFileChanged out (TL.unpack css)

  -- Write temporary stack.yaml file listing all packages
  stackYamlTempPath %> \out -> do
    -- Get resolver from stack.yaml
    stackYamlContent <- readFile' "stack.yaml"
    let stackYamlResolver = stackYamlContent & lines & head

    let
      toList :: String -> [String] -> String
      toList name xs =
        (name <> ":\n")
          <> (xs <&> ("  - " <>) & unlines)

      newContent =
        unlines
          [ stackYamlResolver
          , packages <&> ("./repos/" <>) & toList "packages"
          , toList "extra-deps" extraDeps
          , "allow-newer: true"
          ]

    writeFileChanged out newContent

  docsIndex %> buildDocsIndex

  phony "clean" $ do
    let
      dirsToDelete =
        [ ("docs", "//*")
        , ("node_modules", "//*")
        , ("repos", "//*")
        , ("shakecache", "//*")
        , ("velvet", "//*")
        ]
      deleteDir (dir, pattern) = do
        putNormal $ "Remove directory './" <> dir <> "'"
        removeFilesAfter dir [pattern]

    void $ forP dirsToDelete deleteDir

  "repos/*/*/.git/HEAD" %> \out -> do
    githubToken <- getEnv "GITHUB_TOKEN"

    let
      slug = takeDirectory $ takeDirectory out
      repoName = slug & replace "repos/" ""
      githubUrl = "github.com/"
      url = case githubToken of
        Nothing -> "https://" <> githubUrl
        Just token -> "https://" <> token <> ":x-oauth-basic@" <> githubUrl

    cmd_ ("rm -rf" :: String) [slug <> "/.git"] -- Delete automatically generated stub
    cmd_ ("git clone" :: String) [url <> repoName] slug
