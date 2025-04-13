module Style.Code where

import Clay (
  Auto (auto),
  Color,
  Css,
  None (none),
  Normal (normal),
  Other (other),
  a,
  absolute,
  after,
  backgroundColor,
  black,
  body,
  borderBottomStyle,
  borderBottomWidth,
  borderWidth,
  brightness,
  bsColor,
  color,
  content,
  darken,
  em,
  filter,
  fontFamily,
  fontSize,
  fontWeight,
  height,
  hover,
  hsl,
  left,
  lighten,
  margin,
  marginTop,
  maxWidth,
  monospace,
  outline,
  padding,
  pct,
  position,
  pre,
  px,
  relative,
  shadowWithSpread,
  solid,
  span,
  stringContent,
  sym,
  sym2,
  textDecoration,
  white,
  width,
  yellowgreen,
  zIndex,
  (#),
  (&),
  (?),
 )
import Clay.Box (boxShadow)
import Clay.Flexbox (wrap)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Monoid ((<>))
import Prelude hiding (div, filter, rem, span)


-- | Define foreground colors
fgColor :: Color
fgColor = white


fgMuted :: Color
fgMuted = darken 0.05 fgColor


fgQuiet :: Color
fgQuiet = darken 0.1 fgColor


-- | Define background colors
bgColor :: Color
bgColor = black


bgMuted :: Color
bgMuted = lighten 0.05 bgColor


bgQuiet :: Color
bgQuiet = lighten 0.1 bgColor


bgSilent :: Color
bgSilent = lighten 0.15 bgColor


-- | Stylesheet for syntax highlighting of Haddock output
stylesheet :: Css
stylesheet = do
  body ? do
    backgroundColor bgMuted
    sym padding (em 0)
    sym margin (em 0)
    fontSize (px 12)
  -- zIndex (-20)

  pre ? do
    backgroundColor bgQuiet
    maxWidth (em 60)
    sym2 margin (em 0) auto
    sym2 padding (em 2) (em 2)
    fontWeight normal
    fontFamily ["Source Code Pro", "Inconsolata"] [monospace]
  -- zIndex (-10)

  a ? do
    textDecoration none

    span ? do
      position relative
      zIndex 1

      -- Because the `span` is in the `a` and not vice versa
      -- this workaround is necessary to change the color of the border
      after & do
        content (stringContent "")
        position absolute
        left (em 0)
        height (em 1)
        width (pct 100)
        borderBottomStyle solid
        borderBottomWidth (px 1)
        marginTop (px 2)
        filter (brightness 0.4)
        zIndex (-1)

  a # hover
    <> a # ".hover-highlight" ? do
      span ? do
        backgroundColor bgMuted

        -- Generate a fake padding with box-shadow and outline
        boxShadow $
          NonEmpty.singleton $
            bsColor (other "currentColor") $
              shadowWithSpread (em 0) (em 0) (em 0) (px 3)
        outline solid (px 2) bgMuted

        filter (brightness 1.4)
        after & do
          borderBottomWidth (px 0)

  ":target"
    <> ":target span" ? do
      backgroundColor bgMuted
      boxShadow $
        NonEmpty.singleton $
          bsColor fgQuiet $
            shadowWithSpread (em 0) (em 0) (em 10) (em 0.2)
      outline solid (px 2) bgMuted
      borderWidth (px 0)
      zIndex 10
      after & do
        borderBottomWidth (px 0)

  ".hs-identifier" ? do
    color fgQuiet

  ".hs-var" ? do
    color yellowgreen

  ".hs-type" ? do
    color (hsl 240 33 53)

  ".hs-keyword" ? do
    color (hsl 327 100 34)

  ".hs-string" ? do
    color (hsl 60 50 70)

  ".hs-char" ? do
    color (hsl 18 80 44)

  ".hs-number" ? do
    color (hsl 205 69 49)

  ".hs-operator" ? do
    color (hsl 331 64 52)

  ".hs-glyph" ? do
    color (hsl 1 71 52)

  ".hs-special" ? do
    color (hsl 1 71 52)

  ".hs-comment" ? do
    color (hsl 0 0 54)

  ".hs-pragma" ? do
    color (hsl 175 59 40)

  ".hs-cpp" ? do
    color (hsl 68 100 30)
