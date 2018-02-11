# Feram's Documentation

This repo contains Feram's documentation hosted at
[feram.io/documentation/j8PrKBwA/][docs-url]

It uses a custom theme to improve the design.
![Screenshot syntax highlighting][screenshot]

[docs-url]: https://feram.io/documentation/j8PrKBwA/
[screenshot]: ./images/2018-02-11_syntax_highligthing_browser.png


## Development

Install the "Velvet" Haddock theme:

```sh
git clone https://github.com/sourrust/velvet
cd velvet
npx grunt
```


Then run this to build the custom syntax highlithing theme:

```sh
./Documentation/Shakefile.hs
```


Finally build the documenatation:
(assuming this repo lies in Feram's root directory)

```fish
stack exec -- \
haddock \
  --html \
  --hyperlinked-source \
  --odir=./docs/j8PrKBwA \
  --theme=./velvet/build \
  --source-css=./node_modules/highlight.js/styles/monokai.css \
  --title="Feram's Documentation" \
  --prologue="./prologue.md" \
  (fd --exclude=migrate.hs "\.l?hs\$" "..")
```
