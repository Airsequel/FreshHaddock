# Feram's Documentation

This repo contains Feram's documentation hosted at
[feram.io/documentation/j8PrKBwA](https://feram.io/documentation/j8PrKBwA)


## Development

Install the "Velvet" Haddock theme:

```sh
git clone https://github.com/sourrust/velvet
cd velvet
npm install --save \
  grunt grunt-contrib-less grunt-contrib-copy grunt-contrib-watch
npx grunt
```


Then build the documenatation:
(assuming this repo lies in Feram's root directory)

```fish
stack exec -- \
haddock \
  --html \
  --hyperlinked-source \
  --odir=./docs/j8PrKBwA \
  --theme=./velvet/build \
  --title="Feram's Documentation" \
  --prologue="./prologue.md" \
  (fd --exclude=migrate.hs "\.l?hs\$" "..")
```
