# FreshHaddock

FreshHaddock lets you create a Haddock documentation site
for your Haskell projects with a modern look and feel.

It's powered by [Shake](https://shakebuild.com/manual)
and uses a custom theme to improve the design.

Haddock | Syntax Highlighting
--------|---------------------
![Landing page][landing] | ![Syntax highlighting][syntax]

[docs-url]: https://feram.io/documentation/j8PrKBwA/
[syntax]: ./images/2018-02-11_syntax_highligthing_browser.png
[landing]: ./images/2018-02-11_screenshot_documentation.png


## Usage

1. Clone the repository
1. Update the list of repos and packages in [`app/Main.hs`](./app/Main.hs)
1. Run `make build` to build the documentation site for all the packages
1. Check the `docs` folder for the generated documentation
