.PHONY: help
help: makefile
	@tail -n +4 makefile | grep ".PHONY"


.PHONY: build
build:
	stack run fresh-haddock all


.PHONY: clean
clean:
	stack run fresh-haddock clean
