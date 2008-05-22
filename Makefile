# OPTIONS = "-f-TypeFamilies"
# OPTIONS = --with-ghc=ghc-6.6 --with-ghc-pkg=ghc-pkg-6.6
.PHONY: docs

build: all

all:
	@runhaskell Setup.lhs build

over: clean config all html

clean:
	@runhaskell Setup.lhs clean

config:
	@runhaskell Setup.lhs configure $(OPTIONS)

html:
	@runhaskell Setup.lhs haddock

sdist:
	@runhaskell Setup.lhs sdist

install:
	@runhaskell Setup.lhs install

test:
	find . -name "*.hs" | egrep -v '_darcs' | xargs qc

tags:
	find . -name "*.hs" | egrep -v '_darcs' | xargs hasktags -c
