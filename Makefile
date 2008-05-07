.PHONY: docs

build: all

all:
	@runhaskell Setup.lhs build

over: clean config all html

clean:
	@runhaskell Setup.lhs clean

config:
	@runhaskell Setup.lhs configure

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
