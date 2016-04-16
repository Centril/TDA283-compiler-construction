.PHONY: clean bnfcClean distClean submissionClean

all: distClean submissionClean
	cabal build
	cp dist/build/jlc/jlc ../submission/jlc

test:
	@cabal build > /dev/null 2>&1
	@dist/build/jlc/jlc < contrib/test.jl

bnfc: contrib/Javalette.cf bnfcClean
	bnfc -d contrib/Javalette.cf
	cd Javalette && happy -gca Par.y
	cd Javalette && alex -g Lex.x

bnfcClean:
	rm -rf Javalette

distClean:
	rm -f dist/build/jlc/jlc

submissionClean:
	rm -f ../submission/jlc

clean: bnfcClean