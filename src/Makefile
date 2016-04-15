all:
	rm -f ../submission/jlc
	rm -f dist/build/jlc/jlc
	cabal build
	mv dist/build/jlc/jlc ../submission/jlc

test:
	@cabal build > /dev/null 2>&1
	@dist/build/jlc/jlc < contrib/test.jl

bnfc:
	rm -rf Javalette
	bnfc -d contrib/Javalette.cf
	cd Javalette && happy -gca Par.y
	cd Javalette && alex -g Lex.x
