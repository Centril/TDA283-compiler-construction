all:
	rm -f ../submission/jlc
	rm -f dist/build/jlc/jlc
	cabal build
	mv dist/build/jlc/jlc ../submission/jlc

bnfc:
	rm -rf Javalette
	bnfc -d contrib/Javalette.cf
	cd Javalette && happy -gca Par.y
	cd Javalette && alex -g Lex.x
