all: clean bnfc jlc

clean: bnfcclean jlcclean

bnfc:
	bnfc -d --functor contrib/Javalette.cf
	cd Javalette && happy -gca Par.y
	cd Javalette && alex -g Lex.x

jlc:
	cabal install --only-dependencies
	cabal build
	cp dist/build/jlc/jlc ../jlc

doc: bnfcclean bnfc
	cd Javalette && txt2tags -t tex Doc.txt
	cd Javalette && pandoc -s Doc.tex -o Doc.md
	cd Javalette && sed '1,12d' Doc.md > tmp; mv tmp Doc.md
	cd Javalette && sed 's/{.*}//g' Doc.md > tmp; mv tmp Doc.md
	cd Javalette && mv Doc.md ../../doc/bnfc.md

bnfcclean:
	rm -rf Javalette

jlcclean:
	rm -rf dist/build/jlc/jlc-tmp/
	rm -f dist/build/jlc/jlc
	rm -f ../jlc

test:
	@cabal build > /dev/null 2>&1
	@dist/build/jlc/jlc contrib/test.jl