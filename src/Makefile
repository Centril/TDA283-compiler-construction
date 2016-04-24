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
	cd Javalette && pandoc -f t2t -t markdown_github -o Doc.md Doc.txt
	cd Javalette && sed '1,12d' Doc.md > Doc.sed
	cd Javalette && sed 's/\*\*|\*\*/\*\\\|\* /g' Doc.sed > Doc.md
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