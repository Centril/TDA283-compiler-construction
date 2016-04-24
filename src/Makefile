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

doc: docclean
	mkdir -p build/doc
	cat ../README.md > build/doc/README.md
	cd Javalette && pandoc -f t2t -t markdown_github\
		-o ../build/doc/bnf.md Doc.txt
	cd build/doc && sed '1,4d' bnf.md > tmp && mv tmp bnf.md
	cd build/doc && sed 's/\*\*|\*\*/\*\\\|\* /g'\
		bnf.md > tmp && mv tmp bnf.md
	cd build/doc && cat bnf.md >> README.md
	cd build/doc && pandoc -f markdown_github -t html5\
		-o README.html README.md
	cd build/doc && pandoc -f html -o latex\
		-o README.pdf README.html
	cd build/doc && cp README.* ../../../doc

bnfcclean:
	rm -rf Javalette

jlcclean:
	rm -rf dist/build/jlc/jlc-tmp/
	rm -f dist/build/jlc/jlc
	rm -f ../jlc

docclean: bnfcclean bnfc
	rm -rf build

test:
	@cabal build > /dev/null 2>&1
	@dist/build/jlc/jlc contrib/test.jl