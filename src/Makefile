all:
	rm -f ../../submission/jlc
	cd main && ghc -O2 Jlc.hs -o ../../submission/jlc

bnfc:
	rm -rf bnf
	mkdir bnf
	bnfc -m -haskell -o bnf contrib/Javalette.cf
	cd bnf && make && echo "int main() { return 0; }" |\
		./TestJavalette | sed -n 2p
