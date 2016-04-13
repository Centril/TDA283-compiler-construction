all:
	bnfc -m -haskell -o bnf contrib/Javalette.cf
	cd bnf && make && echo "int main() { return 0; }" |\
		./TestJavalette | sed -n 2p
