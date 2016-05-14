all: clean submitb

submita: submit
	tar -zcf submission/partA-1.tar.gz doc lib src README.md LICENSE.md

submitb: submit
	tar -zcf submission/partB-1.tar.gz doc lib src README.md LICENSE.md

submit: doc clean
	mkdir -p lib
	mkdir -p submission

testa: test
	cd grade && ./Grade . ../submission

testb: test
	cd grade && ./Grade -b LLVM . ../submission

test:
	mkdir -p submission
	cd src && make jlc
	cp jlc submission/
	cp -r lib submission/
	cd grade && make

clean:
	rm -rf submission
	cd src && make doc
	cd src && make clean