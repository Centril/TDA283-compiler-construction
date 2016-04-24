all: clean submita

submita: doc clean
	mkdir -p lib
	mkdir -p submission
	tar -zcf submission/partA-1.tar.gz doc lib src README.md

clean:
	rm -rf submission
	cd src && make doc
	cd src && make clean