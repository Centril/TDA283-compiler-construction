all: clean submita

submita: doc clean
	mkdir -p lib
	mkdir -p submission
	tar -zcf submission/partA-1.tar.gz doc lib src README.md

doc:
	cd src && make doc

clean:
	rm -rf submission
	cd src && make clean