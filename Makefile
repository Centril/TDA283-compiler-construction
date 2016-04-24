all: clean submita

submita:
	mkdir -p lib
	mkdir -p submission
	tar -zcf submission/partA-1.tar.gz doc lib src

clean:
	rm -rf submission