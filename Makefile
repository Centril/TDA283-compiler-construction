all: clean submita

clean:
	rm -rf submission

submita:
	mkdir -p submission
	tar -zcf submission/partA-1.tar.gz doc lib src