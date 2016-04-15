all: 
	rm -rf submission
	mkdir submission
	cd src && make
	cd grade && make
	cd grade &&  ./Grade . ../submission
