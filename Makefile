all: 
	cd src && make
	cd grade && make
	cd grade &&  ./Grade . ../submission
