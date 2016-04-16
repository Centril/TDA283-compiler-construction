.PHONY: grade src

all: src grade

src:
	rm -rf submission
	mkdir submission
	cd src && make

grade:
	cd grade && make
	cd grade && ./Grade . ../submission