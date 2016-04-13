all: 
	cd src && make

test:
	cd src && make
	python Tests.py --compiler ./jlc \
		--submission A --testsuite testsuite

grade:
	cd src && make
	rm -f submission.tar
	tar -zcvf submission.tar src doc lib
	python Grade.py submission.tar 3
