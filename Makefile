all:
	rm submission.tar
	tar -zcvf submission.tar src doc lib
	python Grade.py submission.tar 3
