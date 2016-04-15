all: Grade

tester.tar.gz: Tests.py Grade.py testsuite
	tar zcf tester.tar.gz Tests.py Grade.py testsuite

%: %.hs *.hs
	ghc -main-is $* -threaded --make $<
clean:
	rm *.{hi,o} Grade
