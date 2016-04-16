all:
	cabal build
	[[ -e dist/build/Grade/Grade ]] && cp -u dist/build/Grade/Grade Grade
	[[ -e dist/build/Grade/Grade.exe ]] && cp -u dist/build/Grade/Grade.exe Grade.exe

clean:
	cabal clean
	rm -f Grade.exe
	rm -f Grade