%.prof: %.hs.perf
	gtimeout -s SIGINT 10 ./$< +RTS -p --RTS euler
%.hs.perf: %.hs
	ghc -prof -fprof-auto -rtsopts $< -o $@
clean:
	rm *.{hi,o,perf,prof}

