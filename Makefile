Main: *.hs
	ghc Main.hs

.PHONY: clean

clean:
	rm Main *.o *.hi
