Main: *.hs
	ghc -O2 Main.hs

.PHONY: clean

clean:
	rm Main *.o *.hi
