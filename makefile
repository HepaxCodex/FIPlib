#! /bin/bash

all: $(clean)
	ghc -O2 --make TestDemos.hs 
	@echo "Use \"make doc\" to make the FIPlib Documentation"

dump: $(clean)
	ghc -O2 --make -ddump-simpl TestDemos.hs > dump.txt

doc:
	haddock FIPlib/*.hs --ignore-all-exports -h -o FIPlib/doc

clean:
	rm TestDemos
	rm *.o
	rm *.hi
	rm FIPlib/*.o
	rm FIPlib/*.hi
