#! /bin/bash

all:
	ghc -O2 --make TestDemos.hs 
	#echo -e "Use \"make doc\" to make the FIPlib Documentation"


doc:
	haddock FIPlib/*.hs --ignore-all-exports -h -o FIPlib/doc

clean:
	rm TestDemos
	rm *.o
	rm *.hi
	rm FIPlib/*.o
	rm FIPlib/*.hi
