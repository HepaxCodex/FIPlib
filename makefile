#! /bin/bash

all: 
	make clean
	make doc
	make hlint
	ghc -O2 --make TestDemos.hs 
	make hlint
	@echo "Use \"make doc\" to make the FIPlib Documentation"

benchmarking: Benchmarking.hs
	ghc -O2 --make Benchmarking.hs

dump: 
	make clean
	ghc -O2 --make -ddump-simpl-stats TestDemos.hs > dump.txt

doc: FIPlib/Core.hs FIPlib/Filters.hs
	haddock FIPlib/*.hs --ignore-all-exports -h -o FIPlib/doc

hlint: FIPlib/Core.hs FIPlib/Filters.hs
	hlint --report FIPlib/

clean:
	rm -f TestDemos
	rm -f Benchmarking
	rm -f *.o
	rm -f *.hi
	rm -f FIPlib/*.o
	rm -f FIPlib/*.hi
	rm -f report.html
	rm -f dump.txt


