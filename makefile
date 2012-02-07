#! /bin/bash

all: 
	make clean
	make doc
	#make hlint
	ghc -O2 --make TestDemos.hs 
	#make hlint
	@echo "Use \"make doc\" to make the FIPlib Documentation"
	@echo "Use \"make hlint\" to analyse the code for quality"

benchmarking: Benchmarking.hs
	ghc --make -O2 -rtsopts Benchmarking.hs

profile : Profile.hs FIPlib/Core.hs FIPlib/Filters.hs
	ghc --make -O2 -rtsopts Profile.hs -prof -auto-all -caf-all -fforce-recomp

dump: 
	make clean
	ghc -O2 --make -ddump-simpl-stats TestDemos.hs > dump.txt

doc: FIPlib/Core.hs FIPlib/Filters.hs
	haddock FIPlib/*.hs --ignore-all-exports -h -o FIPlib/doc

hlint: FIPlib/Core.hs FIPlib/Filters.hs
	hlint -c --report FIPlib/

clean:
	rm -f Profile
	rm -f TestDemos
	rm -f Benchmarking
	rm -f *.o
	rm -f *.hi
	rm -f FIPlib/*.o
	rm -f FIPlib/*.hi
	rm -f report.html
	rm -f dump.txt


