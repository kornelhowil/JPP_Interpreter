main:
	cd src && bnfc --functor -d -m grammar.cf
	cd src && make
	ghc --make -outputdir build -o interpreter Main.hs -i.:src:src/Grammar
	
clean:
	rm -rf src/Grammar
	rm src/Makefile
	rm -rf build interpreter