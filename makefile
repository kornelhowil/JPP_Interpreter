main:
	ghc --make -outputdir build -o interpreter Main.hs -i.:src:src/parser

clean:
	rm -rf build interpreter