all: phreak

test: all
	cd test && ./test.rb && cd ..

test_write: all
	cd test && ./test.rb ENABLE_WRITE_MODE && cd ..

common:
	rebuild -use-ocamlfind src/Common.native

phpsyntaxtreeprinter: phpsyntaxtree
	rebuild -use-ocamlfind src/PHPSyntaxTreePrinter.native

phpsyntaxtree: common
	rebuild -use-ocamlfind src/PHPSyntaxTree.native

phpparser: phpsyntaxtree
	rebuild -use-ocamlfind -use-menhir src/phpparser.native

phplexer: phpparser
	rebuild -use-ocamlfind src/phplexer.native

phreak: phpsyntaxtree phpsyntaxtreeprinter
	rebuild -use-ocamlfind src/phreak.native
