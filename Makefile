all: phreak

test: all
	cd test && ./test.rb && cd ..

test_write: all
	cd test && ./test.rb ENABLE_WRITE_MODE && cd ..

common:
	rebuild Common.native

phpsyntaxtreeprinter: phpsyntaxtree
		rebuild PHPSyntaxTreePrinter.native

phpsyntaxtree: common
	rebuild PHPSyntaxTree.native

phpparser: phpsyntaxtree
	rebuild -use-menhir phpparser.native

phplexer: phpparser
	rebuild phplexer.native

phreak: phpsyntaxtree phpsyntaxtreeprinter
	rebuild phreak.native
