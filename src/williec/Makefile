
default: williec

williec: Lexer.o AttributeGrammar.hs Main.hs Env.o Bytecode.o Parser.hs
	ghc -c AttributeGrammar.hs Parser.hs
	ghc -rtsopts Main.hs -o williec
Lexer.o: Lexer.hs
	ghc -c Lexer.hs
Lexer.hs: Lexer.x
	alex -o Lexer.hs -g Lexer.x
Env.o: Env.hs
	ghc -c Env.hs
Bytecode.o: Bytecode.hs
	ghc -c Bytecode.hs
Parser.o: AttributeGrammar.o Tokens.o
	ghc -c Parser.hs
AttributeGrammar.o: AttributeGrammar.hs
	ghc -c AttributeGrammar.hs
AttributeGrammar.hs: AttributeGrammar.ag Makefile
	uuagc -dcfsrv --haskellsyntax -o AttributeGrammar.hs AttributeGrammar.ag
clean:
	rm -f *.hi *.o Lexer.hs AttributeGrammar.hs williec williec.aux ./Tests/out/*.alf
test: williec test1 test2 test3 test4 test5
test1: williec
	./williec ./Tests/example1.willie ./Tests/out/example1.alf
test2: williec
	./williec ./Tests/example2.willie ./Tests/out/example2.alf
test3: williec
	./williec ./Tests/example3.willie ./Tests/out/example3.alf
test4: williec
	./williec ./Tests/example4.willie ./Tests/out/example4.alf
test5: williec
	./williec ./Tests/example5.willie ./Tests/out/example5.alf
test6: williec
	./williec ./Tests/example6.willie ./Tests/out/example6.alf
