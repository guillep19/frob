
default: williec

williec: Lexer.hs AttributeGrammar.hs Williec.hs Ast.hs CodeGenerator.hs Tokens.o Env.o Bytecode.hs Parser.hs
	ghc -c Lexer.hs AttributeGrammar.hs Ast.hs CodeGenerator.hs Bytecode.hs Parser.hs
	ghc Williec.hs -o williec
Lexer.hs: Lexer.x Tokens.o
	alex Lexer.x
Env.o:
	ghc -c Env.hs
Tokens.o:
	ghc -c Tokens.hs
AttributeGrammar.hs: AttributeGrammar.ag Makefile
	uuagc -dcfsrv --haskellsyntax -o AttributeGrammar.hs AttributeGrammar.ag
clean:
	rm -f *.hi *.o Lexer.hs AttributeGrammar.hs williec
test:
	williec ./Tests/example1.willie ./Tests/out/example1.alf
	williec ./Tests/example2.willie ./Tests/out/example2.alf
	williec ./Tests/example3.willie ./Tests/out/example3.alf
	williec ./Tests/example4.willie ./Tests/out/example4.alf
	williec ./Tests/example5.willie ./Tests/out/example5.alf
