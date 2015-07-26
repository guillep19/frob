PROJ_A = Block0HS
PROJ_B = BlockHS
PROJ_C = Block

# choose the default build
PROJ = $(PROJ_A)
#PROJ = $(PROJ_B)
#PROJ = $(PROJ_C)

#AG_OPTS = 
#AG_OPTS := --kennedywarren
AG_OPTS := -O

PROJ_ALL = $(PROJ_A) $(PROJ_B) $(PROJ_C)

default: block

block: $(PROJ).hs
	ghc --make -o block $(PROJ).hs

Block.hs: Block.ag Makefile
	uuagc -dcfsrv $(AG_OPTS) -o Block.hs Block.ag
	#uuagc -dcfsrv --visit --bangpats --strictdata --strictwrap -o Block.hs Block.ag

BlockHS.hs: BlockHS.ag Makefile
	uuagc -dcfsrv $(AG_OPTS) --haskellsyntax -o BlockHS.hs BlockHS.ag

Block0HS.hs: Block0HS.ag Makefile
	uuagc -dcfsrv $(AG_OPTS) --haskellsyntax -o Block0HS.hs Block0HS.ag

clean:
	rm -f block \
		Block.hs Block.o Block.hi \
		BlockHS.hs BlockHS.o BlockHS.hi \
		Block0HS.hs Block0HS.o Block0HS.hi \
		test1.cod test2.cod

test1: test1.blk block Makefile
	./block test1.blk test1.cod

test2: test2.blk block Makefile
	./block test2.blk test2.cod
