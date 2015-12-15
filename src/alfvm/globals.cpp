
#include "globals.h"

WORD stack[256]; //256 bytes
WORD* sp = stack;
WORD fp = 0;
WORD* ip;
WORD instr = 0;
BYTE op_code = 0;
WORD inm = 0;
WORD aux = 0;
WORD aux2 = 0;

Graph graph;

WORD globals[64]; //128 bytes
