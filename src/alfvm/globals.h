#ifndef _GLOBALS_H_
#define _GLOBALS_H_

#include "FrobDefinitions.h"
#include "graph.h"
//#include "serial.h"

extern WORD stack[256]; //512 bytes
extern WORD* sp;
extern WORD fp;
extern WORD* ip;
extern WORD instr;
extern BYTE op_code;
extern WORD inm;
extern WORD aux;
extern WORD aux2;
extern Graph graph;

extern void (*functions[])();

#endif /* _GLOBALS_H_ */
