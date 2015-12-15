#ifndef _GLOBALS_H_
#define _GLOBALS_H_

#include "FrobDefinitions.h"
#include "graph.h"
//#include "serial.h"

extern WORD stack[256];
extern WORD* sp;
extern WORD fp;
extern WORD* ip;
extern WORD instr;
extern BYTE op_code;
extern WORD inm;
extern WORD aux;
extern WORD aux2;
extern Graph graph;

//TODO: Borrar
extern WORD globals[64];

//Total:1/2 kb (arduino nano tiene 2kb)

#endif /* _GLOBALS_H_ */
