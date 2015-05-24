#ifndef FROBDEFINITIONS_H
#define FROBDEFINITIONS_H

#include "mbed.h"

//VM ops
#define t_halt       0x00
//Functions
#define t_call       0x01
#define t_ret        0x02
#define t_load_param 0x03
//FRP combinators
#define t_lift       0x04
#define t_lift2      0x05
#define t_folds      0x06
//I/O operations
#define t_read       0x07
#define t_write      0x08

//Jumps
#define t_jump       0x09 //ip = *sp--;
#define t_jump_false 0x0a //ip = *sp--;
//Binary Boolean comparators
#define t_cmp_eq     0x0b //a=*sp--;*sp = (a == *sp--)
#define t_cmp_neq    0x0c //a=*sp--; *sp = (a != *sp--)
#define t_cmp_gt     0x0d //a=*sp--;*sp = (a > *sp--)
#define t_cmp_lt     0x0e //a=*sp--;*sp = (a < *sp--)
//Binary operators
#define t_add        0x0f //a=*sp--;*sp += a;
#define t_sub        0x10 //a=*sp--;*sp -= a;
#define t_div        0x11 //a=*sp--;*sp = *sp / a;
#define t_mul        0x12 //a=*sp--;*sp = *sp * a;
#define t_op_and     0x13 //a=*sp--;*sp = *sp & a;
#define t_op_or      0x14 //a=*sp--;*sp = *sp | a;
//Unary Boolean operators
#define t_op_not     0x15 //*sp = !(*sp)
//Stack operations
#define t_push       0x16 //*++sp = inmediate (0..255)
#define t_pop        0x17 //sp--
#define t_dup        0x18 //sp--
//Memory operations
#define t_store      0x19 //globals[inm] = *sp--
#define t_load       0x1a //*++sp = globals[inm]

typedef uint8_t BYTE;
typedef int16_t WORD;
typedef const WORD CODE[];

struct Node {
  BYTE id;
  WORD arg[2];
  BYTE arg_new[2]; //arg is new or used
  BYTE arg_count;
  WORD function_loc;
  WORD fwd[5]; //Nodes to fwd data
  BYTE fwd_place[5]; //where to fwd
  BYTE fwd_count;
  WORD value;
};

struct Input {
  WORD fwd[5];
  BYTE fwd_count;
};

struct Output {
  WORD source;
};

struct Graph {
  Input inputs[10];
  Output outputs[10];
  Node nodes[20];
  WORD count;
};


#endif /* FROBDEFINITIONS_H */
