
#include "globals.h"
#include "functions.h"
#include "frp.h"

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

void (*functions[])() = {
  f_halt,
  //Functions
  f_call, f_ret, f_load_param,
  //FRP combinators
  f_lift, f_lift2, f_folds,
  // Input/Output operations
  f_read, f_write,
  f_jump, f_jump_false,
  f_cmp_eq, f_cmp_neq, f_cmp_gt, f_cmp_lt,
  //Binary operators
  f_add, f_sub, f_div,
  f_mul, f_op_and, f_op_or,
  //Unary operators
  f_op_not,
  //Stack operations
  f_push, f_pop, f_dup,
};
