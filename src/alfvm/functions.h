#ifndef _FUNCTIONS_H_
#define _FUNCTIONS_H_

#include "FrobDefinitions.h"

void f_halt();
void f_add();

void f_sub();
void f_div();
void f_mul();
void f_op_and();
void f_op_or();
//Unary operators
void f_op_not();
//Stack operations
void f_push();
void f_pop();
void f_dup();
//Functions
void f_call();
void f_ret();
void f_load_param();
void call_function(WORD location, WORD return_ip);
//Jumps
void f_jump();
void f_jump_false();
void f_cmp_eq();
void f_cmp_neq();
void f_cmp_gt();
void f_cmp_lt();

#endif /* _FUNCTIONS_H_ */
