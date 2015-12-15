#include "functions.h"
#include "globals.h"
#include "vmcode.h"

void f_halt() {
  ip = 0;
}
//Binary
void f_add() {
  aux = *sp--;
  *sp = *sp + aux;
}

void f_sub() {
  aux = *sp--;
  *sp = *sp - aux;
}
void f_div() {
  aux = *sp--;
  *sp = *sp / aux;
}
void f_mul() {
  aux = *sp--;
  *sp = *sp * aux;
}
void f_op_and() {
  aux = *sp--;
  *sp = *sp & aux;
}
void f_op_or() {
  aux = *sp--;
  *sp = *sp | aux;
}
//Unary operators
void f_op_not() {
  *sp = !(*sp);
}
//Stack operations
void f_push() {
  *++sp = *ip++;
}
void f_pop() {
  sp--;
}
void f_dup() {
  sp++;
  *sp = *(sp-1);
}
//helper to call a function
void call_function(WORD location, WORD return_ip) {
  *++sp = fp; //store oldfp
  *++sp = return_ip; //store oldip
  fp = (WORD) (sp - stack); //create new frame
  ip = (WORD*) code + location; //jump to function
}
//Functions
void f_call() { //arg0, arg1, count ! oldfp, oldip
  WORD fun_location = *ip;
  WORD old_ip = (WORD) (++ip - (WORD*) code);
  call_function(fun_location, old_ip);
}
void f_ret() {
  WORD ret_value = *sp;
  sp = stack + fp; //go to beggining of current frame
  //restore ip
  WORD return_index = *sp--; 
  ip = (WORD*) code + return_index; //restore ip
  if (return_index == 0) ip = 0; //to stop run_thread //return ip is 0.
  //restore fp
  fp = *sp--;
  // go to outer frame
  sp = stack + fp;
  *++sp = ret_value; //push result to return
}
void f_load_param() {
  *++sp = stack[fp - inm - 2]; // 2 because of oldfp,oldip
}

//Jumps
void f_jump() {
  aux = *ip;
  ip = (WORD*) code + aux;
}
void f_jump_false() {
  aux = *ip++;
  if (!*sp--) ip = (WORD*) code + aux;
}
void f_cmp_eq() {
  aux = *sp--;
  if (aux == *sp--) *++sp = 1; else *++sp = 0;
}
void f_cmp_neq() {
  aux = *sp--;
  if (aux != *sp--) *++sp = 1; else *++sp = 0;
}
void f_cmp_gt() {
  aux = *sp--;
  if (*sp-- > aux) *++sp = 1; else *++sp = 0;
}
void f_cmp_lt() {
  aux = *sp--;
  if (*sp-- < aux) *++sp = 1; else *++sp = 0;
}
