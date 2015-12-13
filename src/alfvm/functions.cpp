#include "functions.h"
#include "globals.h"

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
