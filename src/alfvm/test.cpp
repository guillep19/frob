
#include <stdio.h>

#include "FrobDefinitions.h"
#include "globals.h"
#include "vmcode.h"
#include "functions.h"

#include "assert.h"

void printStack() {
  printf("Stack: ");
  WORD* p = (WORD*) stack;
  if (p != sp) {
    p++;
    for (; p != sp; p++) {
      printf("-> [%d] ", *p);
    };
    printf("-> [%d] -x", *sp);
  } else {
    printf("-x");
  }
  printf("\n");
}

void test_1() {
  sp = stack;
  *++sp = 5;
  *++sp = 6;
  f_add();
  assert(*sp == 11);
}
void test_2() {
  sp = stack;
  *++sp = 5;
  *++sp = 6;
  f_mul();
  assert(*sp == 30);
}
void test_3() {
  sp = stack;
  // Put argument (19)
  *++sp = 19; //arg 0
  call_function(99, 0);
  assert((ip-code) == 99);
  assert((sp-stack) == 3);
  // Load first argument:
  inm = 0;
  f_load_param();
  printStack();
  assert(*sp == 19);
  assert((sp-stack) == 4);
  // Put the result in the stack:
  *++sp = 50;
  // 19 -> fp=0 -> ip=0 -> 19 -> 50
  assert((sp-stack) == 5);
  // Return from the function:
  f_ret();
  // IP must be 0:
  assert(*sp == 50);
  assert(ip == 0);
  assert((sp-stack) == 1);
}

int main() {
  test_1();
  test_2();
  test_3();
  return 0;
}
