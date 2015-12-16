
#include <stdio.h>

#include "FrobDefinitions.h"
#include "globals.h"
#include "vmcode.h"
#include "functions.h"
#include "frp.h"
#include "graph.h"

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

// Test nested function calls
void test_4() {
  sp = stack;
  call_function(99, 0);
  assert((sp-stack) == 2);
  call_function(99, 1);
  assert((sp-stack) == 4);
  call_function(99, 2);
  *++sp = 19;
  assert((sp-stack) == 7);
  f_ret();
  assert((ip-code) == 2);
  f_ret();
  assert((ip-code) == 1);
  f_ret();
  assert(ip == 0);
  assert(*sp == 19);
  assert((sp-stack) == 1);
}

void test_5() {
  // Test read
  sp = stack;
  graph = create_graph();
  *++sp = 5; // Input 5
  inm = 0; // Signal 0
  assert((sp-stack) == 1);
  f_read();
  assert((sp-stack) == 0);
  assert(graph_size(graph) == 1);
  assert(find_node(graph, 0) != -1);
  assert(find_node(graph, 0) == 0);
  // Test lift
  // lift id source function
  inm = 1;
  CODE test5code = {0, 19}; //source, function
  ip = (WORD*) test5code;
  f_lift();
  assert(graph_size(graph) == 2);
  assert(find_node(graph, 1) != -1);
  assert(find_node(graph, 1) == 1);
  // Test lift2:
  // lift2 id source1 source2 function
  inm = 2;
  CODE lift2args = {0, 1, 20};
  ip = (WORD*) lift2args;
  f_lift2();
  assert(graph_size(graph) == 3);
  assert(find_node(graph, 2) != -1);
  assert(find_node(graph, 2) == 2);
  // Test folds:
  // folds id, src f (start value in stack)
  *++sp = 10;
  inm = 3;
  CODE folds_args = {2, 29};
  ip = (WORD*) folds_args;
  f_folds();
  assert(graph_size(graph) == 4);
  assert(find_node(graph, 3) != -1);
  assert(find_node(graph, 3) == 3);
  // Test output:
  // output id (stack arg)
  *++sp = 1;
  inm = 2;
  f_write();
  assert(graph_size(graph) == 4);
}



int main() {
  test_1();
  test_2();
  test_3();
  test_4();
  return 0;
}
