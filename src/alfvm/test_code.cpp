
#include <stdio.h>
#include <assert.h>

#include "FrobDefinitions.h"
#include "globals.h"
#include "vmcode.h"
#include "functions.h"
#include "frp.h"
#include "graph.h"
#include "vm.h"

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
  graph = create_graph();
  CODE test1code = {
    t_push << 8,
    6,
    t_push << 8,
    5,
    t_add << 8,
    t_halt << 8
  };
  ip = (WORD*) test1code;
  run_thread();
  assert(*sp == 11);
}

int main() {
  printf("Running tests...\n");
  test_1();
  return 0;
  printf("Tests OK\n");
}
