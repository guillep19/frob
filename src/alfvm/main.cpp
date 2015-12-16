#include "mbed.h"
#include "FrobDefinitions.h"
#include "IOInterface.h"
#include "graph.h"
#include "debug.h"
#include "vmcode.h"
#include "globals.h"
#include "functions.h"
#include "frp.h"
#include "vm.h"

void run_vm() {
  graph = create_graph();
  ip = (WORD*) code;
  run_thread();
  while (1) {
    read_inputs();
    update_signals();
    write_outputs();
  }
}

int main() {
  run_vm();
  return 0;
}
