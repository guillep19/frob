#include "vm.h"
#include "globals.h"
#include "IOInterface.h"
#include "frp.h"
#include "functions.h"

void run_thread() {
  while (ip) {
    instr = *ip++;
    op_code = (0xff00 & instr) >> 8;
    inm = 0x00ff & instr;
    (functions[op_code])();
  }
}

void read_inputs() {
  /* Finds out whether there is an input waiting to
     be read. If there is any, it reads it, pushes
     the result in the stack, and returns the ip
     stored in inputs[index], else returns NULL */
  WORD value, iter;
  BYTE fwd_iter, fwd_count;
  for (iter = 0; iter < 10; iter++) {
    fwd_count = graph.inputs[iter].fwd_count;
    if (fwd_count > 0) {
      value = read_input(iter);
      for (fwd_iter = 0; fwd_iter < fwd_count; fwd_iter++) {
        // Give the waiting signal the value.
        WORD id = graph.inputs[iter].fwd[fwd_iter];
        graph.nodes[id].arg[0] = value;
        graph.nodes[id].arg_new[0] = true;
        // Mark the waiting signal as ready.
        graph.ready_nodes[graph.ready_end++] = id;
        if (graph.ready_end == 20) graph.ready_end = 0;
      }
    }
  }
}

void write_outputs() {
  WORD source, iter;
  for (iter = 0; iter < 10; iter++) {
    source = graph.outputs[iter].source;
    if (source != -1) {
      WORD value = graph.nodes[source].value;
      write_output(iter, value);
    }
  }
}

void _propagate_signal(BYTE id) {
  BYTE fwd_count = graph.nodes[id].fwd_count;
  if (fwd_count > 0) {
    WORD value = graph.nodes[id].value;
    for (BYTE fwd_iter = 0; fwd_iter < fwd_count; fwd_iter++) {
      // Give the waiting signal the value.
      WORD fwd = graph.nodes[id].fwd[fwd_iter];
      BYTE fwd_place = graph.nodes[id].fwd_place[fwd_iter];
      graph.nodes[fwd].arg[fwd_place] = value;
      graph.nodes[fwd].arg_new[fwd_place] = true;
      // Mark the waiting signal as ready. (if every value is new)
      BYTE ready = 1;
      for (BYTE place = 0; (ready&&(place < graph.nodes[fwd].arg_count)); place++) {
        if (!(graph.nodes[fwd].arg_new[place])) {
          ready = 0;
        }
      }
      if (ready) {
        graph.ready_nodes[graph.ready_end++] = fwd;
        if (graph.ready_end == 20) graph.ready_end = 0;
      }
    }
  }
}

void _update_signal(BYTE id) {
  /* Improvement: Add memoization */
  WORD function_loc = graph.nodes[id].function_loc;
  if (function_loc == -1) { //no function (id)
    graph.nodes[id].value = graph.nodes[id].arg[0];
  } else {
    //push args and arg_count
    BYTE arg_count = graph.nodes[id].arg_count;

    if (graph.nodes[id].is_fold) { //ONLY FOR FOLD
      *++sp = graph.nodes[id].value;
    }

    for (BYTE iter = 0; iter < arg_count; iter++) {
      graph.nodes[id].arg_new[iter] = 0;
      WORD value = graph.nodes[id].arg[iter];
      *++sp = value;
    }

    if (graph.nodes[id].is_fold) arg_count++; //ONLY FOR FOLD

    *++sp = (WORD) arg_count;
    call_function(function_loc, 0);
    run_thread();
    graph.nodes[id].value = *sp--;
  }
  _propagate_signal(id);
}

void update_signals() {
  while (graph.ready_next != graph.ready_end) {
    _update_signal(graph.ready_nodes[graph.ready_next++]);
    if (graph.ready_next == 20) graph.ready_next = 0;
  }
}
