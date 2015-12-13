//#include <stdio.h>
//#include <stdint.h>

#include "mbed.h"
#include "FrobDefinitions.h"
#include "IOInterface.h"
#include "graph.h"
#include "debug.h"
#include "vmcode.c"
#include "globals.h"
#include "functions.h"

WORD globals[64]; //128 bytes
//Total:1/2 kb (arduino nano tiene 2kb)

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
  sp = stack + fp; //remove current frame
  WORD return_index = *sp--; 
  ip = (WORD*) code + return_index; //restore ip
  if (return_index == 0) ip = 0; //to stop run_thread when 
                                 //return ip is 0. HACK
  fp = *sp--; //restore fp
  aux = *sp--; //get count to pop arguments
  sp -= aux; //remove arguments
  *++sp = ret_value; //push result to return
}
void f_load_param() {
  *++sp = stack[fp - inm - 3]; //3 because of count,oldfp,oldip
}
//FRP combinators
void f_lift() {
    //lift inm=id word=source word=function_loc
    WORD id = inm;
    WORD source = *ip++;
    WORD function_loc = *ip++;
    WORD source_pos = find_node(graph, source);
    if ((source_pos == -1)||(find_node(graph, id) != -1)) {
      return;
    }
    WORD dest_pos = create_node(graph, id, function_loc, 1);
    link_nodes(graph, source_pos, dest_pos, 0);
  }
  void f_lift2() {
    WORD id = inm;
    WORD s1 = *ip++;
    WORD s2 = *ip++;
    WORD function_loc = *ip++;
    WORD s1_pos = find_node(graph, s1);
    WORD s2_pos = find_node(graph, s2);
    if ((s1_pos == -1)||(s2_pos == -1)) {
      return;
    }
    if (find_node(graph, id) != -1) {
      return;
    }
    WORD dest_pos = create_node(graph, id, function_loc, 2);
    link_nodes(graph, s1_pos, dest_pos, 0);
    link_nodes(graph, s2_pos, dest_pos, 1);
  }
  void f_folds() {
    BYTE id = inm;
    BYTE source = *ip++;
    WORD acum = globals[*ip++];
    WORD function_loc = *ip++;
    WORD source_pos = find_node(graph, source);
    if ((source_pos == -1) || (find_node(graph, id) != -1)) {
      return;
    }
    WORD node_pos = create_fold_node(graph, id, function_loc, acum);
    link_nodes(graph, source_pos, node_pos, 0);
}
//IO
void f_read() {
  //read inm=id word=input
  WORD id = inm;
  WORD input = globals[*ip++];
  //add graph node
  WORD node_pos = create_node(graph, id, -1, 1);
  //link input to node
  link_input(graph, input, node_pos);
}
void f_write() {
  //write inm=id word=output
  WORD id = inm;
  WORD output = *ip++;
  WORD node_pos = find_node(graph, id);
  if (node_pos == -1) {
    return;
  }
  link_output(graph, output, id);
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

//Memory operations
void f_store() {
  globals[inm] = *sp--;
}
void f_load() {
  *++sp = globals[inm];
}

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
  // Memory operations
  f_store, f_load
};

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

void propagate_signal(BYTE id) {
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

void update_signal(BYTE id) {
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
  propagate_signal(id);
}

void update_signals() {
  while (graph.ready_next != graph.ready_end) {
    update_signal(graph.ready_nodes[graph.ready_next++]);
    if (graph.ready_next == 20) graph.ready_next = 0;
  }
}

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
