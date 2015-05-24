//#include <stdio.h>
//#include <stdint.h>

#include "mbed.h"
#include "FrobDefinitions.h"
#include "IOInterface.h"
Serial pc(USBTX, USBRX); // tx, rx

#include "vmcode.c"

WORD stack[128] = {0}; //256 bytes
WORD globals[64]; //128 bytes
//Total:1/2 kb (arduino nano tiene 2kb)
WORD* sp = stack;
WORD fp = 0;
WORD* ip;

WORD instr = 0;
BYTE op_code = 0;
WORD inm = 0;
WORD aux = 0;
WORD aux2 = 0;

Graph graph;

//VM ops
void f_halt() {
  ip = 0;
  //pc.printf("halt");
}

//helper to call a function
void call_function(WORD location, WORD return_ip) {
  *++sp = fp; //store oldfp
  *++sp = return_ip; //store oldip
  fp = (WORD) (sp - stack); //create new frame
  ip = (WORD*) code + location; //jump to function
  //pc.printf("call_function location=%d return_ip=%d:\n", location, return_ip);
}
//Functions
void f_call() {
  //arg0, arg1, count ! oldfp, oldip
  WORD fun_location = *ip;
  WORD old_ip = (WORD) (++ip - (WORD*) code);
  call_function(fun_location, old_ip);
}
void f_ret() {
  WORD ret_value = *sp;
  sp = stack + fp; //remove current frame
  ip = (WORD*) code + *sp--; //restore ip
  fp = *sp--; //restore fp
  aux = *sp--; //get count to pop arguments
  sp -= aux; //remove arguments
  *++sp = ret_value; //push result to return
  //pc.printf("ret:");
}
void f_load_param() {
  *++sp = stack[fp - inm - 3]; //3 because of count,oldfp,oldip
  //pc.printf("load_param: inm=%d", inm);
}
//FRP combinators
void f_lift() {
  //lift inm=id word=source word=function_loc
  WORD id = inm;
  WORD source = *ip++;
  WORD function_loc = *ip++;
  WORD iter_s = 0;
  pc.printf("lift: source=%d id=%d fun=%d\n", source, id, function_loc);
  while ((iter_s < graph.count) && (graph.nodes[iter_s].id != source)) iter_s++;
  if (iter_s == graph.count) {
    pc.printf("ERROR: unexistant source=%d\n", source);
    return;
  }
  WORD iter_d = 0;
  while ((iter_d < graph.count) && (graph.nodes[iter_d].id != id)) iter_d++;
  if (iter_d != graph.count) {
    pc.printf("ERROR: destination already exists=%d\n", inm);
    return;
  }
  graph.nodes[iter_s].fwd[graph.nodes[iter_s].fwd_count] = iter_d;
  graph.nodes[iter_s].fwd_place[graph.nodes[iter_s].fwd_count++] = 0;
  graph.nodes[graph.count].id = id; //define signal id
  graph.nodes[graph.count].arg_count = 1;
  graph.nodes[graph.count].function_loc = function_loc;
  graph.nodes[graph.count].fwd_count = 0;
  graph.count++;
}
void f_lift2() {
  BYTE s1 = *ip++;
  BYTE s2 = *ip++;
  WORD iter1 = 0;
  WORD iter2 = 0;
  WORD function_loc = *ip++;
  while ((iter1 < graph.count) && (graph.nodes[iter1].id != s1)) iter1++;
  while ((iter2 < graph.count) && (graph.nodes[iter2].id != s2)) iter2++;
  if ((iter1 == graph.count)||(iter2 == graph.count)) {
    pc.printf("lift2: source_1=%d source_2=%d dest=%d\n", s1, s2, inm);
    pc.printf("ERROR: unexistant source\n");
    return;
  }
  WORD iter_d = 0;
  while ((iter_d < graph.count) && (graph.nodes[iter_d].id != inm)) iter_d++;
  if (iter_d != graph.count) {
    pc.printf("lift2: source_1=%d source_2=%d dest=%d\n", s1, s2, inm);
    pc.printf("ERROR: destination already exists=%d\n", inm);
    return;
  }
  graph.nodes[iter1].fwd[graph.nodes[iter1].fwd_count] = iter_d;
  graph.nodes[iter1].fwd_place[graph.nodes[iter1].fwd_count++] = 0;
  graph.nodes[iter2].fwd[graph.nodes[iter2].fwd_count] = iter_d;
  graph.nodes[iter2].fwd_place[graph.nodes[iter2].fwd_count++] = 1;
  graph.nodes[graph.count].id = inm;  //define signal inm
  graph.nodes[graph.count].arg_count = 1;
  graph.nodes[graph.count].function_loc = function_loc;
  graph.nodes[graph.count].fwd_count = 0;
  graph.count++;
  pc.printf("lift2: source_1=%d source_2=%d dest=%d\n", s1, s2, inm);
}
void f_folds() {
  BYTE s1 = *ip++;
  pc.printf("folds: source=%d dest=%d\n", s1, inm);
}
//IO
void f_read() {
  //read inm=id word=source
  WORD id = inm;
  WORD source = *ip++;
  BYTE count = graph.inputs[globals[source]].fwd_count;
  graph.inputs[globals[source]].fwd[count++] = graph.count;
  graph.inputs[globals[source]].fwd_count = count;

  //add graph node
  graph.nodes[graph.count].id = id;
  graph.nodes[graph.count].arg_count = 1;
  graph.nodes[graph.count].function_loc = -1; //id
  graph.nodes[graph.count].fwd_count = 0;
  graph.count++;

  pc.printf("read: input=%d destnode=%d\n", inm, aux);
}
void f_write() {
  //write inm=outputid word=source
  WORD output = inm;
  WORD source = *ip++;
  graph.outputs[output].source = source;
  pc.printf("write: source=%d output=%d\n", source, output);
}

//Jumps
void f_jump() {
  aux = *ip;
  ip = (WORD*) code + aux;
  //pc.printf("jump: pos=%d", aux);
}
void f_jump_false() {
  aux = *ip++;
  if (!*sp--) ip = (WORD*) code + aux;
  //pc.printf("jump_false: pos=%d", aux);
}
void f_cmp_eq() {
  aux = *sp--;
  if (aux == *sp--) *++sp = 1; else *++sp = 0;
  //pc.printf("cmp_eq:");
}
void f_cmp_neq() {
  aux = *sp--;
  if (aux != *sp--) *++sp = 1; else *++sp = 0;
  //pc.printf("cmp_neq:");
}
void f_cmp_gt() {
  aux = *sp--;
  if (*sp-- > aux) *++sp = 1; else *++sp = 0;
  //pc.printf("cmp_gt:");
}
void f_cmp_lt() {
  aux = *sp--;
  if (*sp-- < aux) *++sp = 1; else *++sp = 0;
  //pc.printf("cmp_lt:");
}
//Binary
void f_add() {
  aux = *sp--; *sp = *sp + aux;
  //pc.printf("add:");
}

void f_sub() {
  aux = *sp--; *sp = *sp - aux;
  //pc.printf("sub:");
}
void f_div() {
  aux = *sp--; *sp = *sp / aux;
  //pc.printf("div:");
}
void f_mul() {
  aux = *sp--; *sp = *sp * aux;
  //pc.printf("mul:");
}
void f_op_and() {
  aux = *sp--; *sp = *sp & aux;
  //pc.printf("and:");
}
void f_op_or() {
  aux = *sp--; *sp = *sp | aux;
  //pc.printf("or:");
}
//Unary operators
void f_op_not() {
  *sp = !(*sp);
  //pc.printf("lnot:");
}
//Stack operations
void f_push() {
  *++sp = *ip++;
  //pc.printf("push:");
}
void f_pop() {
  sp--;
  //pc.printf("pop:");
}
void f_dup() {
  sp++;
  *sp = *(sp-1);
  //pc.printf("dup:");
}
//Memory operations
void f_store() {
  globals[inm] = *sp--;
  //pc.printf("store: globals[%d]=%d", inm, globals[inm]);
}
void f_load() {
  *++sp = globals[inm];
  //pc.printf("load: globals[%d]", inm);
}
//Input/Output operations


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

void print_stack() {
  //pc.printf("Stack: ");
  WORD* p = (WORD*) stack;
  if (p != sp) {
    p++;
    for (; p != sp; p++) {
      //pc.printf("-> [%d] ", *p);
    };
    //pc.printf("-> [%d] -x", *sp);
  } else {
    //pc.printf("-x");
  }
  //pc.printf("\n");
}

void run_thread() {
  while (ip) {
    //pc.getc();
    instr = *ip++;
    op_code = (0xff00 & instr) >> 8;
    inm = 0x00ff & instr;
    (functions[op_code])();
    //pc.printf(" [TOS=%d, IP=%d (%04x)]\n", *sp, ip - (WORD*) code, ip);
    print_stack();
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
      //pc.printf("Read(%d) == %d\n", iter, value);
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
      //pc.printf("Output(%d) == %d\n", iter, graph.nodes[source].value);
      write_output(iter, graph.nodes[source].value);
    }
  }
}

void create_graph() {
  //Reset inputs
  WORD iter;
  for (iter = 0; iter < 10; iter++) {
    graph.inputs[iter].fwd_count = 0;
  }
  //Reset nodes
  graph.count = 0;
  graph.ready_next = 0;
  graph.ready_end = 0;
  //Reset outputs
  for (iter = 0; iter < 10; iter++) {
    graph.outputs[iter].source = -1;
  }
}

void propagate_signal(BYTE id) {
  BYTE fwd_count = graph.nodes[id].fwd_count;
  if (fwd_count > 0) {
    WORD value = graph.nodes[id].value;
    //pc.printf("Propagate signal id=%d value=%d\n", id, value);
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
        //pc.printf("Node ready %d\n", fwd);
        graph.ready_nodes[graph.ready_end++] = fwd;
        if (graph.ready_end == 20) graph.ready_end = 0;
      } else {
        //pc.printf("Node not ready %d\n", fwd);
      }
    }
  }
}

void update_signal(BYTE id) {
  WORD function_loc = graph.nodes[id].function_loc;
  if (function_loc == -1) { //no function (id)
    graph.nodes[id].value = graph.nodes[id].arg[0];
  } else {
    //push args and arg_count
    BYTE arg_count = graph.nodes[id].arg_count;
    for (BYTE iter = 0; iter < arg_count; iter++) {
      graph.nodes[id].arg_new[iter] = 0;
      WORD value = graph.nodes[id].arg[iter];
      *++sp = value;
      //pc.printf("push value %d\n", value);
    }
    *++sp = (WORD) arg_count;
    //pc.printf("push count %d\n", arg_count);
    call_function(function_loc, 0);
    run_thread();
    graph.nodes[id].value = *sp--;
  }
  //pc.printf("Signal %d = %d\n", id, graph.nodes[id].value);
  // Propagate signal
  propagate_signal(id);
}

void update_signals() {
  //pc.printf("update_signals %d to %d\n", graph.ready_next, graph.ready_end);
  while (graph.ready_next != graph.ready_end) {
    //pc.printf("update_signal %d\n", graph.ready_nodes[graph.ready_next]);
    update_signal(graph.ready_nodes[graph.ready_next++]);
    if (graph.ready_next == 20) graph.ready_next = 0;
  }
}

void print_graph() {
  WORD iter;
  BYTE fwd_iter, fwd_count;
  for (iter = 0; iter < 10; iter++) {
    fwd_count = graph.inputs[iter].fwd_count;
    if (fwd_count > 0) {
      for (fwd_iter = 0; fwd_iter < fwd_count; fwd_iter++) {
        pc.printf("Input %d -> Signal %d\n",
                  iter, graph.nodes[graph.inputs[iter].fwd[fwd_iter]].id);
      }
    }
  }
  //Print Signal Functions
  for (iter = 0; iter < graph.count; iter++) {
    fwd_count = graph.nodes[iter].fwd_count;
    Node source = graph.nodes[iter];
    for (fwd_iter = 0; fwd_iter < fwd_count; fwd_iter++) {
      Node dest = graph.nodes[source.fwd[fwd_iter]];
      BYTE fwd_place = source.fwd_place[fwd_iter];
      pc.printf("Signal %d -> Signal %d.%d (fun:%d)\n",
                source.id, dest.id, fwd_place, dest.function_loc);
    }
  }
  //Print output functions
  for (iter = 0; iter < 10; iter++) {
    WORD source = graph.outputs[iter].source;
    if (source != -1) {
      pc.printf("Signal %d -> Output %d\n",
                graph.nodes[source].id, iter);
    }
  }
}

void run_vm() {
  create_graph();
  //pc.printf("Running vm....\n");
  ip = (WORD*) code;
  run_thread();
  print_graph();
  WORD iterations = 1;
  while (1) {
    pc.printf("Alive to read..%d\n", iterations);
    read_inputs();
    pc.printf("Alive to update..%d\n", iterations);
    update_signals();
    pc.printf("Alive to write..%d\n", iterations);
    write_outputs();
    //pc.getc();
    wait(0.2);
    iterations++;
  }
}

int main() {
  initialize_iointerface();
  print_stack();
  run_vm();
  print_stack();
  return 0;
}
