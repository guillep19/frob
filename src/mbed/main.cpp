//#include <stdio.h>
//#include <stdint.h>

#include "mbed.h"
DigitalOut myled(LED1);
Serial pc(USBTX, USBRX); // tx, rx

//VM ops
#define halt       0x00
//Functions
#define call       0x01
#define ret        0x02
#define load_param 0x03
//Tasks
#define start      0x04 //ip_buffer[last++] = *++ip
#define stop       0x05 //running[inmediate] = 0
//Jumps
#define jump       0x06 //ip = *sp--;
#define jump_false 0x07 //ip = *sp--;
//Binary Boolean comparators
#define cmp_eq     0x08 //a=*sp--;*sp = (a == *sp--)
#define cmp_neq    0x09 //a=*sp--; *sp = (a != *sp--)
#define cmp_gt     0x0a //a=*sp--;*sp = (a > *sp--)
#define cmp_lt     0x0b //a=*sp--;*sp = (a < *sp--)
//Binary operators
#define add        0x0C //a=*sp--;*sp += a;
#define sub        0x0D //a=*sp--;*sp -= a;
#define div        0x0E //a=*sp--;*sp = *sp / a;
#define mul        0x0F //a=*sp--;*sp = *sp * a;
#define op_and     0x10 //a=*sp--;*sp = *sp & a;
#define op_or      0x11 //a=*sp--;*sp = *sp | a;
//Unary Boolean operators
#define op_not     0x12 //*sp = !(*sp)
//Stack operations
#define push       0x13 //*++sp = inmediate (0..255)
#define pop        0x14 //sp--
#define dup        0x15 //sp--
//Memory operations
#define store      0x16 //globals[inm] = *sp--
#define load       0x17 //*++sp = globals[inm]
//Input/Output operations
#define read       0x18 //inputs[inmediate] = ip++
#define write      0x19 //outputs[inmediate] = *sp--

typedef uint8_t BYTE;
typedef int16_t WORD;
typedef const WORD CODE[];

#include "vmcode.c"

#define MAX_INPUTS 16
#define MAX_IP_BUFFER 32

WORD stack[128] = {0}; //256 bytes
WORD globals[64]; //128 bytes
WORD* inputs[MAX_INPUTS] = {0}; //32 bytes
WORD outputs[16]; //32 bytes
WORD* ip_buffer[MAX_IP_BUFFER] = {0}; //64 bytes
BYTE ip_buffer_first = 0;
BYTE ip_buffer_last = 0;
//Total:1/2 kb (arduino nano tiene 2kb)
WORD* sp = stack;
WORD fp = 0;
WORD* ip;


WORD instr = 0;
BYTE op_code = 0;
WORD inm = 0;
WORD aux = 0;
WORD aux2 = 0;

//VM ops
void f_halt() {
  ip = 0;
  pc.printf("halt");
}
//Functions
void f_call() {
  //arg0, arg1, count ! oldfp, oldip
  WORD fun_location = *ip;
  *++sp = fp; //store oldfp
  *++sp = (WORD) (++ip - (WORD*) code); //store oldip
  fp = (WORD) (sp - stack); //create new frame
  ip = (WORD*) code + fun_location; //jump to function
  pc.printf("call:");
}
void f_ret() {
  WORD ret_value = *sp;
  sp = stack + fp; //remove current frame
  ip = (WORD*) code + *sp--; //restore ip
  fp = *sp--; //restore fp
  aux = *sp--; //get count to pop arguments
  sp -= aux; //remove arguments
  *++sp = ret_value; //push result to return
  pc.printf("ret:");
}
void f_load_param() {
  *++sp = stack[fp - inm - 3]; //3 because of count,oldfp,oldip
  pc.printf("load_param: inm=%d", inm);
}
//Tasks
void f_start() {
  aux = *ip++;
  ip_buffer[ip_buffer_last++] = (WORD*) code + aux;
  pc.printf("start: taskpos=%d", aux);
}
void f_stop() {
  pc.printf("stop NOT IMPLEMENTED");
}
//Jumps
void f_jump() {
  aux = *ip;
  ip = (WORD*) code + aux;
  pc.printf("jump: pos=%d", aux);
}
void f_jump_false() {
  aux = *ip++;
  if (!*sp--) ip = (WORD*) code + aux;
  pc.printf("jump_false: pos=%d", aux);
}
void f_cmp_eq() {
  aux = *sp--;
  if (aux == *sp--) *++sp = 1; else *++sp = 0;
  pc.printf("cmp_eq:");
}
void f_cmp_neq() {
  aux = *sp--;
  if (aux != *sp--) *++sp = 1; else *++sp = 0;
  pc.printf("cmp_neq:");
}
void f_cmp_gt() {
  aux = *sp--;
  if (aux > *sp--) *++sp = 1; else *++sp = 0;
  pc.printf("cmp_gt:");
}
void f_cmp_lt() {
  aux = *sp--;
  if (aux < *sp--) *++sp = 1; else *++sp = 0;
  pc.printf("cmp_lt:");
}
//Binary
void f_add() {
  aux = *sp--; *sp = *sp + aux;
  pc.printf("add:");
}

void f_sub() {
  aux = *sp--; *sp = *sp - aux;
  pc.printf("sub:");
}
void f_div() {
  aux = *sp--; *sp = *sp / aux;
  pc.printf("div:");
}
void f_mul() {
  aux = *sp--; *sp = *sp * aux;
  pc.printf("mul:");
}
void f_op_and() {
  aux = *sp--; *sp = *sp & aux;
  pc.printf("and:");
}
void f_op_or() {
  aux = *sp--; *sp = *sp | aux;
  pc.printf("or:");
}
//Unary operators
void f_op_not() {
  *sp = !(*sp);
  pc.printf("lnot:");
}
//Stack operations
void f_push() {
  *++sp = *ip++;
  pc.printf("push:");
}
void f_pop() {
  sp--;
  pc.printf("pop:");
}
void f_dup() {
  sp++;
  *sp = *(sp-1);
  pc.printf("dup:");
}
//Memory operations
void f_store() {
  globals[inm] = *sp--;
  pc.printf("store: globals[%d]=%d", inm, globals[inm]);
}
void f_load() {
  *++sp = globals[inm];
  pc.printf("load: globals[%d]", inm);
}
//Input/Output operations
void f_read() {
  inputs[globals[inm]] = ip;
  ip = 0;
  pc.printf("read: inputs[globals[%d]==%d] == %p", 
            inm, globals[inm], inputs[globals[inm]]);
}
void f_write() {
  outputs[inm] = *sp--;
  pc.printf("write: outputs[%d] == %d", inm, outputs[inm]);
  if (inm == 1) {
    myled = outputs[inm];
  };
}

void (*functions[])() = {
  f_halt,
  f_call, f_ret, f_load_param,
  f_start, f_stop,
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
  f_store, f_load,
  // Input/Output operations
  f_read, f_write
};

void print_instruction(WORD instruction) {
  BYTE high = (0xff00 & instruction) >> 8;
  BYTE low = 0x00ff & instruction;
  pc.printf("%02x %02x\n", high, low);
}

void print_code() {
  pc.printf("Code:\n---------------\n");
  WORD* ip;
  for(ip = (WORD*) code; *ip ; ip++) {
    print_instruction(*ip);
  };
  pc.printf("---------------\n");
}

void print_stack() {
  pc.printf("Stack: ");
  WORD* p = (WORD*) stack;
  if (p != sp) {
    p++;
    for (; p != sp; p++) {
      pc.printf("-> [%d] ", *p);
    };
    pc.printf("-> [%d] -x", *sp);
  } else {
    pc.printf("-x");
  }
  pc.printf("\n");
}

void run_thread() {
  while (ip) {
    pc.getc();
    instr = *ip++;
    op_code = (0xff00 & instr) >> 8;
    inm = 0x00ff & instr;
    (functions[op_code])();
    pc.printf(" [TOS=%d, IP=%d (%04x)]\n", *sp, ip - (WORD*) code, ip);
    print_stack();
  }
}


WORD input_iterator = 0;
WORD started = 0;
WORD helper_iterator = 0; //TODO: Remove this variable, its used as stub value for input.

void read_input() {
  /* Finds out whether there is an input waiting to
     be read. If there is any, it reads it, pushes
     the result in the stack, and returns the ip
     stored in inputs[index], else returns NULL */
  started = input_iterator;
  do {
    input_iterator = (input_iterator + 1) % MAX_INPUTS;
    ip = inputs[input_iterator];
    pc.printf("ip == inputs[%d] == %p\n", input_iterator, ip);
  } while ((!ip) && (input_iterator != started));
  if (ip) {
    inputs[input_iterator] = 0;
    /*TODO; This code is a stub only to mimic a predictable input.REMOVE IT */
    WORD value = 0;
    if (++helper_iterator == 4) { value = 1; helper_iterator = 0;}
    sp++; *sp = value;
    pc.printf("TODO: read value from (%d), (stub) pushed %d\n", input_iterator, value);
    /* End of stub. TODO: Implement the real READ */
  }
}

void run_vm() {
  pc.printf("Running vm....\n");
  ip = (WORD*) code;
  while (ip) {
    run_thread();
    if (!ip) {
      pc.printf("Looking for a ready thread...\n");
      //Maybe I could have assumed ip = null..
      if (ip_buffer_first != ip_buffer_last) {
        //There are threads ready to run
        ip = ip_buffer[ip_buffer_first++];
        pc.printf("Thread %d took control.\n", ip_buffer_first - 1);
      } else {
        pc.printf("No threads ready to run.\n");
      }
    }
    if (!ip) {
      pc.printf("Searching for a thread waiting to read...\n");
      read_input();
    }
  }
  pc.printf("Finished\n");
}

int main() {
  print_stack();
  print_code();
  run_vm();
  print_stack();
  return 0;
}
