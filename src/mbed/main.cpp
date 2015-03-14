//#include <stdio.h>
//#include <stdint.h>

#include "mbed.h"
DigitalOut myled(LED1);
Serial pc(USBTX, USBRX); // tx, rx

//VM ops
#define halt 0x00
//Functions
#define call 0x01
#define ret 0x02
//Tasks
#define start 0x03 //running[inmediate] = 1
#define end 0x04 //running[inmediate] = 0
//Jumps
#define jump 0x05 //ip = *sp--;
#define jump_eq 0x06 //a=*sp--;if (a == *sp--) {ip = *sp--}
#define jump_neq 0x07 //a=*sp--;if (a != *sp--) {ip = *sp--}
#define jump_gt 0x08 //a=*sp--;if (a > *sp--) {ip = *sp--}
#define jump_lt 0x09 //a=*sp--;if (a < *sp--) {ip = *sp--}
//Binary operators
#define add 0x0A //a=*sp--;*sp += a;
#define sub 0x0B //a=*sp--;*sp -= a;
#define div 0x0C //a=*sp--;*sp = *sp / a;
#define mul 0x0D //a=*sp--;*sp = *sp * a;
#define op_and 0x0E //a=*sp--;*sp = *sp & a;
#define op_or 0x0F //a=*sp--;*sp = *sp | a;
//Unary operators
#define neg 0x10 //*sp = !(*sp)
//Stack operations
#define push 0x11 //*++sp = inmediate (0..255)
#define pop 0x12 //sp--
#define dup 0x13 //sp--
//Memory operations
#define store 0x14 //globals[inmediate] = *sp--
#define load 0x15 //*++sp = globals[inmediate]
//Input/Output operations
#define read 0x16 //inputs[inmediate] = ip++
#define write 0x17 //outputs[inmediate] = *sp--

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
WORD* ip;

BYTE running = 1;

WORD instr = 0;
BYTE op_code = 0;
WORD inm = 0;
WORD aux = 0;
WORD aux2 = 0;

//VM ops
void f_halt() {
  running = 0;
  pc.printf("halt\n");
}
//Functions
void f_call() {}
void f_ret() {}
//Tasks
void f_start() {}
void f_end() {}
//Jumps
void f_jump() {
  ip += (*sp--) - 1;
  pc.printf("jump (sp:%d) *ip= %04x\n", *sp, *ip);
}
void f_jump_eq() {
  aux = *sp--;
  if (aux == *sp--) ip += (*sp--) - 1; else sp--;
  pc.printf("jump_eq (sp:%d) *ip= %04x\n", *sp, *ip);
}
void f_jump_neq() {
  aux = *sp--;
  if (aux != *sp--) ip += (*sp--) - 1; else sp--;
  pc.printf("jump_neq (sp:%d) *ip= %04x\n", *sp, *ip);
}
void f_jump_gt() {
  aux = *sp--;
  if (aux > *sp--) ip += (*sp--) - 1; else sp--;
  pc.printf("jump_gt (sp:%d) *ip= %04x\n", *sp, *ip);
}
void f_jump_lt() {
  aux = *sp--;
  if (aux < *sp--) ip += (*sp--) - 1; else sp--;
  pc.printf("jump_lt (sp:%d) *ip= %04x\n", *sp, *ip);
}
//Binary
void f_add() {
  aux = *sp--; *sp = *sp + aux;
  pc.printf("add: %d\n", *sp);
}

void f_sub() {
  aux = *sp--; *sp = *sp - aux;
  pc.printf("sub: %d\n", *sp);
}
void f_div() {
  aux = *sp--; *sp = *sp / aux;
  pc.printf("div: %d\n", *sp);
}
void f_mul() {
  aux = *sp--; *sp = *sp * aux;
  pc.printf("mul: %d\n", *sp);
}
void f_and() {
  aux = *sp--; *sp = *sp & aux;
  pc.printf("and: %d\n", *sp);
}
void f_or() {
  aux = *sp--; *sp = *sp | aux;
  pc.printf("or: %d\n", *sp);
}
//Unary operators
void f_neg() {
  *sp = !(*sp);
  pc.printf("neg: %d\n", *sp);
}
//Stack operations
void f_push() {
  sp++; *sp = inm;
  pc.printf("push: %d\n", *sp);
}
void f_pop() {
  sp--;
  pc.printf("pop: %d\n", *sp);
}
void f_dup() {
  sp++;
  *sp = *(sp-1);
  pc.printf("dup: %d\n", *sp);
}
//Memory operations
void f_store() {
  globals[inm] = *sp--;
  pc.printf("store: (sp:%d) globals[%d]=%d\n", *sp, inm, globals[inm]);
}
void f_load() {
  *++sp = globals[inm];
  pc.printf("load: %d == globals[%d] == %d\n", *sp, inm, globals[inm]);
}
//Input/Output operations
void f_read() {
  inputs[globals[inm]] = ++ip;
  running = 0;
  pc.printf("read: (sp:%d) inputs[globals[%d]==%d] == %p", 
         *sp, inm, globals[inm], inputs[globals[inm]]);
}
void f_write() {
  outputs[inm] = *sp--;
  pc.printf("write: (sp:%d) outputs[%d] == %d\n", *sp, inm, outputs[inm]);
  if (inm == 1) {
    myled = outputs[inm];
  };
  /* TODO: set flag, to be processed as soon as possible */
}

void (*functions[])() = {
  &f_halt,
  f_call, f_ret,
  f_start, f_end,
  f_jump, f_jump_eq,
  f_jump_neq, f_jump_gt, f_jump_lt,
  //Binary operators
  f_add, f_sub, f_div,
  f_mul, f_and, f_or,
  //Unary operators
  f_neg,
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
  pc.printf("Stack:\n");
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
  pc.printf("\n------\n");
}

void run_thread() {
  running = 1;
  while (running) {
    instr = *ip;
    op_code = (0xff00 & instr) >> 8;
    inm = 0x00ff & instr;
    if (inm == 0x00ff) { ip++; inm = *ip; }
    pc.printf("%02x %d\n", op_code, inm);
    pc.getc();
    (functions[op_code])();
    ip++;
  }
  ip = 0;
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
    if (++helper_iterator == 4) value = 1;
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
