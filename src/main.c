#include <stdio.h>
#include <stdint.h>

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
#define and 0x0E //a=*sp--;*sp = *sp & a;
#define or 0x0F //a=*sp--;*sp = *sp | a;
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
typedef uint16_t WORD;
typedef const WORD CODE[];

#include "vmcode.c"

WORD stack[128]; //256 bytes
WORD globals[64]; //128 bytes
WORD* inputs[16]; //32 bytes
WORD outputs[16]; //32 bytes
WORD ip_buffer[32]; //64 bytes
//Total:1/2 kb (arduino nano tiene 2kb)
WORD* sp = stack;
WORD* ip;

BYTE running = 1;

WORD instr = 0;
BYTE op_code = 0;
WORD inm = 0;
WORD aux = 0;

//VM ops
void f_halt() {
  running = 0;
  printf("halt\n");
}
//Functions
void f_call() {}
void f_ret() {}
//Tasks
void f_start() {}
void f_end() {}
//Jumps
void f_jump() {
  WORD count = *sp--;
  count--;
  for (;count>0;count--) {
    ip++;
    printf("ip++,");
  }
  printf("*ip= %04x\n", *ip);
}
void f_jump_eq() {
  WORD a = *sp--;
  if (a == *sp--) {
    ip = (WORD*) &code[*sp--];
  }
}
void f_jump_neq() { WORD a = *sp--; if (a != *sp--) { ip = (WORD*) &code[*sp--]; } }
void f_jump_gt() { WORD a = *sp--; if (a > *sp--) { ip = (WORD*) &code[*sp--]; } }
void f_jump_lt() { WORD a = *sp--; if (a < *sp--) { ip = (WORD*) &code[*sp--]; } }
//Binay
void f_add() {
  aux = *sp--; *sp = *sp + aux;
  printf("add: %d\n", *sp);
}

void f_sub() {
  aux = *sp--; *sp = *sp - aux;
  printf("sub: %d\n", *sp);
}
void f_div() {
  aux = *sp--; *sp = *sp / aux;
  printf("div: %d\n", *sp);
}
void f_mul() {
  aux = *sp--; *sp = *sp * aux;
  printf("mul: %d\n", *sp);
}
void f_and() {
  aux = *sp--; *sp = *sp & aux;
  printf("and: %d\n", *sp);
}
void f_or() {
  aux = *sp--; *sp = *sp | aux;
  printf("or: %d\n", *sp);
}
//Unary operators
void f_neg() {
  *sp = !(*sp);
  printf("neg: %d\n", *sp);
}
//Stack operations
void f_push() {
  sp++; *sp = inm;
  printf("push: %d\n", *sp);
}
void f_pop() {
  sp--;
  printf("pop: %d\n", *sp);
}
void f_dup() {
  *sp++ = *sp;
  printf("dup: %d\n", *sp);
}
//Memory operations
void f_store() {
  globals[inm] = *sp--;
  printf("store: %d globals[%d]=%d\n", *sp, inm, globals[inm]);
}
void f_load() {
  *++sp = globals[inm];
  printf("load: %d == globals[%d] == %d\n", *sp, inm, globals[inm]);
}
//Input/Output operations
void f_read() {
  inputs[inm] = ip;
  /* TODO: move ip to next ready thread */
  /* TODO: clear sp */
  /* TODO: this should be here? or should it be in run_vm? */
}
void f_write() {
  outputs[inm] = *sp--;
  /* TODO: set flag, to be processed as soon as possible */
}


void print_instruction(WORD instruction) {
  BYTE high = (0xff00 & instruction) >> 8;
  BYTE low = 0x00ff & instruction;
  printf("%02x %02x\n", high, low);
}

void print_code() {
  printf("Code:\n---------------\n");
  WORD* ip;
  for(ip = (WORD*) code; *ip ; ip++) {
    print_instruction(*ip);
  };
  printf("---------------\n");
}

void print_stack() {
  printf("Stack:\n");
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
  printf("\n------\n");
}


void run_vm() {
  printf("Running vm....\n");
  ip = (WORD*) code;
    while (running) {
      instr = *ip;
      op_code = (0xff00 & instr) >> 8;
      inm = 0x00ff & instr;
      if (inm == 0x00ff) { ip++; inm = *ip; }
      printf("%02x %d\n", op_code, inm);
      getchar();
      switch (op_code) {
        case halt: f_halt(); break;
        case push: f_push(); break;
        case add: f_add(); break;
        case sub: f_sub(); break;
        case jump: f_jump(); break;
        case store: f_store(); break;
        case load: f_load(); break;
        default: break;
      }
      ip++;
    }
  printf("Finished\n");
}

int main(int argc, char* argv[]) {
  print_stack();
  print_code();
  run_vm();
  print_stack();
  return 0;
}
