#include <stdio.h>
#include <stdint.h>

//VM ops
#define halt 0x0000
//Functions
#define call 0x0100
#define ret 0x0200
//Tasks
#define start 0x0300 //running[inmediate] = 1
#define end 0x0400 //running[inmediate] = 0
//Jumps
#define jump 0x0500 //ip = *sp--;
#define jump_eq 0x0600 //a=*sp--;if (a == *sp--) {ip = *sp--}
#define jump_neq 0x0700 //a=*sp--;if (a != *sp--) {ip = *sp--}
#define jump_gt 0x0800 //a=*sp--;if (a > *sp--) {ip = *sp--}
#define jump_lt 0x0900 //a=*sp--;if (a < *sp--) {ip = *sp--}
//Binary operators
#define add 0x0A00 //a=*sp--;*sp += a;
#define sub 0x0B00 //a=*sp--;*sp -= a;
#define div 0x0C00 //a=*sp--;*sp = *sp / a;
#define mul 0x0D00 //a=*sp--;*sp = *sp * a;
#define and 0x0E00 //a=*sp--;*sp = *sp & a;
#define or 0x0F00 //a=*sp--;*sp = *sp | a;
//Unary operators
#define neg 0x1000 //*sp = !(*sp)
//Stack operations
#define push 0x1100 //*++sp = inmediate (0..255)
#define pop 0x1200 //sp--
//Memory operations
#define store 0x1300 //globals[inmediate] = *sp--
#define load 0x1400 //*++sp = globals[inmediate]
//Input/Output operations
#define read 0x1500 //inputs[inmediate] = ip++
#define write 0x1600 //outputs[inmediate] = *sp--

typedef uint8_t BYTE;
typedef uint16_t WORD;
typedef const WORD CODE[];

#include "vmcode.c"

WORD stack[128]; //256 bytes
WORD globals[64]; //128 bytes
WORD inputs[16]; //32 bytes
WORD outputs[16]; //32 bytes
WORD ip_buffer[32]; //64 bytes
//Total:1/2 kb (arduino nano tiene 2kb)
WORD* sp = stack;
WORD* ip = 0;

BYTE running = 1;


//VM ops
void f_halt() { running = 0; }
//Functions
void f_call() {}
void f_ret() {}
//Tasks
void f_start() {}
void f_end() {}
//Jumps
void f_jump() { ip = (WORD*) &(code[*sp--]); }
void f_jump_eq() { WORD a = *sp--; if (a == *sp--) { ip = (WORD*) &code[*sp--]; } }
void f_jump_neq() { WORD a = *sp--; if (a != *sp--) { ip = (WORD*) &code[*sp--]; } }
void f_jump_gt() { WORD a = *sp--; if (a > *sp--) { ip = (WORD*) &code[*sp--]; } }
void f_jump_lt() { WORD a = *sp--; if (a < *sp--) { ip = (WORD*) &code[*sp--]; } }
//Binay
void f_add() { WORD a = *sp--; *sp = *sp + a; }
void f_sub() { WORD a = *sp--; *sp = *sp - a; }
void f_div() { WORD a = *sp--; *sp = *sp / a; }
void f_mul() { WORD a = *sp--; *sp = *sp * a; }
void f_and() { WORD a = *sp--; *sp = *sp & a; }
void f_or() { WORD a = *sp--; *sp = *sp | a; }
//Unary operators
void f_neg() { *sp = !(*sp); }
//Stack operations
void f_push() { *++sp = *ip++; }
void f_pop() { sp--; }
//Memory operations
void f_store() { WORD inm = *sp--; globals[inm] = *sp--; }
void f_load() { WORD inm = *sp--; *++sp = globals[inm]; }
//Input/Output operations
void f_read() { WORD inm = *sp--; inputs[inm] = *ip++; /* empty stack and init ip */ }
void f_write() { WORD inm = *sp--; outputs[inm] = *sp--; }


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
  WORD* p;
  for (p = (WORD*) stack; p != sp; p++) {
    printf("-> [%d] ", *p);
  };
  printf("\n------\n");
}

void run_vm() {
  printf("Running vm....\n");
  while (running) {
     
  }
  printf("Finished");
}

int main(int argc, char* argv[]) {
  print_stack();
  print_code();
  run_vm();
  print_stack();
  return 0;
}
