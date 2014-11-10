
#include <stdio.h>
#include <stdint.h>

typedef uint8_t BYTE;
typedef uint16_t WORD;
typedef const WORD CODE[];
CODE code = {0x0106, 0x0200, 0x0300}; //10 bytes

BYTE stack[16]; //128 bytes
BYTE* sp = stack;

#define halt 0x00
#define add 0x01
#define sub 0x02
#define push 0x03
#define pop 0x04
#define push_var 0x05
#define jump_le 0x06 //*sp <= *sp-1 jump;
#define jump_ge 0x07 //*sp >= *sp-1 jump;
#define jump_l 0x08 //*sp < *sp-1 jump;
#define jump_g 0x09 //*sp > *sp-1 jump;
#define jump_eq 0x0a //*sp == *sp-1 jump;
#define jump_neq 0x0b //*sp != *sp-1 jump;
#define jump 0x0c //jump *sp--;
#define enable 0x0d //inputs[inmediate] = 1
#define disable 0x0e //inputs[inmediate] = 0
#define write 0x0f //output[inmediate] = 

BYTE running = 1;

void halt() {
  running = 0;
}

void add() {
  BYTE a = *sp;
  sp--;
  
}
//WORD instruction(BYTE code, BYTE arg) {
//  WORD word = (code << 8) + arg;
//  return word;
//}

//typedef const char STRING[32];
//STRING labels[128] = {
//  "halt", "add", "sub", "push", "pop", "push_var",
//  "jump_le", "jump_ge", "jump_l", "jump_g", "jump_neq", "jump"
//};

//void print_instruction(WORD instruction) {
 // BYTE high = (0xff00 & instruction) >> 8;
 // BYTE low = 0x00ff & instruction;
 // printf("%02x %02x %s(%d)\n", high, low, labels[high], low);
//}

//void print_code(CODE code) {
  //printf("Code:\n---------------\n");
  //printf("---------------\n");
//}

//void run(CODE code) {
  //printf("Running...\n");
//}

int main(int argc, char* argv[]) {
  //WORD inst = instruction(sub, 6);
  //code[0] = (WORD)inst;
  //print_instruction(inst);
  //print_code(code);
  //run(code);
  return 0;
}
