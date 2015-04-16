#ifndef FROBDEFINITIONS_H
#define FROBDEFINITIONS_H

#include "mbed.h"

//VM ops
#define t_halt       0x00
//Functions
#define t_call       0x01
#define t_ret        0x02
#define t_load_param 0x03
//Tasks
#define t_start      0x04 //ip_buffer[last++] = *++ip
#define t_stop       0x05 //running[inmediate] = 0
//Jumps
#define t_jump       0x06 //ip = *sp--;
#define t_jump_false 0x07 //ip = *sp--;
//Binary Boolean comparators
#define t_cmp_eq     0x08 //a=*sp--;*sp = (a == *sp--)
#define t_cmp_neq    0x09 //a=*sp--; *sp = (a != *sp--)
#define t_cmp_gt     0x0a //a=*sp--;*sp = (a > *sp--)
#define t_cmp_lt     0x0b //a=*sp--;*sp = (a < *sp--)
//Binary operators
#define t_add        0x0C //a=*sp--;*sp += a;
#define t_sub        0x0D //a=*sp--;*sp -= a;
#define t_div        0x0E //a=*sp--;*sp = *sp / a;
#define t_mul        0x0F //a=*sp--;*sp = *sp * a;
#define t_op_and     0x10 //a=*sp--;*sp = *sp & a;
#define t_op_or      0x11 //a=*sp--;*sp = *sp | a;
//Unary Boolean operators
#define t_op_not     0x12 //*sp = !(*sp)
//Stack operations
#define t_push       0x13 //*++sp = inmediate (0..255)
#define t_pop        0x14 //sp--
#define t_dup        0x15 //sp--
//Memory operations
#define t_store      0x16 //globals[inm] = *sp--
#define t_load       0x17 //*++sp = globals[inm]
//Input/Output operations
#define t_read       0x18 //inputs[inmediate] = ip++
#define t_write      0x19 //outputs[inmediate] = *sp--

typedef uint8_t BYTE;
typedef int16_t WORD;
typedef const WORD CODE[];


#endif /* FROBDEFINITIONS_H */
