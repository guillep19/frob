#include "mbed.h"

#define TRUE 1
#define FALSE 0

typedef unsigned char UBYTE;
typedef signed char SBYTE;
typedef unsigned short USHORT;
typedef signed short SSHORT;
typedef unsigned long ULONG;
typedef signed long SLONG;
typedef UBYTE BOOL;

#define g 0x2007c000

extern Serial pc;

extern volatile BOOL avail;
void lib_init(void);
void alloff();
extern void *fcns[];

void run_vm(void);
