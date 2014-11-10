#include "cvm.h"

Serial pc(USBTX, USBRX); // tx, rx

ULONG command[5];
ULONG result [5];
UBYTE buf[256];
#define startup (*(ULONG*) g) 

#define iap_prepare 50
#define iap_writeflash 51
#define iap_eraseflash 52

#define IAP_LOCATION    0x1fff1ff1
typedef void (*IAP_call)(ULONG [], ULONG []);
IAP_call call_iap = (IAP_call) IAP_LOCATION;

/***********************
* comms
*/

ULONG read16(){
    UBYTE c1 = pc.getc();
    UBYTE c2 = pc.getc();
    return (c2<<8)+c1;
}

ULONG read32(){
    UBYTE c1 = pc.getc();
    UBYTE c2 = pc.getc();
    UBYTE c3 = pc.getc();
    UBYTE c4 = pc.getc();
    return (c4<<24)+(c3<<16)+(c2<<8)+c1;
}

void readmemory(){
    ULONG addr = read32();
    ULONG count = read16();
    ULONG i;
    for(i=0;i<count;i++) pc.putc(*((UBYTE*) addr++));
}

void writememory(){
    ULONG addr = read32();
    ULONG count = read16();
    ULONG i;
    for(i=0;i<count;i++) *((UBYTE*) addr++)=pc.getc();
}

void run(){
    startup = 0;
    run_vm();
    pc.putc(0xff);
    startup = 0x12345678;
}


void writeflash(){
    ULONG sector = pc.getc();
    ULONG dstaddr = read32();
    ULONG count = pc.getc();
    if(count==0) count=256;
    ULONG i;
    for(i=0;i<256;i++) buf[i] = 0xff;
    for(i=0;i<count;i++) buf[i] = pc.getc();
    __disable_irq();
    command[0] = iap_prepare;
    command[1] = sector;
    command[2] = sector;
    call_iap(command, result);
    command[0] = iap_writeflash;
    command[1] = dstaddr;
    command[2] = (ULONG)buf;
    command[3] = 256;
    command[4] = SystemCoreClock / 1000;
    call_iap(command, result); 
    __enable_irq();
    pc.putc(result[0]);
}

void eraseflash(){
    ULONG sector = pc.getc();
    __disable_irq();
    command[0] = iap_prepare;
    command[1] = sector;
    command[2] = sector;
    call_iap(command, result);
    command[0] = iap_eraseflash;
    command[1] = sector;
    command[2] = sector;
    command[3] = SystemCoreClock / 1000;
    call_iap(command, result); 
    __enable_irq();
    pc.putc(result[0]);
}

int main() {
    UBYTE c;
    lib_init();
    if(startup==0x12345678) run();
    startup = 0x12345678;
    while(1){
        c = pc.getc();
        if(c==0xff) pc.putc(23);
        else if(c==0xfe) readmemory();
        else if(c==0xfd) writememory();
        else if(c==0xfc) run();
        else if(c==0xfb) writeflash();
        else if(c==0xfa) eraseflash();
    }
}
