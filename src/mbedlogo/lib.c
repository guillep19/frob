#include "cvm.h"

Timer timer;
Ticker ticker;
DigitalOut led1(LED1);
DigitalOut led2(LED2);
DigitalOut led3(LED3);
DigitalOut led4(LED4);
PwmOut fader(p21);
SPI spi(p11, p12, p13);
DigitalOut pin20(p20);

volatile ULONG ticks = 0;
volatile BOOL avail;

/***********************
* library
*/

void handleTick(){
    ticks++;
    avail = pc.readable();
 }

void mwait(ULONG c){
    ULONG endtime = ticks + c;
    while((ticks!=endtime)&&(!avail)){}
}

void primWait(ULONG c){mwait(c*100);}
void uwait(ULONG c){wait_us(c);}

ULONG utimer(){return timer.read_us();}
void resetut(){timer.reset();}

void *lmalloc(ULONG n){return malloc(n);}
void lfree(void *n){free(n);}

void print(SLONG c){pc.printf("%d\n", c);}
void prh(SLONG c){pc.printf("%08x\n", c);}
void prs(UBYTE* s){pc.printf("%s", s);}
void prn(char* s,ULONG n){pc.printf(s, n);}
void cr(){pc.printf("\n");}


void led1on(){led1=1;}
void led1off(){led1=0;}
void led2on(){led2=1;}
void led2off(){led2=0;}
void led3on(){led3=1;}
void led3off(){led3=0;}
void led4on(){led4=1;}
void led4off(){led4=0;}

void alloff(){ led1off();  led2off();  led3off();  led4off();}

ULONG primTime(){
    time_t seconds = time(NULL);
    return (ULONG)localtime(&seconds);
}

void settime(time_t t){set_time(t);}

void spiWrite(UBYTE n){spi.write(n);}
ULONG primTicks(){return ticks;}
void pin20on(){pin20=1;}
void pin20off(){pin20=0;}

void *fcns[] = {
    (void*) 0, (void*) utimer,  (void*) 0, (void*) resetut,  
    (void*) 1, (void*) primWait, (void*) 1, (void*) mwait, (void*) 1, (void*) uwait,
    (void*) 0, (void*) primTime, (void*) 1, (void*) settime,
    (void*) 1, (void*) lmalloc,  (void*) 0, (void*) lfree,  
    (void*) 1, (void*) print,  (void*) 1, (void*) prh,  
    (void*) 1, (void*) prs,  (void*) 2, (void*) prn,  (void*) 0, (void*) cr,
    (void*) 0, (void*) led1on, (void*) 0, (void*) led1off,
    (void*) 0, (void*) led2on, (void*) 0, (void*) led2off,
    (void*) 0, (void*) led3on, (void*) 0, (void*) led3off,
    (void*) 0, (void*) led4on, (void*) 0, (void*) led4off,
    (void*) 0, (void*) alloff,
    (void*) 1, (void*) spiWrite, (void*) 0, (void*) primTicks,
    (void*) 0, (void*) pin20on, (void*) 0, (void*) pin20off,
};

void lib_init(){
    timer.start();
    ticker.attach_us(handleTick, 1000);
    fader.period_ms(10);
    fader.pulsewidth_ms(5);
    spi.format(8,0);
    spi.frequency(250000);
    pin20 = 1;
}
