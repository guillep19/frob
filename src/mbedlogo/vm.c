#include "cvm.h"

#define code 0xe000

#define debug ((ULONG*)(g+0xf0))
#define globals ((SLONG*)(g+0x100))
#define stack ((SLONG*)(g+0x200))

extern void(*prims[])();
void eol_repeat();
void prim_loop();
void eol_runmacro();
void eolr_waituntil();

UBYTE *ip;
SLONG *sp;
SLONG *fp;
SLONG t0, t1;

void run_vm(){
    ip = (UBYTE*) code;
    sp = stack;
    fp = 0;
    while(TRUE){
        if(avail){alloff(); break;}
//        if(buttonedge){buttonedge=0; alloff(); break;}
        UBYTE token = *ip++;
        if((token==0)||(token==0xff)) break;
        (prims[token])();
  }
  debug[0] = (ULONG)ip;
  debug[1] = (ULONG)sp;
}


void eval_done(){}

void eval_byte(){
    *sp++ = *ip++;
}

void eval_num(){
    SLONG t0 = *ip++;
    t0 += (*ip++<<8);
    t0 += (*ip++<<16);
    t0 += (*ip++<<24);
    *sp++ = t0;
}

void eval_list(){
    UBYTE offset = *ip++;
    offset += *ip++<<8;
    *sp++ = (SLONG)ip;
    ip+=offset;
}

void eval_eol(){
    switch(*--sp){
    case 0: ip=(UBYTE*)*--sp; break;
    case 1: eol_repeat(); break;
    case 2: prim_loop(); break;
    case 3: eol_runmacro(); break;
    }
}

void eval_eolr(){eolr_waituntil();}

SLONG *get_local_address(){
    SLONG t0 = (SBYTE) *ip++;
    if(t0<0) return fp-t0-1;
    else return fp-t0-4;
}

void eval_lthing(){
    *sp++ = *get_local_address();
}

void eval_lmake(){
    *get_local_address() = *--sp;
}

void eval_ufun(){
    ULONG newip = *ip++;
    newip += *ip++<<8;
    *sp++ = (SLONG)ip;
    ip = (UBYTE*)(newip+code);
    *sp++ = *ip++;
    *sp++ = (SLONG)fp;
    fp = sp;
    sp+=*ip++;
}

void eval_libfcn(){
    SLONG t0;
    UBYTE fcn = *ip++;
    ULONG args = (ULONG)(fcns[2*fcn]);
    void *fptr = (void*)(fcns[2*fcn+1]);
    switch(args){
        case 0: ((void (*)()) fptr)();
        break;
        case 1: ((void (*)(SLONG)) fptr)(*--sp);
        break;
        case 2: t0 = *--sp; ((void (*)(SLONG, SLONG)) fptr)(*--sp, t0);
        break;
    }
}

void eval_libfcnr(){
    SLONG t0;
    SLONG res=0;
    UBYTE fcn = *ip++;
    ULONG args = (ULONG)(fcns[2*fcn]);
    void *fptr = (void*)(fcns[2*fcn+1]);
    switch(args){
        case 0: res = ((ULONG (*)()) fptr)();
        break;
        case 1: res = ((ULONG (*)(SLONG)) fptr)(*--sp);
        break;
        case 2: t0 = *--sp; res = ((SLONG (*)(SLONG, SLONG)) fptr)(*--sp, t0);
        break;
    }
    *sp++ = res;
}

void prim_stop(){
    SLONG t0;
    if(fp==0) return;
    sp = fp;
    fp = (SLONG*) *--sp;
    t0 = *--sp;
    ip = (UBYTE*)*--sp;
    sp-= t0;
}

void prim_output(){
    SLONG t0;
    SLONG res = *--sp;
    sp = fp;
    fp = (SLONG*) *--sp;
    t0 = *--sp;
    ip = (UBYTE*)*--sp;
    sp-= t0;
    *sp++ = res;
}

void prim_call(){
    SLONG newip = *--sp;
    *sp++ = (SLONG)ip;
    ip = (UBYTE*)(newip+code);
    *sp++ = *ip++;
    *sp++ = (SLONG)fp;
    fp = sp;
    sp+=*ip++;
}


void prim_run(){
    SLONG addr = *--sp;
    *sp++ = (SLONG)ip;
    *sp++ = 0;
    ip = (UBYTE*)addr;
}

void prim_runmacro(){
    SLONG addr = *--sp;
    *sp++ = (SLONG)fp;
    *sp++ = (SLONG)ip;
    *sp++ = 3;
    ip = (UBYTE*)addr;
    fp = (SLONG*) *(fp-1);
}

void eol_runmacro(){
    ip = (UBYTE*)(*--sp);
    fp = (SLONG*)(*--sp);
}

void prim_repeat(){
    *sp++ = (SLONG)ip;
    eol_repeat();
}

void eol_repeat(){
    if(*(sp-3)==0) {ip = (UBYTE*)(*--sp); sp-=2;}
    else {(*(sp-3))--; ip=(UBYTE*)(*(sp-2)); *sp++=1;}
}

void prim_loop(){
    ip = (UBYTE*)*(sp-1);
    *sp++ = 2;
}

void prim_if(){
    SLONG addr = *--sp;
    if(!*--sp) return;
    *sp++ = (SLONG)ip;
    *sp++ = 0;
    ip = (UBYTE*)addr;
}

void prim_ifelse(){
    SLONG addr;
    SLONG faddr = *--sp;
    SLONG taddr = *--sp;
    if(*--sp) addr = taddr;
    else addr = faddr;
    *sp++ = (SLONG)ip;
    *sp++ = 0;
    ip = (UBYTE*)addr;
}

void prim_waituntil(){
    *sp++ = (SLONG) ip;
    ip = (UBYTE*)*(sp-2);
}

void eolr_waituntil(){
    SLONG res = *--sp;
    if(!res) ip = (UBYTE*)*(sp-2);
    else {ip = (UBYTE*)*--sp; --sp;}
}

void prim_gwrite(){SLONG t0 = *--sp; globals[*--sp] = t0;}
void prim_gread(){*sp++ = globals[*--sp];}

void prim_sum(){SLONG t0 = *--sp; *sp++ = *--sp+t0;}
void prim_difference(){SLONG t0 = *--sp; *sp++ = *--sp-t0;}
void prim_product(){SLONG t0 = *--sp; *sp++ = *--sp*t0;}
void prim_quotient(){SLONG t0 = *--sp; *sp++ = *--sp/t0;}

void prim_mod(){
    SLONG t0 = *--sp;
    SLONG res = *--sp%t0;
    if(res<0) res+= t0;
    *sp++=res;
}

void prim_random(){
    SLONG t0 = *--sp;
    *sp++ = ((rand()+(rand()<<15))&0x7fffffff)%t0;
  }

void prim_extend(){*sp++ = (SLONG)(SSHORT)*--sp;}

void prim_equal(){SLONG t0 = *--sp; *sp++ = (SLONG)(*--sp==t0);}
void prim_ne(){SLONG t0 = *--sp; *sp++ = (SLONG)(*--sp!=t0);}
void prim_greater(){SLONG t0 = *--sp; *sp++ = (SLONG)(*--sp>t0);}
void prim_less(){SLONG t0 = *--sp; *sp++ = (SLONG)(*--sp<t0);}

void prim_and(){SLONG t0 = *--sp; *sp++ = *--sp&t0;}
void prim_or(){SLONG t0 = *--sp; *sp++ = *--sp|t0;}
void prim_xor(){SLONG t0 = *--sp; *sp++ = *--sp^t0;}
void prim_not(){if(*--sp) *sp++=0; else *sp++=1;}

void prim_lsl(){
    SLONG cnt = *--sp;
    SLONG n = *--sp;
    if(cnt<0) *sp++ = n>>-cnt;
    else *sp++ = n<<cnt;
}


void prim_g(){*sp++ = g+(ULONG)*--sp;}
void prim_fl(){*sp++ = code+(ULONG)*--sp;}
void prim_readb(){*sp++ = (SLONG)*(UBYTE*)*--sp;}
void prim_writeb(){UBYTE t0 = (UBYTE)*--sp; *(UBYTE*)*--sp = t0;}
void prim_readh(){*sp++ = (SLONG)*(USHORT*)*--sp;}
void prim_writeh(){USHORT t0 = (USHORT)*--sp; *(USHORT*)*--sp = t0;}
void prim_read(){*sp++ = *(SLONG*)*--sp;}
void prim_write(){SLONG t0 = *--sp; *(SLONG*)*--sp = t0;}

void prim_sp(){*sp++ = (SLONG) sp;}

void(*prims[])() = {
    eval_done,
    eval_byte, eval_num,
    eval_list, eval_eol, eval_eolr,
    eval_lthing, eval_lmake,
    eval_ufun, eval_done,
    eval_libfcn, eval_libfcnr,
    prim_stop, prim_output, prim_call,
    prim_run, prim_runmacro, prim_repeat, prim_loop,
    prim_if, prim_ifelse, prim_waituntil,
    prim_gwrite, prim_gread,
    prim_sum, prim_difference, prim_product, prim_quotient, prim_mod,
    prim_random, prim_extend,
    prim_equal, prim_ne, prim_greater, prim_less,
    prim_and, prim_or, prim_xor,
    prim_not, prim_lsl,
    prim_g, prim_fl,
    prim_readb, prim_writeb,
    prim_readh, prim_writeh,
    prim_read, prim_write,
    prim_sp
};
