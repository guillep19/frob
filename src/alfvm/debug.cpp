
#include "debug.h"

void* AllocateLargestFreeBlock(size_t* Size)
{
  size_t s0, s1;
  void* p;

  s0 = ~(size_t)0 ^ (~(size_t)0 >> 1);

  while (s0 && (p = malloc(s0)) == NULL)
    s0 >>= 1;

  if (p)
    free(p);

  s1 = s0 >> 1;

  while (s1)
  {
    if ((p = malloc(s0 + s1)) != NULL)
    {
      s0 += s1;
      free(p);
    }
    s1 >>= 1;
  }

  while (s0 && (p = malloc(s0)) == NULL)
    s0 ^= s0 & -s0;

  *Size = s0;
  return p;
}

size_t GetFreeSize(void)
{
  size_t total = 0;
  void* pFirst = NULL;
  void* pLast = NULL;

  for (;;)
  {
    size_t largest;
    void* p = AllocateLargestFreeBlock(&largest);

    if (largest < sizeof(void*))
    {
      if (p != NULL)
        free(p);
      break;
    }

    *(void**)p = NULL;

    total += largest;

    if (pFirst == NULL)
      pFirst = p;

    if (pLast != NULL)
      *(void**)pLast = p;

    pLast = p;
  }

  while (pFirst != NULL)
  {
    void* p = *(void**)pFirst;
    free(pFirst);
    pFirst = p;
  }

  return total;
}

void print_stack() {
  //pc.printf("Stack: ");
  WORD* p = (WORD*) stack;
  if (p != sp) {
    p++;
    for (; p != sp; p++) {
      //pc.printf("-> [%d] ", *p);
    };
    //pc.printf("-> [%d] -x", *sp);
  } else {
    //pc.printf("-x");
  }
  //pc.printf("\n");
}

void print_graph() {
  WORD iter;
  BYTE fwd_iter, fwd_count;
  for (iter = 0; iter < 10; iter++) {
    fwd_count = graph.inputs[iter].fwd_count;
    if (fwd_count > 0) {
      for (fwd_iter = 0; fwd_iter < fwd_count; fwd_iter++) {
        //pc.printf("I %d -> S %d\n",
        //          iter, graph.nodes[graph.inputs[iter].fwd[fwd_iter]].id);
      }
    }
  }
  //Print Signal Functions
  for (iter = 0; iter < graph.count; iter++) {
    fwd_count = graph.nodes[iter].fwd_count;
    Node source = graph.nodes[iter];
    for (fwd_iter = 0; fwd_iter < fwd_count; fwd_iter++) {
      Node dest = graph.nodes[source.fwd[fwd_iter]];
      BYTE fwd_place = source.fwd_place[fwd_iter];
      //pc.printf("S %d -> S %d.%d (fun:%d)\n",
                //source.id, dest.id, fwd_place, dest.function_loc);
    }
  }
  //Print output functions
  for (iter = 0; iter < 10; iter++) {
    WORD source = graph.outputs[iter].source;
    //if (source != -1) {
      //pc.printf("S %d -> O %d\n",
      //          graph.nodes[source].id, iter);
    //}
  }
}
