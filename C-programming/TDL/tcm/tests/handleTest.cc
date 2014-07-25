/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tcm/LICENSE.TXT)
 */
#include <stdio.h>
#include <string.h>
#include "handle.h"

struct STRING : Ref_Count
{
  const char *string;

  STRING() { string = ""; }
  STRING(const char *str) { string = strdup(str); }
  ~STRING() { fprintf(stderr, "Deleting %s\n", string); delete[] string; }
};

int main (void)
{
  STRING *str1 = new STRING("Hello, World"), *str2 = new STRING("Goodbye");
  Handle<STRING> hand1(str1), hand2(str2), hand3, hand4;

  hand3 = hand1;
  hand1 = hand4;
  hand1 = hand2;
  hand3 = hand2;
}
