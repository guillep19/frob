/*****************************************************************************
 *
 * PROJECT: Task Control Management.
 *
 * (c) Copyright 1996 Reid Simmons.  All rights reserved.
 *
 * FILE: basics.cc
 *
 * ABSTRACT: Basic definitions needed by TCM
 *
 * $Source: /afs/cs.cmu.edu/project/TCA/Master/tcaV8/tcm/src/tcmBasics.cc,v $ 
 * $Revision: 1.5 $
 * $Date: 2009/01/15 17:00:05 $
 * $Author: reids $
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tcm/LICENSE.TXT)
 *
 * REVISION HISTORY:
 *
 * $Log: tcmBasics.cc,v $
 * Revision 1.5  2009/01/15 17:00:05  reids
 * Released under simplified BSD open source license
 *
 * Revision 1.4  2000/07/05 23:17:51  da0g
 * Modified MSecs to be a 8-byte integer, thereby fixing the
 * time-wrapping bug that occurs every 7 weeks or so.
 *
 * Revision 1.3  1998/06/02 10:39:42  reids
 * Increased overall efficiency of TCM.
 *
// Revision 1.2  97/12/04  17:50:23  reids
// Another fairly stable version (except that monitors do not quite work)
// 
// Revision 1.1  97/11/21  14:06:41  reids
// First release of TCM -- seems to be a stable version
// 
 *
 *****************************************************************************/

#include "tcmBasics.h"

#ifndef macintosh
#include <sys/timeb.h>
#else
#include <stdlib.h>
#include <time.h>
#endif

#ifndef macintosh
MSecs timeInMsecs (void)
{
  struct timeval timeBlock;
  
  gettimeofday(&timeBlock, NULL);
  return (   ((MSecs)1000) * ((MSecs)timeBlock.tv_sec)
	   + ((MSecs)(timeBlock.tv_usec / 1000)) );
}
#else
unsigned long timeInMsecs (void)
{
  return ( ((MSecs)clock()) * ((MSecs)1000) ) / ((MSecs)CLOCKS_PER_SEC);
}

char *strdup(const char *str)
{
  char *newString = new char[strlen(str)+1];
  strcpy(newString, str);
  return newString;
}
#endif
