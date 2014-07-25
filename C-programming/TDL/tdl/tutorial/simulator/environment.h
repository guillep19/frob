/*****************************************************************************
 * PROJECT: TDL Tutorial
 *
 * (c) Copyright 2001 Reid Simmons. All rights reserved.
 * (c) Copyright 1994 Richard Goodwin & Reid Simmons. All rights reserved.
 *
 * FILE: environment.h
 *
 * ABSTRACT:
 *
 * Interface to the environment routines.
 *
 * $Source: /afs/cs.cmu.edu/project/TCA/Master/tcaV8/tutorial/simulator/environment.h,v $ 
 * $Revision: 1.3 $
 * $Date: 1996/08/05 16:10:17 $
 * $Author: rich $
 *
 * REVISION HISTORY:
 *
 * $Log: environment.h,v $
 *****************************************************************************/

#ifndef INCLUDED_ENVIRONMENT
#define INCLUDED_ENVIRONMENT

typedef struct _space {
  double    x1, y1, x2, y2;
} *space_type;

#define NUM_SPACE MAX_OBJ

extern int env_x, env_y;

extern struct _space space[];
extern int n_space;

extern void init_environment(char *fname);
extern double distrNormal(double mean, double sigma);

#endif /* INCLUDED_ENVIRONMENT */
