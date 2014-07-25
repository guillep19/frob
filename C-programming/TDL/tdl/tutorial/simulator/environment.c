/*****************************************************************************
 * PROJECT: TDL Tutorial
 *
 * (c) Copyright 2001 Reid Simmons. All rights reserved.
 * (c) Copyright 1994 Richard Goodwin & Reid Simmons. All rights reserved.
 *
 * FILE: environment.c
 *
 * ABSTRACT:
 * 
 * This file provides the routines compute free space and intersections
 * for objects in the environment.
 *
 * $Source: /afs/cs.cmu.edu/project/TCA/Master/tcaV8/tutorial/simulator/environment.c,v $ 
 * $Revision: 1.2 $
 * $Date: 1995/04/07 05:09:50 $
 * $Author: rich $
 *
 * REVISION HISTORY:
 *
 * $Log: environment.c,v $
 *****************************************************************************/

#include <limits.h>
#include <math.h>

#include "common.h"
#include "sim-xwindows.h"
#include "files.h"
#include "environment.h"
#include "action.h"

/*****************************************************************************
 * Global constants
 *****************************************************************************/

/*****************************************************************************
 * Global variables
 *****************************************************************************/

struct _space space[NUM_SPACE];
int n_space = 0;

int env_x=0, env_y=0;

/*****************************************************************************
 * Private variables
 *****************************************************************************/

static int test_intersection(
		      /* returned values */
		      double *x1, double *y1, double *x2, double *y2,
		      double line_x1, double line_y1, 
		      double line_x2, double line_y2,
		      double rect_x1, double rect_y1, 
		      double rect_x2, double rect_y2,
		      int orient)
{
  int intersect = 0;
  
  *x1 = line_x1;
  *y1 = line_y1;
  *x2 = line_x2;
  *y2 = line_y2;
  
  if (orient == 0){  /* line horizontal */
    if ((rect_y1-line_y2)*(rect_y2-line_y2) <= 0.0 &&
	rect_x2 <= *x2 && rect_x2 >= *x1){
      *x2 = rect_x2;
      intersect = 1;
    }
    if ((rect_y1-line_y1)*(rect_y2-line_y1) <= 0.0 &&
	rect_x1 <= *x2 && rect_x1 >= *x1){
      *x1 = rect_x1;
      intersect = 1;
    }
    *x1 += robot_r;
    *y1 -= robot_r;
    *x2 -= robot_r;
    *y2 += robot_r;
    if (*x1 >= *x2 || *y1 >= *y2) intersect = 0;
  } else {   /* line vertical */
    if ((rect_x1-line_x2)*(rect_x2-line_x2) <= 0.0 &&
	rect_y2 <= *y2 && rect_y2 >= *y1) {
      *y2 = rect_y2;
      intersect = 1;
    }
    if ((rect_x1-line_x1)*(rect_x2-line_x1) <= 0.0 &&
	rect_y1 <= *y2 && rect_y1 >= *y1) {
      *y1 = rect_y1;
      intersect = 1;
    }
    *x1 -= robot_r;
    *y1 += robot_r;
    *x2 += robot_r;
    *y2 -= robot_r;
    if (*x1 >= *x2 || *y1 >= *y2) intersect = 0;
  }
  return intersect;
}


/*
 * This routine is used to make free space that corresponds to a doorway.
 * This needs to be fixed for thin alcoves as well, this is a hack
 * to get doors to work.
 */
static void create_new_space(double x1, double y1, double x2, double y2,
			     int room1, int room2)
{
  space[n_space].x1 = MAX(x1,MIN(objs[room1].x1,objs[room2].x1));
  space[n_space].y1 = MAX(y1,MIN(objs[room1].y1,objs[room2].y1));
  space[n_space].x2 = MIN(x2,MAX(objs[room1].x2,objs[room2].x2));
  space[n_space].y2 = MIN(y2,MAX(objs[room1].y2,objs[room2].y2));

  /* This is a hack to prevent a freespace from being stored that has */
  /* edges that would be on the similar edge of either of the rooms */
  
  if (!(objs[room1].type == O_ROOM && objs[room2].type == O_ROOM &&
	(space[n_space].x1==objs[room1].x1 || space[n_space].x1==objs[room2].x1 ||
	 space[n_space].y1==objs[room1].y1 || space[n_space].y1==objs[room2].y1 ||
	 space[n_space].x2==objs[room1].x2 || space[n_space].x2==objs[room2].x2 ||
	 space[n_space].y2==objs[room1].y2 || space[n_space].y2==objs[room2].y2))) {
    /* Make this a freespace */
    n_space++;
    if (n_space > NUM_SPACE) {
      fprintf(stderr, "ERROR: n_space exceeds limits (%d)\n", NUM_SPACE);
      exit(-1);
    }
  }
}

static void compute_freespace(void)
{
  /* precompute regions where robot can legally be in rooms and door ways
   * (not counting obstacles).
   */
  int i,j;
  double x1, x2, y1, y2;
  
  /* mark space within robot radius of each room and doors as free */
  for (i = 0, n_space = 0; i < n_obj; i++){
    if (objs[i].type == O_ROOM || objs[i].type == O_OPEN_DOOR){
      space[n_space].x1 = objs[i].x1 + robot_r;
      space[n_space].y1 = objs[i].y1 + robot_r;
      space[n_space].x2 = objs[i].x2 - robot_r;
      space[n_space].y2 = objs[i].y2 - robot_r;
      if (space[n_space].x2 >= space[n_space].x1 &&
	  space[n_space].y2 >= space[n_space].y1)
	n_space++;
    }
  }
  
  /* mark a corridor of free space between adjacent door and room */
  for (i = 0; i < n_obj; i++){
    if (objs[i].type == O_ROOM || objs[i].type == O_OPEN_DOOR){
      for (j = 0; j < n_obj; j++){
	if (i != j && (objs[j].type == O_ROOM || objs[j].type == O_OPEN_DOOR)){
	  if (test_intersection(&x1,&y1,&x2,&y2,   /* test top wall in i */ 
				objs[i].x1, objs[i].y1, objs[i].x2, objs[i].y1,
				objs[j].x1, objs[j].y1, objs[j].x2, objs[j].y2, 0)
	      == 1)
	    create_new_space(x1,y1,x2,y2,i,j);
	  if (test_intersection(&x1,&y1,&x2,&y2,  /* test bottom wall in i */
				objs[i].x1, objs[i].y2, objs[i].x2, objs[i].y2,
				objs[j].x1, objs[j].y1, objs[j].x2, objs[j].y2, 0)
	      == 1)
	    create_new_space(x1,y1,x2,y2,i,j);
	  if (test_intersection(&x1,&y1,&x2,&y2,   /* test left wall in i */ 
				objs[i].x1, objs[i].y1, objs[i].x1, objs[i].y2,
				objs[j].x1, objs[j].y1, objs[j].x2, objs[j].y2, 1)
	      == 1)
	    create_new_space(x1,y1,x2,y2,i,j);
	  if (test_intersection(&x1,&y1,&x2,&y2,   /* test right wall in i */ 
				objs[i].x2, objs[i].y1, objs[i].x2, objs[i].y2,
				objs[j].x1, objs[j].y1, objs[j].x2, objs[j].y2, 1)
	      == 1)
	    create_new_space(x1,y1,x2,y2,i,j);
	}
      }
    }
  } 
}

void init_environment(char *fname)
{
  int i;
  
  read_environment(fname);
  
  /* determine the largest x and y coordinates used in object definitions */
  env_x = 0;
  env_y = 0;
  for (i = 0; i < n_obj; i++){
    if (env_x < ((int) objs[i].x1)) env_x = ((int) objs[i].x1);
    if (env_x < ((int) objs[i].x2)) env_x = ((int) objs[i].x2);
    if (env_y < ((int) objs[i].y1)) env_y = ((int) objs[i].y1);
    if (env_y < ((int) objs[i].y2)) env_y = ((int) objs[i].y2);
  }
  env_x += 1;
  env_y += 1;
  
  compute_freespace();
}


/************************************************************************
 * distr process generation
 * from zoran@cs
 ************************************************************************/

static void distrInit(void)
{
  static int inited = FALSE;

  if (!inited) {
    inited = TRUE;
    srandom(536870911);		/* 2^29 - 1: I hope it's a prime */
  }
}

static double distrUniform(double lo, double hi)
{
  int i = random();
  double retval = lo + (hi-lo) * ((double) i) / ((double) INT_MAX);

  return retval;
}

double distrNormal(double mean, double sigma)
{
  static int turn = 0;
  double v1, v2, s = 1.0;
  double retval;

  distrInit();
  while (s >= 1) {
    v1 = distrUniform(-1, 1);
    v2 = distrUniform(-1, 1);
    s = v1*v1 + v2*v2;
  }

  if (turn == 0) {
    turn = 1;
    retval = v1*sqrt(-2*log(s)/s);
  }
  else {
    turn = 0;
    retval = v2*sqrt(-2*log(s)/s);
  }

  retval = sigma * retval + mean;
  return retval;
}
