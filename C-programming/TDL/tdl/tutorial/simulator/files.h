/*****************************************************************************
 * PROJECT: TDL Tutorial
 *
 * (c) Copyright 2001 Reid Simmons. All rights reserved.
 * (c) Copyright 1994 Richard Goodwin & Reid Simmons. All rights reserved.
 *
 * FILE: files.h
 *
 * ABSTRACT: Interface to the parameter file reading routines in files.c.
 *
 * $Source: /afs/cs.cmu.edu/project/TCA/Master/tcaV8/tutorial/simulator/files.h,v $ 
 * $Revision: 1.4 $
 * $Date: 1996/08/05 16:10:18 $
 * $Author: rich $
 *
 * REVISION HISTORY:
 *
 * $Log: files.h,v $
 *****************************************************************************/

#ifndef INCLUDED_FILES
#define INCLUDED_FILES

typedef struct _object {
  double    x1, y1, x2, y2;
  int      type;            /* 0=room, 1=closed door, 2=open door, 
			       3=start, 4=goal 5=rect. obstacle 6=round obst*/
  int      status;          /* depending on obj-type */
  int      prev_status;
  double    diameter;
  int      objID;           /* used to identify objects when adding and 
			       deleting them.  It is unique */
  char     *room_name;      /* used to add a room names */
} *object;

#define O_ROOM		0
#define O_CLOSE_DOOR	1
#define O_OPEN_DOOR	2
#define O_START		3
#define O_GOAL		4
#define O_RECTANGLE	5
#define O_ROUND		6

#define ROOM_STATUS 	0
#define CORRIDOR_STATUS	1
#define AREA_STATUS     2

#define MAX_OBJ 100

extern struct _object objs[];
extern int n_obj;
extern int id_obj;
extern int pause_sim;
extern double units_per_cm;

struct _object *add_room(double x1, double y1, double x2, double y2,
			 char *name, int status);
struct _object *add_room_size(double x1, double y1, double width,
			      double height, int status);
struct _object *add_door(double x1, double y1, double x2, double y2, int open);
struct _object *add_door_size(double x1, double y1, double width, int open);

struct _object *add_obst(int status, double x1, double y1,
			 double x2, double y2);
struct _object *add_obst_size(int status, 
			      double x1, double y1, double width, double height);
struct _object *add_round(int status, double x1, double y1, double diameter);
struct _object *redo_obj(void);
struct _object *delete_obj(void);

void read_environment(char *file_name);

#endif /* INCLUDED_FILES */
