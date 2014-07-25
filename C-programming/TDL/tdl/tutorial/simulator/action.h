/*****************************************************************************
 * PROJECT: TDL Tutorial
 *
 * (c) Copyright 2001 Reid Simmons. All rights reserved.
 * (c) Copyright 1994 Richard Goodwin & Reid Simmons. All rights reserved.
 *
 * FILE: action.h
 *
 * ABSTRACT:
 *
 * Interface to the action taking routines.
 *
 * $Source: /afs/cs.cmu.edu/project/TCA/Master/tcaV8/tutorial/simulator/action.h,v $ 
 * $Revision: 1.3 $
 * $Date: 1996/08/05 16:10:13 $
 * $Author: rich $
 *
 * REVISION HISTORY:
 *
 * $Log: action.h,v $
 *****************************************************************************/

#ifndef INCLUDED_ACTION
#define INCLUDED_ACTION

extern double robot_x, robot_y, robot_o, torsoDir;
extern double robot_r;		/* robot's radius */

extern int collision;
extern double robot_min_dist;	/* minimum dist for robot when collide */

extern double forward_noise_level;
extern double turning_noise_level;

extern int actual_object;
extern int actual_freespace;

extern double torsoDir;
extern double rotation_velocity;
extern double rotation_distance;
extern double translation_velocity;
extern double translation_distance;

int check_obstacle(struct _object *lobjs, int ln_obj,
		   double *int_x, double *int_y,
		   double beam_x, double beam_y, double dbeam_x, double dbeam_y,
		   double x1, double y1, double x2, double y2,
		   double offset);

int check_intersection(
		       double *int_x, double *int_y,
		       double beam_x, double beam_y, double 
		       dbeam_x, double dbeam_y,
		       double x1, double y1, double x2, double y2);

void doChange(double deltaX, double deltaY, double deltaT);

void hyperJump(double new_x, double new_y, double new_o);

#endif /* INCLUDED_ACTION */
