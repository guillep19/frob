/*****************************************************************************
 * PROJECT: TDL Tutorial
 *
 * (c) Copyright 2001 Reid Simmons. All rights reserved.
 * (c) Copyright 1994 Richard Goodwin & Reid Simmons. All rights reserved.
 *
 * FILE: action.c
 *
 * ABSTRACT:
 * 
 * This file provides a set for routines for simulating the action of the
 * robot in the environment.
 *
 * $Source: /afs/cs.cmu.edu/project/TCA/Master/tcaV8/tutorial/simulator/action.c,v $ 
 * $Revision: 1.3 $
 * $Date: 1995/04/07 05:09:39 $
 * $Author: rich $
 *
 * REVISION HISTORY:
 *
 * $Log: action.c,v $
 *****************************************************************************/

#include <math.h>

#include "common.h"
#include "sim-xwindows.h"

#include "files.h"
#include "action.h"
#include "updates.h"
#include "sensors.h"
#include "environment.h"
#include "draw_env.h"

/*****************************************************************************
 * Global constants
 *****************************************************************************/

/*****************************************************************************
 * Global variables
 *****************************************************************************/

double robot_x=0.0, robot_y=0.0, robot_o = 0.0;
double robot_r           = 0.0;
int collision           = 0;
int all_goals_reached   = 0;
double robot_min_dist    = 0.1;	/* minimum dist for robot when collide */

double forward_noise_level = 0.0;
double turning_noise_level = 0.0;

int actual_object       = 0;
int actual_freespace    = 0;

double torsoDir = 0.0;
double rotation_velocity = 0.0;
double rotation_distance = 0.0;
double translation_velocity = 0.0;
double translation_distance = 0.0;

static int intersect_lines(double  *int_x, double *int_y,
		    double  x1, double y1, double dx1, double dy1,
		    double  x2, double y2, double dx2, double dy2,
		    int orient)
{ /* dx1|dy1: only horizontal (orient=0) or vertical (orient=1) */
  double lambda, temp;
    
  if (orient == 0){    /* horizontal */
    if (FABS(dy2) > ALMOST_ZERO){  
      /* lines not parallel (otherwise no intersection) */
      lambda = (y1 - y2) / dy2;
      if (lambda > 0.0){ 
	*int_y = y1;
	*int_x = temp = x2 + (lambda * dx2);
	if (temp >= x1 && temp <= x1+dx1)
	  return TRUE;
      }
    }
  } else               /* vertical */
    if (FABS(dx2) > ALMOST_ZERO){  /* not parallel */
      lambda = (x1 - x2) / dx2;
      if (lambda > 0.0){
	*int_x = x1;
	*int_y = temp = y2 + (lambda * dy2);
	if (temp >= y1 && temp <= y1+dy1)
	  return TRUE;
      }
    }
  return FALSE;
}

int check_intersection(double *int_x, double *int_y,
		       double beam_x, double beam_y, double 
		       dbeam_x, double dbeam_y,
		       double x1, double y1, double x2, double y2)
{
  
  if (beam_x < x1 || beam_x > x2 || beam_y < y1 || beam_y > y2)
    return FALSE; /* beam origin not in objext x1y1-x2y2 */
  if ((dbeam_y < 0.0 && 
       intersect_lines(int_x, int_y, x1, y1, x2-x1, 0.0, 
		       beam_x, beam_y, dbeam_x, dbeam_y, 0)) ||
      (dbeam_x < 0.0 &&
       intersect_lines(int_x, int_y, x1, y1, 0.0, y2-y1,
		       beam_x, beam_y, dbeam_x, dbeam_y, 1)) ||
      (dbeam_y > 0.0 &&
       intersect_lines(int_x, int_y, x1, y2, x2-x1, 0.0,
		       beam_x, beam_y, dbeam_x, dbeam_y, 0)) ||
      (dbeam_x > 0.0 &&
       intersect_lines(int_x, int_y, x2, y1, 0.0, y2-y1, 
		       beam_x, beam_y, dbeam_x, dbeam_y, 1))){
    return TRUE;
  }
  return FALSE;
}



int check_obstacle(struct _object *objs_l, int n_obj_l, 
		   double *int_x, double *int_y,
		   double beam_x, double beam_y, 
		   double dbeam_x, double dbeam_y,
		   double x1, double y1, double x2, double y2,
		   double offset)
{
  int intersect = 0, i;
  double dist, min_dist=0.0;
  double closest_int_x=0.0, closest_int_y=0.0;
  double a, b, c, lambda, h1, h2, discr, diameter;
  double x1_minus_offset = x1 - offset, y1_minus_offset = y1 - offset,
        x2_plus_offset = x2 + offset, y2_plus_offset = y2 + offset;
  double temp1, temp2;
 
  for (i = 0; i < n_obj_l; i++){
    if (objs_l[i].type == O_RECTANGLE && objs_l[i].status == 0 && 
	/* rectangle */
	((objs_l[i].x1 >= x1_minus_offset && objs_l[i].x1 <= x2_plus_offset &&
	  objs_l[i].y1 >= y1_minus_offset && objs_l[i].y1 <= y2_plus_offset) ||
	 (objs_l[i].x2 >= x1_minus_offset && objs_l[i].x2 <= x2_plus_offset &&
	  objs_l[i].y1 >= y1_minus_offset && objs_l[i].y1 <= y2_plus_offset) ||
	 (objs_l[i].x1 >= x1_minus_offset && objs_l[i].x1 <= x2_plus_offset &&
	  objs_l[i].y2 >= y1_minus_offset && objs_l[i].y2 <= y2_plus_offset) ||	
	 (objs_l[i].x2 >= x1_minus_offset && objs_l[i].x2 <= x2_plus_offset &&
	  objs_l[i].y2 >= y1_minus_offset && objs_l[i].y2 <= y2_plus_offset))){
      if (dbeam_y > 0.0 && 
	  intersect_lines(int_x, int_y, objs_l[i].x1-offset, 
			  objs_l[i].y1-offset, 
			  objs_l[i].x2-objs_l[i].x1+(2.0*offset), 0.0,
			  beam_x, beam_y, dbeam_x, dbeam_y, 0) &&
	  *int_x >= objs_l[i].x1-offset && *int_x <= objs_l[i].x2+offset){
	temp1 = beam_x - *int_x;
	temp2 = beam_y - *int_y;
	dist = (temp1 * temp1) + (temp2 * temp2);
	if (intersect == FALSE || dist < min_dist){
	  min_dist = dist;
	  intersect = TRUE;
	  closest_int_x = *int_x;
	  closest_int_y = *int_y;
	}
      } else if (dbeam_y < 0.0 &&
		 intersect_lines(int_x, int_y, objs_l[i].x1-offset, 
				 objs_l[i].y2+offset, 
				 objs_l[i].x2-objs_l[i].x1+(2.0*offset), 0.0,
				 beam_x, beam_y, dbeam_x, dbeam_y, 0)
		 && *int_x >= objs_l[i].x1-offset
		 && *int_x <= objs_l[i].x2+offset) {
	temp1 = beam_x - *int_x;
	temp2 = beam_y - *int_y;
	dist = (temp1 * temp1) + (temp2 * temp2);
	if (intersect == FALSE || dist < min_dist) {
	  min_dist = dist;
	  intersect = TRUE;
	  closest_int_x = *int_x;
	  closest_int_y = *int_y;
	}
      }   
      if (dbeam_x > 0.0 && 
	  intersect_lines(int_x, int_y, objs_l[i].x1-offset, 
			  objs_l[i].y1-offset,
			  0.0, objs_l[i].y2-objs_l[i].y1+(2.0*offset),
			  beam_x, beam_y, dbeam_x, dbeam_y, 1) &&
	  *int_y >= objs_l[i].y1-offset && *int_y <=
	  objs_l[i].y2+offset){
	temp1 = beam_x - *int_x;
	temp2 = beam_y - *int_y;
	dist = (temp1 * temp1) + (temp2 * temp2);
	if (intersect == FALSE || dist < min_dist){
	  min_dist = dist;
	  intersect = TRUE;
	  closest_int_x = *int_x;
	  closest_int_y = *int_y;
	}
      } else if (dbeam_x < 0.0 && 
		 intersect_lines(int_x, int_y, objs_l[i].x2+offset, 
				 objs_l[i].y1-offset, 
				 0.0, objs_l[i].y2-objs_l[i].y1+(2.0*offset), 
				 beam_x, beam_y, dbeam_x, dbeam_y, 1)
		 && *int_y >= objs_l[i].y1-offset
		 && *int_y <= objs_l[i].y2+offset) {
	temp1 = beam_x - *int_x;
	temp2 = beam_y - *int_y;
	dist = (temp1 * temp1) + (temp2 * temp2);
	if (intersect == FALSE || dist < min_dist) {
	  min_dist = dist;
	  intersect = TRUE;
	  closest_int_x = *int_x;
	  closest_int_y = *int_y;
	}
      }    
    }
    
    diameter = objs_l[i].diameter + offset;
    if (objs_l[i].type == O_ROUND && objs_l[i].status == 0 && /* rectangle */
	objs_l[i].x1 >= x1-diameter && objs_l[i].x1 <= x2+diameter &&
	objs_l[i].y1 >= y1-diameter && objs_l[i].y1 <= y2+diameter){
      /* intersection circle with line */
      a = (dbeam_x * dbeam_x) + (dbeam_y * dbeam_y);
      b = (beam_x*dbeam_x) + (beam_y*dbeam_y)
	- (dbeam_x*objs_l[i].x1) - (dbeam_y*objs_l[i].y1);
      h1 = beam_x - objs_l[i].x1;
      h2 = beam_y - objs_l[i].y1;
      c = (h1 * h1) + (h2 * h2) - (diameter * diameter);
      if (a != 0.0){
	discr = ((b*b) / (a*a)) - (c/a);
	if (discr > 0.0){
	  discr = sqrt(discr);
	  h1 = 0.0 - (b / a);
	  lambda = h1 - discr;
	  if (lambda < 0.0 && h1 + discr >= 0.0){
	    lambda = 0.0;
	    fprintf(stderr,
		    "\nWarning: object %f %f in obst #%d?",beam_x, beam_y,i);
	  }
	  if (lambda >= 0.0){
	    h1 = lambda * dbeam_x;
	    h2 = lambda * dbeam_y;
	    *int_x = beam_x + h1;
	    *int_y = beam_y + h2;
	    dist = (h1 * h1) + (h2 * h2);
	    if (intersect == FALSE || dist < min_dist){
	      min_dist = dist;
	      intersect = TRUE;
	      closest_int_x = *int_x;
	      closest_int_y = *int_y;
	    }
	  }
	}
      }
    }
  }
  if (intersect){
    *int_x = closest_int_x;
    *int_y = closest_int_y;
  }
  return intersect;
}

void doChange(double deltaX, double deltaY, double deltaT )
{
  int i, space_i, in_new_space, j, test, obst_test;
  double dist=0.0, dx, dy, object_x, object_y, d3, d2;
  double old_x, old_y, old_o;
  double robot2_x=0.0, robot2_y=0.0, robot3_x=0.0, robot3_y=0.0;
  double fwdSq, angle=0.0;
  int tried_before[MAX_OBJ];
  
  if (actual_freespace < 0)
    for (i = 0; i < n_space; i++){
      if (robot_x >= space[i].x1 && robot_x <= space[i].x2 &&
	  robot_y >= space[i].y1 && robot_y <= space[i].y2)
	actual_freespace = i;
    }
  if (actual_freespace < 0){
    fprintf(stderr, "\nWARNING: robot not in free space...");
  }

  if (turning_noise_level != 0) {
    deltaT = distrNormal(deltaT, deltaT*turning_noise_level);
  }
  if (forward_noise_level != 0) {
    deltaX = distrNormal(deltaX, deltaX*forward_noise_level);
    deltaY = distrNormal(deltaY, deltaY*forward_noise_level);
  }

  fwdSq = (double)(SQR(deltaX) + SQR(deltaY));
  if( deltaX == 0 && deltaY == 0 )
    angle = robot_o;
  else
    angle = (double)PI/2-(double)atan2(deltaY,deltaX);
  
  old_o = robot_o;
  robot_o = angle;
  
  old_x = robot_x;
  old_y = robot_y;
  
  for (; robot_o >= TWO_PI; robot_o -= TWO_PI);
  for (; robot_o <   0.0     ; robot_o += TWO_PI);
  collision = 0;
  
  if (check_obstacle(objs, n_obj,
		     &robot_x, &robot_y, old_x, old_y, 
		     sin(robot_o), cos(robot_o), space[actual_freespace].x1,
		     space[actual_freespace].y1, space[actual_freespace].x2, 
		     space[actual_freespace].y2, robot_r) == 0){
    robot_x = old_x + deltaX;
    robot_y = old_y + deltaY;
  } else {
    dist = (((robot_x - old_x) *  (robot_x - old_x)) +
	    ((robot_y - old_y) *  (robot_y - old_y)));
    if (dist < fwdSq) {
      fprintf(stderr, "Collision: At distance %.2f\n", dist);
      collision = 1;
    }
    if (dist >= fwdSq){
      robot_x = old_x + deltaX;
      robot_y = old_y + deltaY;
    }
  }
  
  if (robot_x < space[actual_freespace].x1 ||
      robot_x > space[actual_freespace].x2 ||
      robot_y < space[actual_freespace].y1 ||
      robot_y > space[actual_freespace].y2){ /* robot left last freespace */
    robot_x = object_x = old_x;
    robot_y = object_y = old_y;
    space_i = actual_freespace;
    for (i = 0; i < MAX_OBJ; i++) tried_before[i] = 0;
    tried_before[space_i] = 1;
    in_new_space = 1; 
    do{
      obst_test = check_obstacle(objs, n_obj,
				 &robot2_x, &robot2_y, object_x, object_y, 
				 sin(robot_o), cos(robot_o),
				 space[space_i].x1, space[space_i].y1,
				 space[space_i].x2, space[space_i].y2,
				 robot_r);
      test = check_intersection(&robot3_x, &robot3_y, object_x, object_y,
				sin(robot_o), cos(robot_o),
				space[space_i].x1, space[space_i].y1,
				space[space_i].x2, space[space_i].y2);
      if (obst_test == 1 || test == 1){
	d2 = SQR(robot2_x - object_x) + SQR(robot2_y - object_y);
	d3 = SQR(robot3_x - object_x) + SQR(robot3_y - object_y);
	if (obst_test == 1 && (test == 0 || d2 < d3)){
	  robot_x = robot2_x;
	  robot_y = robot2_y;
	} else {
	  robot_x = robot3_x;
	  robot_y = robot3_y;
	}
	
	dx = robot_x - old_x;
	dy = robot_y - old_y;
	dist = (dx * dx) + (dy * dy);
	if (dist > fwdSq){
	  robot_x = old_x + deltaX;
	  robot_y = old_y + deltaY;
	}
	dx = robot_x - object_x;
	dy = robot_y - object_y;
      }
      in_new_space = 0;
      if (obst_test == 0 && dist <= fwdSq){
	for (j = 0; j < n_space; j++){
	  if (tried_before[j] == 0 && in_new_space == 0 &&
	      robot_x >= space[j].x1 - ALMOST_ZERO &&
	      robot_x <= space[j].x2 + ALMOST_ZERO &&
	      robot_y >= space[j].y1 - ALMOST_ZERO &&
	      robot_y <= space[j].y2 + ALMOST_ZERO){
	    space_i = j;
	    object_x = robot_x;
	    object_y = robot_y;
	    in_new_space = 1;
	    tried_before[space_i] = 1;
	  }
	}
      }
    }
    while (in_new_space != 0);
    if (dist < fwdSq) {
      fprintf(stderr, "Collision: At distance %.2f\n", dist);
      collision = 1;
    }
    
    for (i = 0; i < n_space; i++){
      if (robot_x >= space[i].x1 && robot_x <= space[i].x2 &&
	  robot_y >= space[i].y1 && robot_y <= space[i].y2)
	actual_freespace = i;
    }
    if (robot_x < objs[actual_object].x1 ||
	robot_x > objs[actual_object].x2 ||
	robot_y < objs[actual_object].y1 ||
	robot_y > objs[actual_object].y2){
      actual_object = -1;
      for (i = 0; i < n_obj && actual_object == -1; i++)
	if (robot_x >= objs[i].x1 &&
	    robot_x <= objs[i].x2 &&
	    robot_y >= objs[i].y1 &&
	    robot_y <= objs[i].y2){
	  actual_object = i;
	  break;
	}
    }
  }
  
  if (collision == 1){
    robot_x -= robot_min_dist * sin(robot_o);
    robot_y -= robot_min_dist * cos(robot_o);
  }
  
  robot_o = old_o + deltaT;
}

/* This is equivalent to the act of lifting the robot and moving it
 * to a new location without the encoders registering the movement.
 */
void hyperJump(double new_x, double new_y, double new_o)
{
  robot_x = new_x;
  robot_y = new_y;
  robot_o = new_o;

  /* this function needs a value */
  /* greater than 0 to update the */
  /* position, so this is how long */
  /* a teleportation will take, 1 msec */			 
  updatePosition(1);   
  
  sensors();
  refreshSimDisplay(TRUE);
  RedrawWholeMap();
}
