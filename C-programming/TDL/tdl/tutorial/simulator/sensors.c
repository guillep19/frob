/*****************************************************************************
 * PROJECT: TDL Tutorial
 *
 * (c) Copyright 2001 Reid Simmons. All rights reserved.
 * (c) Copyright 1994 Richard Goodwin & Reid Simmons. All rights reserved.
 *
 * FILE: sensor.c
 *
 * ABSTRACT: Code for obtaining simulated sonar readings.
 *
 * $Source: /afs/cs.cmu.edu/project/TCA/Master/tcaV8/tutorial/simulator/sensors.c,v $ 
 * $Revision: 1.2 $
 * $Date: 1995/04/07 05:09:55 $
 * $Author: rich $
 *
 * REVISION HISTORY:
 *
 * $Log: sensors.c,v $
 *****************************************************************************/

#include <math.h>

#include "common.h"
#include "sim-xwindows.h"

#include "sensors.h"
#include "files.h"
#include "action.h"
#include "environment.h"

/*****************************************************************************
 * Global constants
 *****************************************************************************/

/*****************************************************************************
 * Global variables
 *****************************************************************************/

/* sensor variables for the head sonar */
int n_sensors           = 24;
int n_sensors_fact      = 5;

double sensors_distance  = 0.0;
double sensors_range     = 0.0;

double sensor[50];
double sensor2[200];

double sensor_noise_level  = 0.0;
double sensor_zero  = 0.0; /* Actual angle of sonar zero reading */ 

/*****************************************************************************
 * Private variables
 *****************************************************************************/

void sensors1(int s1, int s2)
{
  int i, j, in_new_room, object_i, test=0, obst_test;
  double final_o_x, final_o_y, object_x, object_y, int_x, int_y, dx, dy;
  double dist=0.0;
  double f_i, min_angle, max_angle, angle, robot_oo;
  double sin_robot_oo_plus_f_i, cos_robot_oo_plus_f_i;
  int tried_before[MAX_OBJ];
  double sensorScale;
  
  robot_oo = robot_o + torsoDir + sensor_zero;
  sensorScale = TWO_PI / ((double) (n_sensors * n_sensors_fact));
  
  for (i = s1*n_sensors_fact; i < (s2+1)*n_sensors_fact; i++){
    f_i = i * sensorScale;
    object_i = actual_object;
    final_o_x = object_x = robot_x;
    final_o_y = object_y = robot_y;
    for (j = 0; j < MAX_OBJ; j++) tried_before[j] = 0;
    tried_before[actual_object] = 1;
    do{
      sin_robot_oo_plus_f_i = sin(robot_oo + f_i);
      cos_robot_oo_plus_f_i = cos(robot_oo + f_i);
      obst_test = check_obstacle(objs, n_obj,
				 &int_x, &int_y, object_x, object_y, 
				 sin_robot_oo_plus_f_i,
				 cos_robot_oo_plus_f_i,
				 objs[object_i].x1, objs[object_i].y1,
				 objs[object_i].x2, objs[object_i].y2,
				 0.001);
      if (obst_test == 0)
	test = check_intersection(&int_x, &int_y, object_x, object_y, 
				  sin_robot_oo_plus_f_i, 
				  cos_robot_oo_plus_f_i,
				  objs[object_i].x1, objs[object_i].y1,
				  objs[object_i].x2, objs[object_i].y2);
      if (obst_test == 1 || test == 1){
	final_o_x = int_x;
	final_o_y = int_y;
	dx = final_o_x - robot_x;
	dy = final_o_y - robot_y;
	dist = sqrt((dx*dx) + (dy*dy));
      }
      else{
	int_x = object_x;
	int_y = object_y;
      }
      in_new_room = 0;
      if (obst_test == 0 && dist <= sensors_distance){
	for (j = 0; j < n_obj; j++){
	  if ((objs[j].type == 0 || objs[j].type == 2 )&&
	      in_new_room == 0 && tried_before[j] == 0 &&
	      int_x >= objs[j].x1 - ALMOST_ZERO &&
	      int_x <= objs[j].x2 + ALMOST_ZERO &&
	      int_y >= objs[j].y1 - ALMOST_ZERO &&
	      int_y <= objs[j].y2 + ALMOST_ZERO){
	    object_i = j;
	    object_x = int_x;
	    object_y = int_y;
	    in_new_room = 1;
	    tried_before[j] = 1;
	  }
	} 
      }
    }
    while (in_new_room != 0);
    
    if (dist > sensors_distance){
      final_o_x = robot_x + 
	((final_o_x - robot_x) / dist * sensors_distance);
      final_o_y = robot_y + 
	((final_o_y - robot_y) / dist * sensors_distance);
      dist = sensors_distance;
    }
    sensor2[i] = dist;
  }
  
  for (i = s1; i < s2+1; i ++){
    sensor[i] = 999999999999.9;
    min_angle = ((double) i) / ((double) n_sensors) * TWO_PI;
    max_angle = min_angle + (sensors_range / 2.0);
    min_angle = min_angle - (sensors_range / 2.0);
    for (j = s1*n_sensors_fact; j < (s2+1)*n_sensors_fact; j++){
      angle = j * sensorScale;
      if (angle >= min_angle && angle <= max_angle &&
	  sensor[i] >= sensor2[j]) 
	sensor[i] = sensor2[j];
    }
    /* Should be +- as opposed to just + */
    sensor[i] = distrNormal(sensor[i], sensor_noise_level);
  }
}

void sensors(void)
{
  sensors1(0, n_sensors-1);
}

