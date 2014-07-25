/*****************************************************************************
 * PROJECT: TDL Tutorial
 *
 * (c) Copyright 2001 Reid Simmons. All rights reserved.
 * (c) Copyright 1994 Richard Goodwin & Reid Simmons. All rights reserved.
 *
 * FILE: sensor.h
 *
 * ABSTRACT: Interface to the sensor.c routines.
 *
 * $Source: /afs/cs.cmu.edu/project/TCA/Master/tcaV8/tutorial/simulator/sensors.h,v $ 
 * $Revision: 1.4 $
 * $Date: 1996/08/05 16:10:19 $
 * $Author: rich $
 *
 * REVISION HISTORY:
 *
 * $Log: sensors.h,v $
 *****************************************************************************/

#include "files.h"

#ifndef INCLUDED_SENSORS
#define INCLUDED_SENSORS

#define CMPERFOOT 30.48

/* sensor variables for the head sonar */
extern int n_sensors;
extern int n_sensors_fact;

extern double sensors_distance;
extern double sensors_range;

extern double sensor[50];
extern double sensor2[200];

extern double sensor_noise_level;
extern double sensor_zero;

void sensors(void);
void sensors1(int s1, int s2);

#endif /* INCLUDED_SENSORS */

