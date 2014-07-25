/*****************************************************************************
 * PROJECT: TDL Tutorial
 *
 * (c) Copyright 2001 Reid Simmons. All rights reserved.
 * (c) Copyright 1994 Richard Goodwin & Reid Simmons. All rights reserved.
 *
 * FILE: updates.c
 *
 * ABSTRACT: This file contains the routines that manage position updates
 *           for the simulator.
 *
 * $Source: /afs/cs.cmu.edu/project/TCA/Master/tcaV8/tutorial/simulator/updates.c,v $ 
 * $Revision: 1.3 $
 * $Date: 1995/04/07 05:10:13 $
 * $Author: rich $
 *
 * REVISION HISTORY:
 *
 * $Log: updates.c,v $
 *****************************************************************************/

#include <sys/time.h>
#include <math.h>

#include "common.h"
#include "sim-xwindows.h"

#define EPSILON 0.01

#include "updates.h"
#include "files.h"
#include "action.h"
#include "sensors.h"
#include "draw_env.h"
#include "ezx.h"

/*****************************************************************************
 * Global constants
 *****************************************************************************/

/*****************************************************************************
 * Global variables
 *****************************************************************************/

unsigned long timeScale = 1000;		/* usec/msec */
unsigned long currentTime = 0;		/* in msec */
unsigned long timeIncrement = 100;	/* in msec */
int refreshRate = 10;
int continuallyUpdateLasers = 1;

/*****************************************************************************
 * Private variables
 *****************************************************************************/

/* Local Variables. */

static int nthUpdate = 0;

static unsigned long stepStartTime=0;
static unsigned long stepEndTime=0;

unsigned long getTime(void)		/* get current time in msecs */
{
  struct timeval tp;
  unsigned long t;
  
  gettimeofday(&tp,0);
  t = tp.tv_sec;
  t = t*1000 + (unsigned long)tp.tv_usec/1000;
  return t;
}

void resetTime(void)
{
  stepStartTime = getTime();
}

void refreshSimDisplay (int force)
{
  nthUpdate++;
  if( force || (nthUpdate == refreshRate) ) {
    nthUpdate = 0;
    RedrawSonar(sonarW,NULL,NULL,NULL);
    plot_robot(0);
    plot_robot(1);
    UpdateElapsedTime();
    RedrawTime();
    RedrawPosition(xposW,NULL,NULL,NULL);
  }
}

static void incPosition(double velTheta, double velDistance, 
			double dt, /* sec */
			double currentHeading, 
			double *incX, double *incY, double *incTheta)
{
  double dtheta, r, dx, dy, cost, sint;
  
  if (velTheta == 0.0) {
    dx = velDistance * dt;
    dy = 0.0;
    dtheta = 0.0;
  }
  else {
    dtheta = velTheta * dt;
    r = velDistance/velTheta;
    dx = r*sin(dtheta); 
    dy = r*cos(dtheta) - r;
  }
  cost = cos(currentHeading);
  sint = sin(currentHeading);
  
  *incX = (dx * cost) - (dy * sint);
  *incY = (dy * cost) + (dx * sint);
  *incTheta = dtheta;
}


/*	Function Name: updatePosition
 *	Arguments:     t -- time of next position (in milliseconds)
 *	Description:   update the robot position in the simulator
 *                     and communicate the change to base_frontend
 *	Returns:       nothing
 *
 * Pre: Given the following for time t0: robot_x, robot_y, robot_o,
 *     rotation_velocity, translation_velocity, (may be positive or negative)
 *     rotation_distance, translation_distance (absolute distance,
 *     without sign)
 * Post: update robot_x, robot_y, robot_o,
 *      rotation_position, translation_position to reflect time t.
 */
void updatePosition(unsigned long tMsec)
{
  double incX, incY, incTheta;
  double tSec, dt, maxRotT, maxTransT; /* In seconds */
  double prev_X, prev_Y;
  int event = 0;
  
  if (tMsec != 0) {

    tSec = (double)tMsec/1000.0;

    prev_X = robot_x;
    prev_Y = robot_y;
  
    dt = tSec;
    if ((rotation_distance != 0.0) && (rotation_velocity != 0.0)) {
      maxRotT = FABS(rotation_distance/rotation_velocity);
      if (maxRotT < dt) dt = maxRotT;
    }
    if ((translation_distance != 0.0) && (translation_velocity != 0.0)) {
      maxTransT = FABS(translation_distance/translation_velocity);
      if (maxTransT < dt) dt = maxTransT;
    }

    incPosition(rotation_velocity, translation_velocity, dt,
		robot_o, &incX, &incY, &incTheta);
  
    /* Change in coordinates: incY = incX and incX = incY. The 
       simulator considers north = 0 degrees and east = 90, while
       we consider north = 90 and east = 0 */
  
    doChange(incY, incX, incTheta);
  
    /* Update distances, stopping the movement if the robot
       has achieve the goal */
  
    if (rotation_distance != 0.0) {
      rotation_distance -= rotation_velocity * dt;
      if (FABS(rotation_distance) < EPSILON) {
	rotation_distance = 0.0;
	rotation_velocity = 0.0;
      }
    }
  
    if (translation_distance != 0.0) {
      translation_distance -= translation_velocity * dt;
      if (FABS(translation_distance) < EPSILON) {
	translation_distance = 0.0;
	translation_velocity = 0.0;
      }
    }
  
    if (dt < tSec) {
    
      /* the rest of the time is moving straigtforward, or it is 
	 rotating in the same place */
    
      dt = tSec - dt;
      incPosition(rotation_velocity, translation_velocity, dt,
		  robot_o, &incX, &incY, &incTheta);

      /* The same change in coordinates */
      doChange(incY, incX, incTheta);
    }
  
    /* check if the robot is blocked */
    if (collision) {
      translation_velocity = 0.0;
      rotation_velocity = 0.0;
    }
  
    currentTime += tMsec;
    /* Force update of display if at end of move, regardless of refresh rate */
    refreshSimDisplay(event);
  }
}

void forceUpdate(void)
{
  unsigned long msec, t;
  
  if( rotation_velocity != 0 || translation_velocity != 0 ) {
    stepEndTime = getTime();
    msec = (double)(stepEndTime - stepStartTime);
    t = 1000*msec/timeScale;
    /* don't count time if we have missed some updates.
     * limit the time jump to 3 standard update times.
     */
    if (t > 3 * timeIncrement) {
      fprintf(stderr,"Missed Simulations steps for %ld msec\n",msec);
      t = 2 * timeIncrement;
    }
    updatePosition( t );
    stepStartTime = stepEndTime;
  }
}

void simUpdate (void)
{
  unsigned long msec, t;
  
  if( rotation_velocity != 0 || translation_velocity != 0 ) {
    stepEndTime = getTime();
    msec = stepEndTime - stepStartTime;
    t = 1000*msec/timeScale;
    if (t >= timeIncrement) {
      if (t > 3 * timeIncrement) {
        fprintf(stderr,"Missed Simulations steps for %ld msec\n",msec);
        t = 2 * timeIncrement;
      }
      updatePosition(t);
      sensors();
      stepStartTime = stepEndTime;
    }
  } else {
    UpdateElapsedTime();
    RedrawTime();
  }
}
