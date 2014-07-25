/*****************************************************************************
 * PROJECT: TDL Tutorial
 *
 * (c) Copyright 2001 Reid Simmons. All rights reserved.
 * (c) Copyright 1994 Reid Simmons & Richard Goodwin. All rights reserved.
 *
 * FILE: simInterface.c
 *
 * ABSTRACT: Interface to simulator for use with TDL tutorial
 *
 * The functions which control movement are currently implemented as
 * blocking functions.  Any movement command will not return until it
 * has completed.
 *
 * $Source: /afs/cs.cmu.edu/project/TCA/Master/tcaV8/tutorial/simulator/simInterface.c,v $
 * $Revision: 1.3 $
 * $Date: 1995/04/07 05:10:08 $
 * $Author: rich $
 *
 * REVISION HISTORY:
 *
 * $Log: simInterface.c,v $
 *****************************************************************************/

#include <ipc.h>

#include "common.h"
#include "ezx.h"
#include "files.h"
#include "action.h"
#include "sensors.h"
#include "updates.h"
#include "simInterface.h"

/* local state variables */
static double currentTransVelocity = DEF_TRANS_VELOCITY;
static double currentRotVelocity = DEF_ROT_VELOCITY;
static int motionStatus = FALSE;

/*---------------------------- simStartBoundedMove ---------------------------

This function tells the robot to move a given distance.  The velocity
will be the current velocity set by the simSetVelocity() function or
the default value set on startup.  The direction of movement
is forward only, in a straight line.  This is a bounded move only
so no check is done to see whether obstacles must be avoided.

Just starts the move -- move is not necessarily ended when the function
returns (ending condition is checked for in handleSimEvents, in sim.c).

The calling syntax is:

	simStartBoundedMove( distance );

where the parameters are as follows:

	distance  - the distance in centimeters for the robot to travel

Returns: nothing

------------------------------------------------------------------------*/

void simStartBoundedMove (CMS distance)
{
  motionStatus = TRUE;
  translation_velocity = currentTransVelocity;
  translation_distance = distance;
  resetTime();
}

/*---------------------------- simStartGuardedMove ---------------------------

This function tells the robot to move a given distance.  The velocity
will be the current velocity set by the simSetVelocity() function or
the default value set on startup.  This is a guarded move so
the sonar is checked to see if obstacles are within a given radius of
the robot.  The radius (in centimeters) is passed in as a parameter.

Just starts the move -- move is not necessarily ended when the function
returns (sonar status is checked for in handleSimEvents, in sim.c).

The calling syntax is:

	rtn = simStartGuardedMove( distance, stopRange );

where the parameters are as follows:

	distance  - the distance in centimeters for the robot to travel
	stopRange - the minimum allowable distance from an obstacle
		    before the robot stops moving, the accuracy for
		    the stop range is about 5 centimeters

Returns: none

------------------------------------------------------------------------*/

void simStartGuardedMove (CMS distance, CMS stopRange)
{
  motionStatus = TRUE;
  translation_velocity = currentTransVelocity;
  translation_distance = distance;
  sensors();
  resetTime();
}


/*---------------------------- simStartTurn ----------------------------------

This function tells the robot to rotate in place.  The rotation is
given in degrees, relative to the current orientation of the robot.
Positive means clockswise, and negative means counterclockwise

Just starts the move -- move is not necessarily ended when the function
returns (ending condition is checked for in handleSimEvents, in sim.c).

The calling syntax is:

	simStartTurn( turn );

where the parameters are as follows:

	turn - the angle to rotate, positive means clockswise,
		  negative means counterclockwise

Returns: nothing

------------------------------------------------------------------------*/

void simStartTurn (DEGREES turn)
{
  motionStatus = TRUE;

  rotation_velocity = DEG_TO_RAD(currentRotVelocity);
  rotation_distance = DEG_TO_RAD(turn);
  if (turn < 0) {
    rotation_velocity = -rotation_velocity;
  }
  resetTime();
}



/*---------------------------- simStop ---------------------------------

This function tells the robot to stop all movement.

The calling syntax is:

	simStop();

Returns: nothing

------------------------------------------------------------------------*/

void simStop(void)
{
  motionStatus = FALSE;
  translation_velocity = rotation_velocity = 0.0;
}


/*---------------------------- simSetVelocity ---------------------------

This function sets the velocity at which the robot will move when it
receives a movement command.  The velocity is given in centimeters per second.

The calling syntax is:

	simSetVelocity(transVelocity, rotVelocity);

where the parameters are as follows:

	transVelocity - velocity in centimeters per second
	rotVelocity - velocity in radians per second

Returns: nothing

------------------------------------------------------------------------*/

void simSetVelocity(double transVelocity, double rotVelocity)
{
  currentTransVelocity = transVelocity;
  currentTransVelocity = rotVelocity;
}


/*---------------------------- simVelocity ---------------------------

This function will return the current velocity used for movement.

The calling syntax is:

	simVelocity(&transVelocity, &rotVelocity);

Where: transVelocity - is set to the current translational velocity (cm/sec)
       rotVelocity - is set to the current rotational velocity (rad/sec)

------------------------------------------------------------------------*/

void simVelocity(double *transVelocity, double *rotVelocity)
{
  *transVelocity = currentTransVelocity;
  *rotVelocity = currentTransVelocity;
}


/*---------------------------- simTalk ---------------------------

This function makes the robot "talk".

The calling syntax is:

	simTalk( char *message );

where the parameters are as follows:

	message - a string to "speak"

Returns: nothing

------------------------------------------------------------------------*/

void simTalk(char *message)
{
    printf("%s\n", message);
}


/*---------------------------- sonarScan ---------------------------

This function returns a set of 24 sonar readings, with sonar 0 facing forward,
and numbers increasing clockwise.  The sonar readings are in centimeters and
are calculated from the center of the robot.

The calling syntax is:

	sonarScan( sonarData );

where the parameters are as follows:

	sonarData - an array of 24 sonar readings (in centimeters)

Returns: an array containing the distance to any obstacle detected
	 in the sonarData parameter.  A value greater or equal to
	 MAX_SONAR_RANGE indicates no obstacle was detected

------------------------------------------------------------------------*/

void sonarScan(CMS sonarData[24])
{
  int i;
  
  sensors();
  for (i=0; i<24; i++) sonarData[i] = sensor[i];
}


/*---------------------------- simInMotion  ---------------------------

  This function will return whether the robot is currently moving.
  
  The calling syntax is:
  
  inMotion = simInMotion();
  
  Returns: if the robot is currently moving (TRUE/1 == moving;
  FALSE/0 == stopped)
  
  ---------------------------------------------------------------------------*/

int simInMotion (void)
{
  return motionStatus;
}
