/*****************************************************************************
 * PROJECT: TDL Tutorial
 *
 * (c) Copyright 2001 Reid Simmons. All rights reserved.
 * (c) Copyright 1994 Reid Simmons & Richard Goodwin. All rights reserved.
 *
 * FILE: simMessages.h
 *
 * ABSTRACT: IPC message interface to simulator.  Contains the definitions 
 *           for the message names, data structures, and IPC data formats.
 *
 * The following messages and queries are supported:
 * 
 * 		  BOUNDED_MOVE_COMMAND
 * 		  UNBOUNDED_MOVE_COMMAND
 * 		  GUARDED_MOVE_COMMAND
 * 		  TURN_COMMAND
 * 		  STOP_COMMAND
 * 		  SET_VELOCITY_COMMAND
 * 		  TALK_COMMAND
 * 		  SONAR_PING_QUERY
 * 		  SONAR_SCAN_QUERY
 * 		  SONAR_SCAN_LIST_QUERY
 * 		  SONAR_SCAN_VAR_QUERY
 * 		  VELOCITY_QUERY
 * 		  DISTANCE_MOVED_QUERY
 * 
 * 		For more information on the behavior and function of
 * 		each of these messages, see the corresponding functions
 * 		in simInterface.c.
 *
 * $Source: /afs/cs.cmu.edu/project/TCA/Master/tcaV8/tutorial/simulator/simMessages.h,v $
 * $Revision: 1.5 $
 * $Date: 1996/08/05 16:10:23 $
 * $Author: rich $
 *
 * REVISION HISTORY:
 *
 * $Log: simMessages.h,v $
 *****************************************************************************/

#ifndef SIM_MESSAGES_H
#define SIM_MESSAGES_H

#include <ipc.h>
#include "common.h"

#define NULL_REPLY_MSG "nullReply"
#define SUCCESS_MSG    "commandSucceeded"

typedef struct {
  char *failure;
  char *dataFormat;
  IPC_VARCONTENT_TYPE failureData;
} FAILURE_TYPE, *FAILURE_PTR;
#define FAILURE_MSG    "commandFailed"
#define FAILURE_FORMAT "{string, string, {uint, <byte:1>}}"

/*---------------------------------------------------------------------------
  Message definitions
  
  The messages are defined in this section.  Each message is defined
  with it's associated message form, and a typedef for the argument(s)
  that get passed to the handler.
  */

typedef CMS BOUNDED_MOVE_COMMAND_TYPE, *BOUNDED_MOVE_COMMAND_PTR;

#define BOUNDED_MOVE_COMMAND	"bounded_move_c"
#define BOUNDED_MOVE_FORM	CMS_FORMAT


typedef void UNBOUNDED_MOVE_COMMAND_TYPE, *UNBOUNDED_MOVE_COMMAND_PTR;

#define UNBOUNDED_MOVE_COMMAND	"unbounded_move_c"
#define UNBOUNDED_MOVE_FORM	NULL


/* structure for sending guarded move message */
typedef struct { CMS  distance;
		 CMS  stopRange;
	       } GUARDED_MOVE_COMMAND_TYPE, *GUARDED_MOVE_COMMAND_PTR;

#define GUARDED_MOVE_COMMAND "guarded_move_c"
#define GUARDED_MOVE_FORM    "{cms, cms}"

#define GUARDED_MOVE_FAILURE      "GuardedMoveFailure"
#define GUARDED_MOVE_FAILURE_FORM "cms"

typedef DEGREES TURN_COMMAND_TYPE, *TURN_COMMAND_PTR;

#define TURN_COMMAND "turn_c"
#define TURN_FORM    "degrees"

#define TERMINATED_CMD_FAILURE "TerminatedCmdFailure"

typedef void STOP_COMMAND_TYPE, *STOP_COMMAND_PTR;

#define STOP_COMMAND "stop_c"
#define STOP_FORM    NULL


typedef struct { double transVelocity;
		 double rotVelocity;
	       } SET_VELOCITY_INFORM_TYPE, *SET_VELOCITY_INFORM_PTR;

#define SET_VELOCITY_INFORM "set_velocity_m"
#define SET_VELOCITY_FORM   "{double, double}"


typedef char *TALK_COMMAND_TYPE, **TALK_COMMAND_PTR;

#define TALK_COMMAND "talk_c"
#define	TALK_FORM    "string"


typedef int SONAR_PING_QUERY_TYPE, *SONAR_PING_QUERY_PTR;

typedef CMS SONAR_PING_REPLY_TYPE, *SONAR_PING_REPLY_PTR;

#define SONAR_PING_QUERY	"sonar_ping_q"
#define SONAR_PING_QUERY_FORM	"int"
#define SONAR_PING_REPLY	"sonar_ping_r"
#define SONAR_PING_REPLY_FORM	CMS_FORMAT


/* Structure for sonar scan query. 
 * Return data only for the sonars numbered between "startSonar" and "endSonar"
 * Sonar 0 points straight forward, and the numbers increase clockwise, ending
 * with sonar 23.  You can cross zero, so {startSonar=20, endSonar=3} is legal.
 */
typedef struct { int startSonar;
		 int endSonar;
	       } SONAR_SCAN_QUERY_TYPE, *SONAR_SCAN_QUERY_PTR;

/* Structure for returning sonar data in array
 * The first (startSonar-endSonar+1) entries are filled with real data,
 * the rest are zeroed.
 */
typedef struct { CMS  sonarData[24];
	       } SONAR_SCAN_REPLY_TYPE, *SONAR_SCAN_REPLY_PTR;

#define SONAR_SCAN_QUERY        "sonar_scan_q"
#define	SONAR_SCAN_QUERY_FORM	"{int,int}"
#define SONAR_SCAN_REPLY        "sonar_scan_r"
#define	SONAR_SCAN_REPLY_FORM	"{[cms:24]}"


typedef SONAR_SCAN_QUERY_TYPE SONAR_SCAN_VAR_QUERY_TYPE;
typedef SONAR_SCAN_QUERY_TYPE *SONAR_SCAN_VAR_QUERY_PTR;

/* structure for returning sonar data in variable length array */
typedef struct { int numItems;
		 CMS *sonarData;
	       } SONAR_SCAN_VAR_REPLY_TYPE, *SONAR_SCAN_VAR_REPLY_PTR;

#define SONAR_SCAN_VAR_QUERY	  "sonar_scan_var_q"
#define	SONAR_SCAN_VAR_QUERY_FORM "{int,int}"
#define SONAR_SCAN_VAR_REPLY	  "sonar_scan_var_r"
#define	SONAR_SCAN_VAR_REPLY_FORM "{int, <cms:1>}"


typedef SONAR_SCAN_QUERY_TYPE SONAR_SCAN_LIST_QUERY_TYPE; 
typedef SONAR_SCAN_QUERY_TYPE *SONAR_SCAN_LIST_QUERY_PTR; 

/* Structure for returning sonar data in linked list.
 * The actual value returned is a POINTER to the list
 */
typedef struct sonar_list_tag { CMS sonarData;
				struct sonar_list_tag *next;
			      } SONAR_LIST_TYPE, *SONAR_LIST_PTR;

typedef SONAR_LIST_PTR SONAR_SCAN_LIST_REPLY_TYPE, *SONAR_SCAN_LIST_REPLY_PTR;

#define SONAR_SCAN_LIST_QUERY	   "sonar_scan_list_q"
#define	SONAR_SCAN_LIST_QUERY_FORM "{int,int}"
#define SONAR_SCAN_LIST_REPLY	   "sonar_scan_list_r"
#define	SONAR_SCAN_LIST_REPLY_FORM "*{cms, *!}"


typedef void VELOCITY_QUERY_TYPE, *VELOCITY_QUERY_PTR;

typedef SET_VELOCITY_INFORM_TYPE VELOCITY_REPLY_TYPE, *VELOCITY_REPLY_PTR;

#define VELOCITY_QUERY      "velocity_q"
#define	VELOCITY_QUERY_FORM NULL
#define VELOCITY_REPLY      "velocity_r"
#define VELOCITY_REPLY_FORM SET_VELOCITY_FORM


typedef void MOTION_QUERY_TYPE, *MOTION_QUERY_PTR;

typedef int MOTION_REPLY_TYPE, *MOTION_REPLY_PTR;

#define MOTION_QUERY      "motion_q"
#define	MOTION_QUERY_FORM NULL
#define MOTION_REPLY      "motion_r"
#define MOTION_REPLY_FORM "boolean"


typedef void DISTANCE_MOVED_QUERY_TYPE, *DISTANCE_MOVED_QUERY_PTR;

typedef CMS DISTANCE_MOVED_REPLY_TYPE, *DISTANCE_MOVED_REPLY_PTR;

#define DISTANCE_MOVED_QUERY      "distance_moved_q"
#define	DISTANCE_MOVED_QUERY_FORM NULL
#define DISTANCE_MOVED_REPLY      "distance_moved_r"
#define DISTANCE_MOVED_REPLY_FORM CMS_FORMAT

#define CONTROLLER_MODULE "Simulator"

#endif /* SIM_MESSAGES_H */
