/*****************************************************************************
 * PROJECT: TDL Tutorial
 *
 * (c) Copyright 2001 Reid Simmons. All rights reserved.
 * (c) Copyright 1994 Richard Goodwin & Reid Simmons. All rights reserved.
 *
 * FILE: sim.c
 *
 * ABSTRACT:
 *
 * Simulator main program.
 * Includes all the IPC message handlers and registration code
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
 * 		in simInterface.c (see also simMessages.h).
 *
 * $Source: /afs/cs.cmu.edu/project/TCA/Master/tcaV8/tutorial/simulator/sim.c,v $
 * $Revision: 1.13 $
 * $Date: 97/05/29 15:27:39 $
 * $Author: reids $
 *
 * REVISION HISTORY:
 *
 * $Log:	sim.c,v $
 *****************************************************************************/

#include <errno.h>
#include <sys/time.h>
#include <ipc.h>

#include "common.h"
#include "ezx.h"

#include "sim-xwindows.h"
#include "updates.h"
#include "draw_env.h"
#include "environment.h"
#include "sensors.h"
#include "files.h"
#include "action.h"
#include "simMessages.h"
#include "simInterface.h"

/*****************************************************************************
 * Global variables
 *****************************************************************************/
static MSG_INSTANCE currentCmd = NULL;
static CMS commandedMove = 0.0;
static CMS guardedMoveStopRange = 25.0;
static CMS lastMoveDistance = 0.0;

static void terminateCurrentCommand (void)
{
  if (currentCmd != NULL) {
    MSG_INSTANCE ref;
    FAILURE_TYPE failure;

    lastMoveDistance = ABS(commandedMove -
			   (!strcmp(IPC_msgInstanceName(currentCmd), 
				    TURN_COMMAND)
			    ? rotation_distance : translation_distance));

    simStop();

    failure.failure = TERMINATED_CMD_FAILURE;
    failure.dataFormat = NULL;
    failure.failureData.length = 0;
    ref = currentCmd;
    currentCmd = NULL; // Clear before sending response, to avoid race
    IPC_respondData(ref, FAILURE_MSG, &failure);
  }
}

static void setupCommand (MSG_INSTANCE cmdRef, CMS cmdMove)
{
  terminateCurrentCommand();
  currentCmd = cmdRef;
  commandedMove = cmdMove;
}

static void handleSimEvents(void)
{
  fd_set listenList = IPC_getConnections();
  int xSocket = ConnectionNumber(theDisplay);
  XtInputMask mask;
  struct timeval time;
  int stat;
  CMS distanceLeft;
  MSG_INSTANCE ref;
  
  FD_SET(xSocket, &listenList);
  time.tv_sec = 0; time.tv_usec = 10000;
  do {
    stat = select(FD_SETSIZE, &listenList, 0, 0, &time);
  } while (stat < 0 && errno == EINTR);
  
  if (stat < 0 ) {
    fprintf(stderr,"handleSimEvents: Select failed %d\n",errno);
    return;
  }
  
  if (FD_ISSET(xSocket, &listenList)) {
    while ((mask = (XtAppPending(app_context) & (XtIMXEvent | XtIMTimer)))!= 0)
      XtAppProcessEvent(app_context, mask);
  } else {
    IPC_listenClear(0);
  }
  simUpdate();

  if (currentCmd) {
    if ( translation_velocity == 0 && rotation_velocity == 0) {
      lastMoveDistance = commandedMove;
      ref = currentCmd;
      currentCmd = NULL; // Clear before sending response, to avoid race
      IPC_respond(ref, SUCCESS_MSG, 0, NULL);
    } else if (!strcmp(IPC_msgInstanceName(currentCmd), GUARDED_MOVE_COMMAND) &&
	       ((sensor[0]  <= guardedMoveStopRange) ||
		(sensor[23] <= guardedMoveStopRange) ||
		(sensor[1]  <= guardedMoveStopRange))) {
      lastMoveDistance = ABS(commandedMove - translation_distance);
      distanceLeft = ABS(translation_distance);
      simStop();
      ref = currentCmd;
      currentCmd = NULL; // Clear before sending response, to avoid race
      if (distanceLeft <= 0.05) {
	IPC_respond(ref, SUCCESS_MSG, 0, NULL);
      } else {
	FAILURE_TYPE failure;
	failure.failure = GUARDED_MOVE_FAILURE;
	failure.dataFormat = GUARDED_MOVE_FAILURE_FORM;

	IPC_marshall(IPC_parseFormat(failure.dataFormat),
		     &distanceLeft, &failure.failureData);
	IPC_respondData(ref, FAILURE_MSG, &failure);
	IPC_freeByteArray(failure.failureData.content);
      }
    }
  }
}

/*--------------------------------------------------
  
  Handler for the BOUNDED_MOVE_COMMAND message.
  
  ---------------------------------------------------------------------------*/

static void boundedMoveCommandHandler(MSG_INSTANCE ref, void *data,
				      void *dummy)
{
  BOUNDED_MOVE_COMMAND_PTR boundedMoveData = (BOUNDED_MOVE_COMMAND_PTR)data;

  setupCommand(ref, *boundedMoveData);
  simStartBoundedMove(*boundedMoveData);
  IPC_freeData(IPC_msgInstanceFormatter(ref), data);
  IPC_delayResponse(ref);
}


/*-----------------------------------------------
  
  Handler for the UNBOUNDED_MOVE_COMMAND message.
  
  ---------------------------------------------------------------------------*/

/* ARGSUSED */
static void unboundedMoveCommandHandler(MSG_INSTANCE ref, void *data,
					void *dummy)
{
#define VERY_LARGE_DISTANCE 1000000.0
  setupCommand(ref, VERY_LARGE_DISTANCE);
  simStartBoundedMove(VERY_LARGE_DISTANCE);
  IPC_freeData(IPC_msgInstanceFormatter(ref), data);
  IPC_delayResponse(ref);
}


/*-----------------------------------------------
  
  Handler for the GUARDED_MOVE_COMMAND message.
  
  ---------------------------------------------------------------------------*/

static void guardedMoveCommandHandler (MSG_INSTANCE ref, void *data,
				       void *dummy)
{
  GUARDED_MOVE_COMMAND_PTR guardedMoveData = (GUARDED_MOVE_COMMAND_PTR)data;
  
  setupCommand(ref, guardedMoveData->distance);
  guardedMoveStopRange = guardedMoveData->stopRange + robot_r;
  simStartGuardedMove(guardedMoveData->distance, guardedMoveData->stopRange);
  IPC_freeData(IPC_msgInstanceFormatter(ref), data);
  IPC_delayResponse(ref);
}


/*-----------------------------------------------
  
  Handler for the TURN_COMMAND message.
  
  ---------------------------------------------------------------------------*/

static void turnCommandHandler(MSG_INSTANCE ref, void *data, void *dummy)
{
  TURN_COMMAND_PTR turnData = (TURN_COMMAND_PTR)data;
  
  setupCommand(ref, *turnData);
  simStartTurn(*turnData);
  IPC_freeData(IPC_msgInstanceFormatter(ref), data);
  IPC_delayResponse(ref);
}


/*--------------------------------------------------------------
  
  Handler for the STOP_COMMAND message.
  
  ---------------------------------------------------------------------------*/

/* ARGSUSED */
static void stopCommandHandler(MSG_INSTANCE ref, void *data, void *dummy)
{
  terminateCurrentCommand();
  simStop();
  IPC_freeData(IPC_msgInstanceFormatter(ref), data);
  IPC_respond(ref, SUCCESS_MSG, 0, NULL);
}


/*-----------------------------------------------
  
  Handler for the SET_VELOCITY_INFORM message.
  
  ---------------------------------------------------------------------------*/

static void setVelocityInformHandler(MSG_INSTANCE ref, void *data, void *dummy)
{
  SET_VELOCITY_INFORM_PTR setVelocityData;
  
  setVelocityData = (SET_VELOCITY_INFORM_PTR)data;
  
  simSetVelocity(setVelocityData->transVelocity, setVelocityData->rotVelocity);
  IPC_freeData(IPC_msgInstanceFormatter(ref), data);
}


/*-----------------------------------------------
  
  Handler for the VELOCITY_QUERY message.
  
  ---------------------------------------------------------------------------*/

/* ARGSUSED */
static void velocityQueryHandler(MSG_INSTANCE ref, void *data, void *dummy)
{
  VELOCITY_REPLY_TYPE velocity;
  
  simVelocity(&velocity.transVelocity, &velocity.rotVelocity);
  IPC_respondData(ref, VELOCITY_REPLY, &velocity);
  IPC_freeData(IPC_msgInstanceFormatter(ref), data);
}


/*-----------------------------------------------
  
  Handler for the TALK_COMMAND message.
  
  ---------------------------------------------------------------------------*/

static void talkCommandHandler(MSG_INSTANCE ref, void *data, void *dummy)
{
  TALK_COMMAND_PTR talkData;
  
  talkData = (TALK_COMMAND_PTR)data;
  
  simTalk(*talkData);
  IPC_respond(ref, SUCCESS_MSG, 0, NULL);
  IPC_freeData(IPC_msgInstanceFormatter(ref), data);
}


/*-----------------------------------------------------
  
  Handler for the SONAR_PING_QUERY message.
  
  -------------------------------------------------------------------------*/

static void sonarPingQueryHandler(MSG_INSTANCE ref, void *data, void *dummy)
{
  CMS sonarData[24];
  SONAR_PING_QUERY_TYPE sonarPingData;
  SONAR_PING_REPLY_TYPE sonarPingReply;
  
  sonarPingData = *(SONAR_PING_QUERY_PTR)data;
  
  if (sonarPingData < 0 || sonarPingData > 23) {
    fprintf(stderr, "Sonar Ping request out of range: %d\n", sonarPingData);
    IPC_respond(ref, NULL_REPLY_MSG, 0, NULL);
  } else {
    sonarScan(sonarData);
    sonarPingReply = sonarData[sonarPingData];
    IPC_respondData(ref, SONAR_PING_REPLY, &sonarPingReply);
  }
  IPC_freeData(IPC_msgInstanceFormatter(ref), data);
}


/*-----------------------------------------------
  
  Handler for the SONAR_SCAN_QUERY message.
  
  ---------------------------------------------------------------------------*/

static void sonarScanQueryHandler(MSG_INSTANCE ref, void *data, void *dummy)
{
  CMS sonarData[24];
  SONAR_SCAN_QUERY_PTR sonarScanData;
  SONAR_SCAN_REPLY_TYPE sonarScanReply;
  int i, j, num;
  
  sonarScanData = (SONAR_SCAN_QUERY_PTR)data;
  
  if (sonarScanData->startSonar < 0 || sonarScanData->startSonar > 23 ||
      sonarScanData->endSonar < 0 || sonarScanData->endSonar > 23) {
    fprintf(stderr, "Sonar Scan request out of range: (%d..%d)\n",
	    sonarScanData->startSonar, sonarScanData->endSonar);
    IPC_respond(ref, NULL_REPLY_MSG, 0, NULL);
  } else {
    sonarScan(sonarData);
    for (j=0;j<24;j++) sonarScanReply.sonarData[j] = 0.0;
    num = (sonarScanData->endSonar - sonarScanData->startSonar + 1 + 24)%24; 
    for (i=sonarScanData->startSonar, j=0; j<num; i++, j++) {
      sonarScanReply.sonarData[j] = sonarData[i%24];
    }
    IPC_respondData(ref, SONAR_SCAN_REPLY, &sonarScanReply);
  }
  IPC_freeData(IPC_msgInstanceFormatter(ref), data);
}


/*----------------------- SonarScanVarArray ------------------------
  
  Handler for the SONAR_SCAN_VAR message.
  
  ---------------------------------------------------------------------------*/

static void sonarScanVarQueryHandler(MSG_INSTANCE ref, void *data, void *dummy)
{
  CMS sonarData[24], sonarReplyData[24];
  SONAR_SCAN_VAR_QUERY_PTR sonarScanVarData;
  SONAR_SCAN_VAR_REPLY_TYPE sonarScanVarReply;
  int i, j, num;
  
  sonarScanVarData = (SONAR_SCAN_VAR_QUERY_PTR)data;
  
  if (sonarScanVarData->startSonar < 0 || sonarScanVarData->startSonar > 23 ||
      sonarScanVarData->endSonar < 0 || sonarScanVarData->endSonar > 23) {
    fprintf(stderr, "Sonar Scan Var request out of range: (%d..%d)\n",
	    sonarScanVarData->startSonar, sonarScanVarData->endSonar);
    IPC_respond(ref, NULL_REPLY_MSG, 0, NULL);
  } else {
    sonarScan(sonarData);
    num = (sonarScanVarData->endSonar-sonarScanVarData->startSonar+1 + 24)%24; 
    for (i=sonarScanVarData->startSonar, j=0; j<num; i++, j++) {
      sonarReplyData[j] = sonarData[i%24];
    }
    sonarScanVarReply.numItems = num;
    sonarScanVarReply.sonarData = sonarReplyData;
    IPC_respondData(ref, SONAR_SCAN_VAR_REPLY, &sonarScanVarReply);
  }
  IPC_freeData(IPC_msgInstanceFormatter(ref), data);
}


/*-----------------------------------------------
  
  Handler for the SONAR_SCAN_LIST_QUERY message.
  
  Note: we are not freeing any list space here - bad move
  
  ---------------------------------------------------------------------------*/

static void sonarScanListQueryHandler(MSG_INSTANCE ref, void *data,
				      void *dummy)
{
  CMS sonarData[24];
  SONAR_SCAN_LIST_QUERY_PTR sonarScanListData;
  SONAR_SCAN_LIST_REPLY_TYPE sonarScanListReply;
  SONAR_LIST_PTR nextItem;
  int i, j, num;
  
  sonarScanListData = (SONAR_SCAN_LIST_QUERY_PTR)data;
  
  if (sonarScanListData->startSonar < 0 || 
      sonarScanListData->startSonar > 23 ||
      sonarScanListData->endSonar < 0 || sonarScanListData->endSonar > 23) {
    fprintf(stderr, "Sonar Scan List request out of range: (%d..%d)\n",
	    sonarScanListData->startSonar, sonarScanListData->endSonar);
    IPC_respond(ref, NULL_REPLY_MSG, 0, NULL);
  } else {
    sonarScan(sonarData);
    sonarScanListReply = NULL;
    num = (sonarScanListData->endSonar-sonarScanListData->startSonar+1 +24)%24;
    for (i=sonarScanListData->startSonar, j=0; j<num; i++, j++) {
      nextItem = (SONAR_LIST_PTR)calloc(1,sizeof(SONAR_LIST_TYPE));
      nextItem->next = sonarScanListReply;
      nextItem->sonarData = sonarData[i%24];
      sonarScanListReply = nextItem;
    }
    IPC_respondData(ref, SONAR_SCAN_LIST_REPLY, &sonarScanListReply);
  }
  
  IPC_freeData(IPC_msgInstanceFormatter(ref), data);
  IPC_freeDataElements(IPC_msgInstanceFormatter(ref), &sonarScanListReply);
}


/*-----------------------------------------------
  
  Handler for the MOTION_QUERY message.
  
  ---------------------------------------------------------------------------*/

/* ARGSUSED */
static void motionQueryHandler(MSG_INSTANCE ref, void *data, void *dummy)
{
  MOTION_REPLY_TYPE moving;
  
  moving = simInMotion();
  IPC_respondData(ref, MOTION_REPLY, &moving);
  IPC_freeData(IPC_msgInstanceFormatter(ref), data);
}

/*-----------------------------------------------
  
  Handler for the DISTANCE_MOVED_QUERY message.
  
  ---------------------------------------------------------------------------*/

/* ARGSUSED */
static void distanceMovedQueryHandler(MSG_INSTANCE ref, void *data,
				      void *dummy)
{
  IPC_respondData(ref, DISTANCE_MOVED_REPLY, &lastMoveDistance);
  IPC_freeData(IPC_msgInstanceFormatter(ref), data);
}

static void registerAll(void)
{
  IPC_defineMsg(SUCCESS_MSG, 0, NULL);
  IPC_defineMsg(NULL_REPLY_MSG, 0, NULL);
  IPC_defineMsg(FAILURE_MSG, IPC_VARIABLE_LENGTH, FAILURE_FORMAT);

  IPC_defineFormat(CMS_NAME, CMS_FORMAT);
  IPC_defineFormat(DEGREES_NAME, DEGREES_FORMAT);
  
  IPC_defineMsg(BOUNDED_MOVE_COMMAND, IPC_VARIABLE_LENGTH, BOUNDED_MOVE_FORM);
  IPC_subscribeData(BOUNDED_MOVE_COMMAND, boundedMoveCommandHandler, NULL);
  
  IPC_defineMsg(UNBOUNDED_MOVE_COMMAND, IPC_VARIABLE_LENGTH, 
		UNBOUNDED_MOVE_FORM);
  IPC_subscribeData(UNBOUNDED_MOVE_COMMAND, unboundedMoveCommandHandler, NULL);
  
  IPC_defineMsg(GUARDED_MOVE_COMMAND, IPC_VARIABLE_LENGTH, GUARDED_MOVE_FORM);
  IPC_subscribeData(GUARDED_MOVE_COMMAND, guardedMoveCommandHandler, NULL);
  
  IPC_defineMsg(TURN_COMMAND, IPC_VARIABLE_LENGTH, TURN_FORM);
  IPC_subscribeData(TURN_COMMAND, turnCommandHandler, NULL);
  
  IPC_defineMsg(STOP_COMMAND, IPC_VARIABLE_LENGTH, STOP_FORM);
  IPC_subscribeData(STOP_COMMAND, stopCommandHandler, NULL);
  
  IPC_defineMsg(SET_VELOCITY_INFORM, IPC_VARIABLE_LENGTH, SET_VELOCITY_FORM);
  IPC_subscribeData(SET_VELOCITY_INFORM, setVelocityInformHandler, NULL);
  
  IPC_defineMsg(TALK_COMMAND, IPC_VARIABLE_LENGTH, TALK_FORM);
  IPC_subscribeData(TALK_COMMAND, talkCommandHandler, NULL);
  
  IPC_defineMsg(SONAR_PING_QUERY, IPC_VARIABLE_LENGTH, SONAR_PING_QUERY_FORM);
  IPC_defineMsg(SONAR_PING_REPLY, IPC_VARIABLE_LENGTH, SONAR_PING_REPLY_FORM);
  IPC_subscribeData(SONAR_PING_QUERY, sonarPingQueryHandler, NULL);
  
  IPC_defineMsg(SONAR_SCAN_QUERY, IPC_VARIABLE_LENGTH, SONAR_SCAN_QUERY_FORM);
  IPC_defineMsg(SONAR_SCAN_REPLY, IPC_VARIABLE_LENGTH, SONAR_SCAN_REPLY_FORM);
  IPC_subscribeData(SONAR_SCAN_QUERY, sonarScanQueryHandler, NULL);
  
  IPC_defineMsg(SONAR_SCAN_LIST_QUERY, IPC_VARIABLE_LENGTH, 
		SONAR_SCAN_LIST_QUERY_FORM);
  IPC_defineMsg(SONAR_SCAN_LIST_REPLY, IPC_VARIABLE_LENGTH,
		SONAR_SCAN_LIST_REPLY_FORM);
  IPC_subscribeData(SONAR_SCAN_LIST_QUERY, sonarScanListQueryHandler, NULL);
  
  IPC_defineMsg(SONAR_SCAN_VAR_QUERY, IPC_VARIABLE_LENGTH,
		SONAR_SCAN_VAR_QUERY_FORM);
  IPC_defineMsg(SONAR_SCAN_VAR_REPLY, IPC_VARIABLE_LENGTH,
		SONAR_SCAN_VAR_REPLY_FORM);
  IPC_subscribeData(SONAR_SCAN_VAR_QUERY, sonarScanVarQueryHandler, NULL);
  
  IPC_defineMsg(VELOCITY_QUERY, IPC_VARIABLE_LENGTH, VELOCITY_QUERY_FORM);
  IPC_defineMsg(VELOCITY_REPLY, IPC_VARIABLE_LENGTH, VELOCITY_REPLY_FORM);
  IPC_subscribeData(VELOCITY_QUERY, velocityQueryHandler, NULL);
  
  IPC_defineMsg(MOTION_QUERY, IPC_VARIABLE_LENGTH, MOTION_QUERY_FORM);
  IPC_defineMsg(MOTION_REPLY, IPC_VARIABLE_LENGTH, MOTION_REPLY_FORM);
  IPC_subscribeData(MOTION_QUERY, motionQueryHandler, NULL);
  
  IPC_defineMsg(DISTANCE_MOVED_QUERY, IPC_VARIABLE_LENGTH, 
		DISTANCE_MOVED_QUERY_FORM);
  IPC_defineMsg(DISTANCE_MOVED_REPLY, IPC_VARIABLE_LENGTH, 
		DISTANCE_MOVED_REPLY_FORM);
  IPC_subscribeData(DISTANCE_MOVED_QUERY, distanceMovedQueryHandler, NULL);

  IPC_setCapacity(2); // Allow commands and queries to be handled concurrently
}

int main(int argc, char *argv[])
{
  
  if (argc < 2 || !strcmp(argv[1], "-h")) {
    fprintf(stderr, "Usage: %s parameter-file\n", argv[0]);
  }
  
  init_simx(argv[1]);
  
  RedrawWholeMap();
  
  sensors();  
  
  RedrawPosition(xposW,NULL,NULL,NULL);
  
  plot_robot(1);
  
  IPC_connect(CONTROLLER_MODULE);
  
  registerAll();
  
  ResetTime();
  
  while (TRUE) handleSimEvents();
  return 0;
}
