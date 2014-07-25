/*****************************************************************************
 * 
 * PROJECT: Task Control Management.
 *
 * (c) Copyright 1999 Reid Simmons.  All rights reserved.
 *
 * FILE: tcmLogInterface.cc
 *
 * ABSTRACT: These functions are the interface for more general logging,
 *           including interfaces with the outside world (eg, for remote 
 *           monitoring).  By default, they use logging functions that
 *           are human-readable and are parseable by tview.  But, the
 *           user can override the defaults and do his own logging (eg
 *           sending messages to a monitoring process)
 *
 * $Source: /afs/cs.cmu.edu/project/TCA/Master/tcaV8/tcm/src/tcmLogInterface.cc,v $ 
 * $Revision: 1.4 $
 * $Date: 2009/01/15 17:00:05 $
 * $Author: reids $
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tcm/LICENSE.TXT)
 *
 * REVISION HISTORY:
 * $Log: tcmLogInterface.cc,v $
 * Revision 1.4  2009/01/15 17:00:05  reids
 * Released under simplified BSD open source license
 *
 * Revision 1.3  2002/07/11 03:49:25  da0g
 * Added NULL_CHAR macro.
 *
 * Revision 1.2  1999/08/04 13:55:46  reids
 * Reduced the number of system calls needed to log information -- should
 *   make the logging much more efficient.
 *
 * Revision 1.1  1999/06/06 13:48:09  reids
 * Changes to TCM 1.3.0:
 *   New list data structures.
 *   New, generalized logging interface.
 *   Fixed firstChild to work if there are no children.
 *   Fixed code to deallocate a node.
 *   In certain circmumstances, don't send termination signal when task
 *     itself terminating.
 *
 ****************************************************************/

#include "tcmGlobal.h"
#include "tcmLogInterface.h"

/****************************************************************
 *
 * The default logging functions (output can be parsed by tview) 
 *
 * NOTE: Write to a string first to save (lots of) system calls.
 *
 ****************************************************************/

static void displayStatus1 (Logger logger, Task_Tree_Ref ref,
			    State_Enum oldState, State_Enum newState)
{
  static char line[200];
  int error = TRUE, printed = TRUE;
  Phase_Enum phase = stateToPhase(newState);
  int id = ref->getNodeId();

  if (!logger.file()) return;

  if (phase == Handling_Phase || phase == Terminating_Phase) {
    line[0] = NULL_CHAR;
    switch(oldState) {
    case Unknown_State:
      if (newState == Unhandled_State) {
	tcmStrMessage(logger, line, "%-9s %15s",
		      ref->className(), ref->instanceName());
	tcmStrLogId(logger, line, id, LOGGING_MESSAGE);
	tcmStrMessage(logger, line, ": %10s", "TCM");
	tcmStrLogParentId(logger, line, ref->getParent()->getNodeId(),
			  LOGGING_MESSAGE);
	tcmStrMessage(logger, line, " --> ON HOLD         (Inactive)");
	error = FALSE;
      }
      break;

    case Unhandled_State:
      if (newState == Handling_State) {
	if (!ref->isCurrentState(Alive_State)) {
	  printed = FALSE;
	} else {
	  tcmStrMessage(logger, line, "%-9s %15s",
			ref->className(), ref->instanceName());
	  tcmStrLogId(logger, line, id, LOGGING_MESSAGE);
	  tcmStrMessage(logger, line, ":  ON HOLD ");
	  tcmStrMessage(logger, line, " --> %-15s (Sent)", "TCM");
	}
	error = FALSE;
      }
      break;

    case Handling_State:
      if (newState == Handled_State) {
	if (!ref->isCurrentState(Alive_State)) {
	  printed = FALSE;
	} else {
	  tcmStrMessage(logger, line, (ref->isMonitor() ? " Complete " :
				       (ref->getNodeFailed()
					? "  Failure " : "  Success ")));
	  tcmStrMessage(logger, line, "%15s", ref->instanceName());
	  tcmStrLogId(logger, line, id, LOGGING_MESSAGE);
	  tcmStrMessage(logger, line, ":");
	}
	error = FALSE;
      }
      break;

    case Alive_State:
      if (newState == Terminating_State) {
	tcmStrStatus(logger, line, "Will Terminate %s", ref->instanceName());
	tcmStrLogId(logger, line, id, LOGGING_STATUS);
	tcmStrStatus(logger, line, " when all references to it are released");
	error = FALSE;
      }
      break;

    case Terminating_State:
      if (newState == Terminated_State) {
	tcmStrStatus(logger, line, "Terminated %s", ref->instanceName());
	tcmStrLogId(logger, line, id, LOGGING_STATUS);
	error = FALSE;
      }
      break;

    default: error = TRUE; break;
    }
  
    if (error) {
      tcmWarning("_displayStatus: %s", ref->instanceName());
      tcmLogId(id, LOGGING_MESSAGE);
      tcmLog(": %s --> %s", stateName(oldState), stateName(newState));
    }
  
    if (printed) {
      tcmStrLogTime(logger, line, 1); 
      tcmStrMessage(logger, line, "\n");
      fprintf(logger.file(), line);
    }
  }
}

static void displayStatus (Task_Tree_Ref ref,
			   State_Enum oldState, State_Enum newState)
{
  displayStatus1(GET_TCM_GLOBAL(terminalLogger), ref, oldState, newState);
  displayStatus1(GET_TCM_GLOBAL(fileLogger), ref, oldState, newState);
}

static void logNodeDefault (const Task_Tree_Ref &ref)
{
  UNUSED(ref);
  // By default, nothing gets logged here
}

static void logChildDefault (const Task_Tree_Ref &parent,
			     const Task_Tree_Ref &child)
{
  UNUSED(parent);
  displayStatus(child, Unknown_State, Unhandled_State);
}

static void logStateChangeDefault (const Task_Tree_Ref &ref,
				   State_Enum newState)
{
  displayStatus(ref, ref->theCurrentState(newState), newState);
}

static void logSuspension1 (Logger logger,
			    const Task_Tree_Ref &ref, BOOLEAN suspended)
{
  static char line[80];

  if (logger.file()) {
    line[0] = NULL_CHAR;
    tcmStrStatus(logger, line, "%s %s",
		 (suspended ? "Suspended" : "Unsuspended"),
		 ref->instanceName());
    tcmStrLogId(logger, line, ref->getNodeId(), LOGGING_STATUS);
    tcmStrLogTime(logger, line, 1); 
    tcmStrMessage(logger, line, "\n");
    fprintf(logger.file(), line);
  }
}

static void logSuspensionDefault (const Task_Tree_Ref &ref, BOOLEAN suspended)
{
  logSuspension1(GET_TCM_GLOBAL(terminalLogger), ref, suspended);
  logSuspension1(GET_TCM_GLOBAL(fileLogger), ref, suspended);
}

static void logConstraintDefault (const Task_Tree_Ref &constrainedRef,
				  Signal_Enum constraintSignal,
				  const Task_Tree_Ref &constrainingRef,
				  State_Enum constrainingState,
				  MSecs delay)
{
  UNUSED(constrainedRef);  UNUSED(constraintSignal);
  UNUSED(constrainingRef); UNUSED(constrainingState);  UNUSED(delay);
  // By default, nothing gets logged here
}

static void logActionDataDefault (const Task_Tree_Ref &ref)
{
  UNUSED(ref);
  // By default, nothing gets logged here
}

Logging_Fns::Logging_Fns()
{
  setLoggingFns(logNodeDefault, logChildDefault, logStateChangeDefault,
		logSuspensionDefault, logConstraintDefault,
		logActionDataDefault);
}

void Logging_Fns::setLoggingFns (LOG_NODE_FN theLogNodeFn,
				 LOG_CHILD_FN theLogChildFn,
				 LOG_STATE_CHANGE_FN theLogStateChangeFn,
				 LOG_SUSPENSION_FN theLogSuspensionFn,
				 LOG_CONSTRAINT_FN theLogConstraintFn,
				 LOG_ACTION_DATA_FN theLogActionDataFn)
{
  _logNodeFn        = theLogNodeFn;
  _logChildFn       = theLogChildFn;
  _logStateChangeFn = theLogStateChangeFn;
  _logSuspensionFn  = theLogSuspensionFn;
  _logConstraintFn  = theLogConstraintFn;
  _logActionDataFn  = theLogActionDataFn;
}

/* Set the various TCM logging functions.
   Way to enable users to define their own logging functions
*/
void TCM_SetLoggingFns (LOG_NODE_FN logNodeFn, LOG_CHILD_FN logChildFn,
			LOG_STATE_CHANGE_FN logStateChangeFn,
			LOG_SUSPENSION_FN logSuspensionFn,
			LOG_CONSTRAINT_FN logConstraintFn,
			LOG_ACTION_DATA_FN logActionDataFn)
{
  GET_TCM_GLOBAL(loggingFns).setLoggingFns(logNodeFn, logChildFn,
					   logStateChangeFn, logSuspensionFn, 
					   logConstraintFn, logActionDataFn);
}
