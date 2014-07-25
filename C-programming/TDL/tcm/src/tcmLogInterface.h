/*****************************************************************************
 * 
 * PROJECT: Task Control Management.
 *
 * (c) Copyright 1999 Reid Simmons.  All rights reserved.
 *
 * FILE: tcmLogInterface.h
 *
 * ABSTRACT: These functions are the interface for more general logging,
 *           including interfaces with the outside world (eg, for remote 
 *           monitoring).  By default, they use logging functions that
 *           are human-readable and are parseable by tview.  But, the
 *           user can override the defaults and do his own logging (eg
 *           sending messages to a monitoring process)
 *
 * $Source: /afs/cs.cmu.edu/project/TCA/Master/tcaV8/tcm/src/tcmLogInterface.h,v $ 
 * $Revision: 1.2 $
 * $Date: 2009/01/15 17:00:05 $
 * $Author: reids $
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tcm/LICENSE.TXT)
 *
 * REVISION HISTORY:
 * $Log: tcmLogInterface.h,v $
 * Revision 1.2  2009/01/15 17:00:05  reids
 * Released under simplified BSD open source license
 *
 * Revision 1.1  1999/06/06 13:48:10  reids
 * Changes to TCM 1.3.0:
 *   New list data structures.
 *   New, generalized logging interface.
 *   Fixed firstChild to work if there are no children.
 *   Fixed code to deallocate a node.
 *   In certain circmumstances, don't send termination signal when task
 *     itself terminating.
 *
 ****************************************************************/

#ifndef INCtcmLogInterface
#define INCtcmLogInterface

#include "taskTree.h"
#include "tplConstr.h"

/****************************************************************
 *
 * Provide hooks to interface user-defined logging functions to TCM
 *
 *****************************************************************/

typedef void (* LOG_NODE_FN) (const Task_Tree_Ref &ref);
typedef void (* LOG_CHILD_FN) (const Task_Tree_Ref &parent,
			       const Task_Tree_Ref &child);
typedef void (* LOG_STATE_CHANGE_FN) (const Task_Tree_Ref &ref,
				      State_Enum newState);
typedef void (* LOG_SUSPENSION_FN) (const Task_Tree_Ref &ref, 
				    BOOLEAN suspended);
typedef void (* LOG_CONSTRAINT_FN) (const Task_Tree_Ref &constrainedRef,
				    Signal_Enum constraintSignal,
				    const Task_Tree_Ref &constrainingRef,
				    State_Enum constrainingState,
				    MSecs delay);
typedef void (* LOG_ACTION_DATA_FN) (const Task_Tree_Ref &ref);

class Logging_Fns
{
 public:
  Logging_Fns();
  void setLoggingFns (LOG_NODE_FN, LOG_CHILD_FN, LOG_STATE_CHANGE_FN,
		      LOG_SUSPENSION_FN, LOG_CONSTRAINT_FN,
		      LOG_ACTION_DATA_FN);

  void logNode(const Task_Tree_Ref &ref) const
    { if (_logNodeFn) (*_logNodeFn)(ref); }
  void logChild(const Task_Tree_Ref &parent, const Task_Tree_Ref &child) const
    { if (_logChildFn) (*_logChildFn)(parent, child); }
  void logStateChange(const Task_Tree_Ref &ref, State_Enum newState) const
    { if (_logStateChangeFn) (*_logStateChangeFn)(ref, newState); }
  void logSuspension(const Task_Tree_Ref &ref, BOOLEAN suspended) const
    { if (_logSuspensionFn) (*_logSuspensionFn)(ref, suspended); }
  void logConstraint(const Task_Tree_Ref &constrainedRef,
		     Signal_Enum constraintSignal,
		     const Task_Tree_Ref &constrainingRef,
		     State_Enum constrainingState,
		     MSecs delay) const
    { if (_logConstraintFn) 
      (*_logConstraintFn)(constrainedRef, constraintSignal,
			  constrainingRef, constrainingState, delay); }
  void logActionData(const Task_Tree_Ref &ref) const
    { if (_logActionDataFn) (*_logActionDataFn)(ref); }

 private:
  LOG_NODE_FN         _logNodeFn;
  LOG_CHILD_FN        _logChildFn;
  LOG_STATE_CHANGE_FN _logStateChangeFn;
  LOG_SUSPENSION_FN   _logSuspensionFn;
  LOG_CONSTRAINT_FN   _logConstraintFn;
  LOG_ACTION_DATA_FN  _logActionDataFn;
};

/* Set the various TCM logging functions.
   Way to enable users to define their own logging functions
*/
void TCM_SetLoggingFns (LOG_NODE_FN, LOG_CHILD_FN, LOG_STATE_CHANGE_FN,
			LOG_SUSPENSION_FN, LOG_CONSTRAINT_FN,
			LOG_ACTION_DATA_FN);

#endif // INCtcmLogInterface
