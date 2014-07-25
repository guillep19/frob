/*****************************************************************************
 * 
 * PROJECT: Task Control Management.
 *
 * (c) Copyright 1996 Reid Simmons.  All rights reserved.
 *
 * FILE: tcm.h
 *
 * ABSTRACT: Public include file for the Task Control Management library.
 *           Defines the TCM API.
 *
 * $Source: /afs/cs.cmu.edu/project/TCA/Master/tcaV8/tcm/src/tcm.h,v $ 
 * $Revision: 1.42 $
 * $Date: 2009/01/15 17:00:05 $
 * $Author: reids $
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tcm/LICENSE.TXT)
 *
 * REVISION HISTORY:
 *
 * $Log: tcm.h,v $
 * Revision 1.42  2009/01/15 17:00:05  reids
 * Released under simplified BSD open source license
 *
 * Revision 1.41  2008/07/11 15:47:02  reids
 * Merged the two previous ways that exceptions were implemented (the original
 * version, and one for TDL).  Extended to handle distributed exceptions.
 * Added the TRACE_UBER flag for even more detailed tracing.
 * Added API functions: TCM_FailureNode and TCM_ExceptionHandlerNode.
 *
 * Revision 1.40  2003/10/23 12:18:47  reids
 * Fixed several memory leaks, including one big one caused by a bug in
 *   g++ version 3.2.3 (this necessitated a change to some tcm.h signatures,
 *   hence the new TCM minor version number -- 2.8.0)
 *
 * Revision 1.39  2003/06/24 17:07:11  da0g
 * Removed const from "TCM_Task_Tree_Ref &" arg in ACTION_CREATION_FN.
 *
 * Revision 1.38  2003/04/17 23:41:51  da0g
 * Missed a const.
 *
 * Revision 1.37  2003/04/17 21:10:29  da0g
 * Added code to free (cleanup) data associated with distributed invocation.
 * Added code to support [taskname] overloaded tasks.
 * Changed clearQueues/TCM_ProcessAgenda to allow for
 *   return-when-all-work-is-done option.
 * Added silentlyAutoCorrectArguments option to TCM_ProcessAgenda to
 *   address waitingAllowed/relativeTimeout swappage.
 *
 * Revision 1.36  2003/01/29 20:30:23  da0g
 * Added userData to TaskTree.
 *
 * Revision 1.35  2002/12/23 02:25:53  da0g
 * Added Node-Class routines, User-specified Task-Thread mapping routines.
 * Added TCM_PushUserTaskForThisThreadForDurationOfObject class.
 *
 * Revision 1.34  2002/09/16 22:49:39  da0g
 * Both Virtual and Actual distributed nodes have same instance name.
 * Added infinite-loop detection (and overrides).
 *
 * Revision 1.33  2002/07/11 03:53:14  da0g
 * Addressed String = (char*) vs (const char *) issues.
 * Added TCM_PURE_VIRTUAL_METHOD macro.
 * TCM_AllocateDistributedNode() now sets either/both
 *   the local-instance-name and the remote-instance-name.
 *
 * Revision 1.32  2002/06/26 16:50:12  reids
 * Made a distinction between the type-name and instance-name of a node.
 *  Enable instance name of node to be set.
 *
 * Revision 1.31  2002/05/10 03:29:20  reids
 * Added TCM_InvokeWhenTerminating and TCM_InvokeWhenTerminated
 *
 * Revision 1.30  2002/03/22 02:27:47  da0g
 * Added Exception-Handler-Ordering code.
 *
 * Revision 1.29  2002/01/18 14:05:00  reids
 * Added TCM_ThisAgent
 *
 * Revision 1.28  2001/10/23 22:52:59  da0g
 * Added Threading support.  Cached lastchild.
 *
 * Revision 1.27  2001/08/08 01:00:41  da0g
 * Added TCM_IsDistributedNode().
 *
 * Revision 1.26  2001/07/24 12:49:04  reids
 * Enable monitor client data to be sent with all the monitor activation
 *   functions (ActivateAt, ActivateIn, ActivateInAfter).  Fixed bugs in
 *   the way monitor data was being set.
 * Separate templates for virtual nodes from regular templates (compiled in
 *   only when DISTRIBUTED option is used -- makes code cleaner).
 *
 * Revision 1.25  2001/06/11 15:54:48  reids
 * No longer need to use -DDISTRIBUTED in order to compile against tcm.h
 *
 * Revision 1.24  2001/04/04 14:26:13  reids
 * Task tree nodes are now garbage collected, by default, after they are
 *   completely achieved.  Can change this behavior using TCM_SetPersistence.
 * Also, cleaned up some memory leaks: Now seems to lose little, if any, memory
 *
 * Revision 1.23  2001/02/27 20:27:23  trey
 * added TCM_CreateLocalNode() call
 *
 * Revision 1.22  2001/02/27 02:37:03  trey
 * added client data to action and allocation callbacks; made communications go to the main IPC context by default when we don't explicitly connect
 *
 * Revision 1.21  2001/02/17 03:10:11  reids
 * Changed the name of a few parameters to better reflect their use
 *
 * Revision 1.20  2000/07/05 23:12:22  da0g
 * Added rudimentary Run-Time-Type-Identification to _Action, Monitor_Action,
 * and Polling_Monitor_Action.
 *
 * Modified monitor actions & tcm.h monitor interface to support the
 * allocate-then-set-attributes model of TDL.
 *
 * Polling_Monitor_Action with an infinite time period will behave as
 * standard non-polling Monitor_Action.
 *
 * Added Get/Set-Actual-Action functions.  They behave like the
 * standard functions, only they dereference monitors and get/set
 * the Monitor_Action's (or monitor instance's) action.
 *
 * Added a means of accessing Ref-Count Failure Data for
 * TDL exception support.
 *
 * Revision 1.19  2000/02/02 22:09:47  da0g
 * Fixed improper templating bugs with tcmHandle's refIncr and refDecr
 *
 * Revision 1.18  2000/01/19 21:26:43  reids
 * Added two new top-level functions:
 *   TCM_IsDoneHandling(ref) -- returns TRUE if the ref has raised success or
 * 			     failure.
 *   TCM_IsPostponed(ref) -- returns TRUE if the ref's action has finished
 * 		          executing, but the node has not yet raised
 * 			  success or failure.
 *
 * Revision 1.17  1999/08/04 14:00:19  reids
 * Distributed version of TCM.  Provides the ability for one process to
 *  add tasks, constraints, exception handlers, etc. with another process.
 *  The distributed version is created by compiling with -DDISTRIBUTED flag,
 *   which can be gotten by "gmaking" with DISTRIBUTED=1.
 *
 * Revision 1.16  1998/12/21 17:56:13  reids
 * Added TCM_GetAction.
 *
 * Revision 1.15  1998/12/16 03:08:20  reids
 * Added support for "on termination" functions.
 *   Also enabled tca.h and tcm.h to co-exist (needed to change values
 *   of several constants).
 *
 * Revision 1.14  98/12/14  15:37:06  da0g
 * tcm.h
 * 
 * Revision 1.13  98/12/14  15:20:08  da0g
 * Added relativeTimeout to TCM_ProcessAgenda
 * 
 * Revision 1.12  98/10/30  11:16:33  da0g
 * Added ExternalEventBypassHandler.
 * 
 * Revision 1.11  1998/09/15 18:45:23  da0g
 * Enhanced exceptions to support multiple-name resolution and Ref_Count (automatically-destroyed) Data.
 *
 * Revision 1.10  1998/08/05 10:57:28  reids
 * Made a compiler option for printing out debugging information.
 *   Created macros for TCM_TerminateAtAfter and TCM_DelayUntilAfter, for
 *   backwards compatibility.
 *
 * Revision 1.9  98/07/14  17:21:04  reids
 * Changed TCM_DelayUntilAfter to TCM_DelayForAfter, changed TCM_
 *   TerminateAtAfter to TCM_TerminateInAfter to TCM_DelayForAfter, and
 *   changed order of arguments.
 * Added TCM_ActivateInAfter.
 * 
 * Revision 1.8  98/04/21  12:46:13  reids
 * Added InvokeWhen/InvokeAfter functionality, to invoke a function when a
 *   particular event occurs.
 * Added the DelayUntilAfter/TerminateAtAfter functionality, to delay/terminate
 *   a node by waiting some msecs after a particular event has occurred.
 * 
 * Revision 1.7  98/01/30  14:51:04  reids
 * Updated to compile under gcc 2.7.2 and under Linux.
 * Also, made STRING "const char *" and changed API to take const arguments,
 *   where applicable.
 * 
 * Revision 1.6  97/12/30  12:29:05  reids
 * Added option to *not* wait for timeouts and external events.
 *   Added flag to indicate whether node raised an exception (ie, failed).
 *   Added a "timer" monitor, which is like a polling monitor, except when
 *     activated it just invokes a function (rather than adding a new node)
 * 
 * Revision 1.5  97/12/29  17:06:33  reids
 * Version that has the basic functionality needed to support TDL.
 * 
 * Revision 1.4  97/12/22  16:53:04  reids
 * Basically, added "data" field for CALLBACK_ACTION,
 *  and started using "nodeData" for the activation data associated
 *  with monitors, and the failure data associated with exceptions.
 * 
 * Revision 1.3  97/12/18  00:21:50  reids
 * Changing ACTION_PTR to a handle, to aid in garbage collection.
 * 
 * Revision 1.2  97/12/04  17:50:22  reids
 * Another fairly stable version (except that monitors do not quite work)
 * 
 * Revision 1.1  97/11/21  14:06:40  reids
 * First release of TCM -- seems to be a stable version
 * 
 *
 *****************************************************************************/

#ifndef INCtcm
#define INCtcm

#include <stdio.h>
#include "tcmHandle.h"

DECLARE_REF_COUNT_FUNCTIONS(_Action);
DECLARE_REF_COUNT_FUNCTIONS(Exception_Handler);
DECLARE_REF_COUNT_FUNCTIONS(Task_Tree_Node);
DECLARE_REF_COUNT_FUNCTIONS(Ref_Count);

enum TCM_Return_Type { TCM_Ok, TCM_Error };

/*****************************************************************
 * 
 * Types and functions having to do with creating task tree nodes
 *   and defining handlers/callbacks.
 *
 *****************************************************************/

typedef tcmHandle<class Task_Tree_Node> TCM_Task_Tree_Ref;

typedef void (*SIMPLE_CALLBACK_FN_TYPE)(const void *callbackData);

typedef void (*CALLBACK_FN_TYPE)(const TCM_Task_Tree_Ref &nodeRef,
				 const void *callbackData);

typedef void (*EXT_CALLBACK_FN_TYPE)(const TCM_Task_Tree_Ref &nodeRef,
				     const void *callbackData,
				     const void *classData);

class _Action : public Ref_Count
{
protected:
  char *     distributedCleanupRef;
  void *     distributedCleanupData;
  void   ( * distributedCleanupFunction ) (const char *, void *);

 public:
  static  STRING TCM_getStaticName() { return "Unknown-Generic-_Action";}
  virtual STRING TCM_getActionName() { return TCM_getStaticName(); }

  _Action()
    : distributedCleanupRef      ( (char *) NULL ),
      distributedCleanupData     ( (void *) NULL ),
      distributedCleanupFunction (          NULL )
    {}

  virtual ~_Action();

  virtual void execute (const TCM_Task_Tree_Ref &node) TCM_PURE_VIRTUAL_METHOD;

  virtual BOOLEAN operator==(const _Action & otherAction) const 
    { return this == & otherAction; }

  void setDistributedCleanupData ( const char * ref, void * data,
				   void ( * function ) (const char *, void *) )
    {
      if ( distributedCleanupRef != (char *) NULL )
        free ( distributedCleanupRef );
      distributedCleanupRef      = strdup ( ref );
      distributedCleanupData     = data;
      distributedCleanupFunction = function;
    }
};

typedef tcmHandle<_Action> TCM_Action_Ref;

const TCM_Task_Tree_Ref & TCM_RootNode (void);

TCM_Task_Tree_Ref TCM_AllocateGoalNode (STRING nodeTypeName);

// Version where one can set the type and instance names separately
TCM_Task_Tree_Ref TCM_AllocateGoalNode (STRING nodeTypeName,
					STRING instanceName);

TCM_Task_Tree_Ref TCM_AllocateCommandNode (STRING nodeTypeName);

// Version where one can set the type and instance names separately
TCM_Task_Tree_Ref TCM_AllocateCommandNode (STRING nodeTypeName,
					   STRING instanceName);

TCM_Return_Type TCM_DeallocateNode (const TCM_Task_Tree_Ref &nodeRef);

TCM_Return_Type TCM_SetAction (const TCM_Task_Tree_Ref &nodeRef, 
			       const TCM_Action_Ref action);
TCM_Return_Type TCM_SetActualAction (const TCM_Task_Tree_Ref &nodeRef, 
				     const TCM_Action_Ref action);

TCM_Action_Ref TCM_GetAction (const TCM_Task_Tree_Ref &nodeRef);
TCM_Action_Ref TCM_GetActualAction (const TCM_Task_Tree_Ref &nodeRef);

TCM_Return_Type TCM_SetData (const TCM_Task_Tree_Ref &nodeRef,
			     const void *data);

const void * TCM_GetUserData (const TCM_Task_Tree_Ref &nodeRef);
TCM_Return_Type TCM_SetUserData (const TCM_Task_Tree_Ref &nodeRef,
				 const void *data);

const void * TCM_GetActualUserData (const TCM_Task_Tree_Ref &nodeRef);
TCM_Return_Type TCM_SetActualUserData (const TCM_Task_Tree_Ref &nodeRef,
				       const void *data);


TCM_Return_Type TCM_InsertNode (const TCM_Task_Tree_Ref &parentRef,
				const TCM_Task_Tree_Ref &childRef,
				BOOLEAN isLastChild = TRUE);

// These are shorthands for a series of allocate/setAction/insert calls

TCM_Task_Tree_Ref TCM_CreateGoalNode (const TCM_Task_Tree_Ref &parentRef,
				      STRING nodeTypeName,
				      const TCM_Action_Ref &action,
				      BOOLEAN isLastChild = TRUE);

TCM_Task_Tree_Ref TCM_CreateGoalNode (const TCM_Task_Tree_Ref &parentRef,
				      STRING nodeTypeName, 
				      const void *callbackData,
				      CALLBACK_FN_TYPE callback,
				      BOOLEAN isLastChild = TRUE);

TCM_Task_Tree_Ref TCM_CreateCommandNode (const TCM_Task_Tree_Ref &parentRef,
					 STRING nodeTypeName,
					 const TCM_Action_Ref &action,
					 BOOLEAN isLastChild = TRUE);

TCM_Task_Tree_Ref TCM_CreateCommandNode (const TCM_Task_Tree_Ref &parentRef,
					 STRING nodeTypeName, 
					 const void *callbackData,
					 CALLBACK_FN_TYPE callback,
					 BOOLEAN isLastChild = TRUE);

TCM_Return_Type TCM_Success (const TCM_Task_Tree_Ref &node);

// If "true", the node persists even after it has been achieved.
// If "false" (now the default), it gets destroyed after achievement.
TCM_Return_Type TCM_SetPersistence (const TCM_Task_Tree_Ref & nodeRef,
				    BOOLEAN value = TRUE);

/**************************************************************************
 *
 * Functions to map between a task tree node and its id number
 *
 **************************************************************************/

int TCM_Remember (const TCM_Task_Tree_Ref &nodeRef);

TCM_Task_Tree_Ref TCM_Recall (int refID);

void TCM_Forget (int refID);

// Combines TCM_Recall & TCM_Forget
TCM_Task_Tree_Ref TCM_Retrieve (int refID);

/*****************************************************************
 * 
 * Types and functions having to do with temporal constraints
 *
 *****************************************************************/

enum TCM_Interval_Enum { Unknown_Interval,  Handling_Interval, 
			 Planning_Interval, Achieving_Interval };

enum TCM_Point_Enum { Unknown_Point, Start_Point, End_Point};

#ifndef INCtca // Need this to enable tca.h and tcm.h to both be included
const int SEQ_ACH        = 1;
const int SEQ_PLANNING   = 2;
const int PLAN_FIRST     = 4;
const int DELAY_PLANNING = 8;
#endif

class TCM_Interval
{
  friend class TCM_Point;
 public:
  TCM_Interval(const TCM_Interval_Enum interval, const TCM_Task_Tree_Ref &node)
    : _which(interval), _ref(node) { };

 protected:
  TCM_Interval_Enum _which;
  TCM_Task_Tree_Ref _ref;
};

class TCM_Point
{
 public:
  TCM_Point(const TCM_Point_Enum point, const TCM_Interval &interval)
    : _which(point), _interval(interval) {};
  BOOLEAN valid (void) const;
  TCM_Point_Enum point(void) const { return _which; }
  TCM_Interval_Enum interval(void) const { return _interval._which; }
  TCM_Task_Tree_Ref node(void) const { return _interval._ref; }

 protected:
  TCM_Point_Enum _which;
  TCM_Interval _interval;
};

TCM_Interval TCM_HandlingOf (const TCM_Task_Tree_Ref &nodeRef);
TCM_Interval TCM_PlanningOf (const TCM_Task_Tree_Ref &nodeRef);
TCM_Interval TCM_AchievingOf (const TCM_Task_Tree_Ref &nodeRef);

TCM_Point TCM_StartOf (const TCM_Interval &interval);
TCM_Point TCM_EndOf (const TCM_Interval &interval);

TCM_Return_Type TCM_AddConstraint (const TCM_Task_Tree_Ref &nodeRef,
				   int tplConstraints);

// Planning and Achievement of "beforeRef" end before start of "afterRef"
TCM_Return_Type TCM_Serialize (const TCM_Task_Tree_Ref &beforeRef,
			       const TCM_Task_Tree_Ref &afterRef);

TCM_Return_Type TCM_DelayUntil (TCM_Point const &timePoint,
				TCM_Point const &delayTimePoint);
TCM_Return_Type TCM_DelayUntil (TCM_Point const &timePoint,
				MSecs absoluteTime);
TCM_Return_Type TCM_DelayFor (TCM_Point const &timePoint,
			      MSecs relativeWait);
// Delay this timepoint for "relativeWait" msecs after 
//  the "delayTimePoint" have passed.
TCM_Return_Type TCM_DelayForAfter (TCM_Point const &timePoint,
				   MSecs relativeWait,
				   TCM_Point const &delayTimePoint);
// For backwards compatibility
#define TCM_DelayUntilAfter(tp, dtp, rw) TCM_DelayForAfter(tp, rw, dtp)

// Don't enable this timepoint until TCM_Signal is invoked for that timepoint
TCM_Return_Type TCM_DelayUntilSignal (TCM_Point const &timePoint);

// Send an explicit signal to enable this timepoint
TCM_Return_Type TCM_Signal (TCM_Point const &timePoint);

TCM_Return_Type TCM_WaitUntil (const TCM_Point & waitTimePoint);
TCM_Return_Type TCM_WaitUntil (MSecs absoluteTime);
TCM_Return_Type TCM_WaitFor   (MSecs relativeWait);

BOOLEAN TCM_IsPast (TCM_Point const &timePoint);
BOOLEAN TCM_IsPast (MSecs absoluteTime);

TCM_Task_Tree_Ref TCM_CreateDelayCommand (const TCM_Task_Tree_Ref &parentRef,
					  MSecs relativeWait);

// Clear all the timeouts associated with the node 
// Actually sends the signals associated with the timeouts (except for
//  Activate and Timeout signals)
TCM_Return_Type TCM_ClearTimeouts (const TCM_Task_Tree_Ref &nodeRef);

/*****************************************************************
 * 
 * Functions having to do with traversing task trees.
 *
 *****************************************************************/

TCM_Task_Tree_Ref TCM_Parent (const TCM_Task_Tree_Ref &childRef);
TCM_Task_Tree_Ref TCM_FirstChild (const TCM_Task_Tree_Ref &parentRef);
TCM_Task_Tree_Ref TCM_LastChild (const TCM_Task_Tree_Ref &parentRef);
TCM_Task_Tree_Ref TCM_NextChild (const TCM_Task_Tree_Ref &childRef);
TCM_Task_Tree_Ref TCM_PreviousChild (const TCM_Task_Tree_Ref &childRef);

TCM_Task_Tree_Ref TCM_ChildNamed (const TCM_Task_Tree_Ref &parentRef,
				  STRING childName);
TCM_Task_Tree_Ref TCM_AncestorNamed (const TCM_Task_Tree_Ref &descendantRef,
				     STRING ancestorName);

STRING TCM_NodeName (const TCM_Task_Tree_Ref &nodeRef);

STRING TCM_NodeTypeName (const TCM_Task_Tree_Ref &nodeRef);


enum TCM_NodeClassType_Enum { TCM_Unknown, TCM_Goal, TCM_Command, TCM_Monitor,
                              TCM_Virtual };

TCM_NodeClassType_Enum TCM_NodeClassType (const TCM_Task_Tree_Ref & nodeRef);
STRING TCM_NodeClassTypeToString ( TCM_NodeClassType_Enum  theValue );
STRING TCM_NodeClassString (const TCM_Task_Tree_Ref & nodeRef);


TCM_Return_Type TCM_SetInstanceName (const TCM_Task_Tree_Ref &nodeRef,
				     const STRING newInstanceName);

/*****************************************************************
 * 
 * Functions having to do with terminating and suspending nodes
 *
 *****************************************************************/

TCM_Return_Type TCM_TerminateNode (const TCM_Task_Tree_Ref &nodeRef);
TCM_Return_Type TCM_TerminateChildren (const TCM_Task_Tree_Ref &nodeRef);
TCM_Return_Type TCM_TerminateSiblings (const TCM_Task_Tree_Ref &nodeRef);
TCM_Return_Type TCM_SuspendNode (const TCM_Task_Tree_Ref &nodeRef);
TCM_Return_Type TCM_UnsuspendNode (const TCM_Task_Tree_Ref &nodeRef);
BOOLEAN TCM_IsSuspended (const TCM_Task_Tree_Ref &nodeRef);

BOOLEAN TCM_IsDoneHandling (const TCM_Task_Tree_Ref &nodeRef);
BOOLEAN TCM_IsPostponed    (const TCM_Task_Tree_Ref &nodeRef);

TCM_Return_Type TCM_TerminateAt (const TCM_Task_Tree_Ref &nodeRef,
				 TCM_Point const &terminateTimePoint);
TCM_Return_Type TCM_TerminateAt (const TCM_Task_Tree_Ref &nodeRef,
				 MSecs absoluteTime);
TCM_Return_Type TCM_TerminateIn (const TCM_Task_Tree_Ref &nodeRef,
				 MSecs relativeWait);
// Terminate this node in "relativeWait" msecs after 
//  the "terminateTimePoint" have passed.
TCM_Return_Type TCM_TerminateInAfter (const TCM_Task_Tree_Ref &nodeRef,
				      MSecs relativeWait,
				      TCM_Point const &terminateTimePoint);
// For backwards compatibility
#define TCM_TerminateAtAfter(nr, ttp, rw) TCM_TerminateInAfter(nr, rw, ttp)

TCM_Return_Type TCM_OnTermination (const TCM_Task_Tree_Ref &nodeRef,
				   const TCM_Task_Tree_Ref &terminationRef);

TCM_Return_Type TCM_OnTermination (const TCM_Task_Tree_Ref &nodeRef,
				   STRING nodeTypeName,
				   const TCM_Action_Ref &terminationAction);

TCM_Return_Type TCM_OnTermination (const TCM_Task_Tree_Ref &nodeRef,
				   STRING nodeTypeName,
				   const void *callbackData,
				   CALLBACK_FN_TYPE callback);

/*****************************************************************
 * 
 * Functions having to do with exceptions
 *
 *****************************************************************/

class TCM_Exception : public Ref_Count
{
 protected:
  STRING      name;
  const void *data;

public:
  TCM_Exception() : name(strdup("UNKNOWN")), data(NULL) {}
  TCM_Exception(STRING name) : name(strdup(name)), data(NULL) {}
  TCM_Exception(STRING name, const void *data)
    : name(strdup(name)), data(data) {}
  virtual ~TCM_Exception();

  typedef TCM_Exception * (*Creator) (STRING name, const void *data);

  STRING      getExceptionName() const { return name; }
  void        setExceptionName(char *theName) { name = strdup(theName); }

  virtual const void *getExceptionData() const { return data; }
//  void        setExceptionData(void *theData) { data = theData; }

  virtual BOOLEAN matches ( STRING theString ) const;
  virtual TCM_Exception *clone (void) const;
};

typedef tcmHandle<TCM_Exception> TCM_Exception_Ref;

TCM_Return_Type TCM_Failure (const TCM_Task_Tree_Ref &node, 
			     TCM_Exception *exception);

TCM_Return_Type TCM_Failure (const TCM_Task_Tree_Ref &node, 
			     STRING type, void *failureData = NULL);

TCM_Return_Type TCM_AddExceptionHandler(const TCM_Task_Tree_Ref &nodeRef,
					STRING type,
					const TCM_Action_Ref &action,
					unsigned int maxInvocations = MAXINT,
					signed int index
					 = DEFAULT_EXCEPTION_HANDLER_PRIORITY);

TCM_Return_Type TCM_AddExceptionHandler(const TCM_Task_Tree_Ref &nodeRef,
					STRING type,
					const CALLBACK_FN_TYPE callback,
					unsigned int maxInvocations = MAXINT);

TCM_Return_Type TCM_RemoveExceptionHandler(const TCM_Task_Tree_Ref &nodeRef,
					   STRING type,
					   const TCM_Action_Ref &action);

TCM_Return_Type TCM_RemoveExceptionHandler(const TCM_Task_Tree_Ref &nodeRef,
					   STRING type,
					   const CALLBACK_FN_TYPE callback);

TCM_Return_Type TCM_Bypass(const TCM_Task_Tree_Ref &nodeRef);

TCM_Return_Type TCM_Retry(const TCM_Task_Tree_Ref &nodeRef,
			  const void *retryData);

const void *TCM_FailureData (const TCM_Task_Tree_Ref &nodeRef);

TCM_Exception *TCM_FailureException (const TCM_Task_Tree_Ref &nodeRef);

/* Return the node that raised the exception */
TCM_Task_Tree_Ref TCM_FailureNode (const TCM_Task_Tree_Ref &nodeRef);

/* Return the node associated with the exception handler */
TCM_Task_Tree_Ref TCM_ExceptionHandlerNode (const TCM_Task_Tree_Ref &nodeRef);


/*****************************************************************
 * 
 * Functions having to do with monitors
 *
 *****************************************************************/

TCM_Task_Tree_Ref TCM_AllocateMonitorNode (STRING nodeTypeName);

// Version where one can set the type and instance names separately
TCM_Task_Tree_Ref TCM_AllocateMonitorNode (STRING nodeTypeName,
					   STRING instanceName);

TCM_Return_Type 
TCM_SetMonitorParameters (const TCM_Task_Tree_Ref &monitorRef,
			  const TCM_Action_Ref &activationAction,
			  unsigned int maxActivations = MAXINT,
			  unsigned int maxTriggers = MAXINT,
			  STRING activationTypeName = NULL,
			  int activationConstraints = 0);

TCM_Task_Tree_Ref TCM_AllocatePollingMonitorNode (STRING nodeTypeName);

// Version where one can set the type and instance names separately
TCM_Task_Tree_Ref TCM_AllocatePollingMonitorNode (STRING nodeTypeName,
						  STRING instanceName);

TCM_Return_Type 
TCM_SetPollingMonitorParameters (const TCM_Task_Tree_Ref &monitorRef,
				 const TCM_Action_Ref &activationAction,
				 MSecs period = seconds(1),
				 unsigned int maxActivations = MAXINT,
				 unsigned int maxTriggers = MAXINT,
				 STRING activationTypeName = NULL,
				 int activationConstraints = 0,
				 BOOLEAN initialWait = FALSE);

TCM_Task_Tree_Ref TCM_CreateMonitorNode(const TCM_Task_Tree_Ref &parentRef,
					STRING nodeTypeName, 
					const TCM_Action_Ref &activationAction,
					unsigned int maxActivations = MAXINT,
					unsigned int maxTriggers = MAXINT,
					STRING activationTypeName = NULL,
					int activationConstraints = 0);

TCM_Task_Tree_Ref TCM_CreateMonitorNode (const TCM_Task_Tree_Ref &parentRef,
					 STRING nodeTypeName, 
					 const void *callbackData,
					 CALLBACK_FN_TYPE activationCallback,
					 unsigned int maxActivations = MAXINT,
					 unsigned int maxTriggers = MAXINT,
					 STRING activationTypeName = NULL,
					 int activationConstraints = 0);

TCM_Task_Tree_Ref
TCM_CreatePollingMonitorNode (const TCM_Task_Tree_Ref &parentRef,
			      STRING nodeTypeName, 
			      const TCM_Action_Ref &activationAction,
			      MSecs period = seconds(1),
			      unsigned int maxActivations = MAXINT,
			      unsigned int maxTriggers = MAXINT,
			      STRING activationTypeName = NULL,
			      int activationConstraints = 0,
			      BOOLEAN initialWait = FALSE);

TCM_Task_Tree_Ref
TCM_CreatePollingMonitorNode (const TCM_Task_Tree_Ref &parentRef,
			      STRING nodeTypeName, 
			      const void *callbackData,
			      CALLBACK_FN_TYPE activationCallback,
			      MSecs period = seconds(1),
			      unsigned int maxActivations = MAXINT,
			      unsigned int maxTriggers = MAXINT,
			      STRING activationTypeName = NULL,
			      int activationConstraints = 0,
			      BOOLEAN initialWait = FALSE);


// TDL utilizes a mechanism that requires independent allocation,
// set-constaints, and set-action functions.
TCM_Task_Tree_Ref
TCM_AllocateCompleteMonitorNode (
			STRING       theNodeTypeName,
			STRING       theInstanceName          = STRING(NULL),
			int          theActivationConstraints = 0,
			unsigned int theMaximumActivations    = MAXINT,
			unsigned int theMaximumTriggers       = MAXINT,
			MSecs        thePeriod                = INFINITE_TIME,
			BOOLEAN      theInitialWait           = FALSE,
			STRING       theActivationTypeName    = STRING(NULL) );
TCM_Return_Type
TCM_SetMonitorMaximumActivations ( const TCM_Task_Tree_Ref & theMonitorRef,
				   unsigned int theMaximumActivates );
unsigned int
TCM_GetMonitorMaximumActivations ( const TCM_Task_Tree_Ref & theMonitorRef );

TCM_Return_Type
TCM_SetMonitorMaximumTriggers ( const TCM_Task_Tree_Ref & theMonitorRef,
				unsigned int theMaximumTriggers );
unsigned int
TCM_GetMonitorMaximumTriggers ( const TCM_Task_Tree_Ref & theMonitorRef );

TCM_Return_Type
TCM_SetMonitorActivationConstraints ( const TCM_Task_Tree_Ref & theMonitorRef,
				      int theActivationConstraints );
int
TCM_GetMonitorActivationConstraints( const TCM_Task_Tree_Ref & theMonitorRef );

TCM_Return_Type
TCM_SetMonitorPeriod ( const TCM_Task_Tree_Ref & theMonitorRef,
		       MSecs thePeriod );
MSecs
TCM_GetMonitorPeriod ( const TCM_Task_Tree_Ref & theMonitorRef );

TCM_Return_Type
TCM_SetMonitorInitialWait ( const TCM_Task_Tree_Ref & theMonitorRef,
			    BOOLEAN theInitialWait );
BOOLEAN
TCM_GetMonitorInitialWait ( const TCM_Task_Tree_Ref & theMonitorRef );


TCM_Return_Type TCM_Trigger (const TCM_Task_Tree_Ref &monitorRef);

TCM_Return_Type TCM_Activate (const TCM_Task_Tree_Ref &monitorRef,
			      const void *activationData=NULL);

TCM_Return_Type TCM_ActivateAt (const TCM_Task_Tree_Ref &monitorRef,
				TCM_Point const &activateTimePoint,
				const void *activationData);
TCM_Return_Type TCM_ActivateAt (const TCM_Task_Tree_Ref &monitorRef,
				MSecs absoluteTime,
				const void *activationData);
TCM_Return_Type TCM_ActivateIn (const TCM_Task_Tree_Ref &monitorRef,
				MSecs relativeWait,
				const void *activationData);
// Activate this node in "relativeWait" msecs after 
//  the "activateTimePoint" have passed.
TCM_Return_Type TCM_ActivateInAfter (const TCM_Task_Tree_Ref &monitorRef,
				     MSecs relativeWait,
				     TCM_Point const &activateTimePoint,
				     const void *activationData);

/* For backwards compatibility */
TCM_Return_Type TCM_ActivateAt (const TCM_Task_Tree_Ref &monitorRef,
				TCM_Point const &activateTimePoint);
TCM_Return_Type TCM_ActivateAt (const TCM_Task_Tree_Ref &monitorRef,
				MSecs absoluteTime);
TCM_Return_Type TCM_ActivateIn (const TCM_Task_Tree_Ref &monitorRef,
				MSecs relativeWait);
// Activate this node in "relativeWait" msecs after 
//  the "activateTimePoint" have passed.
TCM_Return_Type TCM_ActivateInAfter (const TCM_Task_Tree_Ref &monitorRef,
				     MSecs relativeWait,
				     TCM_Point const &activateTimePoint);

const void *TCM_ActivationData (const TCM_Task_Tree_Ref &nodeRef);
unsigned int TCM_NumTriggers (const TCM_Task_Tree_Ref &nodeRef);
unsigned int TCM_NumActivations (const TCM_Task_Tree_Ref &nodeRef);

/*****************************************************************
 * 
 * Functions to invoke functions (not adding tree nodes) in 
 * response to particular events.
 *
 *****************************************************************/

TCM_Return_Type TCM_InvokeWhen (TCM_Point const &timePoint,
				SIMPLE_CALLBACK_FN_TYPE callback,
				const void *callbackData);

TCM_Return_Type TCM_InvokeWhen (MSecs absoluteTime,
				SIMPLE_CALLBACK_FN_TYPE callback,
				const void *callbackData);

TCM_Return_Type TCM_InvokeAfter (MSecs relativeWait,
				 SIMPLE_CALLBACK_FN_TYPE callback,
				 const void *callbackData);

TCM_Return_Type TCM_InvokeWhenTerminating (const TCM_Task_Tree_Ref &nodeRef,
					   SIMPLE_CALLBACK_FN_TYPE callback,
					   const void *callbackData);

TCM_Return_Type TCM_InvokeWhenTerminated (const TCM_Task_Tree_Ref &nodeRef,
					  SIMPLE_CALLBACK_FN_TYPE callback,
					  const void *callbackData);

/*****************************************************************
 * 
 * Types and functions for interacting with TCM
 *
 *****************************************************************/

TCM_Return_Type TCM_Initialize (void);

/*
 *   If "waitingAllowed" is TRUE, when TCM_ProcessAgenda() has finished
 * its work, (empty _taskQueue, empty _eventQueue), it will invoke
 * _externalEvents.dispatchEvents(...) with a timeout based upon
 * the _timerQueue, HasThreadWithUnfinishedWork(), and clamped
 * appropriately to prevent indefinite blocking.
 *   Of course, _externalEvents.dispatchEvents(...) can process input at
 * any time, and return early before this timeout period fully elapses.
 * Thus permitting TCM_ProcessAgenda() (actually Agenda::clearQueues())
 * to resume processing early on any newly created tasks.
 *   (Addendum: _externalEvents.dispatchEvents(...) is additionally invoked
 * with a timeout of zero-time to prevent starvation, irregardless of the
 * "waitingAllowed" value.)
 *
 *   If "waitingAllowed" is FALSE, TCM_ProcessAgenda() will spin-loop
 * without any blocking mechanism, potentially using up large quantities
 * of CPU time while it waits for timer-events to expire.
 *
 *   If "returnWhenAllWorkIsDone" is TRUE, TCM_ProcessAgenda() will return
 * when (1) the relativeTimeout time-period has elapsed, OR (2) as soon as
 * all work is completed.  (E.g.: _taskQueue, _eventQueue, and _timerQueue
 * are all empty.  HasThreadWithUnfinishedWork() is FALSE.)
 *   (Addendum: For the purposes of deciding whether or not work still remains
 * to be completed, we do **NOT** consider possible future incoming tasks from
 * _externalEvents.dispatchEvents(...).  E.g: Possible future incoming
 * Distributed tasks.)
 *
 *   If "returnWhenAllWorkIsDone" is FALSE, TCM_ProcessAgenda() will ONLY
 * return when the relativeTimeout time-period has elapsed.
 *
 *   If "silentlyAutoCorrectArguments" is TRUE,
 *   AND "waitingAllowed" is *NOT* equal to TRUE or FALSE,
 *   AND "relativeTimeout" *IS* equal to TRUE or FALSE,
 * the values for "waitingAllowed" and "relativeTimeout" will be swapped.
 *   This circumvents an ongoing issue wherein  "BOOLEAN", "MSecs", and
 * the "bool" type either all are, or autopromote to, integer types.
 * Thereby allowing these arguments to be trivially swapped, and preventing
 * us from addressing this via static compile-time type-checking signatures.
 */

TCM_Return_Type TCM_ProcessAgenda(
		     BOOLEAN waitingAllowed,              /* = TRUE          */
		     MSecs   relativeTimeout,             /* = INFINITE_TIME */
		     BOOLEAN returnWhenAllWorkIsDone      = FALSE,
		     BOOLEAN silentlyAutoCorrectArguments = TRUE   );

inline
TCM_Return_Type TCM_ProcessAgendaInfiniteBlock()
		 { return TCM_ProcessAgenda ( TRUE, INFINITE_TIME,   FALSE ); }
inline
TCM_Return_Type TCM_ProcessAgenda()
		 { return TCM_ProcessAgenda ( TRUE, INFINITE_TIME,   TRUE  ); }
inline
TCM_Return_Type TCM_ProcessAgenda( MSecs  relativeTimeout /*= INFINITE_TIME*/ )
		 { return TCM_ProcessAgenda ( TRUE, relativeTimeout, FALSE ); }


typedef void (*SD_CALLBACK_FN_TYPE)(int sd, const void *callbackData);

TCM_Return_Type TCM_AddExternalEvent (int sd, SD_CALLBACK_FN_TYPE callback,
				      const void *callbackData);
	
TCM_Return_Type TCM_RemoveExternalEvent (int sd);

typedef void (* EXTERNAL_EVENT_BYPASS_HANDLER_TYPE)
				      ( BOOLEAN (*)(void *, struct timeval * ),
					void *,
					struct timeval *,
					BOOLEAN );
/* IE: 
 * void
 * ExternalEventBypassHandler ( BOOLEAN (* tcmDispatchExternalEventsHandler)
 *                                           ( void           * theTcmData,
 *					       struct timeval * theTimeout ),
 *				   void           * theTcmData,
 *				   struct timeval * theTimeout,
 *				   BOOLEAN          theHasTcmExternalEvents  );
 */
void TCM_SetExternalEventBypassHandler (
			 EXTERNAL_EVENT_BYPASS_HANDLER_TYPE theBypassHandler );

TCM_Return_Type TCM_DisplayTree(FILE *stream, const TCM_Task_Tree_Ref &nodeRef);

typedef enum { Log_None=0x0, Log_Msg=0x2, Log_Status=0x4, Log_Time=0x8,
	       Log_Id=0x10, Log_ParentId=0x20, 
	       Log_Ignore=0x40 } Logging_Options_Enum;

void TCM_SetTerminalLoggingOptions (int options);
void TCM_SetFileLoggingOptions (int options);

int TCM_TerminalLoggingOptions (void);
int TCM_FileLoggingOptions (void);


BOOLEAN         TCM_GetAllowInfiniteTimeouts();
void            TCM_SetAllowInfiniteTimeouts( BOOLEAN theAllowValue = TRUE );
struct timeval  TCM_GetInfiniteTimeTimeout();
void            TCM_SetInfiniteTimeTimeout( struct timeval theTimeout );



/*****************************************************************
 * 
 * Types and functions having to do with Distributed nodes
 *
 *****************************************************************/

	/* It would be bad if the theOverloadedTaskNameIndex contained *
	 * the delimiter char, or if the delimiter was NULL_CHAR.      */
#define DEFAULT_OVERLOADED_TASK_NAME_INDEX              ""
#define USE_VALUE_FROM_ALLOCATE_DISTRIBUTED_NODE        STRING(NULL)
#define DELIMITER_CHAR_FOR_OVERLOADED_TASK_NAME_INDEX   ' '


TCM_Task_Tree_Ref TCM_AllocateDistributedNode (
				STRING theAgentName,
				STRING theNodeTypeName,
				STRING theInstanceName = STRING(NULL),
				STRING theOverloadedTaskNameIndex
					= DEFAULT_OVERLOADED_TASK_NAME_INDEX );

BOOLEAN TCM_IsDistributedNode ( const TCM_Task_Tree_Ref & nodeRef );


 /* If theOverloadedTaskNameIndex is
  * "USE_VALUE_FROM_ALLOCATE_DISTRIBUTED_NODE", we default to the value of
  * theOverloadedTaskNameIndex that was used for TCM_AllocateDistributedNode().
  * If it is ANYTHING ELSE, it will override the value previously used in
  * TCM_AllocateDistributedNode().  (A feature exploited by TDL.)
  */
TCM_Return_Type TCM_SetDistributedAction (
      const TCM_Task_Tree_Ref & nodeRef, 
      const void *              args,
      STRING                    theOverloadedTaskNameIndex
				  = USE_VALUE_FROM_ALLOCATE_DISTRIBUTED_NODE );

// This is a shorthand for a series of allocate/setAction/insert calls
TCM_Task_Tree_Ref TCM_CreateDistributedNode(
	     STRING                    agentName,
	     const TCM_Task_Tree_Ref & parentRef,
	     STRING                    nodeTypeName,
	     void *                    args,
	     BOOLEAN                   isLastChild = TRUE,
	     STRING                    theOverloadedTaskNameIndex
					= DEFAULT_OVERLOADED_TASK_NAME_INDEX );

TCM_Task_Tree_Ref TCM_CreateLocalNode(
	     const TCM_Task_Tree_Ref & parentRef,
	     STRING                    nodeTypeName,
	     void *                    args,
	     BOOLEAN                   isLastChild = TRUE,
	     STRING                    theOverloadedTaskNameIndex
					= DEFAULT_OVERLOADED_TASK_NAME_INDEX );


typedef TCM_Task_Tree_Ref (*TASK_ALLOCATION_FN) (void *clientData);
typedef TCM_Action_Ref    (*ACTION_CREATION_FN) (TCM_Task_Tree_Ref & node,
						 void *args,
						 void *clientData);

TCM_Return_Type TCM_RegisterDistributedTask (
		  STRING             nodeTypeName, 
		  TASK_ALLOCATION_FN allocationFn,
		  void *             allocationFnClientData,
		  ACTION_CREATION_FN actionFn,
		  void *             actionFnClientData,
		  STRING             taskDataFormat,
		  STRING             theOverloadedTaskNameIndex
			               = DEFAULT_OVERLOADED_TASK_NAME_INDEX );


/* Must be called once before connection distributed agents.
   "myHostMachine" defaults to the local machine. */
TCM_Return_Type TCM_EnableDistributedComm (STRING myName,
					   STRING myHostMachine = NULL);

/* Must be called for each agent that you want to send tasks to
   (or receive tasks from).
   "hostMachine" defaults to the local machine. */
TCM_Return_Type TCM_ConnectDistributedAgent (STRING agentName,
					     STRING hostMachine = NULL);

TCM_Return_Type TCM_RegisterDistributedException (STRING exceptionName, 
						  STRING exceptionDataFormat);

TCM_Return_Type
TCM_RegisterDistributedException (STRING exceptionName, 
				  STRING exceptionDataFormat,
				  TCM_Exception::Creator creator);

STRING TCM_ThisAgent (void);


/*****************************************************************
 * 
 * Types and functions having to do with THREADING SUPPORT
 *
 *****************************************************************/

BOOLEAN TCM_LockMasterMutex  (const char * theErrorString =(const char *)NULL);
BOOLEAN TCM_UnlockMasterMutex(const char * theErrorString =(const char *)NULL);

  /* Sometimes, we need to temporarily remove all the current locks while
   * we wait for some event to transpire, task to exectute, etc.
   * These two functions accomplish this.
   */
unsigned int TCM_FullyUnlockMasterMutexReturningNumberOfLocks (
			  const char * theErrorLocation = (const char *)NULL );
void TCM_FullyRelockMasterMutexNumberOfLocksTimes (
			  unsigned int theNumberOfLocks,
			  const char * theErrorLocation = (const char *)NULL );

int             TCM_GetNumberOfThreads();
TCM_Return_Type TCM_SetNumberOfThreads(unsigned int theMaximumThreadsUnsigned);

int  TCM_GetMaximumNumberOfDynamiclyAllocatedThreads();
void TCM_SetMaximumNumberOfDynamiclyAllocatedThreads( int theMaximumNumber );

void TCM_WaitForAllThreadsToFinishWork();


    /* This function is *ONLY* available when TCM has been compiled    *
     * with threading enabled.                                         *
     *                                                                 *
     * In this manner, attempting to use threads when TCM has not been *
     * compiled for threads will result in a link-time error.          */
TCM_Return_Type TCM_SetIsThreadedTask (
			      const TCM_Task_Tree_Ref & theTaskTreeRef,
			      BOOLEAN                   theIsThreaded = TRUE );

BOOLEAN TCM_GetIsThreadedTask(const TCM_Task_Tree_Ref & theTaskTreeRef);



  /* Use of TCM_WaitUntil() instead of this function is recommended. *
   * TCM_WaitUntil() will invoke this function as necessary.         *
   *                                                                 *
   * This function is *ONLY* available when TCM has been compiled    *
   * with threading enabled.                                         */
void TCM_BlockThreadUntilPoint ( const TCM_Point & theWaitTimePoint );


  /* This is for debugging the internals of TDL/TCM.                 *
   *                                                                 *
   * This function is *ONLY* available when TCM has been compiled    *
   * with threading enabled.                                         */
void TCM_DebugGetPrimaryThreadId( void * theThreadId );



  /*
   * Thread-Task mapping retrieving/binding functions.
   * These are primarily intended for internal use in TDL.
   * NOTE: TDL automatically invokes these functions!
   * NOTE: TCM will automatically set the current TCM_Task_Tree_Ref for 
   * the current thread whenever a task is handled.  The user-overrides are
   * exploited by TDL to accommodate resume tasks, and are in turn, themselves,
   * overriden by TCM when additional tasks are handled.
   * See Also:  <tcmThread.h> <tcmThread.cc>
   */
TCM_Task_Tree_Ref TCM_GetCurrentTaskTreeRefForThisThread(
				   BOOLEAN theCheckUserSpecifiedOnly = FALSE );
   /* Pushing a NULL value will revert us back to the system default. *
    * (Until it is popped back off the stack.)                        */
void              TCM_PushUserTaskForThisThread(
				    const TCM_Task_Tree_Ref & theTaskTreeRef );
TCM_Return_Type   TCM_PopUserTaskForThisThread();
void              TCM_DebugPrintUserTaskForThreadStack(
				      STRING  theHeading      = STRING(NULL),
				      BOOLEAN theShowThreadId = TRUE         );
  /*
   * Oftentimes, we want a Task-Thread mapping to exist for the duration
   * of the local stack-frame.  By declaring this object in that stack frame,
   * the necessary Pop is guaranteed to occur when the stack frame unrolls.
   */
class TCM_PushUserTaskForThisThreadForDurationOfObject
{
protected:
  int needsPop;

public:
	/* Only push us onto the stack if we need it... */
  void init ( const TCM_Task_Tree_Ref & theTaskTreeRef )
    {
      needsPop
	= ( TCM_GetCurrentTaskTreeRefForThisThread( TRUE ) != theTaskTreeRef );

      if ( needsPop )
	TCM_PushUserTaskForThisThread ( theTaskTreeRef );
    }

  void cleanup()
    {
      if ( needsPop )
	TCM_PopUserTaskForThisThread();
      needsPop = FALSE;
    }

  TCM_PushUserTaskForThisThreadForDurationOfObject(
				    const TCM_Task_Tree_Ref & theTaskTreeRef )
    { init ( theTaskTreeRef ); }

	/* Pushing a NULL value will revert us back to the system default. *
         * (Until it is popped back off the stack.)                        */
  TCM_PushUserTaskForThisThreadForDurationOfObject()
    { init ( TCM_Task_Tree_Ref() ); }


  ~TCM_PushUserTaskForThisThreadForDurationOfObject()
    { cleanup(); }
};


#endif /* INCtcm */

