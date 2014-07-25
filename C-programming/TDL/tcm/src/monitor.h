/**************************************************************************
 * 
 * PROJECT: Task Control Management.
 *
 * (c) Copyright 1996 Reid Simmons.  All rights reserved.
 *
 * FILE: monitor.h
 *
 * ABSTRACT: Classes for implementing monitors (point, polling, demon, event)
 *
 * EXPORTS:
 *
 * $Revision: 1.15 $
 * $Date: 2009/01/15 17:00:05 $
 * $Author: reids $
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tcm/LICENSE.TXT)
 *
 * REVISION HISTORY:
 *
 * $Log: monitor.h,v $
 * Revision 1.15  2009/01/15 17:00:05  reids
 * Released under simplified BSD open source license
 *
 * Revision 1.14  2003/10/23 12:18:46  reids
 * Fixed several memory leaks, including one big one caused by a bug in
 *   g++ version 3.2.3 (this necessitated a change to some tcm.h signatures,
 *   hence the new TCM minor version number -- 2.8.0)
 *
 * Revision 1.13  2002/07/11 03:50:25  da0g
 * Addressed String = (char*) vs (const char *) issues.
 *
 * Revision 1.12  2002/06/26 16:49:30  reids
 * Insure warns that "malloc" (which strup uses) must be paired with "free"
 *
 * Revision 1.11  2000/07/05 23:09:06  da0g
 * Added rudimentary Run-Time-Type-Identification to _Action, Monitor_Action,
 * and Polling_Monitor_Action.
 *
 * Modified monitor actions & tcm.h monitor interface to support the
 * allocate-then-set-attributes model of TDL.
 *
 * Polling_Monitor_Action with an infinite time period will behave as
 * standard non-polling Monitor_Action.
 *
 * Added kludge to Polling_Monitor_Action's to detect termination
 * and force the monitor to stop running.
 *
 * Fixed bug with _initialWait being reset incorrectly.
 *
 * Fixed delete of const pointer bug.
 *
 * Revision 1.10  1998/04/21 12:46:00  reids
 * Added InvokeWhen/InvokeAfter functionality, to invoke a function when a
 *   particular event occurs.
 * Added the DelayUntilAfter/TerminateAtAfter functionality, to delay/terminate
 *   a node by waiting some msecs after a particular event has occurred.
 *
 * Revision 1.9  98/03/06  13:12:43  reids
 * Added a way to stop the compiler from complaining about unused function
 *   arguments (apparently, g++ ignores the "#pragma unused" statement).
 * 
 * Revision 1.8  98/03/06  12:42:35  reids
 * Modifications made to support Solaris.
 * 
 * Revision 1.7  98/01/30  14:50:53  reids
 * Updated to compile under gcc 2.7.2 and under Linux.
 * Also, made STRING "const char *" and changed API to take const arguments,
 *   where applicable.
 * 
 * Revision 1.6  97/12/30  12:28:10  reids
 * Added a "timer" monitor, which is like a polling monitor, except when
 *   activated it just invokes a function (rather than adding a new node)
 * 
 * Revision 1.5  97/12/29  17:06:24  reids
 * Version that has the basic functionality needed to support TDL.
 * 
 * Revision 1.4  97/12/22  16:52:56  reids
 * Basically, added "data" field for CALLBACK_ACTION,
 *  and started using "nodeData" for the activation data associated
 *  with monitors, and the failure data associated with exceptions.
 * 
 * Revision 1.3  97/12/18  00:21:45  reids
 * Changing ACTION_PTR to a handle, to aid in garbage collection.
 * 
 * Revision 1.2  97/12/04  17:50:13  reids
 * Another fairly stable version (except that monitors do not quite work)
 * 
 * Revision 1.1  97/11/21  14:06:34  reids
 * First release of TCM -- seems to be a stable version
 * 
 *
 **************************************************************************/

#ifndef INCmonitor
#define INCmonitor

#include "taskTree.h"

class Monitor_Action : public _Action
{
 public:
  static  STRING TCM_getStaticName() { return "Monitor_Action"; }
  virtual STRING TCM_getActionName() { return TCM_getStaticName(); }

  Monitor_Action(const TCM_Action_Ref &activationAction,
		 STRING activationName,
		 unsigned int maxActivations, int activationConstraints)
    {
      _maxActivations = maxActivations; _numActivations = 0;
      _activationAction = activationAction; 
      _activationName = strdup(activationName);
      _activationConstraints = activationConstraints;
    };

  Monitor_Action ( STRING       theActivationName,
		   int          theActivationConstraints,
		   unsigned int theMaximumActivations    )
    : _activationAction      (                           ),
      _activationName        ( strdup(theActivationName) ),
      _activationConstraints ( theActivationConstraints  ),
      _maxActivations        ( theMaximumActivations     ),
      _numActivations        ( 0                         )
    {}

  // Insure warns that "malloc" (which strup uses) must be paired with "free"
  virtual ~Monitor_Action() { 
    _activationAction = NULL; free((void *)_activationName);
  }
 
#ifndef macintosh
  void execute (Task_Tree_Ref const &node) { UNUSED(node); };
#else
  void execute (Task_Tree_Ref const &node);
#endif
  virtual void activate (Task_Tree_Ref const &node);
  unsigned int numActivations (void) const { return _numActivations; }

  TCM_Action_Ref getActivationAction() const { return _activationAction; }
  void setActivationAction ( const TCM_Action_Ref & theActivationAction )
    { _activationAction = theActivationAction; }

  STRING getActivationName() const { return _activationName; }
  void   setActivationName ( STRING theActivationName )
    { _activationName = theActivationName; }

  int  getActivationConstraints() const { return _activationConstraints; }
  void setActivationConstraints ( int theActivationConstraints )
    { _activationConstraints = theActivationConstraints; }

  unsigned int getMaximumActivations () const { return _maxActivations; }
  void setMaximumActivations ( unsigned int theMaximumActivations )
    { _maxActivations = theMaximumActivations; }

 protected:
  TCM_Action_Ref _activationAction;
  STRING _activationName;
  int _activationConstraints;
  unsigned int _maxActivations;
  unsigned int _numActivations;
};

#ifdef macintosh // Keep compiler from complaining
inline void Monitor_Action::execute (Task_Tree_Ref const &node)
{
  UNUSED(node)
}
#endif

class Polling_Monitor_Action : public Monitor_Action
{
 public:
  static  STRING TCM_getStaticName() { return "Polling_Monitor_Action"; }
  virtual STRING TCM_getActionName() { return TCM_getStaticName(); }

  Polling_Monitor_Action(const TCM_Action_Ref &activationAction, 
			 STRING activationName,
			 MSecs period, unsigned int maxActivations,
			 int activationConstraints, BOOLEAN initialWait)
    : Monitor_Action(activationAction, activationName, maxActivations, 
		     activationConstraints)
    { _period = period; _initialWait = initialWait; };

  Polling_Monitor_Action ( STRING       theActivationName,
			   int          theActivationConstraints,
			   unsigned int theMaximumActivations,
			   MSecs        thePeriod,
			   BOOLEAN      theInitialWait )
    : Monitor_Action ( theActivationName,
		       theActivationConstraints,
		       theMaximumActivations     ),
      _period        ( thePeriod                 ),
      _initialWait   ( theInitialWait            )
    {}

  virtual void execute (Task_Tree_Ref const &node);

  MSecs getPeriod() const { return _period; }
  void  setPeriod ( MSecs thePeriod ) { _period = thePeriod; }

  BOOLEAN getInitialWait() const { return _initialWait; }
  void setInitialWait (BOOLEAN theInitialWait) {_initialWait = theInitialWait;}

 protected:
  MSecs _period;
  BOOLEAN _initialWait;
};

class Task_Tree_Monitor_Node : public Task_Tree_Command_Node
{
 public:
  Task_Tree_Monitor_Node(STRING nodeName) : Task_Tree_Command_Node(nodeName)
    { _maxTriggers = _numTriggers = 0; }
  Task_Tree_Monitor_Node(STRING nodeName, const TCM_Action_Ref &nodeAction,
			 unsigned int maxTriggers)
    : Task_Tree_Command_Node(nodeName, nodeAction) 
    { _maxTriggers = maxTriggers; _numTriggers = 0; }

  virtual STRING className(void) const { return "Monitor"; }
  virtual BOOLEAN isMonitor(void) { return TRUE; }

  void trigger (void);
  unsigned int numTriggers (void) const { return _numTriggers; }
  unsigned int numActivations (void) const
    { return ((Monitor_Action *)*_action)->numActivations(); }

  void setMaxTriggers (unsigned int maxTriggers) { _maxTriggers = maxTriggers;}
  unsigned int getMaxTriggers() { return _maxTriggers; }

  void activate (const void *data);
  
 private:
   // Remove all activation and timeout signals from handled monitors
  void _finishHandlingMonitor (void);

 private:
  // Different classes of nodes may handle the same signal in different ways.
  virtual void _handleSignal (Signal_Enum signal);

  unsigned int _maxTriggers;
  unsigned int _numTriggers;
};

Task_Tree_Monitor_Node *monitorDereference (TCM_Task_Tree_Ref const &nodeRef);

#endif // INCmonitor
