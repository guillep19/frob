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
 * $Source: /afs/cs.cmu.edu/project/TCA/Master/tcaV8/tcm/src/tcmPriv.h,v $ 
 * $Revision: 1.60 $
 * $Date: 2009/05/04 19:44:49 $
 * $Author: reids $
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tcm/LICENSE.TXT)
 *
 * REVISION HISTORY:
 *
 * $Log: tcmPriv.h,v $
 * Revision 1.60  2009/05/04 19:44:49  reids
 * Changed to using snprintf to avoid corrupting the stack on overflow
 *
 * Revision 1.59  2009/01/15 17:00:05  reids
 * Released under simplified BSD open source license
 *
 * Revision 1.58  2009/01/06 18:10:45  reids
 * Fixed compiler error for machines with long pointer types
 * Fixed compiler warning -- had been doing a really nasty cast.
 *
 * Revision 1.57  2008/07/16 06:15:10  reids
 * Updates for newer (pickier) compilers
 *
 * Revision 1.56  2003/10/23 12:18:48  reids
 * Fixed several memory leaks, including one big one caused by a bug in
 *   g++ version 3.2.3 (this necessitated a change to some tcm.h signatures,
 *   hence the new TCM minor version number -- 2.8.0)
 *
 * Revision 1.55  2003/10/18 12:45:24  reids
 * Fixed a memory leak
 *
 * Revision 1.54  2003/07/30 09:10:35  da0g
 * Endless loop detection now uses the unsuppressible tcmWarning()
 *   instead of tcmMessage().
 * Increased tcmWarning() buffer size.
 *
 * Revision 1.53  2003/07/16 03:10:02  reids
 * Update for gcc 3.2.x
 *
 * Revision 1.52  2003/06/24 17:07:53  da0g
 * Commented out default value in TCM_SetAllowInfiniteTimeouts().
 * Removed const from "TCM_Task_Tree_Ref &" arg in ACTION_CREATION_FN.
 *
 * Revision 1.51  2003/04/17 21:11:15  da0g
 * Added code to free (cleanup) data associated with distributed invocation.
 * Added code to support [taskname] overloaded tasks.
 * Changed clearQueues/TCM_ProcessAgenda to allow for
 *   return-when-all-work-is-done option.
 * Added silentlyAutoCorrectArguments option to TCM_ProcessAgenda to
 *   address waitingAllowed/relativeTimeout swappage.
 * Added MSECS_PRINTF_STRING.
 * Added to tcmList.h a non-operator method for obtaining a pointer value.
 *
 * Revision 1.50  2003/01/29 20:33:29  da0g
 * Added CYGWIN/Windows tests to #ifdefs.
 * Added userData to TaskTree.
 * Added support for threads vs THREADS directory hierarchy.
 *
 * Revision 1.49  2002/12/23 02:27:39  da0g
 * Added tcmThread.cc
 * ThreadManager: Overrides User-Specified (Resume) Task-Thread mappings,
 *   if necessary.
 * ThreadManager: Assorted cleanup.  Added Mutexes to reduce race conditions.
 * ThreadManager: setNumberOfThreads() is now forced to be non-reentrant. Etc.
 * ThreadManager: Added mutexed getCurrentTaskRef()
 * ThreadManager: Added getTaskTreeRefForThisThread()
 * External_Events::dispatchEvents() now sets the system thread's task
 *   to TCM_RootNode(), overriding any user-specified (Resume) values.
 * Added Node-Class routines.
 * Added TCM_PushUserTaskForThisThreadForDurationOfObject class.
 * Added System Task-Thread mapping routines.
 * Added User-specified Task-Thread mapping routines.
 * tcmThread: ExecuteTask now establishes the System Task-Thread mapping,
 *   overriding any User-specified Task-Thread mappings.
 *
 * Revision 1.48  2002/09/16 23:13:35  da0g
 * Fixed runaway process consumes 100% of CPU issue.
 * Added infinite-loop detection (and overrides).
 * Both Virtual and Actual distributed nodes have same instance name.
 * Updated external.cc comments as part of other work.
 * Fixed minor typo bug in tcmThread.h.
 *
 * Revision 1.47  2002/07/11 03:54:35  da0g
 * Addressed String = (char*) vs (const char *) issues.
 * Patched distributed instance name setting.
 * Addressed minor threading issues.
 * TCM_AllocateDistributedNode() now sets either/both
 *   the local-instance-name and the remote-instance-name.
 * Added NULL_CHAR macro.
 * Added TCM_PURE_VIRTUAL_METHOD macro.
 * Fixed STRING typedef #ifdefs.
 * Altered tcm.cc::monitorActivationName() (removed static variable),
 *   TCM_AllocateCompleteMonitorNode() now exploits monitorActivationName().
 *
 * Revision 1.46  2002/06/26 16:50:56  reids
 * Made a distinction between the type-name and instance-name of a node.
 *  Enable instance name of node to be set in a distributed fashion.
 *  Removed a memory leak in distributedCallbackFn.
 *  Added casts to satisfy gcc 3.0.
 *  Insure warns that "malloc" (which strup uses) must be paired with "free"
 *  Fixed an array-bounds error found by Insure.
 *  Removed signatures of library functions that are already defined.
 *
 * Revision 1.45  2002/06/10 18:50:30  reids
 * Another try at fixing the "postponed" bug -- the previous fix did not
 *   work if the node that was postponed was a descendant of the one being
 *   terminated.  The new version works correctly.
 *
 * Revision 1.44  2002/06/07 20:56:25  reids
 * Fixed a bug where "postponed" tasks could not be terminated (since they
 *  had not officially completed handling).  Now, "terminate" forces postponed
 *  tasks to be "done handling", so they can be terminated normally.
 *
 * Revision 1.43  2002/05/10 03:28:01  reids
 * tcm.h
 *
 * Revision 1.42  2002/04/02 17:25:29  reids
 * Fixed the problem with "delayTermination" of exception nodes -- now, nodes
 *   that are marked "delay termination" are terminated after they are
 *   achieved only if explicitly requested to be so.
 *
 * Revision 1.41  2002/03/26 05:19:53  reids
 * Significant bug fixes to handling of distributed nodes to take into account
 *   non-persistent task tree nodes.  Also, cleaning up of the "virtualNodes"
 *   list when task tree nodes complete, and significant change to the way
 *   "when after" constraints are handled.
 *
 * Revision 1.40  2002/03/22 02:30:26  da0g
 * TCM_* distributed functions now properly lock/unlock the master mutex.
 * Removed TCM_IsDistributedNode() from distributed.cc.
 * Added TCM_IsDistributedNode() to tcm.cc.
 * Removed USE_CACHED_LAST_CHILD code.
 * Added isVirtual() method.
 * Added Exception-Handler-Ordering code.
 *
 * Revision 1.39  2002/02/05 17:45:17  reids
 * Backed out the getLocal function for distributed nodes -- instead,
 *   "getVirtual" creates a new virtual node only if the "host" given as the
 *   virtual address is not the current agent (o/w the node is "local").
 * Fixed several bugs relating to race conditions in the distributed version.
 *
 * Revision 1.38  2002/01/18 14:21:49  reids
 * Handling signals from timers better;
 *   Upgraded tracing options;
 *   Added "getLocal" to prevent race condition bugs with deleted virtual nodes;
 *   Commented out use of _cachedLastChild;
 *   Delay "forgetVirtual" to help prevent race conditions
 *
 * Revision 1.37  2002/01/11 02:18:31  da0g
 * Fix: distributed.cc needed to keep copy of string, not reference.
 *
 * Revision 1.36  2001/11/20 19:21:52  reids
 * Moved a few definitions around (forgetVirtual and virtualNodes) so that one
 *  can compile TCM with the -DDISTRIBUTED flag, but then link against it
 *  without having to incorporate any of the distributed files (which, in
 *  turn, need IPC and ipcInterface).
 *
 * Revision 1.35  2001/10/23 22:52:59  da0g
 * Added Threading support.  Cached lastchild.
 *
 * Revision 1.34  2001/09/07 02:48:06  reids
 * Fixed two minor, but annoying bugs:
 *  1) Monitors that were terminated with TCM_TerminateNode did not, in fact,
 *     ever really terminate.
 *  2) "ConstrainWhenAfter" signals that were deleted before running did not
 *     release
 *
 * Revision 1.33  2001/07/24 12:49:04  reids
 * Enable monitor client data to be sent with all the monitor activation
 *   functions (ActivateAt, ActivateIn, ActivateInAfter).  Fixed bugs in
 *   the way monitor data was being set.
 * Separate templates for virtual nodes from regular templates (compiled in
 *   only when DISTRIBUTED option is used -- makes code cleaner).
 *
 * Revision 1.32  2001/07/23 16:22:12  reids
 * removeRequestEvents => removeRequestEvent (just delete one event, not all).
 *
 * Revision 1.31  2001/06/15 19:00:49  reids
 * Fixed a bug that David Apfelbaum found having to do with destroying a node
 *   that had a "constrainWhenAfter" event attached to it.
 *
 * Revision 1.30  2001/06/11 21:39:24  reids
 * Remove destroyed/cleaned-up nodes from the "virtual nodes" list, so they
 *  can be garbage collected.
 *
 * Revision 1.29  2001/06/11 15:56:10  reids
 * Fixed a problem with distributed nodes being confused about which node is
 *   the root node (basically made it possible to determine whether a virtual
 *   node is the root of some other tree).
 *
 * Revision 1.28  2001/04/04 14:26:13  reids
 * Task tree nodes are now garbage collected, by default, after they are
 *   completely achieved.  Can change this behavior using TCM_SetPersistence.
 * Also, cleaned up some memory leaks: Now seems to lose little, if any, memory
 *
 * Revision 1.27  2001/03/26 21:40:40  trey
 * changed list<T> type to be tcmList<T> to avoid conflict with STL lists
 *
 * Revision 1.26  2001/03/02 12:51:51  reids
 * Made millisecond-level timing much more accurate, especially under RH5.2
 *
 * Revision 1.25  2001/02/18 18:17:09  reids
 * Fixed a bug having to do with sending distributed task arguments
 *
 * Revision 1.24  2001/02/17 03:29:25  reids
 * Improved distributed version of TCM.  No longer does each agent have to
 *  connect to a separate server.  Also, simplified the communications
 *  interface since IPC now (version 3.4) supports automatic unmarshalling of
 *  data when invoking message handlers.
 *
 * Revision 1.23  2000/08/18 17:22:05  da0g
 * Bugfix: monitorDereference() not being used.
 *
 * Revision 1.22  2000/07/05 23:14:08  da0g
 * Fixed delete of const pointer bug.
 *
 * Revision 1.21  2000/02/03 14:41:25  reids
 * Fixed improper templating bugs with tcmHandle's refIncr and refDecr (da0g)
 *
 * Revision 1.20  2000/01/19 21:26:43  reids
 * Added two new top-level functions:
 *   TCM_IsDoneHandling(ref) -- returns TRUE if the ref has raised success or
 * 			     failure.
 *   TCM_IsPostponed(ref) -- returns TRUE if the ref's action has finished
 * 		          executing, but the node has not yet raised
 * 			  success or failure.
 *
 * Revision 1.19  1999/08/05 17:22:37  reids
 * Changes needed because the API to the commInterface library was updated
 *   to be more generally applicable.  Removed all dependencies (at least in
 *   theory) on IPC.
 *
 * Revision 1.18  1999/08/04 14:00:19  reids
 * Distributed version of TCM.  Provides the ability for one process to
 *  add tasks, constraints, exception handlers, etc. with another process.
 *  The distributed version is created by compiling with -DDISTRIBUTED flag,
 *   which can be gotten by "gmaking" with DISTRIBUTED=1.
 *
 * Revision 1.17  1999/06/06 13:48:10  reids
 * Changes to TCM 1.3.0:
 *   New list data structures.
 *   New, generalized logging interface.
 *   Fixed firstChild to work if there are no children.
 *   Fixed code to deallocate a node.
 *   In certain circmumstances, don't send termination signal when task
 *     itself terminating.
 *
 * Revision 1.16  1999/03/02 01:31:14  da0g
 * Updated TCM_FailureData to use getNodeRefCountData
 *
 * Revision 1.15  1999/02/26 18:05:42  da0g
 * Modified Agenda::clearQueues not to exit until timePoint/timeout has passed.
 *
 * Revision 1.14  98/12/21  17:56:13  reids
 * Added TCM_GetAction.
 * 
 * Revision 1.13  1998/12/16 03:08:21  reids
 * Added support for "on termination" functions.
 *   Also enabled tca.h and tcm.h to co-exist (needed to change values
 *   of several constants).
 *
 * Revision 1.12  98/09/15  18:45:24  da0g
 * Enhanced exceptions to support multiple-name resolution and Ref_Count (automatically-destroyed) Data.
 * 
 * Revision 1.11  1998/08/05 10:57:30  reids
 * Made a compiler option for printing out debugging information.
 *   Created macros for TCM_TerminateAtAfter and TCM_DelayUntilAfter, for
 *   backwards compatibility.
 *
 * Revision 1.10  98/07/14  17:21:31  reids
 * Fixed a bug in handling Activate_Signal.
 * Changed TCM_DelayUntilAfter to TCM_DelayForAfter, changed TCM_
 *   TerminateAtAfter to TCM_TerminateInAfter to TCM_DelayForAfter, and
 *   changed order of arguments.
 * Added TCM_ActivateInAfter.
 * 
 * Revision 1.9  98/06/02  10:39:46  reids
 * Increased overall efficiency of TCM.
 * 
 * Revision 1.8  98/05/30  12:00:34  reids
 * Fixed "waitFor" if timepoint is achieved but there are still events on
 *   the timer queue.
 * Clear timeouts related to nodes that are completely achieved.
 * 
 * Revision 1.7  98/04/21  12:46:15  reids
 * Added InvokeWhen/InvokeAfter functionality, to invoke a function when a
 *   particular event occurs.
 * Added the DelayUntilAfter/TerminateAtAfter functionality, to delay/terminate
 *   a node by waiting some msecs after a particular event has occurred.
 * 
 * Revision 1.6  98/03/06  12:42:37  reids
 * Modifications made to support Solaris.
 * 
 * Revision 1.5  98/01/30  14:51:07  reids
 * Updated to compile under gcc 2.7.2 and under Linux.
 * Also, made STRING "const char *" and changed API to take const arguments,
 *   where applicable.
 * 
 * Revision 1.4  97/12/30  12:29:06  reids
 * Added option to *not* wait for timeouts and external events.
 *   Added flag to indicate whether node raised an exception (ie, failed).
 *   Added a "timer" monitor, which is like a polling monitor, except when
 *     activated it just invokes a function (rather than adding a new node)
 * 
 * Revision 1.3  97/12/29  17:06:36  reids
 * Version that has the basic functionality needed to support TDL.
 * 
 * Revision 1.2  97/12/04  17:50:30  reids
 * Another fairly stable version (except that monitors do not quite work)
 * 
 * Revision 1.1  97/11/21  14:06:48  reids
 * First release of TCM -- seems to be a stable version
 *
 *****************************************************************************/

#ifndef INCtcmPriv
#define INCtcmPriv

#define TCM_VERSION_MAJOR  2
#define TCM_VERSION_MINOR  10
#define TCM_VERSION_MICRO  1
#define TCM_VERSION_DATE "May-04-09"
#define TCM_COMMIT_DATE "$Date: 2009/05/04 19:44:49 $"

class Named_Object
{
 public:
  Named_Object() { name = NULL; }
  Named_Object(STRING theName) { name = strdup(theName); }
  Named_Object ( Named_Object const & obj )
    { name = (obj.name == NULL ? ((STRING) NULL) : strdup(obj.name));  }
  // Insure warns that "malloc" (which strup uses) must be paired with "free"
  ~Named_Object() { if (name) free((void *)name); }

  // Insure warns that "malloc" (which strup uses) must be paired with "free"
  Named_Object &operator=(Named_Object const &obj)
    { if (name) free((void *)name); name = strdup(obj.name); return *this; }
  BOOLEAN operator==(Named_Object const &obj) const
    { return streq(name, obj.name); }
  BOOLEAN operator< (Named_Object const &obj) const
    { return strcmp(name, obj.name) < 0; }
  STRING getName(void) const { return name; }

 protected:
  STRING name;
};

typedef tcmHandle<class Task_Tree_Node> Task_Tree_Ref;

inline BOOLEAN validRef(Task_Tree_Ref const &ref) { return *ref != NULL; }

#endif /* INCtcmPriv */
