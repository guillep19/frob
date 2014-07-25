
/*****************************************************************************
 * 
 * PROJECT: Task Control Management.
 *
 * (c) Copyright 2001 Reid Simmons.  All rights reserved.
 *
 * FILE: tcmThread.h
 *
 * ABSTRACT: Encapsulates basic functional interface to threading sub-system.
 *           Used both with and without THREADED being defined.
 *           Extended to include Thread <-> Current Task mapping.
 *
 * $Source: /afs/cs.cmu.edu/project/TCA/Master/tcaV8/tcm/src/tcmThread.h,v $ 
 * $Revision: 1.4 $
 * $Date: 2009/01/15 17:00:05 $
 * $Author: reids $
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tcm/LICENSE.TXT)
 *
 * REVISION HISTORY:
 *
 * $Log: tcmThread.h,v $
 * Revision 1.4  2009/01/15 17:00:05  reids
 * Released under simplified BSD open source license
 *
 * Revision 1.3  2002/12/23 02:26:25  da0g
 * Added System Task-Thread mapping routines.
 * Added User-specified Task-Thread mapping routines.
 * ExecuteTask() now establishes the System Task-Thread mapping,
 *   overriding any User-specified Task-Thread mappings.
 *
 * Revision 1.2  2002/09/16 22:50:14  da0g
 * Fixed minor typo bug.
 *
 * Revision 1.1  2001/10/23 22:53:00  da0g
 * Added Threading support.  Cached lastchild.
 *
 *
 *****************************************************************************/

#ifndef TCM_THREAD_H
#define TCM_THREAD_H

#include "tcmBasics.h"
#include "taskTree.h"
#include "tcmGlobal.h"
#include "tcm.h"

#ifdef THREADED
#include "ThreadManager.h"
#endif /* THREADED */


/*
 * Users can specify a Task-Thread mapping that overrides the default system
 * values established by TCM.  Typical use case occurs within Resumed Tasks.
 *
 * Note: There can be no distinction between the main thread and other threads
 *       with respect to the "user" API.  Furthermore, we cannot limit
 *       ourselves to only the threads that TCM knows about.
 *
 * Note: The system must override any user-specified choices when new tasks
 *       are running (handled).  Case in point:  TDL Resume'd tasks specify
 *       a USER Task-Thread mapping, then spawn a task with WAIT in a
 *       non-threaded system.  Subtask now uses the Resume's parent node
 *       (via the USER Task-Thread mapping) instead of itself for it's
 *       children tasks.
 *
 * Note: CurrentUserTaskForThread must work with both the THREADED and
 *       non-THREADED cases.
 *
 * See also:  <tcm.h> TCM_PushUserTaskForThisThreadForDurationOfObject class.
 */
void            pushUserTaskForThisThread(
				    const TCM_Task_Tree_Ref & theTaskTreeRef );
TCM_Return_Type popUserTaskForThisThread();
void            debugPrintUserTaskForThreadStack(
					    STRING theHeading = STRING(NULL),
					    BOOLEAN theShowThreadId = TRUE  );

/*
 * TCM interface for establishing the "System" Task-Thread mapping.
 *
 * The objective here is to implement a stack of current Tasks.
 * The stack task-data is stored on the actual stack,
 * in ExecuteTask and External_Events::dispatchEvents,
 * by swapping the current task in and out.
 */
Task_Tree_Ref getMainThreadCurrentSystemTaskTreeRef();
Task_Tree_Ref setMainThreadCurrentSystemTaskTreeRef(
				        const Task_Tree_Ref & theTaskTreeRef );

/*
 * Obtains the current user Task_Tree_Ref for this thread, if one is available.
 * Otherwise, if theCheckUserSpecifiedOnly is FALSE, it falls back to the
 * current system task-tree-ref.
 */
TCM_Task_Tree_Ref getCurrentTaskTreeRefForThisThread(
				   BOOLEAN theCheckUserSpecifiedOnly = FALSE );


inline void ExecuteTask ( const Task_Tree_Ref & theTask )
{
  if ( validRef ( theTask ) )
  {


#ifdef THREADED
    if ( theTask -> getIsThreaded() )
    {
      ThreadManager::getGlobalThreadManager().executeTask ( theTask );
    }
    else
    {
      	/* If we are threaded, fully unlock ourself before waiting... */
      unsigned int numberOfLocks
	= TCM_FullyUnlockMasterMutexReturningNumberOfLocks();
#endif /* THREADED */


        /* Push main thread's current Task_Tree_Node. */
      Task_Tree_Ref previousTaskTreeRef
	= setMainThreadCurrentSystemTaskTreeRef ( theTask );

	/* Override any user-specified Task-Thread mappings, if necessary */
      TCM_PushUserTaskForThisThreadForDurationOfObject
	ifNecessaryOverrideUserTaskThreadMappingWithSystemDefault;

	/* Run Task in non-threaded mode. */
      theTask -> execute();

	/*Cancel override of user-specified Task-Thread mapping, if necessary*/
      ifNecessaryOverrideUserTaskThreadMappingWithSystemDefault . cleanup();

	/* Pop main thread's current Task_Tree_Node back to what it was. */
      setMainThreadCurrentSystemTaskTreeRef ( previousTaskTreeRef );



#ifdef THREADED
	/* If we are threaded, fully re-lock ourself afterwards... */
      TCM_FullyRelockMasterMutexNumberOfLocksTimes ( numberOfLocks );
    }
#endif /* THREADED */


  }
  else
  {
    tcmWarning("ExecuteTask: Reference to NULL node\n");
  }
}



inline BOOLEAN SetPrimaryThread (
	    BOOLEAN   theReportError                       = TRUE,
	    BOOLEAN   theReturnValueIfAlreadyPrimaryThread = TRUE,
	    BOOLEAN   theReportIfAlreadyPrimaryThread      = TRUE,
	    BOOLEAN * theWasAlreadyPrimaryThread           = (BOOLEAN *) NULL )
{
  BOOLEAN returnValue = TRUE;

#ifdef THREADED
  returnValue = TO_BOOLEAN (
       ThreadManager::getGlobalThreadManager() . setPrimaryThread (
	 theReportError,
	 ((theReturnValueIfAlreadyPrimaryThread == TRUE) ? SUCCESS : FAILURE),
	 theReportIfAlreadyPrimaryThread,
	 theWasAlreadyPrimaryThread )
    == SUCCESS );
#endif /* THREADED */  

  return returnValue;
}


inline BOOLEAN ClearPrimaryThread()
{
  BOOLEAN returnValue = TRUE;

#ifdef THREADED
  returnValue = TO_BOOLEAN (    ThreadManager::getGlobalThreadManager()
				  . clearPrimaryThread()
			     == SUCCESS );
#endif /* THREADED */  

  return returnValue;
}


#ifdef THREADED
inline THREAD_ID GetPrimaryThreadId()
{
  return ThreadManager::getGlobalThreadManager().getPrimaryThreadId();
}
#endif /* THREADED */


inline BOOLEAN HasThreadWithUnfinishedWork()
{
  BOOLEAN returnValue = FALSE;

#ifdef THREADED
  returnValue
    = ThreadManager::getGlobalThreadManager() . hasThreadWithUnfinishedWork();
#endif /* THREADED */  

  return returnValue;
}

#endif /* TCM_THREAD_H */
