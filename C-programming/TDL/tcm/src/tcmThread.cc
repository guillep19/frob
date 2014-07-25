/*****************************************************************************
 * 
 * PROJECT: Task Control Management.
 *
 * (c) Copyright 2001 Reid Simmons.  All rights reserved.
 *
 * FILE: tcmThread.cc
 *
 * ABSTRACT: Encapsulates basic functional interface to threading sub-system.
 *           Used both with and without THREADED being defined.
 *           Extended to include Thread <-> Current Task mapping.
 *
 * $Source: /afs/cs.cmu.edu/project/TCA/Master/tcaV8/tcm/src/tcmThread.cc,v $
 * $Revision: 1.3 $
 * $Date: 2009/01/15 17:00:05 $
 * $Author: reids $
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tcm/LICENSE.TXT)
 *
 * REVISION HISTORY:
 *
 * $Log: tcmThread.cc,v $
 * Revision 1.3  2009/01/15 17:00:05  reids
 * Released under simplified BSD open source license
 *
 * Revision 1.2  2008/07/16 06:15:10  reids
 * Updates for newer (pickier) compilers
 *
 * Revision 1.1  2002/12/23 02:26:58  da0g
 * Added System Task-Thread mapping routines.
 * Added User-specified Task-Thread mapping routines.
 *
 *
 *
 *****************************************************************************/

#include "tcmThread.h"

// Since all the list functions are template functions, need to include
// the CC file here, so the right template functions can be generated.
#include "tcmList.cc"

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

class CurrentUserTaskForThread
{
protected:
  TCM_Task_Tree_Ref  taskTreeRef;

#ifdef THREADED
  THREAD_ID          threadId;
#endif /* THREADED */


public:
  CurrentUserTaskForThread ( TCM_Task_Tree_Ref theTaskTreeRef )
    : taskTreeRef ( theTaskTreeRef        )
#ifdef THREADED
      ,threadId   ( Thread::getThreadId() )
#endif /* THREADED */
  {}

  CurrentUserTaskForThread ( const CurrentUserTaskForThread & theObjectToCopy )
    : taskTreeRef ( theObjectToCopy . getTaskTreeRef() )
#ifdef THREADED
      ,threadId   ( theObjectToCopy . getThreadId()    )
#endif /* THREADED */
  {}

  CurrentUserTaskForThread ( )
    : taskTreeRef ( )
    { 
#ifdef THREADED
      Thread::clearThread ( & threadId );
#endif /* THREADED */
    }
  

  ~CurrentUserTaskForThread() {}


  const TCM_Task_Tree_Ref & getTaskTreeRef() const { return taskTreeRef; }


#ifdef THREADED
  const THREAD_ID         & getThreadId()    const { return threadId;    }
#endif /* THREADED */


  int  isForCurrentThread() { 
#ifdef THREADED
                              return Thread::equalsCurrent ( getThreadId() ); }
#else  /* THREADED */
                              return TRUE; }
#endif /* THREADED */


	/*********************************/
	/* Interface for tcmList<> class */
	/*********************************/
  int operator == ( const CurrentUserTaskForThread & theObjectToCompare )
  {
    return  ( getTaskTreeRef() == theObjectToCompare.getTaskTreeRef() )
#ifdef THREADED      
      && ( Thread::equals ( getThreadId(), theObjectToCompare.getThreadId() ) )
#endif /* THREADED */
      ;
  }

  CurrentUserTaskForThread & operator = (
			    const CurrentUserTaskForThread & theObjectToCopy )
  {
    taskTreeRef = theObjectToCopy.getTaskTreeRef();
#ifdef THREADED
    threadId    = theObjectToCopy.getThreadId();
#endif /* THREADED */
    return *this;
  }
};

  /* Template declaration of a tcmList for CurrentUserTaskForThread */
template class tcmList<CurrentUserTaskForThread>;
template class List_Element<CurrentUserTaskForThread>;
template <class CurrentUserTaskForThread> tcmList<CurrentUserTaskForThread> tcmList<CurrentUserTaskForThread>::_freeList;
template <class CurrentUserTaskForThread> CurrentUserTaskForThread tcmList<CurrentUserTaskForThread>::nullItem;
template class Const_List_Iterate<CurrentUserTaskForThread>;
template class List_Iterate<CurrentUserTaskForThread>;

  /* The actual tcmList we utilize */
static tcmList<CurrentUserTaskForThread>  currentUserTaskForThreadStack;


  /* If theTaskTreeRef == NULL, we revert back to the system default.
   * (Until it is popped back off the stack.)
   */
void
pushUserTaskForThisThread( const TCM_Task_Tree_Ref & theTaskTreeRef )
{
  TCM_LockMasterMutex("pushUserTaskForThisThread");
  currentUserTaskForThreadStack
    . insertFirst ( CurrentUserTaskForThread ( theTaskTreeRef ) );
  TCM_UnlockMasterMutex("pushUserTaskForThisThread");
}

TCM_Return_Type
popUserTaskForThisThread()
{
  TCM_LockMasterMutex("popUserTaskForThisThread");
    /* List_Iterate will, by default, go from the first to the last element */
  for ( List_Iterate<CurrentUserTaskForThread>
	  iterator ( currentUserTaskForThreadStack );  iterator;  iterator++ )
  {
    if ( iterator . getObject() . isForCurrentThread() )
    {
      iterator . removeCurrent();
      TCM_UnlockMasterMutex("popUserTaskForThisThread");
      return TCM_Ok;
    }
  }

  tcmWarning("popUserTaskForThisThread:  No task found for this thread\n" );
  TCM_UnlockMasterMutex("popUserTaskForThisThread");
  return TCM_Error;
}

void
debugPrintUserTaskForThreadStack( STRING  theHeading,     /*= NULL*/
				  BOOLEAN theShowThreadId /*= TRUE*/ )
{
  TCM_LockMasterMutex("debugPrintUserTaskForThreadStack");
  for ( List_Iterate<CurrentUserTaskForThread>
	  iterator ( currentUserTaskForThreadStack );  iterator;  iterator++ )
  {
    fprintf (
      stderr,

#define FPRINTF_STRING_DEBUG_PRINT_USER_TASK_FOR_THREAD_STACK \
           "%s UserTaskForThreadStack:  %p (%s)   "
      ( ( theShowThreadId == TRUE )
       ? FPRINTF_STRING_DEBUG_PRINT_USER_TASK_FOR_THREAD_STACK
#ifdef THREADED
	  PRINTF_THREAD_ID_STRING
#endif /* THREADED */
	  "\n"
       : FPRINTF_STRING_DEBUG_PRINT_USER_TASK_FOR_THREAD_STACK "\n" ),
#undef FPRINTF_STRING_DEBUG_PRINT_USER_TASK_FOR_THREAD_STACK

      ( (theHeading == STRING(NULL)) ? "" : theHeading ),

      iterator . getObject() . getTaskTreeRef() . operator*(),

      ( (iterator . getObject() . getTaskTreeRef() . isNotNull())
       ? TCM_NodeName ( iterator . getObject() . getTaskTreeRef() )
       : "nil" )

#ifdef THREADED
      ,iterator . getObject() . getThreadId()
#endif /* THREADED */
      );
  }
  TCM_UnlockMasterMutex("debugPrintUserTaskForThreadStack");
}




/*
 * TCM interface for establishing the "System" Task-Thread mapping.
 *
 * The objective here is to implement a stack of current Tasks.
 * The stack task-data is stored on the actual stack,
 * in ExecuteTask and External_Events::dispatchEvents,
 * by swapping the current task in and out.
 */
static Task_Tree_Ref mainThreadCurrentSystemTaskTreeRef;

Task_Tree_Ref getMainThreadCurrentSystemTaskTreeRef()
{
  TCM_LockMasterMutex("getMainThreadCurrentSystemTaskTreeRef");
  Task_Tree_Ref returnTaskTreeRef = mainThreadCurrentSystemTaskTreeRef;
  TCM_UnlockMasterMutex("getMainThreadCurrentSystemTaskTreeRef");
  return returnTaskTreeRef;
}

Task_Tree_Ref setMainThreadCurrentSystemTaskTreeRef (
				         const Task_Tree_Ref & theTaskTreeRef )
{
  TCM_LockMasterMutex("setMainThreadCurrentSystemTaskTreeRef");
  Task_Tree_Ref returnTaskTreeRef = mainThreadCurrentSystemTaskTreeRef;
  mainThreadCurrentSystemTaskTreeRef = theTaskTreeRef;
  TCM_UnlockMasterMutex("setMainThreadCurrentSystemTaskTreeRef");
  return returnTaskTreeRef;
}




/*
 * Obtains the current user task-tree-ref for this thread, if one is available.
 * Otherwise falls back to the current system task-tree-ref.
 */

TCM_Task_Tree_Ref getCurrentTaskTreeRefForThisThread(
				BOOLEAN theCheckUserSpecifiedOnly /*= FALSE*/ )
{
  TCM_LockMasterMutex("getCurrentTaskTreeRefForThisThread");

  TCM_Task_Tree_Ref returnTaskTreeRef; /* = NULL, initially */


	/* Check user-specified task-thread bindings */

    /* List_Iterate will, by default, go from the first to the last element */
  for ( List_Iterate<CurrentUserTaskForThread>
	  iterator ( currentUserTaskForThreadStack );  iterator;  iterator++ )
  {
    if ( iterator . getObject() . isForCurrentThread() )
    {
      returnTaskTreeRef = iterator . getObject() . getTaskTreeRef();

	/* If theTaskTreeRef == NULL, we revert back to the system default. *
         * (Until it is popped back off the stack.)                         */
      if ( returnTaskTreeRef . isNull() )
	break;

      /* ELSE: */
      TCM_UnlockMasterMutex("getCurrentTaskTreeRefForThisThread");
      return returnTaskTreeRef;
    }
  }
  
	/* If we are only checking the user-specified Task-Thread mappings. */
  if ( theCheckUserSpecifiedOnly != FALSE )
  {
    TCM_UnlockMasterMutex("getCurrentTaskTreeRefForThisThread");
    return returnTaskTreeRef;
  }


	/* Check system primary thread binding */

#ifdef THREADED
  if ( ThreadManager::getGlobalThreadManager()
         . getIsThisThePrimaryThread()         == TRUE )
  {
#endif /* THREADED */


    returnTaskTreeRef = getMainThreadCurrentSystemTaskTreeRef();


#ifdef THREADED
  }
  else
  {
	/* Check system sub-threads bindings */
	/* Note: This will set returnTaskTreeRef to NULL if nothing is found.*/
    returnTaskTreeRef = ThreadManager::getGlobalThreadManager()
			  . getTaskTreeRefForThisThread();
  }
#endif /* THREADED */

  TCM_UnlockMasterMutex("getCurrentTaskTreeRefForThisThread");
  return returnTaskTreeRef;
}


