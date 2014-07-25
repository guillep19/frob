/*
 * This is the place for code that is utilized by compiled TDL code.
 * Naturally, this code can be used directly.  But its purpose is to
 * support (implement) specific TDL features.
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

#ifndef TDL_H
#define TDL_H

/*
 * Under gcc 3.2.3, to discourage the use of the now obsolete iostream.h
 * interface, we are treated to warning messages during compilation.
 * However, rewriting the code will break compatibility with older systems,
 * and the iostream.h file still provides the precise interface required.
 * So, for now, we are just disabling these messages.
 */
#define _CPP_BACKWARD_BACKWARD_WARNING_H

 /* Standard C++ Header Files */
// The definition of GNUC_PREREQ in g++ 3.4.2 is buggy!
#undef __GNUC_PREREQ
#define __GNUC_PREREQ(maj,min) (((__GNUC__ << 16) + __GNUC_MINOR__) >= (((maj) << 16) + (min)))

#if __GNUC_PREREQ(3,3)
#include <iostream>
#include <iomanip>
#include <fstream>
#include <sstream>
#define STRINGSTREAM ostringstream
#else
#include <iostream.h>
#include <iomanip.h>
#include <fstream.h>
#include <strstream.h>
#define STRINGSTREAM ostrstream
#endif
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>  /* Used to be sys/time.h */
#include <sys/types.h>
#include <unistd.h>
#include <errno.h>
#include <limits.h>
#include <signal.h>
#include <sys/wait.h>

    /* TCM defines a const variable called MAXINT.
     * Unfortunately, so does math.h.  Only math.h #defines MAXINT...
     */
#undef MAXINT

 /* TCM Header Files */
#include <tcm.h>

 /* TDL Header Files */
#include "stdtypes.H"
#include "_TDL_InterfaceToTCM_2.H"
#include "_TDL_AllocationFunction.H"
#include "tdldef.H"
#include "_TDL_BaseAction.H"
#include "_TDL_InterfaceToTCM.H"
#include "_TDL_ActionOrVoid.H"
#include "TDL_StringBuffer.H"


/* Support for the return-value from:                         *
 *  extern  _TDL_Initialize_Return_Class TDL_Initialize(...); */
class _TDL_Initialize_Return_Class
{
private: char foo;  /* Classes with no associated data bother some compilers */
public:  _TDL_Initialize_Return_Class() : foo ( NULL_CHAR ) {}
         ~_TDL_Initialize_Return_Class() {} /* NOT Virtual */
         _TDL_Initialize_Return_Class connectAgent (
			    STRING theAgent,
			    STRING theCentralHost = STRING(NULL) );
};

/* We wish to replace TCM_Initialize() with TDL_Initialize(), which will be
 * invoked automatically, and will return an instance of a class which can be
 * used to connect distributed agents. Class/Functions are defined in tdldef.C.
 */

extern _TDL_Initialize_Return_Class TDL_Initialize (
		      /*BOOLEAN*/int theHideTcmMessages,
		      /*BOOLEAN*/int theHideTdlMessages  );
extern _TDL_Initialize_Return_Class TDL_Initialize (
		      STRING         theAgent,
		      STRING         theCentralHost,
		      /*BOOLEAN*/int theHideTcmMessages,
		      /*BOOLEAN*/int theHideTdlMessages );

	/* Convenience versions... */
inline _TDL_Initialize_Return_Class TDL_Initialize (
		      /*BOOLEAN*/int theHideTcmMessages,
		      /*BOOLEAN*/int theHideTdlMessages,
		      STRING         theAgent,
		      STRING         theCentralHost     = STRING(NULL) )
	  { return TDL_Initialize ( theAgent, theCentralHost,
				    theHideTcmMessages, theHideTdlMessages ); }

inline _TDL_Initialize_Return_Class TDL_Initialize (
		      /*BOOLEAN*/int theHideMessages    = TRUE )
	  { return TDL_Initialize ( theHideMessages, theHideMessages ); }

inline _TDL_Initialize_Return_Class TDL_Initialize (
		      /*BOOLEAN*/int      theHideMessages,
		      STRING         theAgent,
		      STRING         theCentralHost     = STRING(NULL) )
	  { return TDL_Initialize ( theAgent, theCentralHost,
				    theHideMessages, theHideMessages ); }

inline _TDL_Initialize_Return_Class TDL_Initialize (
		      STRING         theAgent,
		      STRING         theCentralHost     = STRING(NULL),
		      /*BOOLEAN*/int theHideMessages    = TRUE )
	  { return TDL_Initialize ( theAgent, theCentralHost,
				    theHideMessages, theHideMessages ); }

inline _TDL_Initialize_Return_Class TDL_Initialize (
		      STRING         theAgent,
		      /*BOOLEAN*/int theHideMessages )
	  { return TDL_Initialize ( theAgent, STRING(NULL),
				    theHideMessages, theHideMessages ); }

inline _TDL_Initialize_Return_Class TDL_Initialize (
		      STRING         theAgent,
		      /*BOOLEAN*/int theHideTcmMessages,
		      /*BOOLEAN*/int theHideTdlMessages )
	  { return TDL_Initialize ( theAgent, STRING(NULL),
				    theHideTcmMessages, theHideTdlMessages ); }







	/* Create this as a global, so that spawns outside of tasks can
	 * refer to "PARENT".  (Ie: For the #define to work right...)
	 * Now used in place of TCM_RootNode() to provide a rather hideous
	 * mechanism for inserting external nodes (spawns outside of tasks)
	 * into the TCM Task Tree at locations other than the TCM_RootNode.
	 */
extern const TCM_Task_Tree_Ref & _TDL_ENCLOSING_TASK; /* = TCM_RootNode() */


	/* Macro to mark variables as "USED" to the compiler, so as to   *
	 * prevent variable-not-used warnings on TDLC-created variables. *
	 * TDLC utilizes (generates) _TDL_MARKUSED(...) commands.        *
	 * If users utilize MARKUSED(...), the two can be distinguished. */
#define _TDL_MARKUSED(X)  ((void)(&(X)))
#define MARKUSED(X)       ((void)(&(X)))

	/* Monitor functions need to reference _TDL_ENCLOSING_TASK */
#define getNumberOfTriggers()    ( TCM_NumTriggers    ( _TDL_ENCLOSING_TASK ) )
#define getNumberOfActivates()   ( TCM_NumActivations ( _TDL_ENCLOSING_TASK ) )

	/* Note:  This is a case-insenstive macro.  The TDLC program         *
	 * automatically translates the keyword "TRIGGER" to all upper-case. */
#define TRIGGER()                ( TCM_Trigger ( _TDL_ENCLOSING_TASK ) )


	/* Trival means for user to access TDL Tasks as             *
	 * "const TCM_Task_Tree_Ref &"'s for use with TCM functions */
#define TDL_REF(X)   _TDL_SpawnedTasks [ # X ]


	/* Folks may want to implement an exception handler for BaseException*/
#define TDL_BaseException _TDL_BaseException

	/* Sometimes it's nice to know the Derived-Exception-name of */
	/* a Base-Exception.  Only useful inside Exception Handlers. */
#define TDL_DerivedExceptionName(X)  ((X).getExceptionName())

	/* Somewhat complicated way to access arguments from Tasks while
	 * outside the scope of those tasks...  Useful for POSTPONE'ed tasks.
	 * Ie:  TDL_TaskData_Ref  taskDataRef;
	 *      TCM_Task_Tree_Ref taskRef;
         *      Goal foo ( int i ) { taskDataRef = TDL_TASKDATA();
         *                           task        = TDL_REF ( PARENT );
         *                           POSTPONE; }
         *      cout << TDL_ARG     (taskDataRef, foo, filename_tdl, 0, i);
         *      cout << TDL_TASK_ARG(taskRef,     foo, filename_tdl, 0, i);
	 * Note: theFilename is case-sensitive.  Replace '.' and other
	 *       illegal (not [a-zA-Z0-9_]) characters with '_'.
	 * Note: theIndex refers to overloaded task names.  It's 0 for
	 *       all non-overloaded cases, and the first instance of an
	 *       overloaded Task.
	 */
typedef TCM_Action_Ref TDL_TaskData_Ref;
typedef _Action        TDL_TaskData;
#define TDL_TASKDATA()    (this)
#define TDL_CLASS_NAME( theTaskName, theFileName, theIndex )            \
  _TDL_ ## theTaskName ## _ ## theFileName ## _ ## theIndex
#define TDL_ARG(  theTaskDataRef, theTaskName, theFileName, theIndex,   \
                  theArgumentName                                     ) \
 ( ((TDL_CLASS_NAME( theTaskName, theFileName, theIndex) *)             \
    ((theTaskDataRef).operator*())) -> theArgumentName)
#define TDL_TASK_ARG( theTaskRef, theTaskName, theFileName, theIndex,   \
                      theArgumentName                                 ) \
 ( ((TDL_CLASS_NAME( theTaskName, theFileName, theIndex) *)             \
    (TCM_GetActualAction(theTaskRef).operator*())) -> theArgumentName )


	/* POSTPONE'ed Tasks need a means to be marked SUCCEEDED or FAILED. */

	/* Ie: void bar() { TDL_SUCCESS ( task ); } */
#define TDL_SUCCESS( theTaskRefToMarkAsSucceeded )                            \
			   _TDL_DO_TCM_SUCCESS ( theTaskRefToMarkAsSucceeded )

	/* Note:  TheFailureArgs must be surrounded by parenthesis!!!!    */
	/* Ie: void bar() { TDL_FAIL ( task, engineOnFire, ("f3", 1) ); } */
#define TDL_FAIL( theTaskRefToFail, theFailureName, theFailureArgs )          \
  _TDL_DO_TCM_FAIL ( (theTaskRefToFail),                                      \
		     _TDL_Exception_ ## theFailureName ## ::exceptionMatches, \
		     _TDL_CreateException_ ## theFailureName theFailureArgs  )

	/* And POSTPONE'd Exception-Handlers need to be */ 
	/* able to access their Exception's data.       */
#define TDL_FAIL_DATA_ARG( theTaskDataRef, theExceptionName, theArgumentName )\
			    ( ( (_TDL_Exception_ ## theExceptionName *)       \
				(TCM_FailureException ( theTaskDataRef ) ) \
			       ) -> theArgumentName )



	/* Macros to test if a TASK has completed execution. */
#define TDL_TASK_IS_COMPLETED( theTCMTaskTreeRef )                            \
	 ( TCM_IsPast ( TCM_EndOf ( TCM_AchievingOf ( theTCMTaskTreeRef ) ) ) )
#define TDL_IS_COMPLETED( theTask )                                           \
	 ( TDL_TASK_IS_COMPLETED ( TDL_REF ( theTask ) ) )

	/* Macros to test if a TASK has started. */
#define TDL_TASK_IS_STARTED( theTCMTaskTreeRef )                              \
	 ( TCM_IsPast ( TCM_StartOf ( TCM_HandlingOf ( theTCMTaskTreeRef ) ) ))
#define TDL_IS_STARTED( theTask )                                             \
	 ( TDL_TASK_IS_STARTED ( TDL_REF ( theTask ) ) )

	/* Macros to test if a TASK is of a particular Task-type. *
	 *   Returns TRUE for direct instances [SPAWN foo();],    *
	 *   both Monitor and Monitor-child instances ["ACT-"],   *
	 *   and SPAWN-with-WAIT macro instances ["-auto,wait"].  */
#define TDL_TASK_INSTANCE_OF( theTCMTaskTreeRef, theTaskName )                \
	 ( TDL::checkForTaskInstanceOf ( TCM_NodeName ( theTCMTaskTreeRef ),  \
					 # theTaskName ) )
#define TDL_INSTANCE_OF( theTask, theTaskName )                               \
	 ( TDL_TASK_INSTANCE_OF ( TDL_REF ( theTask ), theTaskName ) )


	/* Macro to implement the real RESUME functionality. *
	 * Used internally to TDL.                           */
#define _TDL_INTERNAL_RESUME( theTaskClassName, theTCMTaskTreeRefToResume )   \
    ( (theTaskClassName *)                                                    \
      ( ( (_TDL_BaseAction *)                                                 \
	  ( TCM_GetActualAction ( theTCMTaskTreeRefToResume ) . operator*() ) \
	) -> _TDL_setTcmTaskTreeNodeAndVerifyActionsMatch (                   \
                theTCMTaskTreeRefToResume . operator *(),                     \
                theTaskClassName::TCM_getStaticName(),                        \
                # theTaskClassName "::resume()"            )                  \
      )                                                                       \
    )  -> resume

#define TDL_RESUME_ARGUMENT_DECLARATIONS  \
	  const TCM_Task_Tree_Ref & _TDL_ENCLOSING_TASK


	/* KLUDGE to allow us to spawn children under an alternate
	 * Task Tree Node inside a task.   Untested.  Use at own risk.
	 */
#define TDL_BlackMagic_SwitchParentTaskNodeTo( theTCMTaskTreeRef )            \
	   _TDL_SpawnedTasks . setEnclosingTaskReference ( theTCMTaskTreeRef )



	/* Originally intended to simplify testing of Distributed code,
	 * these functions may prove useful for developing Distributed
	 * applications as well.
	 */
extern void TDL_ForkChildren (
	      void ( *     theFunctions [] )(u_int4),
	      const char * theNames [],
	      u_int4       theNumberOfFunctions,
	      MSecs        theDurationTimeInMilliSeconds,
	      MSecs        theTimeBetweenStartingProcessesInMilliseconds = 0,
	      BOOLEAN      thePrintTimeStampsForDebugging = FALSE            );

	/* Convenience function to simplify  Central/Server/Client  case. */
extern void TDL_ForkChildren (
		  void ( * theServerFunction  )(u_int4),
		  void ( * theClientFbunction )(u_int4),
		  MSecs    theDurationTimeInMilliSeconds,
		  MSecs    theTimeBetweenStartingProcessesInMilliseconds = 0,
		  BOOLEAN  thePrintTimeStampsForDebugging = FALSE            );


	/* Lets put all the Delayed-Allocation mumble-jumble in one place. *
	 * No, this can't be a function.  It *MUST* be a macro to prevent  *
	 * unnecessary invocations of ALLOCATION.                          */
#define _TDL_ProcessDelayedAllocation_CanAbort( LOCATION,                     \
						_TDL_DelayedAllocationObject, \
						_TDL_ENCLOSING_TASK,          \
						ALLOCATION_INVOCATION )       \
  do {                                                                        \
    if ( _TDL_DelayedAllocationObject . isDataBad( LOCATION ) )               \
    {                                                                         \
      TDL::getLogStream()                                                     \
	<< endl << endl                                                       \
	<< "[" LOCATION                                                       \
           "]:  Error:  Bad data in _TDL_DelayedAllocationObject."            \
	   "Aborting CreateAction.  Aborting Task."                           \
	<< endl << endl;                                                      \
      return _TDL_ActionOrVoid();                                             \
    }                                                                         \
                                                                              \
    if (   ( _TDL_DelayedAllocationObject . getIsDelayedAllocation() )        \
	&& ( _TDL_ENCLOSING_TASK          . isNull()                 ) )      \
    {                                                                         \
      _TDL_ENCLOSING_TASK = ALLOCATION_INVOCATION;                            \
    }                                                                         \
                                                                              \
    if ( _TDL_ENCLOSING_TASK . isNull() )                                     \
    {                                                                         \
      TDL::getLogStream()                                                     \
	<< endl << endl                                                       \
	<< "[" LOCATION                                                       \
           "]:  ERROR:  _TDL_ENCLOSING_TASK is NULL, unallocated."            \
	<< endl << endl;                                                      \
      return _TDL_ActionOrVoid();                                             \
    }                                                                         \
  } while ( 0 );




#ifndef _TDL_INTERNAL_

#include "_TDL_Snode.H"
#include "_TDL_Slist.H"
#include "_TDL_Dnode.H"
#include "_TDL_Dlist.H"
#include "_TDL_ArrayList.H"
#include "_TDL_NamesList.H"
#include "_TDL_TreeNode.H"
#include "_TDL_TreeNodeBranch.H"
#include "_TDL_IterationIndex.H"
#include "_TDL_TDLStatement.H"
#include "_TDL_SpawnStatement.H"
#include "_TDL_SpawnStatementData.H"
#include "_TDL_SpawnStatementTreeNode.H"
#include "_TDL_Exceptions.H"
#include "_TDL_Constraints.H"
#include "_TDL_WithStatement.H"
#include "_TDL_WithStatementData.H"
#include "_TDL_WithStatementTreeNode.H"
#include "_TDL_HandleManager.H"
#include "_TDL_functions_2.H"
#include "_TDL_DistributedRegistryEntry.H"
#include "_TDL_DelayedAllocation.H"

#endif /* ifndef _TDL_INTERNAL_ */



     /* Miscellaneous:
      *     Does our compiler conform to the new (1998) ANSI C++ Standard?
      *
      * Note:  The new [1998] ANSI C++ Standard has changed a number of things.
      * At present, both g++ 2.8.1 and egcs support the "new" Standard.  Which,
      * unfortunately, is syntactically incompatible with the "old" Standard
      * supported by g++ 2.7.2.x.
      */
#if  ( ! defined ( ANSI_STANDARD_CXX ) ) && __GNUC_PREREQ(2,8)

#define ANSI_STANDARD_CXX 1

#endif 


#endif /* TDL_H */

