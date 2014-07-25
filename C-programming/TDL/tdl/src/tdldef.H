/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

#ifndef TDL_DEF_H
#define TDL_DEF_H

#include "_TDL_AllocationFunction.H"

  /*
   * Class TDL exists primarily for use as a namespace,
   *  and should NEVER be constructed.
   */
class TDL
{
public:
  enum  REPORTING_LEVELS
  {
    NO_REPORTING                = 0x0000,

	/* Basic stuff that you almost always want... */
    SPAWN_FAILURE_OUTSIDE_TASK  = 0x0001,
    MISMATCH_TASK_TYPE          = 0x0002,
    MISMATCH_ALLOCATION_TYPE    = 0x0004,
    MISMATCH_ACTION_TASK        = 0x0008,
    EVERYTHING_BASIC            = (   SPAWN_FAILURE_OUTSIDE_TASK
				    | MISMATCH_TASK_TYPE
				    | MISMATCH_ALLOCATION_TYPE
				    | MISMATCH_ACTION_TASK ),

	/* Informational Reporting Verbosity... */
    VERBOSE_BRIEF               = 0x0020,
    VERBOSE_FULL                = 0x0040,
    VERBOSE                     = VERBOSE_BRIEF | VERBOSE_FULL,

	/* Everything up above all OR'ed together. (With room for additions) */
    EVERYTHING                  = 0x00ff,

	/* Debugging reports are not included by default. */
    TDL_DEBUG                   = 0x0100,
    TDL_DEBUG_TIME              = 0x0200,

	/* Everything -- including debugging reports! */
    ALL                         = 0xffff
  };


protected:
  static u_int4    reportingLevel;            /* = _TDL_REPORTING_LEVEL *
				               *    or TDL::EVERYTHING  */
  static BOOLEAN   abortOnActionTaskMismatch; /* = TRUE */
  static BOOLEAN   closeLogStream;            /* = FALSE */
  static ostream * logStream;                 /* = & cerr */

public:
  static ostream &  getLogStream()  { return * TDL::logStream; }
  static ostream &  getLogStreamNotInlined();

  static void setLogStream ( ostream & theOstream )
  {
    if ( TDL::closeLogStream == TRUE )
      delete TDL::logStream;

    TDL::closeLogStream = FALSE;
    TDL::logStream      = & theOstream;
  }

  static void setLogStream ( const char * theFilename )
  {
    if ( TDL::closeLogStream == TRUE )
      delete TDL::logStream;

    TDL::closeLogStream = TRUE;
    TDL::logStream      = new ofstream ( theFilename );
  }


  static u_int4 getReportingLevel ( )   { return TDL::reportingLevel; }
  static void   setReportingLevel ( u_int4 theReportingLevel )
		  { TDL::reportingLevel = theReportingLevel; }

  static void   addReportingLevel    ( u_int4 theReportingLevel )
		  { setReportingLevel (       getReportingLevel() 
					  |   theReportingLevel   ); }

  static void   removeReportingLevel ( u_int4 theReportingLevel )
		  { setReportingLevel (       getReportingLevel() 
					| ( ~ theReportingLevel ) ); }


  static inline int getIsReporting ( u_int4 theReportingLevel )
	 { return ( ( theReportingLevel & TDL::getReportingLevel() ) != 0 ); }


  static inline BOOLEAN shouldReportError ( TCM_Return_Type theResult,
					    int             theErrorLevel )
  {
    return (   ( theResult != TCM_Ok                   )
	    && ( TDL::getIsReporting ( theErrorLevel ) ) );
  }

	/* Note: Return TRUE if we Succeed, FALSE for Failure */
  static inline BOOLEAN testAndHandleFailure (
					TCM_Return_Type theResult,
					int             theErrorLevel,
					const char *    theErrorMessage1,
					const char *    theErrorMessage2 = "",
					const char *    theErrorMessage3 = "" )
  {
    if ( TDL::shouldReportError ( theResult, theErrorLevel ) )
    {
      getLogStream()
	<< theErrorMessage1 << theErrorMessage2 << theErrorMessage3 << endl;
    }
    return ( theResult == TCM_Ok );
  }

	/* Note: Return TRUE if we Succeed, FALSE for Failure */
  static inline BOOLEAN verifyAllocation (
				 const char *               theLocation,
				 const TCM_Task_Tree_Ref &  theTCMTaskTreeRef )
  {
    return testAndHandleFailure (
	    (  ( theTCMTaskTreeRef . operator*()  ==  (Task_Tree_Node *) NULL )
	      ? TCM_Error : TCM_Ok ),
	    TDL::SPAWN_FAILURE_OUTSIDE_TASK,
	    theLocation,
	    "  Failure:  Allocation/Access Failure.  "
	    "(Perhaps this task has been destroyed?)"  );

  }

	/* Note: Return TRUE if we Succeed, FALSE for Failure */
  static inline BOOLEAN verifySetAction  ( const char *     theLocation,
					   TCM_Return_Type  theResult   )
  {
    return testAndHandleFailure ( theResult,
				  TDL::SPAWN_FAILURE_OUTSIDE_TASK,
				  theLocation,
				  "  Failure:  _TDL_SetAction_..." );
  }

	/* Note: Return TRUE if we Succeed, FALSE for Failure */
  static inline BOOLEAN verifyInsertNode ( const char *     theLocation,
					   TCM_Return_Type  theResult   )
  {
    return testAndHandleFailure ( theResult,
				  TDL::SPAWN_FAILURE_OUTSIDE_TASK,
				  theLocation,
				  "  Failure:  TCM_InsertNode" );
  }

	/* Note: Return TRUE if we Succeed, FALSE for Failure */
  static inline BOOLEAN verifyConstraint ( const char *     theLocation,
					   const char *     theConstraint,
					   TCM_Return_Type  theResult     )
  {
    return testAndHandleFailure ( theResult,
				  TDL::SPAWN_FAILURE_OUTSIDE_TASK,
				  theLocation,
				  "  Constraint Failure:  ",
				  theConstraint );
  }

  static inline TCM_Task_Tree_Ref doSetOnTerminateTaskAction (
				  const char *              theLocation,
				  const TCM_Task_Tree_Ref & theTaskReference,
				  _Action *                 theTaskAction    )
  {
    if (   TDL::verifyAllocation ( theLocation, theTaskReference )
	&& TDL::verifySetAction  ( theLocation,
				   TCM_SetActualAction ( theTaskReference,
							 theTaskAction    ) )
	)
      return theTaskReference;
    else
      return NULL;
  }


	/* Returns TRUE for direct instances [SPAWN foo();],   *
	 * both Monitor and Monitor-child instances ["ACT-"],  *
	 * and SPAWN-with-WAIT macro instances ["-auto,wait"]. */
  static BOOLEAN checkForTaskInstanceOf ( const char * theActualTaskName,
					  const char * theNameToCheck );


  static void verifyNodeClassType ( const TCM_Task_Tree_Ref & theTaskReference,
				    TCM_NodeClassType_Enum    theNodeClassType,
				    const char *              theLocation )
  {
    if ( /*NOT*/! TDL::getIsReporting ( MISMATCH_TASK_TYPE ) )
      return;

    if ( theTaskReference . isNull() )
    {
      getLogStream()
	<< endl << endl << theLocation
	<< ":  WARNING:  Expected NodeClassType of "
	<< TCM_NodeClassTypeToString ( theNodeClassType )
	<< ".  Actually has a NULL value for theTaskReference."
	<< endl << endl << endl;
    }

    else if ( TCM_NodeClassType ( theTaskReference ) != theNodeClassType )
    {
      getLogStream()
	<< endl << endl << theLocation
	<< ":  WARNING:  Expected NodeClassType of "
	<< TCM_NodeClassTypeToString ( theNodeClassType )
	<< ".  Actually has NodeClassType of "
	<< TCM_NodeClassString ( theTaskReference )
	<< "." << endl << endl << endl;
    }
  }

  static void verifyNodeAllocationFunction (
		     const TCM_Task_Tree_Ref &       theTaskReference,
		     _TDL_AllocationFunction::TYPES  theAllocationFunctionType,
		     const char *                    theLocation )
  {
    if ( /*NOT*/! TDL::getIsReporting ( MISMATCH_ALLOCATION_TYPE ) )
      return;

    if ( theTaskReference . isNull() )
    {
      getLogStream()
	<< endl << endl << theLocation
	<< ":  WARNING:  Expected Allocation-Function of to be of type \""
	<< _TDL_AllocationFunction::getStringForType(theAllocationFunctionType)
	<< "\".  Actually has a NULL value for theTaskReference."
	<< endl << endl << endl;
      return;
    }

	/* ELSE... */
    switch ( _TDL_AllocationFunction::verifyAllocationFunctionType (
						  theTaskReference,
						  theAllocationFunctionType ) )
    {
      case _TDL_AllocationFunction::VERIFIED_FALSE:
	getLogStream()
	  << endl << endl << theLocation
	  << ":  WARNING:  Expected Allocation-Function of to be of type \""
	<< _TDL_AllocationFunction::getStringForType(theAllocationFunctionType)
	  << "\".  Actually had an Allocation-Function of type \""
	  << ((const char *)
	      ( _TDL_INVOKE_TCM_GET_ACTUAL_USER_DATA ( theTaskReference ) ))
	  << "\"." << endl << endl << endl;
	break;

      case _TDL_AllocationFunction::VERIFIED_TRUE:
	break;

      case _TDL_AllocationFunction::UNABLE_TO_VERIFY:
#ifndef IGNORE_UNABLE_TO_VERIFY_ALLOCATION_FUNCTION_MATCH
	getLogStream()
	  << endl << theLocation
	  << ": Warning:  Unable to verify expected Allocation-Function "
	  << "(of type \""
	<< _TDL_AllocationFunction::getStringForType(theAllocationFunctionType)
	  << "\").  Actually found an Allocation-Function (pointer value) of "
	  << ( _TDL_INVOKE_TCM_GET_ACTUAL_USER_DATA ( theTaskReference ) )
	  << ".  This does not necessarily indicate a problem.  "
	  << "In all likelihood, this was caused by use of the "
	  << "TCM_SetUserData/TCM_SetActualUserData functions.  Recompile TDL "
	  << "with -DIGNORE_UNABLE_TO_VERIFY_ALLOCATION_FUNCTION_MATCH to "
	  << "disable this warning." << endl << endl << endl;
#endif /* IGNORE_UNABLE_TO_VERIFY_ALLOCATION_FUNCTION_MATCH */
	break;
    }
  }


  static BOOLEAN getAbortOnActionTaskMismatch()
    { return TDL::abortOnActionTaskMismatch; }

  static void setAbortOnActionTaskMismatch ( BOOLEAN theValue = TRUE )
    { TDL::abortOnActionTaskMismatch = theValue; }

  static void verifyResumeMatchesAction (
				    const TCM_Task_Tree_Ref & theTaskRef,
				    STRING                    theActionName,
				    const char *              theLocation )
  {
    if ( /*NOT*/! TDL::getIsReporting ( MISMATCH_ACTION_TASK ) )
      return;

    if ( theTaskRef . isNull() )
    {
      getLogStream()
	<< endl << endl << theLocation
	<< ":  ERROR:  Expected to find an Action of type \""
	<< theActionName
	<< "\".  Instead, found a NULL value for theTaskRef."
	<< endl << endl << endl;
      if ( getAbortOnActionTaskMismatch() == TRUE )
	exit ( -1 );
    }

    else if ( TCM_GetActualAction(theTaskRef).isNull() )
    {
      getLogStream()
	<< endl << endl << theLocation
	<< ":  ERROR:  Expected to find an Action of type \""
	<< theActionName
	<< "\".  Instead, found a NULL value for "
	<< "TCM_GetActualAction(theTaskRef)."
	<< endl << endl << endl;
      if ( getAbortOnActionTaskMismatch() == TRUE )
	exit ( -1 );
    }

    else if ( StringEqual ( TCM_GetActualAction(theTaskRef)
			      .  operator*()
			      -> TCM_getActionName(),
			    theActionName )
	      == FALSE )
    {
      getLogStream()
	<< endl << endl << theLocation
	<< ":  ERROR:  Expected to find an Action of type \""
	<< theActionName
	<< "\".  Instead, found an Action of type \""
	<< TCM_GetActualAction(theTaskRef) . operator*() -> TCM_getActionName()
	<< "\".  The cause of this is a TDL_Resume_...() invocation "
	<< "who's leading TCM_Task_Tree_Ref argument corresponds to a TDL "
	<< "Task declaration other than the one that this TDL_Resume...() "
	<< "operation corresponds to.  (There is a 1-N mapping between TDL "
	<< "Goals/Commands/Monitors/ExceptionHandlers and TDL Resume Tasks.)  "
	<< "This is a really *REALLY* _BAD_ thing.  Due to internal casting "
	<< "operations, any task-arguments / persistent task arguments "
	<< "will be severely mangled, along with possible corruption of the "
	<< "internal virtual pointer table for the underlying _Action class."
	<< endl << endl << endl;
      if ( getAbortOnActionTaskMismatch() == TRUE )
	exit ( -1 );
    }
  }

private:
  TDL(){} /* Disable construction of this class */

}; /* class TDL */


#endif /* TDL_DEF_H */

