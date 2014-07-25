/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

#ifndef _TDL_INTERFACE_TO_TCM_H
#define _TDL_INTERFACE_TO_TCM_H

#include "tdl.H" /* (Which includes "tcm.h" properly). */
#include <time.h>
#include "_TDL_BaseAction.H"
#include "tdldef.H"
#include "_TDL_InterfaceToTCM_2.H"

  /*****************************************/
  /*******  TDL Support Functions... *******/
  /*****************************************/


/*
 * Note:  For historical reasons the following are considered equivalent:
 *          Handling    === Handling   (Abbreviated 'H')
 *          Planning    === Expansion  (Abbreviated 'P')
 *          Achievement === Execution  (Abbreviated 'A')
 *
 * Note:  _TDL_InterfaceToTCM.H provides part of the fundamental interface
 *        between TDL C++ code and the corresponding TCM library code.
 *        All interfacing (and invocations of) TCM code from the generated TDL
 *        code, or from the TDL library code, occurs either inside this file,
 *        or inside DataComponent.java's string constants.
 */


#define _TDL_NO_TIME                        (MSecs(0))
#define _TDL_NO_TIME_HOURS                  (u_int4(0))
#define _TDL_NO_TIME_MINUTES                (u_int4(0))
#define _TDL_NO_TIME_SECONDS                (u_int4(0))
#define _TDL_NO_TIME_FRACTIONS_OF_A_SECOND  (double(0))

#define _TDL_MAXIMUM_TRIGGERS		    MAXINT
#define _TDL_MAXIMUM_ACTIVATES		    MAXINT


/* Convert Seconds to Msecs, without going into floating-point UNLESS we are
 *  *ALREADY* using floating point. (In which case we need to round off.)
 * Also: Deal with negative values gracefully.
 * Note: Use (* & theSeconds) to force the compiler to put theSeconds onto the
 * stack, thereby avoiding a really nasty compiler bug that trashes memory.
 */
#define _TDL_SecondsToMSecs_BODY {return MSecs( * & theSeconds) * MSecs(1000);}
inline MSecs _TDL_SecondsToMSecs(u_int1 theSeconds) _TDL_SecondsToMSecs_BODY
inline MSecs _TDL_SecondsToMSecs(u_int2 theSeconds) _TDL_SecondsToMSecs_BODY
inline MSecs _TDL_SecondsToMSecs(u_int4 theSeconds) _TDL_SecondsToMSecs_BODY
inline MSecs _TDL_SecondsToMSecs(u_int8 theSeconds) _TDL_SecondsToMSecs_BODY
#undef _TDL_SecondsToMSecs_BODY

#define _TDL_SecondsToMSecs_BODY                                              \
{                                                                             \
  if ( (* & theSeconds) < 0 )                                                 \
  {                                                                           \
    TDL::getLogStream()                                                       \
      << "[_TDL_SecondsToMSecs]  Assuming negative value should be "          \
      << "positive. (" << int8(theSeconds) << ")" << endl;                    \
    theSeconds = - theSeconds;                                                \
  }                                                                           \
  return MSecs(theSeconds) * MSecs(1000);                                     \
}
inline MSecs _TDL_SecondsToMSecs(  int1 theSeconds) _TDL_SecondsToMSecs_BODY
inline MSecs _TDL_SecondsToMSecs(  int2 theSeconds) _TDL_SecondsToMSecs_BODY
inline MSecs _TDL_SecondsToMSecs(  int4 theSeconds) _TDL_SecondsToMSecs_BODY
inline MSecs _TDL_SecondsToMSecs(  int8 theSeconds) _TDL_SecondsToMSecs_BODY
#undef _TDL_SecondsToMSecs_BODY

#define _TDL_SecondsToMSecs_BODY                                              \
{                                                                             \
  if ( (* & theSeconds) < 0 )                                                 \
  {                                                                           \
    TDL::getLogStream()                                                       \
      << "[_TDL_SecondsToMSecs]  Assuming negative value should be "          \
      << "positive: (" << theSeconds << ")" << endl;                          \
   theSeconds = - theSeconds;                                                 \
  }                                                                           \
  return MSecs( ( theSeconds * 1000.0 ) + 0.5);                               \
}
inline MSecs _TDL_SecondsToMSecs(double theSeconds) _TDL_SecondsToMSecs_BODY
inline MSecs _TDL_SecondsToMSecs(float  theSeconds) _TDL_SecondsToMSecs_BODY
#undef _TDL_SecondsToMSecs_BODY


/*
 * Don't use seconds() from tcmBasics...
 * Conversion from int to double could trash low-bits (seconds) of time...
 */
inline MSecs
_TDL_translateSecondsToMSecs( time_t theSeconds, double theFractionsOfASecond )
{
	/* Note:  Use *&theFractionsOfASecond to force the compiler to put *
	 * theFractionsOfASecond onto the stack, thereby avoiding a really *
	 * *REALLY* nasty compiler bug that trashes memory.                *
	 *                                                                 *
	 * Note: Use _TDL_SecondsToMSecs() as it rounds off properly.      */
  return _TDL_SecondsToMSecs (     theSeconds            )
      +  _TDL_SecondsToMSecs ( * & theFractionsOfASecond );
}

inline MSecs
_TDL_translateTimeToMSecs ( u_int4 theHours,
			    u_int4 theMinutes,
			    u_int4 theSeconds,
			    double theFractionsOfASecond )
{
  if (   ( theHours              == _TDL_NO_TIME_HOURS                 )
      && ( theMinutes            == _TDL_NO_TIME_MINUTES               )
      && ( theSeconds            == _TDL_NO_TIME_SECONDS               )
      && ( theFractionsOfASecond == _TDL_NO_TIME_FRACTIONS_OF_A_SECOND ) )
  {
    return _TDL_NO_TIME;
  }
  else
  {
    return _TDL_translateSecondsToMSecs (   ( theHours   * 3600 )
					  + ( theMinutes *   60 )
					  + ( theSeconds        ),
					  theFractionsOfASecond    );
  }
}


inline MSecs
_TDL_translateAbsoluteTimeToMSecs ( u_int4  theHours,
				    u_int4  theMinutes,
				    u_int4  theSeconds,
				    double  theFractionsOfASecond,
				    BOOLEAN theIsLocalTimeZone = TRUE )
{
  time_t    utcTime, secondsSinceMidnight, timeAtMidnight, timeToUse;
  struct tm tmpTime;

  utcTime              = time ( (time_t *) NULL );
  secondsSinceMidnight = (theHours * 3600) + (theMinutes * 60) + theSeconds;

	/* local time needs to be adjusted for the timezone... */
  if ( theIsLocalTimeZone )
    tmpTime = * localtime ( & utcTime );
  else
    tmpTime = * gmtime    ( & utcTime );

  tmpTime.tm_sec  = 0;
  tmpTime.tm_min  = 0;
  tmpTime.tm_hour = 0;

	/* local time needs to be adjusted for the timezone... */
  if ( theIsLocalTimeZone )
    timeAtMidnight = _TDL_MAKE_TIME_LOCAL ( & tmpTime );
  else
    timeAtMidnight = _TDL_MAKE_TIME_GMT   ( & tmpTime );


  timeToUse = timeAtMidnight + secondsSinceMidnight;


	/* If we are more than 24 hours in the past, something is wrong... */
  if ( (timeToUse + 86400) < utcTime )
  {
    TDL::getLogStream()
      << "[_TDL_translateAbsoluteTimeToMSecs]  Warning:  "
      << "Adjusted target time is more than 24 hours in the past:" << endl
      << "     Current Time: " << ctime ( & utcTime   )            << endl;
    TDL::getLogStream()
      << "     Target  Time: " << ctime ( & timeToUse )            << endl;
  }


   /* Note:  The idea has been proposed that absolute time should start
    * at the current "now" time, and roll foward over the next 24 hours.
    * By default, this does not happen.  By default, absolute time starts
    * at midnight, and goes forward for exactly one day.  But this can be
    * altered (here) if users so desire...
    */
#ifdef _TDL_ABSOLUTE_TIME_GOES_FROM_NOW_UNTIL_24_HOURS_FROM_NOW
	/* Let the absolute time wrap over the day boundary */
  if ( timeToUse < utcTime )
    timeToUse += 86400; /* 86400 = 24 * 60 * 60 */
#endif


  
  if ( TDL::getIsReporting ( TDL::TDL_DEBUG_TIME ) )
  {
    TDL::getLogStream()
      << "[_TDL_translateAbsoluteTimeToMSecs]  TDL_DEBUG:  " << endl
      << "     Current Time: " << ctime ( & utcTime   )       << endl;
    TDL::getLogStream()
      << "     Target  Time: " << ctime ( & timeToUse )       << endl;
  }

  return _TDL_translateSecondsToMSecs ( timeToUse, theFractionsOfASecond );
}




inline void
_TDL_PRINT_INTERVAL ( ostream                 &  theOstream,
		      const TCM_Interval_Enum &  theInterval )
{
  switch ( theInterval )
  {
    case Handling_Interval:  theOstream << "HANDLING_INTERVAL";     break;
    case Planning_Interval:  theOstream << "EXPANSION_INTERVAL";    break;
    case Achieving_Interval: theOstream << "EXECUTION_INTERVAL";    break;
    case Unknown_Interval:   theOstream << "**UNKNOWN_INTERVAL**";  break;
    default:                 theOstream << "**INVALID_INTERVAL**";  break;
  }
}

inline void
_TDL_PRINT_STATE ( ostream              &  theOstream,
		   const TCM_Point_Enum &  theState   )
{
  switch ( theState )
  {
    case Start_Point:    theOstream << "ENABLED_STATE";      break;
    case End_Point:      theOstream << "COMPLETED_STATE";    break;
    case Unknown_Point:  theOstream << "**UNKNOWN_STATE**";  break;
    default:             theOstream << "**INVALID_STATE**";  break;
  }
}


inline TCM_Interval
_TDL_DO_TCM_getIntervalFor( const TCM_Task_Tree_Ref &  theReferenceNode,
			    const TCM_Interval_Enum &  theInterval,
			    const TCM_Point_Enum    &  theState = Start_Point )
{
  switch ( theInterval )
  {
    case Handling_Interval:  return TCM_HandlingOf  ( theReferenceNode );

    case Planning_Interval:  return TCM_PlanningOf  ( theReferenceNode );

    case Achieving_Interval: return TCM_AchievingOf ( theReferenceNode );

    case Unknown_Interval:
    default:
      if ( theState == Start_Point )
	return TCM_HandlingOf  ( theReferenceNode );
      else
	return TCM_AchievingOf ( theReferenceNode );
  }
}



inline TCM_Point
_TDL_DO_TCM_getPointFor ( const TCM_Interval   &  theInterval,
			  const TCM_Point_Enum &  theState )
{
  switch ( theState )
  {
    case Start_Point:
      return TCM_StartOf ( theInterval );

    case End_Point:
    default:
      return TCM_EndOf ( theInterval );
  }
}




inline TCM_Return_Type
_TDL_ADD_TCM_MONITOR_TASK_LEVEL_CONSTRAINT (
			  const TCM_Task_Tree_Ref &  theNodeToConstrain,
			  int                        theActivationConstraints )
{
  return TCM_SetMonitorActivationConstraints (
	   theNodeToConstrain,
	   (   TCM_GetMonitorMaximumTriggers ( theNodeToConstrain )
	     | theActivationConstraints ) );
}

inline TCM_Return_Type
_TDL_DO_TCM_EXPAND_FIRST_MONITOR_TASK_LEVEL (
				const TCM_Task_Tree_Ref &  theNodeToConstrain )
{
  return _TDL_ADD_TCM_MONITOR_TASK_LEVEL_CONSTRAINT ( theNodeToConstrain,
						      PLAN_FIRST );
}

inline TCM_Return_Type
_TDL_DO_TCM_DELAY_EXPANSION_MONITOR_TASK_LEVEL (
				const TCM_Task_Tree_Ref &  theNodeToConstrain )
{
  return _TDL_ADD_TCM_MONITOR_TASK_LEVEL_CONSTRAINT ( theNodeToConstrain,
						      DELAY_PLANNING );
}

inline TCM_Return_Type
_TDL_DO_TCM_SEQUENTIAL_EXPANSION_MONITOR_TASK_LEVEL (
				const TCM_Task_Tree_Ref &  theNodeToConstrain )
{
  return _TDL_ADD_TCM_MONITOR_TASK_LEVEL_CONSTRAINT ( theNodeToConstrain,
						      SEQ_PLANNING );
}

inline TCM_Return_Type
_TDL_DO_TCM_SEQUENTIAL_EXECUTION_MONITOR_TASK_LEVEL (
				const TCM_Task_Tree_Ref &  theNodeToConstrain )
{
  return _TDL_ADD_TCM_MONITOR_TASK_LEVEL_CONSTRAINT ( theNodeToConstrain,
						      SEQ_ACH );
}

inline TCM_Return_Type
_TDL_DO_TCM_SERIAL_MONITOR_TASK_LEVEL (
				const TCM_Task_Tree_Ref &  theNodeToConstrain )
{
  return _TDL_ADD_TCM_MONITOR_TASK_LEVEL_CONSTRAINT ( theNodeToConstrain,
						        DELAY_PLANNING
						      | SEQ_ACH );
}





inline TCM_Return_Type
_TDL_DO_TCM_EXPAND_FIRST ( const TCM_Task_Tree_Ref &  theNodeToConstrain )
{
   /* EP(x) <= SA(x) */
  /*constrainBefore ( TCM_EndOf   ( TCM_PlanningOf  ( theNodeToConstrain ) ),
   *		      TCM_StartOf ( TCM_AchievingOf ( theNodeToConstrain ) ) );
   */
  return TCM_AddConstraint ( theNodeToConstrain, PLAN_FIRST );
}



inline TCM_Return_Type
_TDL_DO_TCM_DELAY_EXPANSION ( const TCM_Task_Tree_Ref &  theNodeToConstrain )
{
   /* SP(x) == SA(x) */

   /* Ie:  Given: SP(x) <= SA(x),
    *      doing: SA(x) <= SP(x)
    *    implies: SP(x) <= SA(x) <= SP(x)
    *         or: SP(x) == SA(x)
    */

  /*constrainBefore ( TCM_StartOf ( TCM_AchievingOf ( theNodeToConstrain ) ),
   *                  TCM_StartOf ( TCM_PlanningOf  ( theNodeToConstrain ) ) );
   */
  return TCM_AddConstraint ( theNodeToConstrain, DELAY_PLANNING );
}



inline TCM_Return_Type
_TDL_DO_TCM_SEQUENTIAL_HANDLING( const TCM_Task_Tree_Ref & theReferenceNode,
				 const TCM_Task_Tree_Ref & theNodeToConstrain )
{
	/* Treat as NO-OP in the NULL-reference case. */
  if ( theReferenceNode . isNull() )
  {
    TDL::getLogStream()
      << "[_TDL_DO_TCM_SEQUENTIAL_HANDLING]  Warning:  "
      << "theReferenceNode is NULL.  Treating as a NO-OP." << endl;
    return TCM_Ok;
  }

   /* EH(x) <= SH(y) */
  /*constrainBefore ( TCM_EndOf   ( TCM_HandlingOf ( theReferenceNode   ) ),
   *		      TCM_StartOf ( TCM_HandlingOf ( theNodeToConstrain ) ) );
   */
  return
    TCM_DelayUntil ( TCM_StartOf ( TCM_HandlingOf ( theNodeToConstrain ) ),
		     TCM_EndOf   ( TCM_HandlingOf ( theReferenceNode   ) ) );
}



inline TCM_Return_Type
_TDL_DO_TCM_SEQUENTIAL_EXPANSION( const TCM_Task_Tree_Ref & theReferenceNode,
				  const TCM_Task_Tree_Ref & theNodeToConstrain)
{
	/* Treat as NO-OP in the NULL-reference case. */
  if ( theReferenceNode . isNull() )
  {
    TDL::getLogStream()
      << "[_TDL_DO_TCM_SEQUENTIAL_EXPANSION]  Warning:  "
      << "theReferenceNode is NULL.  Treating as a NO-OP." << endl;
    return TCM_Ok;
  }

   /* EP(x) <= SP(y) */
  /* return TCM_AddConstraint ( theNodeToConstrain, SEQ_PLANNING ); */
  return
    TCM_DelayUntil ( TCM_StartOf ( TCM_PlanningOf ( theNodeToConstrain ) ),
		     TCM_EndOf   ( TCM_PlanningOf ( theReferenceNode   ) ) );
}



inline TCM_Return_Type
_TDL_DO_TCM_SEQUENTIAL_EXECUTION( const TCM_Task_Tree_Ref & theReferenceNode,
				  const TCM_Task_Tree_Ref & theNodeToConstrain)
{
	/* Treat as NO-OP in the NULL-reference case. */
  if ( theReferenceNode . isNull() )
  {
    TDL::getLogStream()
      << "[_TDL_DO_TCM_SEQUENTIAL_EXECUTION]  Warning:  "
      << "theReferenceNode is NULL.  Treating as a NO-OP." << endl;
    return TCM_Ok;
  }

   /* EA(x) <= SA(y) */
  /* return TCM_AddConstraint ( theNodeToConstrain, SEQ_ACH ); */
  return
    TCM_DelayUntil ( TCM_StartOf ( TCM_AchievingOf ( theNodeToConstrain ) ),
		     TCM_EndOf   ( TCM_AchievingOf ( theReferenceNode   ) ) );
}



inline TCM_Return_Type
_TDL_DO_TCM_SERIAL ( const TCM_Task_Tree_Ref &  theReferenceNode,
		     const TCM_Task_Tree_Ref &  theNodeToConstrain )
{
	/* Treat as NO-OP in the NULL-reference case. */
  if ( theReferenceNode . isNull() )
  {
    TDL::getLogStream()
      << "[_TDL_DO_TCM_SERIAL]  Warning:  "
      << "theReferenceNode is NULL.  Treating as a NO-OP." << endl;
    return TCM_Ok;
  }

   /* EA(x) <= SP(y) */
  /*constrainBefore ( TCM_EndOf   ( TCM_AchievingOf ( theReferenceNode   ) ),
   *                  TCM_StartOf ( TCM_PlanningOf  ( theNodeToConstrain ) ) );
   */
  return TCM_Serialize ( theReferenceNode, theNodeToConstrain );
}



inline TCM_Return_Type
_TDL_DO_TCM_WAIT ( const TCM_Task_Tree_Ref &  theTaskTreeReference )
{
	/* Deal with NULL theTaskTreeReference */
  if ( theTaskTreeReference . isNull() )
  {
    TDL::getLogStream()
      << "[_TDL_DO_TCM_WAIT]  Warning:  "
      << "theTaskTreeReference is NULL.  Failing." << endl;
    return TCM_Error;
  }

   /* Do a blocking wait until theTaskTreeReference finishes... */
  return
    TCM_WaitUntil ( TCM_EndOf ( TCM_AchievingOf ( theTaskTreeReference ) ) );
}



inline TCM_Return_Type
_TDL_DO_TCM_DISABLE_UNTIL_EVENT (
			  const TCM_Task_Tree_Ref & theReferenceNode,
			  const TCM_Interval_Enum & theReferenceInterval,
			  const TCM_Point_Enum    & theReferenceState,
			  const TCM_Task_Tree_Ref & theNodeToConstrain,
			  const TCM_Interval_Enum & theNodeToConstrainInterval)

{
	/* Deal with NULL theNodeToConstrain */
  if ( theNodeToConstrain . isNull() )
  {
    TDL::getLogStream()
      << "[_TDL_DO_TCM_DISABLE_UNTIL_EVENT]  Warning:  "
      << "theNodeToConstrain is NULL.  Failing." << endl;
    return TCM_Error;
  }

	/* Treat as NO-OP in the NULL-reference case. */
  if ( theReferenceNode . isNull() )
  {
    TDL::getLogStream()
      << "[_TDL_DO_TCM_DISABLE_UNTIL_EVENT]  Warning:  "
      << "theReferenceNode is NULL.  Treating as a NO-OP." << endl;
    return TCM_Ok;
  }

    /* DISABLE [ this-node's-interval (Handling/Expansion/Execution) ]
     *   UNTIL reference-node [ reference-node's-interval (H/P/A) ]
     *         reference-node-state (enabled/active/completed)
     */

   /* referencePoint <= start-of ( nodeToConstrainInterval ) */
  return TCM_DelayUntil (

    TCM_StartOf ( _TDL_DO_TCM_getIntervalFor ( theNodeToConstrain,
					       theNodeToConstrainInterval,
					       Start_Point ) ),

    _TDL_DO_TCM_getPointFor( _TDL_DO_TCM_getIntervalFor( theReferenceNode,
							 theReferenceInterval,
							 theReferenceState   ),
			     theReferenceState ) );
}



inline TCM_Return_Type
_TDL_DO_TCM_DISABLE_UNTIL(const TCM_Task_Tree_Ref & theNodeToConstrain,
			  const TCM_Interval_Enum & theNodeToConstrainInterval,
			  MSecs                     theAbsoluteTimeInMSecs )
{
	/* Deal with NULL theNodeToConstrain */
  if ( theNodeToConstrain . isNull() )
  {
    TDL::getLogStream()
      << "[_TDL_DO_TCM_DISABLE_UNTIL]  Warning:  "
      << "theNodeToConstrain is NULL.  Failing." << endl;
    return TCM_Error;
  }

  return TCM_DelayUntil (

      TCM_StartOf ( _TDL_DO_TCM_getIntervalFor ( theNodeToConstrain, 
						 theNodeToConstrainInterval, 
						 Start_Point ) ),

      theAbsoluteTimeInMSecs
      );
}

inline TCM_Return_Type
_TDL_DO_TCM_DISABLE_UNTIL(const TCM_Task_Tree_Ref & theNodeToConstrain,
			  const TCM_Interval_Enum & theNodeToConstrainInterval,
			  u_int4                    theHours,
			  u_int4                    theMinutes,
			  u_int4                    theSeconds,
			  double                    theFractionsOfASecond,
			  BOOLEAN                   theIsLocalTimeZone = TRUE )
{
  return _TDL_DO_TCM_DISABLE_UNTIL (

      theNodeToConstrain,

      theNodeToConstrainInterval,

      _TDL_translateAbsoluteTimeToMSecs ( theHours,
					  theMinutes,
					  theSeconds,
					  theFractionsOfASecond,
					  theIsLocalTimeZone )
      );
}



inline TCM_Return_Type
_TDL_DO_TCM_DISABLE_FOR( const TCM_Task_Tree_Ref &  theNodeToConstrain,
			 const TCM_Interval_Enum &  theNodeToConstrainInterval,
			 MSecs                      theRelativeTimeInMSecs )
{
	/* Deal with NULL theNodeToConstrain */
  if ( theNodeToConstrain . isNull() )
  {
    TDL::getLogStream()
      << "[_TDL_DO_TCM_DISABLE_FOR]  Warning:  "
      << "theNodeToConstrain is NULL.  Failing." << endl;
    return TCM_Error;
  }

    /* DISABLE [ this-node's-interval (Handling/Expansion/Execution) ]
     *     FOR relative-time
     */
  return
    TCM_DelayFor (

      TCM_StartOf ( _TDL_DO_TCM_getIntervalFor ( theNodeToConstrain, 
						 theNodeToConstrainInterval, 
						 Start_Point ) ),

      theRelativeTimeInMSecs

      );
}

inline TCM_Return_Type
_TDL_DO_TCM_DISABLE_FOR( const TCM_Task_Tree_Ref &  theNodeToConstrain,
			 const TCM_Interval_Enum &  theNodeToConstrainInterval,
			 u_int4                     theHours,
			 u_int4                     theMinutes,
			 u_int4                     theSeconds,
			 double                     theFractionsOfASecond )
{
  return _TDL_DO_TCM_DISABLE_FOR (

      theNodeToConstrain,

      theNodeToConstrainInterval,

      _TDL_translateTimeToMSecs ( theHours, theMinutes, theSeconds,
				  theFractionsOfASecond )
      );
}



inline TCM_Return_Type
_TDL_DO_TCM_DISABLE_FOR( const TCM_Task_Tree_Ref &  theReferenceNode,
			 const TCM_Interval_Enum &  theReferenceInterval,
			 const TCM_Point_Enum    &  theReferenceState,
			 const TCM_Task_Tree_Ref &  theNodeToConstrain,
			 const TCM_Interval_Enum &  theNodeToConstrainInterval,
			 MSecs                      theRelativeTimeInMSecs )
{
	/* Deal with NULL theNodeToConstrain */
  if ( theNodeToConstrain . isNull() )
  {
    TDL::getLogStream()
      << "[_TDL_DO_TCM_DISABLE_FOR]  Warning:  "
      << "theNodeToConstrain is NULL.  Failing." << endl;
    return TCM_Error;
  }

	/* Deal with NULL theReferenceNode */
  if ( theReferenceNode . isNull() )
  {
    TDL::getLogStream()
      << "[_TDL_DO_TCM_DISABLE_FOR]  Warning:  "
      << "theReferenceNode is NULL.  Event component ignored." << endl;

    return _TDL_DO_TCM_DISABLE_FOR ( theNodeToConstrain,
				     theNodeToConstrainInterval,
				     theRelativeTimeInMSecs );
  }


    /* DISABLE [ this-node's-interval (Handling/Expansion/Execution) ]
     *     FOR relative-time
     *   AFTER reference-node [ reference-node's-interval (P/H/A) ]
     *         reference-node-state (enabled/active/completed)
     */

  return TCM_DelayForAfter (

    TCM_StartOf ( _TDL_DO_TCM_getIntervalFor ( theNodeToConstrain, 
					       theNodeToConstrainInterval, 
					       Start_Point ) ),

    theRelativeTimeInMSecs,

    _TDL_DO_TCM_getPointFor( _TDL_DO_TCM_getIntervalFor( theReferenceNode,
							 theReferenceInterval,
							 theReferenceState   ),
			     theReferenceState )
    );
}

inline TCM_Return_Type
_TDL_DO_TCM_DISABLE_FOR( const TCM_Task_Tree_Ref &  theReferenceNode,
			 const TCM_Interval_Enum &  theReferenceInterval,
			 const TCM_Point_Enum    &  theReferenceState,
			 const TCM_Task_Tree_Ref &  theNodeToConstrain,
			 const TCM_Interval_Enum &  theNodeToConstrainInterval,
			 u_int4                     theHours,
			 u_int4                     theMinutes,
			 u_int4                     theSeconds,
			 double                     theFractionsOfASecond )
{
  return _TDL_DO_TCM_DISABLE_FOR (

      theReferenceNode,
      theReferenceInterval,
      theReferenceState,
      theNodeToConstrain,
      theNodeToConstrainInterval,

      _TDL_translateTimeToMSecs ( theHours, theMinutes, theSeconds,
				  theFractionsOfASecond )
    );
}




inline TCM_Return_Type
_TDL_DO_TCM_TERMINATE_AT ( const TCM_Task_Tree_Ref & theNodeToConstrain,
			   MSecs                     theAbsoluteTimeInMSecs )
{
  return TCM_TerminateAt ( theNodeToConstrain,
			   theAbsoluteTimeInMSecs );
}

inline TCM_Return_Type
_TDL_DO_TCM_TERMINATE_AT ( const TCM_Task_Tree_Ref & theNodeToConstrain,
			   u_int4                    theHours,
			   u_int4                    theMinutes,
			   u_int4                    theSeconds,
			   double                    theFractionsOfASecond,
			   BOOLEAN                   theIsLocalTimeZone = TRUE)
{
  return _TDL_DO_TCM_TERMINATE_AT (

      theNodeToConstrain,

      _TDL_translateAbsoluteTimeToMSecs ( theHours,
					  theMinutes,
					  theSeconds,
					  theFractionsOfASecond,
					  theIsLocalTimeZone )
      );
}



inline TCM_Return_Type
_TDL_DO_TCM_TERMINATE_IN ( const TCM_Task_Tree_Ref &  theNodeToConstrain,
			   MSecs                      theRelativeTimeInMSecs )
{
    /* TERMINATE IN relative-time */

  return TCM_TerminateIn ( theNodeToConstrain,
			   theRelativeTimeInMSecs );
}

inline TCM_Return_Type
_TDL_DO_TCM_TERMINATE_IN ( const TCM_Task_Tree_Ref &  theNodeToConstrain,
			   u_int4                     theHours,
			   u_int4                     theMinutes,
			   u_int4                     theSeconds,
			   double                     theFractionsOfASecond )
{
  return _TDL_DO_TCM_TERMINATE_IN (

      theNodeToConstrain,

      _TDL_translateTimeToMSecs ( theHours, theMinutes, theSeconds,
				  theFractionsOfASecond )
      );
}



inline TCM_Return_Type
_TDL_DO_TCM_TERMINATE_IN ( const TCM_Task_Tree_Ref &  theReferenceNode,
			   const TCM_Interval_Enum &  theReferenceInterval,
			   const TCM_Point_Enum    &  theReferenceState,
			   const TCM_Task_Tree_Ref &  theNodeToConstrain,
			   MSecs                      theRelativeTimeInMSecs )
{
	/* Deal with NULL theReferenceNode */
  if ( theReferenceNode . isNull() )
  {
    TDL::getLogStream()
      << "[_TDL_DO_TCM_TERMINATE_IN]  Warning:  "
      << "theReferenceNode is NULL.  Event component ignored." << endl;

    return _TDL_DO_TCM_TERMINATE_IN ( theNodeToConstrain,
				      theRelativeTimeInMSecs );
  }

    /* TERMINATE IN relative-time
     *   AFTER reference-node [ reference-node's-interval (H/P/A) ]
     *         reference-node-state (enabled/active/completed)
     */

  return TCM_TerminateInAfter (

    theNodeToConstrain,
    theRelativeTimeInMSecs,

    _TDL_DO_TCM_getPointFor( _TDL_DO_TCM_getIntervalFor( theReferenceNode,
							 theReferenceInterval,
							 theReferenceState   ),
			     theReferenceState )
    );
}

inline TCM_Return_Type
_TDL_DO_TCM_TERMINATE_IN ( const TCM_Task_Tree_Ref &  theReferenceNode,
			   const TCM_Interval_Enum &  theReferenceInterval,
			   const TCM_Point_Enum    &  theReferenceState,
			   const TCM_Task_Tree_Ref &  theNodeToConstrain,
			   u_int4                     theHours,
			   u_int4                     theMinutes,
			   u_int4                     theSeconds,
			   double                     theFractionsOfASecond )
{
  return _TDL_DO_TCM_TERMINATE_IN (

    theReferenceNode,
    theReferenceInterval,
    theReferenceState,
    theNodeToConstrain,

    _TDL_translateTimeToMSecs ( theHours, theMinutes, theSeconds,
				theFractionsOfASecond )
    );
}


inline TCM_Return_Type
_TDL_DO_TCM_TERMINATE_AT_EVENT( const TCM_Task_Tree_Ref & theReferenceNode,
				const TCM_Interval_Enum & theReferenceInterval,
				const TCM_Point_Enum    & theReferenceState,
				const TCM_Task_Tree_Ref & theNodeToConstrain )
{
	/* Deal with NULL theReferenceNode */
  if ( theReferenceNode . isNull() )
  {
    TDL::getLogStream()
      << "[_TDL_DO_TCM_TERMINATE_AT_EVENT]  Warning:  "
      << "theReferenceNode is NULL.  Terminating in zero time." << endl;

    return _TDL_DO_TCM_TERMINATE_IN ( theNodeToConstrain, 0 );
  }


    /* TERMINATE AT reference-node [ reference-node's-interval (H/P/A) ]
     *              reference-state (Enabled/Active/Completed)
     */

  return
    TCM_TerminateAt (

      theNodeToConstrain,

      _TDL_DO_TCM_getPointFor(
			     _TDL_DO_TCM_getIntervalFor( theReferenceNode,
							 theReferenceInterval,
							 theReferenceState   ),
			     theReferenceState )
      );
}



inline TCM_Return_Type
_TDL_DO_TCM_TERMINATE_IMMEDIATE (const TCM_Task_Tree_Ref &  theNodeToConstrain)
{
    /* TERMINATE */
  return TCM_TerminateNode ( theNodeToConstrain );
}




inline TCM_Return_Type
_TDL_DO_TCM_ACTIVATE_AT_EVENT( const TCM_Task_Tree_Ref & theReferenceNode,
			       const TCM_Interval_Enum & theReferenceInterval,
			       const TCM_Point_Enum    & theReferenceState,
			       const TCM_Task_Tree_Ref & theMonitorToConstrain)
{
	/* Deal with NULL theMonitorToConstrain, in case of a recursive loop */
  if ( theMonitorToConstrain . isNull() )
  {
    TDL::getLogStream()
      << "[_TDL_DO_TCM_ACTIVATE_AT_EVENT]  Error:  "
      << "theMonitorToConstrain is NULL.  Failing." << endl;
    return TCM_Error;
  }

	/* Deal with NULL theReferenceNode */
  if ( theReferenceNode . isNull() )
  {
    TDL::getLogStream()
      << "[_TDL_DO_TCM_ACTIVATE_AT_EVENT]  Warning:  "
      << "theReferenceNode is NULL.  "
      << "Activating in zero time after handling starts." << endl;

    return _TDL_DO_TCM_ACTIVATE_AT_EVENT ( theMonitorToConstrain,
					   Handling_Interval,
					   Start_Point,
					   theMonitorToConstrain );
  }


    /* ACTIVATE AT reference-node [ reference-node's-interval (H/P/A) ]
     *              reference-state (Enabled/Active/Completed)
     */

  return
    TCM_ActivateAt (

      theMonitorToConstrain,

      _TDL_DO_TCM_getPointFor (
			     _TDL_DO_TCM_getIntervalFor( theReferenceNode,
							 theReferenceInterval,
							 theReferenceState   ),
			     theReferenceState )
      );
}



inline TCM_Return_Type
_TDL_DO_TCM_ACTIVATE_AT ( const TCM_Task_Tree_Ref & theMonitorToConstrain,
			  MSecs                     theAbsoluteTimeInMSecs )
{
  return TCM_ActivateAt ( theMonitorToConstrain,
			  theAbsoluteTimeInMSecs );
}

inline TCM_Return_Type
_TDL_DO_TCM_ACTIVATE_AT ( const TCM_Task_Tree_Ref & theMonitorToConstrain,
			  u_int4                    theHours,
			  u_int4                    theMinutes,
			  u_int4                    theSeconds,
			  double                    theFractionsOfASecond,
			  BOOLEAN                   theIsLocalTimeZone = TRUE)
{
  return _TDL_DO_TCM_ACTIVATE_AT (

      theMonitorToConstrain,

      _TDL_translateAbsoluteTimeToMSecs ( theHours,
					  theMinutes,
					  theSeconds,
					  theFractionsOfASecond,
					  theIsLocalTimeZone )
      );
}



inline TCM_Return_Type
_TDL_DO_TCM_ACTIVATE_IN ( const TCM_Task_Tree_Ref &  theMonitorToConstrain,
			  MSecs                      theRelativeTimeInMSecs )
{
    /* ACTIVATE IN relative-time */

  return TCM_ActivateIn ( theMonitorToConstrain,
			  theRelativeTimeInMSecs );
}

inline TCM_Return_Type
_TDL_DO_TCM_ACTIVATE_IN ( const TCM_Task_Tree_Ref &  theMonitorToConstrain,
			  u_int4                     theHours,
			  u_int4                     theMinutes,
			  u_int4                     theSeconds,
			  double                     theFractionsOfASecond )
{
  return _TDL_DO_TCM_ACTIVATE_IN (

      theMonitorToConstrain,

      _TDL_translateTimeToMSecs ( theHours, theMinutes, theSeconds,
				  theFractionsOfASecond )
      );
}



inline TCM_Return_Type
_TDL_DO_TCM_ACTIVATE_IN ( const TCM_Task_Tree_Ref &  theReferenceNode,
			  const TCM_Interval_Enum &  theReferenceInterval,
			  const TCM_Point_Enum    &  theReferenceState,
			  const TCM_Task_Tree_Ref &  theMonitorToConstrain,
			  MSecs                      theRelativeTimeInMSecs )
{
	/* Deal with NULL theReferenceNode */
  if ( theReferenceNode . isNull() )
  {
    TDL::getLogStream()
      << "[_TDL_DO_TCM_ACTIVATE_IN]  Warning:  "
      << "theReferenceNode is NULL.  Event component ignored." << endl;

    return _TDL_DO_TCM_ACTIVATE_IN ( theMonitorToConstrain,
				     theRelativeTimeInMSecs );
  }

    /* ACTIVATE IN relative-time
     *   AFTER reference-node [ reference-node's-interval (H/P/A) ]
     *         reference-node-state (enabled/active/completed)
     */

  return TCM_ActivateInAfter (

    theMonitorToConstrain,

    theRelativeTimeInMSecs,

    _TDL_DO_TCM_getPointFor( _TDL_DO_TCM_getIntervalFor( theReferenceNode,
							 theReferenceInterval,
							 theReferenceState   ),
			     theReferenceState )
    );
}

inline TCM_Return_Type
_TDL_DO_TCM_ACTIVATE_IN ( const TCM_Task_Tree_Ref &  theReferenceNode,
			  const TCM_Interval_Enum &  theReferenceInterval,
			  const TCM_Point_Enum    &  theReferenceState,
			  const TCM_Task_Tree_Ref &  theMonitorToConstrain,
			  u_int4                     theHours,
			  u_int4                     theMinutes,
			  u_int4                     theSeconds,
			  double                     theFractionsOfASecond )
{
  return _TDL_DO_TCM_ACTIVATE_IN (

    theReferenceNode,
    theReferenceInterval,
    theReferenceState,
    theMonitorToConstrain,

    _TDL_translateTimeToMSecs ( theHours, theMinutes, theSeconds,
				theFractionsOfASecond )
    );
}


inline TCM_Return_Type
_TDL_DO_TCM_ACTIVATE_IMMEDIATE (
			      const TCM_Task_Tree_Ref & theMonitorToConstrain,
			      const void              * theActivationData
                                                         = (const void *)NULL )
{
/*
 * Note:  TCM has a race condition if we activate before handling starts.
 *        So we can't do this yet:
 *  return TCM_Activate ( theMonitorToConstrain, theActivationData );
 *
 * So instead we will use this hack:
 */

  ((void)(&theActivationData));
  return _TDL_DO_TCM_ACTIVATE_AT_EVENT ( theMonitorToConstrain,
					 Handling_Interval,
					 Start_Point,
					 theMonitorToConstrain );
}




inline TCM_Return_Type
_TDL_DO_SET_MONITOR_MAXIMUM_ACTIVATIONS (
			      const TCM_Task_Tree_Ref & theMonitorToConstrain,
			      unsigned int              theMaximumActivates )
{
  return TCM_SetMonitorMaximumActivations ( theMonitorToConstrain,
					    theMaximumActivates   );
}

inline unsigned int
_TDL_DO_GET_MONITOR_MAXIMUM_ACTIVATIONS (
			      const TCM_Task_Tree_Ref & theMonitorReference )
{
  return TCM_GetMonitorMaximumActivations ( theMonitorReference );
}


inline TCM_Return_Type
_TDL_DO_SET_MONITOR_MAXIMUM_TRIGGERS (
			      const TCM_Task_Tree_Ref & theMonitorToConstrain,
			      unsigned int              theMaximumTriggers )
{
  return TCM_SetMonitorMaximumTriggers ( theMonitorToConstrain,
					 theMaximumTriggers    );
}

inline unsigned int
_TDL_DO_GET_MONITOR_MAXIMUM_TRIGGERS (
			      const TCM_Task_Tree_Ref & theMonitorReference )
{
  return TCM_GetMonitorMaximumTriggers ( theMonitorReference );
}


inline TCM_Return_Type
_TDL_DO_SET_MONITOR_ACTIVATION_CONSTRAINTS (
			   const TCM_Task_Tree_Ref & theMonitorToConstrain,
			   int                       theActivationConstraints )
{
  return TCM_SetMonitorActivationConstraints ( theMonitorToConstrain,
					       theActivationConstraints );
}

inline int
_TDL_DO_GET_MONITOR_ACTIVATION_CONSTRAINTS (
			      const TCM_Task_Tree_Ref & theMonitorReference )
{
  return TCM_GetMonitorActivationConstraints ( theMonitorReference );
}


inline TCM_Return_Type
_TDL_DO_SET_MONITOR_PERIOD (  const TCM_Task_Tree_Ref & theMonitorToConstrain,
			      MSecs                     thePeriod )
{
  return TCM_SetMonitorPeriod ( theMonitorToConstrain, thePeriod );
}

inline TCM_Return_Type
_TDL_DO_SET_MONITOR_PERIOD (  const TCM_Task_Tree_Ref & theMonitorToConstrain,
			      u_int4                    theHours,
			      u_int4                    theMinutes,
			      u_int4                    theSeconds,
			      double                    theFractionsOfASecond )
{
  return _TDL_DO_SET_MONITOR_PERIOD (
      theMonitorToConstrain,
      _TDL_translateTimeToMSecs ( theHours, theMinutes, theSeconds,
				  theFractionsOfASecond ) );
}

inline MSecs
_TDL_DO_GET_MONITOR_PERIOD (  const TCM_Task_Tree_Ref & theMonitorReference )
{
  return TCM_GetMonitorPeriod ( theMonitorReference );
}


inline TCM_Return_Type
_TDL_DO_SET_MONITOR_INITIAL_WAIT (
			      const TCM_Task_Tree_Ref & theMonitorToConstrain,
			      BOOLEAN                   theInitialWait )
{
  return TCM_SetMonitorInitialWait ( theMonitorToConstrain, theInitialWait );
}

inline BOOLEAN
_TDL_DO_GET_MONITOR_INITIAL_WAIT (
			      const TCM_Task_Tree_Ref & theMonitorReference )
{
  return TCM_GetMonitorInitialWait ( theMonitorReference );
}



inline TCM_Task_Tree_Ref
_TDL_DO_ALLOCATE_COMPLETE_MONITOR_NODE( STRING theNodeTypeName,
					STRING theInstanceName,
					int    theActivationConstraints )
{
  return TCM_AllocateCompleteMonitorNode ( theNodeTypeName, theInstanceName,
					   theActivationConstraints );
}




/*
 * Interface to TCM's _Action class.
 *
 * Inherits, through TCM's _Action class, the (public) Ref_Count class
 * and this (public) _Action::execution method:
 *      virtual void execute (const TCM_Task_Tree_Ref &node) //= 0
 *                                                    TCM_PURE_VIRTUAL_METHOD ;
 *
 * (Note:  Because Polling_Monitor_Action is a derived class of _Action,
 *  and we would like to be able to cast Polling_Monitor_Action to _TDL_Action,
 *  _TDL_Action is just being defined through a #define macro.)
 */
#define _TDL_Action _Action


/*
 * Interface to TCM's TCM_Action_Ref class.
 *
 * (Note:  Because of all the handle<> constructors and operator=(), which
 *  are not inherited, this is just being defined through a #define macro.)
 */
#define _TDL_ActionRef TCM_Action_Ref


/*
 * _TDL_BaseException needs to be used in here...
 * Therefore, we will define a base-class for _TDL_BaseException here
 * to prevent a circular reference...
 */
typedef TCM_Exception _TDL_BaseException_TCMInterfaceBaseClass;

/*
 * _TDL_BaseHandler needs to inherit from _TDL_Action, and provide a
 * _TDL_getHandledExceptionName() method which is used below.  Unfortunately,
 * this kludge is necessary to avoid the obvious circular dependency between
 * _TDL_Exceptions.H and this file.
 */
class _TDL_BaseHandler_TCMInterfaceBaseClass : public _TDL_BaseAction
{
public:
  _TDL_BaseHandler_TCMInterfaceBaseClass() {}
  /* virtual ~_TDL_BaseHandler_TCMInterfaceBaseClass(); */

  virtual STRING _TDL_getHandledExceptionName() const TCM_PURE_VIRTUAL_METHOD ;
  virtual u_int4 _TDL_getMaximumActivates()     const TCM_PURE_VIRTUAL_METHOD ;

/* Defined for each derived subclass:
 *  static BOOLEAN exceptionMatches ( STRING theString );
 */
};


inline TCM_Return_Type
_TDL_DO_ADD_EXCEPTION_HANDLER (
      const TCM_Task_Tree_Ref                & theNode,
      _TDL_BaseHandler_TCMInterfaceBaseClass * theExceptionHandler,
      int4                                     theIndex             )
{
  return TCM_AddExceptionHandler (
			theNode,
			theExceptionHandler -> _TDL_getHandledExceptionName(),
			theExceptionHandler,
			theExceptionHandler -> _TDL_getMaximumActivates(),
			theIndex );
}



inline TCM_Return_Type
_TDL_DO_TCM_ON_TERMINATION (
			  const TCM_Task_Tree_Ref & theNodeThatIsRunning,
			  const TCM_Task_Tree_Ref & theNodeToRunOnTermination )
{
  return TCM_OnTermination ( theNodeThatIsRunning, theNodeToRunOnTermination );
}



inline TCM_Return_Type
_TDL_DO_TCM_SUCCESS ( const TCM_Task_Tree_Ref & theEnclosingNode )
{
  return TCM_Success ( theEnclosingNode );
}


inline TCM_Return_Type
_TDL_DO_TCM_FAIL ( const TCM_Task_Tree_Ref                  & theEnclosingNode,
		   _TDL_BaseException_TCMInterfaceBaseClass * theException    )
{
  return TCM_Failure ( theEnclosingNode, theException );
}


inline TCM_Return_Type
_TDL_DO_TCM_BYPASS ( const TCM_Task_Tree_Ref & theEnclosingNode )
{
  return TCM_Bypass ( theEnclosingNode );
}



	/* We need a test to distinguish whether a TCM_Task_Tree_Ref is a *
	 * regular (goal/command/monitor/etc) or distributed-local node.  */
inline BOOLEAN
_TDL_DO_CHECK_IF_TASK_DISTRIBUTED ( const TCM_Task_Tree_Ref & theTaskRef )
{
  return TCM_IsDistributedNode ( theTaskRef );
}


inline TCM_Return_Type
_TDL_DO_SET_THREADED_TASK ( const TCM_Task_Tree_Ref &  theNodeToConstrain )
{
  return TCM_SetIsThreadedTask ( theNodeToConstrain, TRUE );
}

inline TCM_Return_Type
_TDL_DO_SET_ACTUAL_ACTION ( const TCM_Task_Tree_Ref & theNodeToSet, 
			    const TCM_Action_Ref    & theAction    )
{
  return TCM_SetActualAction ( theNodeToSet, theAction );
}

inline TCM_Return_Type
_TDL_DO_SET_PERSISTENT ( const TCM_Task_Tree_Ref & theNodeToSet, 
			 BOOLEAN                   theValue      )
{
  return TCM_SetPersistence ( theNodeToSet, theValue );
}

/* Located in tdldef-distributed.C to resolve distributed linking issues... */
extern TCM_Return_Type
_TDL_INVOKE_TCM_SET_DISTRIBUTED_ACTION (
			const TCM_Task_Tree_Ref & theNode,
			const void *              theArgs,
			STRING                    theOverloadedTaskNameIndex );

#endif /* _TDL_INTERFACE_TO_TCM_H */

