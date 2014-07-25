/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

#ifndef _TDL_FUNCTIONS_2_H
#define _TDL_FUNCTIONS_2_H

#define _TDL_INTERNAL_
#include "tdl.H"
#include "_TDL_Constraints.H"


inline TCM_Return_Type
_TDL_EXPAND_FIRST ( _TDL_SpawnStatementTreeNode & theNodeToConstrain )
{
  _TDL_ExpandFirst    constraint;

  return constraint . applyConstraints ( theNodeToConstrain );
}


inline TCM_Return_Type
_TDL_DELAY_EXPANSION ( _TDL_SpawnStatementTreeNode & theNodeToConstrain )
{
  _TDL_DelayExpansion  constraint;

  return constraint . applyConstraints ( theNodeToConstrain );
}


inline TCM_Return_Type
_TDL_SEQUENTIAL_HANDLING ( _TDL_TreeNode               & theReferenceTreeNode,
			   _TDL_SpawnStatementTreeNode & theNodeToConstrain   )
{
  _TDL_SequentialHandling  constraint ( theReferenceTreeNode );

  return constraint . applyConstraints ( theNodeToConstrain );
}


inline TCM_Return_Type
_TDL_SEQUENTIAL_EXPANSION ( _TDL_TreeNode               & theReferenceTreeNode,
			    _TDL_SpawnStatementTreeNode & theNodeToConstrain  )
{
  _TDL_SequentialExpansion  constraint ( theReferenceTreeNode );

  return constraint . applyConstraints ( theNodeToConstrain );
}


inline TCM_Return_Type
_TDL_SEQUENTIAL_EXECUTION ( _TDL_TreeNode               & theReferenceTreeNode,
			    _TDL_SpawnStatementTreeNode & theNodeToConstrain  )
{
  _TDL_SequentialExecution  constraint ( theReferenceTreeNode );

  return constraint . applyConstraints ( theNodeToConstrain );
}


inline TCM_Return_Type
_TDL_SERIAL ( _TDL_TreeNode               & theReferenceTreeNode,
	      _TDL_SpawnStatementTreeNode & theNodeToConstrain   )
{
  _TDL_Serial  constraint ( theReferenceTreeNode );

  return constraint . applyConstraints ( theNodeToConstrain );
}


inline TCM_Return_Type
_TDL_PARALLEL ( _TDL_SpawnStatementTreeNode & theNodeToConstrain )
{
  _TDL_Parallel  constraint;

  return constraint . applyConstraints ( theNodeToConstrain );
}


inline TCM_Return_Type
_TDL_WAIT ( _TDL_SpawnStatementTreeNode & theNodeToConstrain )
{
  _TDL_Wait  constraint;

  return constraint . applyConstraints ( theNodeToConstrain );
}


inline TCM_Return_Type
_TDL_DISABLE_UNTIL_EVENT (
		     _TDL_TreeNode               & theReferenceTreeNode,
		     const TCM_Interval_Enum     & theReferenceInterval,
		     const TCM_Point_Enum        & theReferenceState,
		     _TDL_SpawnStatementTreeNode & theNodeToConstrain,
		     const TCM_Interval_Enum     & theNodeToConstrainInterval )
{
  _TDL_DisableUntilEvent  constraint ( theReferenceTreeNode,
				       theReferenceInterval,
				       theReferenceState,
				       theNodeToConstrainInterval );

  return constraint . applyConstraints ( theNodeToConstrain );
}


inline TCM_Return_Type
_TDL_DISABLE_UNTIL ( _TDL_SpawnStatementTreeNode & theNodeToConstrain,
		     const TCM_Interval_Enum     & theNodeToConstrainInterval,
		     u_int4                        theHours,
		     u_int4                        theMinutes,
		     u_int4                        theSeconds,
		     double                        theFractionsOfASecond )
{
  _TDL_DisableUntilTime  constraint ( theNodeToConstrainInterval,
				      theHours,
				      theMinutes,
				      theSeconds,
				      theFractionsOfASecond );

  return constraint . applyConstraints ( theNodeToConstrain );
}


inline TCM_Return_Type
_TDL_DISABLE_FOR ( _TDL_SpawnStatementTreeNode & theNodeToConstrain,
		   const TCM_Interval_Enum     & theNodeToConstrainInterval,
		   u_int4                        theHours,
		   u_int4                        theMinutes,
		   u_int4                        theSeconds,
		   double                        theFractionsOfASecond )
{
  _TDL_DisableForTime  constraint ( theNodeToConstrainInterval,
				    theHours,
				    theMinutes,
				    theSeconds,
				    theFractionsOfASecond );

  return constraint . applyConstraints ( theNodeToConstrain );
}


inline TCM_Return_Type
_TDL_DISABLE_FOR ( _TDL_TreeNode               & theReferenceTreeNode,
		   const TCM_Interval_Enum     & theReferenceInterval,
		   const TCM_Point_Enum        & theReferenceState,
		   _TDL_SpawnStatementTreeNode & theNodeToConstrain,
		   const TCM_Interval_Enum     & theNodeToConstrainInterval,
		   u_int4                        theHours,
		   u_int4                        theMinutes,
		   u_int4                        theSeconds,
		   double                        theFractionsOfASecond )
{
  _TDL_DisableForTime  constraint ( theReferenceTreeNode,
				    theReferenceInterval,
				    theReferenceState,
				    theNodeToConstrainInterval,
				    theHours,
				    theMinutes,
				    theSeconds,
				    theFractionsOfASecond );

  return constraint . applyConstraints ( theNodeToConstrain );
}


inline TCM_Return_Type
_TDL_TERMINATE_AT_EVENT ( _TDL_TreeNode               & theReferenceTreeNode,
			  const TCM_Interval_Enum     & theReferenceInterval,
			  const TCM_Point_Enum        & theReferenceState,
			  _TDL_SpawnStatementTreeNode & theNodeToConstrain   )
{
  _TDL_TerminateAtEvent  constraint ( theReferenceTreeNode,
				      theReferenceInterval,
				      theReferenceState );

  return constraint . applyConstraints ( theNodeToConstrain );
}


inline TCM_Return_Type
_TDL_TERMINATE_AT ( _TDL_SpawnStatementTreeNode & theNodeToConstrain,
		    u_int4                        theHours,
		    u_int4                        theMinutes,
		    u_int4                        theSeconds,
		    double                        theFractionsOfASecond )
{
  _TDL_TerminateAtTime  constraint ( theHours,
				     theMinutes,
				     theSeconds,
				     theFractionsOfASecond );

  return constraint . applyConstraints ( theNodeToConstrain );
}


inline TCM_Return_Type
_TDL_TERMINATE_IN ( _TDL_SpawnStatementTreeNode & theNodeToConstrain,
		    u_int4                        theHours,
		    u_int4                        theMinutes,
		    u_int4                        theSeconds,
		    double                        theFractionsOfASecond )
{
  _TDL_TerminateInTime  constraint ( theHours,
				     theMinutes,
				     theSeconds,
				     theFractionsOfASecond );

  return constraint . applyConstraints ( theNodeToConstrain );
}


inline TCM_Return_Type
_TDL_TERMINATE_IN ( _TDL_TreeNode               & theReferenceTreeNode,
		    const TCM_Interval_Enum     & theReferenceInterval,
		    const TCM_Point_Enum        & theReferenceState,
		    _TDL_SpawnStatementTreeNode & theNodeToConstrain,
		    u_int4                        theHours,
		    u_int4                        theMinutes,
		    u_int4                        theSeconds,
		    double                        theFractionsOfASecond )
{
  _TDL_TerminateInTime  constraint ( theReferenceTreeNode,
				     theReferenceInterval,
				     theReferenceState,
				     theHours,
				     theMinutes,
				     theSeconds,
				     theFractionsOfASecond );

  return constraint . applyConstraints ( theNodeToConstrain );
}



inline TCM_Return_Type
_TDL_ACTIVATE_AT_EVENT ( _TDL_TreeNode               & theReferenceTreeNode,
			 const TCM_Interval_Enum     & theReferenceInterval,
			 const TCM_Point_Enum        & theReferenceState,
			 _TDL_SpawnStatementTreeNode & theNodeToConstrain   )
{
  _TDL_ActivateAtEvent  constraint ( theReferenceTreeNode,
				     theReferenceInterval,
				     theReferenceState );

  return constraint . applyConstraints ( theNodeToConstrain );
}


inline TCM_Return_Type
_TDL_ACTIVATE_AT ( _TDL_SpawnStatementTreeNode & theNodeToConstrain,
		   u_int4                        theHours,
		   u_int4                        theMinutes,
		   u_int4                        theSeconds,
		   double                        theFractionsOfASecond )
{
  _TDL_ActivateAtTime  constraint ( theHours,
				    theMinutes,
				    theSeconds,
				    theFractionsOfASecond );

  return constraint . applyConstraints ( theNodeToConstrain );
}


inline TCM_Return_Type
_TDL_ACTIVATE_IN ( _TDL_SpawnStatementTreeNode & theNodeToConstrain,
		   u_int4                        theHours,
		   u_int4                        theMinutes,
		   u_int4                        theSeconds,
		   double                        theFractionsOfASecond )
{
  _TDL_ActivateInTime  constraint ( theHours,
				    theMinutes,
				    theSeconds,
				    theFractionsOfASecond );

  return constraint . applyConstraints ( theNodeToConstrain );
}


inline TCM_Return_Type
_TDL_ACTIVATE_IN ( _TDL_TreeNode               & theReferenceTreeNode,
		   const TCM_Interval_Enum     & theReferenceInterval,
		   const TCM_Point_Enum        & theReferenceState,
		   _TDL_SpawnStatementTreeNode & theNodeToConstrain,
		   u_int4                        theHours,
		   u_int4                        theMinutes,
		   u_int4                        theSeconds,
		   double                        theFractionsOfASecond )
{
  _TDL_ActivateInTime  constraint ( theReferenceTreeNode,
				    theReferenceInterval,
				    theReferenceState,
				    theHours,
				    theMinutes,
				    theSeconds,
				    theFractionsOfASecond );

  return constraint . applyConstraints ( theNodeToConstrain );
}



inline TCM_Return_Type
_TDL_Add_Exception_Handler ( _TDL_SpawnStatementTreeNode & theNodeToConstrain,
			     _TDL_BaseHandler            * theExceptionHandler,
			     int4                          theIndex           )
{
  _TDL_AddExceptionHandler constraint ( theExceptionHandler, theIndex );

  return constraint . applyConstraints ( theNodeToConstrain );
}



#endif /* _TDL_FUNCTIONS_2_H */

