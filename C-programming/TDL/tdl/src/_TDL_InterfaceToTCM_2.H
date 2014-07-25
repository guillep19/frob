/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

/*
 * Background:
 *
 *  tdldef.H needs functions/enums from _TDL_AllocationFunction.H
 *  _TDL_AllocationFunction.H invokes these functions.
 *  _TDL_InterfaceToTCM.H subclasses class from _TDL_BaseAction.H
 *  _TDL_BaseAction.H uses TDL::verify method from tdldef.H.
 *
 * Moving these functions from _TDL_InterfaceToTCM.H to here was considered
 * the best way of breaking this interdependency chain.
 */

#ifndef _TDL_INTERFACE_TO_TCM_2_H
#define _TDL_INTERFACE_TO_TCM_2_H

#include "tdl.H" /* (Which includes "tcm.h" properly). */

inline TCM_Return_Type
_TDL_INVOKE_TCM_SET_ACTUAL_USER_DATA ( const TCM_Task_Tree_Ref & theNode,
				       const void * theData               )
{
  return TCM_SetActualUserData ( theNode, theData );
}

inline const void *
_TDL_INVOKE_TCM_GET_ACTUAL_USER_DATA ( const TCM_Task_Tree_Ref & theNode )
{
  return TCM_GetActualUserData ( theNode );
}

#endif /* _TDL_INTERFACE_TO_TCM_2_H */
