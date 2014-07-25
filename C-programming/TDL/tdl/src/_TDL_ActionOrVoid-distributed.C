/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

#include "_TDL_ActionOrVoid.H"


 /* Thesea are located here to resolve distributed linking issues. */

_TDL_ActionOrVoid::_TDL_ActionOrVoid ( void * theVoidPointer )
    : distributedActionFunction ( _TDL_INVOKE_TCM_SET_DISTRIBUTED_ACTION   ),
      isActionNotVoid           ( FALSE                                    ),
      overloadedTaskNameIndex   ( USE_VALUE_FROM_ALLOCATE_DISTRIBUTED_NODE ),
      shouldDeleteVoidPointer   ( TRUE                                     )
{
  pointer.voidPointer = theVoidPointer;
}


void
_TDL_ActionOrVoid::setVoidPointer ( void * theVoidPointer )
{
  distributedActionFunction = _TDL_INVOKE_TCM_SET_DISTRIBUTED_ACTION;
  isActionNotVoid           = FALSE;
  pointer.voidPointer       = theVoidPointer;
}


void
_TDL_ActionOrVoid::setVoidPointer ( void * theVoidPointer,
				    STRING theOverloadedTaskNameIndex )
{
  setVoidPointer             ( theVoidPointer             );
  setOverloadedTaskNameIndex ( theOverloadedTaskNameIndex );
}


_TDL_ActionOrVoid &
_TDL_ActionOrVoid::operator= ( void * theVoidPointer )
{
  distributedActionFunction = _TDL_INVOKE_TCM_SET_DISTRIBUTED_ACTION;
  setVoidPointer ( theVoidPointer );
  return *this;
}

