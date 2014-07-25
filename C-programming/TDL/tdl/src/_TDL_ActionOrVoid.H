/*
 * This wrapper class is being created to facilitate utilizing
 * the same automatically generated function (_TDL_CreateAction_...)
 * for both standard and distributed Tasks.  (Which simplifies certain
 * constrant-generation mechanisms.)
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

#ifndef _TDL_ACTION_OR_VOID
#define _TDL_ACTION_OR_VOID

  /* We need some standard defintions, which used to come from stdtypes.H
   * However, these have been supplanted by other definitions in tcm.h...
   * Note: _TDL_INTERNAL_ may have been set elsewhere, in which case
   *       we need to keep it set.
   */
#ifndef _TDL_INTERNAL_
#define _TDL_INTERNAL_
#define _TDL_UNDEF_INTERNAL_ACTION_OR_VOID
#endif

#include "tdl.H"

#ifdef _TDL_UNDEF_INTERNAL_ACTION_OR_VOID
#undef _TDL_UNDEF_INTERNAL_ACTION_OR_VOID
#undef _TDL_INTERNAL_
#endif


typedef TCM_Return_Type ( * _TDL_DistributedActionFunctionType )
			( const TCM_Task_Tree_Ref & theNode,
			  const void *              theArgs,
			  STRING                    theOverloadedTaskNameIndex
			);

class _TDL_ActionOrVoid
{
protected:
	/* Pointer to function, used to resolve distributed linking issues. */
  _TDL_DistributedActionFunctionType  distributedActionFunction;

	/* The Action/Void value. */
  BOOLEAN   isActionNotVoid;
  union
  {
    _TDL_Action  *  actionPointer;
    void         *  voidPointer;
  }         pointer;


	/* For the Distributed/Void case, we want to utilize the create-action
	 * function to determine the correct overloaded-task-name by index,
	 * overriding the value previously set in the allocate function.
	 * Note:  Defaults to USE_VALUE_FROM_ALLOCATE_DISTRIBUTED_NODE.
	 */
  STRING    overloadedTaskNameIndex;

	/* To resolve a core leak, we need to free the void pointer by default.
	 * However, it can't be done here without dealing with reference
	 * counting, so this is just a "suggestion" to
	 * _TDL_HandleManager::setAction().
	 */
  BOOLEAN   shouldDeleteVoidPointer;


public:
  _TDL_ActionOrVoid()
    : distributedActionFunction ( _TDL_DistributedActionFunctionType(NULL) ),
      isActionNotVoid           ( FALSE                                    ),
      overloadedTaskNameIndex   ( USE_VALUE_FROM_ALLOCATE_DISTRIBUTED_NODE ),
      shouldDeleteVoidPointer   ( TRUE                                     )
    { pointer.voidPointer = (void *)NULL; }

  _TDL_ActionOrVoid( _TDL_Action * theActionPointer )
    : distributedActionFunction ( _TDL_DistributedActionFunctionType(NULL) ),
      isActionNotVoid           ( TRUE                                     ),
      overloadedTaskNameIndex   ( USE_VALUE_FROM_ALLOCATE_DISTRIBUTED_NODE ),
      shouldDeleteVoidPointer   ( TRUE                                     )
    { pointer.actionPointer = theActionPointer; }

  _TDL_ActionOrVoid( const _TDL_ActionRef & theActionRef )
    : distributedActionFunction ( _TDL_DistributedActionFunctionType(NULL) ),
      isActionNotVoid           ( TRUE                                     ),
      overloadedTaskNameIndex   ( USE_VALUE_FROM_ALLOCATE_DISTRIBUTED_NODE ),
      shouldDeleteVoidPointer   ( TRUE                                     )
    { pointer.actionPointer = theActionRef.operator*(); }

	/* Locate this in .C file to resolve distributed linking issues. */
  _TDL_ActionOrVoid( void * theVoidPointer );

	/* Copy constructor */
  _TDL_ActionOrVoid ( const _TDL_ActionOrVoid & theActionOrVoid )
    : distributedActionFunction ( theActionOrVoid.distributedActionFunction ),
      isActionNotVoid           ( theActionOrVoid.getIsActionNotVoid()      ),
      overloadedTaskNameIndex   ( theActionOrVoid.overloadedTaskNameIndex   ),
      shouldDeleteVoidPointer   ( theActionOrVoid.shouldDeleteVoidPointer   )
    { pointer = theActionOrVoid.pointer; }

	/* Equals operators */
  _TDL_ActionOrVoid & operator= ( const _TDL_ActionOrVoid & theActionOrVoid )
    {
      distributedActionFunction = theActionOrVoid.distributedActionFunction;
      isActionNotVoid           = theActionOrVoid.isActionNotVoid;
      pointer                   = theActionOrVoid.pointer;
      overloadedTaskNameIndex   = theActionOrVoid.overloadedTaskNameIndex;
      shouldDeleteVoidPointer   = theActionOrVoid.shouldDeleteVoidPointer;
      return *this;
    }

	/* Warning:  NOT VIRTUAL! */
  ~_TDL_ActionOrVoid()
    {
      if ( getIsActionNotVoid() == TRUE )
	pointer.actionPointer = (_TDL_Action *)NULL;
      else
	pointer.voidPointer   = (void        *)NULL;
    }


  BOOLEAN getShouldDeleteVoidPointer() const{ return shouldDeleteVoidPointer; }
  void    setShouldDeleteVoidPointer(  BOOLEAN    theShouldDeleteVoidPointer )
		      { shouldDeleteVoidPointer = theShouldDeleteVoidPointer; }


  void setDistributedActionFunction (
	      _TDL_DistributedActionFunctionType theDistributedActionFunction )
    {
      distributedActionFunction = theDistributedActionFunction;
    }

  _TDL_DistributedActionFunctionType getDistributedActionFunction () const
    {
      return distributedActionFunction;
    }

  BOOLEAN hasDistributedActionFunction() const
    {
      return TO_BOOLEAN (    getDistributedActionFunction()
			  != _TDL_DistributedActionFunctionType(NULL) );
    }



  void setActionPointer ( _TDL_Action * theActionPointer )
    {
      isActionNotVoid       = TRUE;
      pointer.actionPointer = theActionPointer;
    }

  _TDL_ActionOrVoid & operator= ( _TDL_Action * theActionPointer )
    {
      setActionPointer ( theActionPointer );
      return *this;
    }

	/* Locate these in .C file to resolve distributed linking issues. */
  void setVoidPointer ( void * theVoidPointer );
  void setVoidPointer ( void * theVoidPointer,
			STRING theOverloadedTaskNameIndex );
  _TDL_ActionOrVoid & operator= ( void * theVoidPointer );


  BOOLEAN getIsActionNotVoid() const { return isActionNotVoid; }

  void *  getVoidPointer() const
    {
      if ( getIsActionNotVoid() == FALSE )
	return pointer.voidPointer;
      else
      {
	TDL::getLogStream()
	  << "[_TDL_ActionOrVoid:getVoidPointer]  Error: "
	  << "Accessing (void*) of _TDL_Action pointer.  Returning NULL!"
	  << endl;
	return (void *)NULL;
      }
    }

  _TDL_Action *  getTdlAction() const
    {
      if ( getIsActionNotVoid() == TRUE )
	return pointer.actionPointer;
      else
      {
	TDL::getLogStream()
	  << "[_TDL_ActionOrVoid:getVoidPointer]  Error: "
	  << "Accessing (_TDL_Action *) of void pointer.  Returning NULL!"
	  << endl;
	return (_TDL_Action *)NULL;
      }
    }


	/* The real magic:  Ability to auto-convert to necesssary types
	 * to establish backwards compatibility.
	 */
  operator _TDL_Action *  () const { return getTdlAction(); }
  operator _TDL_ActionRef () const { return getTdlAction(); }



  STRING getOverloadedTaskNameIndex() const { return overloadedTaskNameIndex; }
  void   setOverloadedTaskNameIndex(      STRING  theOverloadedTaskNameIndex )
		      { overloadedTaskNameIndex = theOverloadedTaskNameIndex; }

};

#endif /* _TDL_ACTION_OR_VOID */
