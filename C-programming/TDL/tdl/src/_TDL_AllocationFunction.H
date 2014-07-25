/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

#ifndef _TDL_AllocationFunction_H
#define _TDL_AllocationFunction_H

  /* _TDL_INTERNAL_ may have been set elsewhere, *
   * in which case we need to keep it set.       */
#ifndef _TDL_INTERNAL_
#define _TDL_INTERNAL_
#define _TDL_UNDEF_INTERNAL_ALLOCATION_FUNCTION
#endif

#include "tdl.H"
#include "_TDL_InterfaceToTCM_2.H"

#ifdef _TDL_UNDEF_INTERNAL_ALLOCATION_FUNCTION
#undef _TDL_UNDEF_INTERNAL_ALLOCATION_FUNCTION
#undef _TDL_INTERNAL_
#endif


/*
 * This class encapsulates our TDL Standard Allocation Function signatures...
 * (Along with standard tests in invocation proceedures.)
 */

typedef TCM_Task_Tree_Ref (* _TDL_AllocationFunctionType_Local)
			  ( STRING );
typedef TCM_Task_Tree_Ref (* _TDL_AllocationFunctionType_Either)
			  ( STRING, STRING, BOOLEAN );
typedef TCM_Task_Tree_Ref (* _TDL_AllocationFunctionType_Distributed)
			  ( STRING, STRING );


class _TDL_AllocationFunction
{
public:
  enum TYPES
  {
    LOCAL_NONDISTRIBUTED_ONLY   = 0, /* Original, non-distributed. */
    EITHER_LOCAL_OR_DISTRIBUTED = 1, /* Either non-distributed or distributed*/
    DISTRIBUTED_ONLY            = 2, /* ONLY distributed. */
    DELAYED_ALLOCATION          = 3, /* Used for Delayed Allocation */

    NULL_FUNCTION               = -1,
    UNKNOWN                     = -2  /* We are having a bad day... */
  };

  enum CONSTANTS
  {
    NUMBER_OF_TYPES_STRINGS = 7
  };

  static const char * TYPES_STRINGS [
			    _TDL_AllocationFunction::NUMBER_OF_TYPES_STRINGS ];

  static const char * getStringForType (_TDL_AllocationFunction::TYPES theType)
  {
    int i;
    switch ( theType )
    {
      case _TDL_AllocationFunction::LOCAL_NONDISTRIBUTED_ONLY:   i = 0; break;
      case _TDL_AllocationFunction::EITHER_LOCAL_OR_DISTRIBUTED: i = 1; break;
      case _TDL_AllocationFunction::DISTRIBUTED_ONLY:            i = 2; break;
      case _TDL_AllocationFunction::DELAYED_ALLOCATION:          i = 3; break;
      case _TDL_AllocationFunction::NULL_FUNCTION:               i = 4; break;
      case _TDL_AllocationFunction::UNKNOWN:                     i = 5; break;
      default:                                                   i = 6; break;
    }
    return TYPES_STRINGS [ i ];
  }

  static void cacheAllocationFunctionType (
		    const TCM_Task_Tree_Ref &       theTaskTreeRef,
		    _TDL_AllocationFunction::TYPES  theAllocationFunctionType )
  {
    _TDL_INVOKE_TCM_SET_ACTUAL_USER_DATA (
      theTaskTreeRef,
      getStringForType ( theAllocationFunctionType ) );
  }

  enum VERIFIY_RESULTS
  {
    VERIFIED_FALSE,
    VERIFIED_TRUE,
    UNABLE_TO_VERIFY
  };

  static VERIFIY_RESULTS verifyAllocationFunctionType (
		    const TCM_Task_Tree_Ref &       theTaskTreeRef,
		    _TDL_AllocationFunction::TYPES  theAllocationFunctionType )
  {
    if (    _TDL_INVOKE_TCM_GET_ACTUAL_USER_DATA ( theTaskTreeRef )
	 == getStringForType (  theAllocationFunctionType  ) )
      return _TDL_AllocationFunction::VERIFIED_TRUE;

    if ( _TDL_INVOKE_TCM_GET_ACTUAL_USER_DATA ( theTaskTreeRef ) == NULL )
      return _TDL_AllocationFunction::VERIFIED_FALSE;

    for ( int4 i=0;  i < _TDL_AllocationFunction::NUMBER_OF_TYPES_STRINGS; i++)
    {
      if (    _TDL_INVOKE_TCM_GET_ACTUAL_USER_DATA ( theTaskTreeRef )
	   == getStringForType ( _TDL_AllocationFunction::TYPES(i) ) )
	return _TDL_AllocationFunction::VERIFIED_FALSE;
    }

    return _TDL_AllocationFunction::UNABLE_TO_VERIFY;
  }

	/* This resolves the header-file interdependency between
	 * _TDL_AllocationFunction.H and tdldef.H.
	 */
  static ostream & getLogStream();



	/* Instance data: */
protected:
  union
  {
    _TDL_AllocationFunctionType_Local        local;
    _TDL_AllocationFunctionType_Either       either;
    _TDL_AllocationFunctionType_Distributed  distributed;
  } function;

  _TDL_AllocationFunction::TYPES  type;


	/* Instance methods: */
public:
  _TDL_AllocationFunction ()
    : type      ( _TDL_AllocationFunction::NULL_FUNCTION )
  { function.local =  _TDL_AllocationFunctionType_Local ( NULL ); }

  _TDL_AllocationFunction ( _TDL_AllocationFunctionType_Local  theLocal )
    : type      ( _TDL_AllocationFunction::LOCAL_NONDISTRIBUTED_ONLY )
  { function.local = theLocal; }

  _TDL_AllocationFunction ( _TDL_AllocationFunctionType_Either  theEither )
    : type      ( _TDL_AllocationFunction::EITHER_LOCAL_OR_DISTRIBUTED )
  { function.either = theEither; }

  _TDL_AllocationFunction (
		      _TDL_AllocationFunctionType_Distributed  theDistributed )
    : type      ( _TDL_AllocationFunction::DISTRIBUTED_ONLY )
  { function.distributed = theDistributed; }

  _TDL_AllocationFunction ( _TDL_AllocationFunction::TYPES theType )
    : type      ( theType )
  { function.local =  _TDL_AllocationFunctionType_Local ( NULL ); }

  _TDL_AllocationFunction (
			const _TDL_AllocationFunction & theAllocationFunction )
    : type      ( theAllocationFunction.type )
  { function = theAllocationFunction.function; }

  _TDL_AllocationFunction & operator = (
			const _TDL_AllocationFunction & theAllocationFunction )
  {
    function  = theAllocationFunction.function;
    type      = theAllocationFunction.type;
    return * this;
  }


  ~_TDL_AllocationFunction()
  {
    function.local = _TDL_AllocationFunctionType_Local ( NULL );
    type           = _TDL_AllocationFunction::UNKNOWN;
  }



  _TDL_AllocationFunction::TYPES getType() const { return type; }

  const char * getTypeString() const { return getStringForType ( getType() ); }



  void * getVoidFunctionPointer() const
  {
    switch ( getType() )
    {
      case _TDL_AllocationFunction::LOCAL_NONDISTRIBUTED_ONLY:
	return (void *) (function.local);

      case _TDL_AllocationFunction::EITHER_LOCAL_OR_DISTRIBUTED:
	return (void *) (function.either);

      case _TDL_AllocationFunction::DISTRIBUTED_ONLY:
	return (void *) (function.distributed);

      default:
	return (void *) NULL;
    }
  }


  _TDL_AllocationFunctionType_Local  getLocalFunction() const
  {
    if ( getType() == _TDL_AllocationFunction::LOCAL_NONDISTRIBUTED_ONLY )
      return function.local;
    else
    {
      _TDL_AllocationFunction::getLogStream()
	<< "[_TDL_AllocationFunction:getLocalFunction]  Error:  "
	<< "Tried to access LOCAL_NONDISTRIBUTED_ONLY function.  "
	<< "Actual function type is " << getTypeString() << endl;
      return _TDL_AllocationFunctionType_Local ( NULL );
    }
  }

  _TDL_AllocationFunctionType_Either  getEitherFunction() const
  {
    if ( getType() == _TDL_AllocationFunction::EITHER_LOCAL_OR_DISTRIBUTED )
      return function.either;
    else
    {
      _TDL_AllocationFunction::getLogStream()
	<< "[_TDL_AllocationFunction:getEitherFunction]  Error:  "
	<< "Tried to access EITHER_LOCAL_OR_DISTRIBUTED function.  "
	<< "Actual function type is " << getTypeString() << endl;
      return _TDL_AllocationFunctionType_Either ( NULL );
    }
  }


  _TDL_AllocationFunctionType_Distributed  getDistributedFunction() const
  {
    if ( getType() == _TDL_AllocationFunction::DISTRIBUTED_ONLY )
      return function.distributed;
    else
    {
      _TDL_AllocationFunction::getLogStream()
	<< "[_TDL_AllocationFunction:getDistributedFunction]  Error:  "
	<< "Tried to access DISTRIBUTED_ONLY function."
	<< "Actual function type is " << getTypeString() << endl;
      return _TDL_AllocationFunctionType_Distributed ( NULL );
    }
  }


  BOOLEAN isNull() const
  {
    switch ( getType() )
    {
      case _TDL_AllocationFunction::LOCAL_NONDISTRIBUTED_ONLY:
	return TO_BOOLEAN (    function.local
			    == _TDL_AllocationFunctionType_Local(NULL) );

      case _TDL_AllocationFunction::EITHER_LOCAL_OR_DISTRIBUTED:
	return TO_BOOLEAN (    function.either
			    == _TDL_AllocationFunctionType_Either(NULL) );

      case _TDL_AllocationFunction::DISTRIBUTED_ONLY:
	return TO_BOOLEAN (    function.distributed
			    == _TDL_AllocationFunctionType_Distributed(NULL) );

	/* All other cases... */
      default:
	return TRUE;
    }
  }


  BOOLEAN isDistributable() const
  {
    return TO_BOOLEAN (
	( getType() == _TDL_AllocationFunction::EITHER_LOCAL_OR_DISTRIBUTED )
     || ( getType() == _TDL_AllocationFunction::DISTRIBUTED_ONLY ) );
  }


  BOOLEAN isDelayedAllocation() const
  {
    return
      TO_BOOLEAN ( getType() == _TDL_AllocationFunction::DELAYED_ALLOCATION );
  }


  TCM_Task_Tree_Ref invokeFunction ( STRING theTaskName  = STRING(NULL),
				     STRING theAgentName = STRING(NULL) ) const
  {
    switch ( getType() )
    {
      case _TDL_AllocationFunction::LOCAL_NONDISTRIBUTED_ONLY:
	return function.local ( theTaskName );

      case _TDL_AllocationFunction::EITHER_LOCAL_OR_DISTRIBUTED:
	    /* Third argument is "theIsDistributedRemoteSide = FALSE" */
	return function.either ( theTaskName, theAgentName, FALSE );

      case _TDL_AllocationFunction::DISTRIBUTED_ONLY:
	if ( theAgentName !=  STRING(NULL) )
	  return function.distributed ( theTaskName, theAgentName );
	else
	{
	  _TDL_AllocationFunction::getLogStream()
	    << "[_TDL_AllocationFunction:invokeFunction]  Error:  "
	    << "Trying to invoke a DISTRIBUTED_ONLY function with a NULL "
	    << "agent-name.  Aborting.  Returning NULL TCM_Task_Tree_Ref."
	    << endl;

	  return TCM_Task_Tree_Ref(); /* Return a temporary NULL value */
	}

      case _TDL_AllocationFunction::DELAYED_ALLOCATION:
	return TCM_Task_Tree_Ref(); /* Return a temporary NULL value */

      default:
	_TDL_AllocationFunction::getLogStream()
	  << "[_TDL_AllocationFunction:invokeFunction]  Error:  "
	  << "Unknown function type (" << getTypeString()
	  << ").  Aborting.  Returning NULL TCM_Task_Tree_Ref." << endl;

	return TCM_Task_Tree_Ref(); /* Return a temporary NULL value */
    }
  }

};


#endif /* _TDL_AllocationFunction_H */
