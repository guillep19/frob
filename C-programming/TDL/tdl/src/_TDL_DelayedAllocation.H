/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

#ifndef _TDL_DelayedAllocation_H
#define _TDL_DelayedAllocation_H

#include "tdl.H"

/*
 * Delayed Allocation mechanism to for static compile-time type checking.
 */

class _TDL_DelayedAllocation_Base
{
protected:
  _TDL_SpawnStatementData * data;

	/* Alternative, used ONLY if data is NULL */
  const char *              taskName;
  const char *              taskAgentName;
  BOOLEAN                   isDelayedAllocation;


public:
  _TDL_DelayedAllocation_Base ( _TDL_HandleManager & theHandleManager )
    : data                ( (   theHandleManager . getCurrentTask()
			     != (_TDL_SpawnStatementTreeNode *) NULL )
			   ? & (theHandleManager .  getCurrentTask()
				                 -> getSpawnStatementData() )
			   : (_TDL_SpawnStatementData *) NULL                ),
      taskName            ( (const char *) NULL                              ),
      taskAgentName       ( (const char *) NULL                              ),
      isDelayedAllocation ( FALSE                                            )
    {}

  _TDL_DelayedAllocation_Base ( const char * theName  = (const char *)NULL,
				const char * theAgent = (const char *)NULL,
				BOOLEAN      theIsDelayed = FALSE )
    : data                ( (_TDL_SpawnStatementData *) NULL ),
      taskName            ( theName                          ),
      taskAgentName       ( theAgent                         ),
      isDelayedAllocation ( theIsDelayed                     )
    {}

  virtual ~_TDL_DelayedAllocation_Base() {};

  const char * getTaskName()      const
    { if ( data != (_TDL_SpawnStatementData *) NULL )
        return data -> getTaskName();
      else
	return taskName;
    }
  const char * getTaskAgentName() const
    { if ( data != (_TDL_SpawnStatementData *) NULL )
        return data -> getTaskAgentName();
      else
        return taskAgentName;
    }

  BOOLEAN getIsDelayedAllocation() const
    { if ( data != (_TDL_SpawnStatementData *) NULL )
        return data -> getSpawnStatement()
	            .  getAllocationFunction()
	            .  isDelayedAllocation();
      else
        return isDelayedAllocation;
    }

  virtual BOOLEAN isDataBad ( const char * theLocation ) const = 0;
};


/*
 * Classes for CreateAction to utilize.
 */
class _TDL_DelayedAllocation_Local : public _TDL_DelayedAllocation_Base
{
public:
  _TDL_DelayedAllocation_Local ( _TDL_HandleManager & theData )
    : _TDL_DelayedAllocation_Base ( theData )
    {}

  _TDL_DelayedAllocation_Local ( const char * theName  = (const char *)NULL,
				 const char * theAgent = (const char *)NULL,
				 BOOLEAN      theIsDelayed = FALSE )
    : _TDL_DelayedAllocation_Base ( theName, theAgent, theIsDelayed )
    {}

  virtual ~_TDL_DelayedAllocation_Local() {};

  virtual BOOLEAN isDataBad ( const char * theLocation ) const
    {
      if ( getTaskAgentName() != (const char *) NULL )
      {
	_TDL_AllocationFunction::getLogStream()
	  << "[_TDL_DelayedAllocation_Local - " << theLocation
	  << "]  WARNING:  On-Agent constraint (ON \"" << getTaskAgentName()
	  << "\") found for local-invocation-only Task.  "
	  << "This *should* *NOT* have been possible.  "
	  << "PLEASE REPORT THIS ERROR!!!  (Continuing as local Task.)"
	  << endl;
      }

	/* getTaskName() can be NULL in the default case. */

      return FALSE;
    }
};


class _TDL_DelayedAllocation_Either : public _TDL_DelayedAllocation_Base
{
public:
  _TDL_DelayedAllocation_Either ( _TDL_HandleManager & theData )
    : _TDL_DelayedAllocation_Base ( theData )
    {}

  _TDL_DelayedAllocation_Either ( const char * theName  = (const char *)NULL,
				  const char * theAgent = (const char *)NULL,
				  BOOLEAN      theIsDelayed = FALSE )
    : _TDL_DelayedAllocation_Base ( theName, theAgent, theIsDelayed )
    {}

  virtual ~_TDL_DelayedAllocation_Either() {};

  virtual BOOLEAN isDataBad ( const char * theLocation ) const
    {
	/* getTaskName() can be NULL in the default case. */
	/* getTaskAgentName() is NULL for the local case. */
      
      MARKUSED ( theLocation );
      return FALSE;
    }
};


class _TDL_DelayedAllocation_Distributed : public _TDL_DelayedAllocation_Base
{
public:
  _TDL_DelayedAllocation_Distributed ( _TDL_HandleManager & theData )
    : _TDL_DelayedAllocation_Base ( theData )
    {}

  _TDL_DelayedAllocation_Distributed (
				  const char * theName  = (const char *)NULL,
				  const char * theAgent = (const char *)NULL,
				  BOOLEAN      theIsDelayed = FALSE )
    : _TDL_DelayedAllocation_Base ( theName, theAgent, theIsDelayed )
    {}

  virtual ~_TDL_DelayedAllocation_Distributed() {};

  virtual BOOLEAN isDataBad ( const char * theLocation ) const
    {
      if ( getTaskAgentName() == (const char *) NULL )
      {
	_TDL_AllocationFunction::getLogStream()
	  << "[_TDL_DelayedAllocation_Distributed - " << theLocation
	  << "]  ERORR:  On-Agent-name is NULL for distribted-invocation-only "
	  << "Task.  This *should* *NOT* have been possible.  "
	  << "PLEASE REPORT THIS ERROR!!!  (Aborting Task.)"
	  << endl;
	return TRUE;
      }

	/* getTaskName() can be NULL in the default case. */
      
      return FALSE;
    }
};


/*
 * Classes for invoking CreateAction.
 */
class _TDL_InvokeDelayedAllocation_Local
  : public _TDL_DelayedAllocation_Local,
    public _TDL_DelayedAllocation_Either
{
public:
  _TDL_InvokeDelayedAllocation_Local ( _TDL_HandleManager & theData )
    :   _TDL_DelayedAllocation_Local  ( theData ),
        _TDL_DelayedAllocation_Either ( theData )
    {}

  _TDL_InvokeDelayedAllocation_Local ( const char * theName,
				       BOOLEAN      theIsDelayed )
    : _TDL_DelayedAllocation_Local (theName, (const char *)NULL, theIsDelayed),
      _TDL_DelayedAllocation_Either(theName, (const char *)NULL, theIsDelayed)
    {}

	/* BOOLEAN / TRUE / FALSE incompatibility */
  _TDL_InvokeDelayedAllocation_Local ( const char * theName,
				       int          theIsDelayed )
    : _TDL_DelayedAllocation_Local (theName, (const char *)NULL, theIsDelayed),
      _TDL_DelayedAllocation_Either(theName, (const char *)NULL, theIsDelayed)
    {}

  _TDL_InvokeDelayedAllocation_Local (
				  const char * theName  = (const char *)NULL,
				  const char * theAgent = (const char *)NULL,
				  BOOLEAN      theIsDelayed = FALSE )
    : _TDL_DelayedAllocation_Local  ( theName, theAgent, theIsDelayed ),
      _TDL_DelayedAllocation_Either ( theName, theAgent, theIsDelayed )
    {}

  virtual ~_TDL_InvokeDelayedAllocation_Local() {};
};

class _TDL_InvokeDelayedAllocation_Either
  : public _TDL_DelayedAllocation_Either
{
public:
  _TDL_InvokeDelayedAllocation_Either ( _TDL_HandleManager & theData )
    :   _TDL_DelayedAllocation_Either ( theData )
  {}
  _TDL_InvokeDelayedAllocation_Either (
				  const char * theName  = (const char *)NULL,
				  const char * theAgent = (const char *)NULL,
				  BOOLEAN      theIsDelayed = FALSE )
    : _TDL_DelayedAllocation_Either ( theName, theAgent, theIsDelayed )
    {}
  virtual ~_TDL_InvokeDelayedAllocation_Either() {};
};

class _TDL_InvokeDelayedAllocation_Distributed
  : public _TDL_DelayedAllocation_Distributed,
    public _TDL_DelayedAllocation_Either
{
public:
  _TDL_InvokeDelayedAllocation_Distributed ( _TDL_HandleManager & theData)
    :   _TDL_DelayedAllocation_Distributed ( theData ),
        _TDL_DelayedAllocation_Either      ( theData )
  {}
  _TDL_InvokeDelayedAllocation_Distributed (
				  const char * theName  = (const char *)NULL,
				  const char * theAgent = (const char *)NULL,
				  BOOLEAN      theIsDelayed = FALSE )
    : _TDL_DelayedAllocation_Distributed ( theName, theAgent, theIsDelayed ),
      _TDL_DelayedAllocation_Either      ( theName, theAgent, theIsDelayed )
    {}
  virtual ~_TDL_InvokeDelayedAllocation_Distributed() {};
};


#endif /* _TDL_DelayedAllocation_H */
