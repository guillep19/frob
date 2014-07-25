/*
 * This class is utilized with respect to Distributed Tasks.
 * It allows task's to register themselves via the initialization a
 * static variable containing an instance of this class.
 * Later on, in "TDL_Initialize()", we exploit these registrations
 * to register these distributed-tasks appropriately with TCM.
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

#define _TDL_INTERNAL_
#include "tdl.H"
#include "_TDL_Snode.H"
#include "_TDL_Slist.H"

#ifndef _TDL_DISTRIBUTED_REGISTRY_ENTRY
#define _TDL_DISTRIBUTED_REGISTRY_ENTRY

typedef TCM_Task_Tree_Ref ( * _TDL_DistributedAllocationFunctionType ) 
				 ( STRING, STRING, BOOLEAN );

typedef TCM_Action_Ref ( * _TDL_DistributedCreateRemoteActionFunctionType ) 
				 ( TCM_Task_Tree_Ref &, void *, void * );

class _TDL_DistributedRegistryEntry : _TDL_Snode
{
	/* Class methods */
protected:
  static _TDL_Slist & getAllDistributedRegistriesList();

  static status_t addRegistryToMasterList (
		 _TDL_DistributedRegistryEntry * theDistributedRegistryEntry )
    { return _TDL_DistributedRegistryEntry::getAllDistributedRegistriesList()
	       . appendNode ( theDistributedRegistryEntry ); }


public:
  static int4              registerAllDistributedRegistryEntries();
  static TCM_Task_Tree_Ref genericDistributedRemoteTaskAlloctionFunction ( 
						     void * theTcmClientData );



	/* Instance variables */
protected:
  STRING                                 taskName;

  STRING                                 overloadedTaskNameIndex;

  _TDL_DistributedAllocationFunctionType distributedAllocationFunction;

  _TDL_DistributedCreateRemoteActionFunctionType 
					 distributedCreateRemoteActionFunction;

  STRING                                 distributedFormatString;

  BOOLEAN                                hasBeenRegisteredWithTcm;



	/* Instance methods */
public:
  _TDL_DistributedRegistryEntry (
    STRING                            theTaskName,
    STRING                            theOverloadedTaskNameIndex,
    _TDL_DistributedAllocationFunctionType
				      theDistributedAllocationFunction,
    _TDL_DistributedCreateRemoteActionFunctionType
				      theDistributedCreateRemoteActionFunction,
    STRING                            theDistributedFormatString )

    : _TDL_Snode                    (                                        ),
      taskName                      ( theTaskName                            ),
      overloadedTaskNameIndex       ( theOverloadedTaskNameIndex             ),
      distributedAllocationFunction ( theDistributedAllocationFunction       ),
      distributedCreateRemoteActionFunction (
				    theDistributedCreateRemoteActionFunction ),
      distributedFormatString       ( (    theDistributedFormatString
					== STRING(NULL) )
				     ? STRING(NULL)
				     : NewString (theDistributedFormatString)),
      hasBeenRegisteredWithTcm      ( FALSE                                  )
    {
      _TDL_DistributedRegistryEntry::addRegistryToMasterList ( this );
    }


  virtual ~_TDL_DistributedRegistryEntry();

  
  STRING getTaskName() const
    { return taskName; }

  STRING getOverloadedTaskNameIndex() const
    { return overloadedTaskNameIndex; }

  _TDL_DistributedAllocationFunctionType
  getDistributedAllocationFunction()     const
    { return distributedAllocationFunction; }

  _TDL_DistributedCreateRemoteActionFunctionType 
  getDistributedCreateRemoteActionFunction()     const
    { return distributedCreateRemoteActionFunction; }

  STRING getDistributedFormatString() const
    { return distributedFormatString; }

  BOOLEAN  getHasBeenRegisteredWithTcm() const
    { return hasBeenRegisteredWithTcm; }


	/* Abstract iostreamBase-interface method */
  virtual ostream & printObject ( ostream    & theOstream,
				  const char * theIndentString = "") const;

protected:
  void     setHasBeenRegisteredWithTcm()
    { hasBeenRegisteredWithTcm = TRUE; }


	/* These operations are disabled (prohibited) & unimplemented */
private:
  _TDL_DistributedRegistryEntry (
		       const _TDL_DistributedRegistryEntry & theObjectToCopy );
  _TDL_DistributedRegistryEntry & operator = (
		       const _TDL_DistributedRegistryEntry & theObjectToCopy );
};

class _TDL_DistributedExceptionRegistryEntry
  : public _TDL_DistributedRegistryEntry
{
public:
  _TDL_DistributedExceptionRegistryEntry ( STRING theExceptionName,
					   STRING theExceptionDataFormat,
					   TCM_Exception::Creator theCreator )
    : _TDL_DistributedRegistryEntry ( theExceptionName, "", NULL, NULL, 
				      theExceptionDataFormat ),
      creator(theCreator) { }

  TCM_Exception::Creator getExceptionCreator ( void ) const { return creator; }

protected:
  TCM_Exception::Creator creator;
};

#endif /* _TDL_DISTRIBUTED_REGISTRY_ENTRY */
