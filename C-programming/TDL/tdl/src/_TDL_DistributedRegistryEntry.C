/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

#include "_TDL_DistributedRegistryEntry.H"

	/* Class Methods... */

/*static*/
_TDL_Slist &
_TDL_DistributedRegistryEntry::getAllDistributedRegistriesList()
{
  static _TDL_Slist *  globalAllDistributedRegistriesList
#ifdef DONT_INITIALIZE_globalAllDistributedRegistriesList
  ;
#else
  = (_TDL_Slist *) NULL;
#endif

  if ( globalAllDistributedRegistriesList == (_TDL_Slist *) NULL )
  {
    globalAllDistributedRegistriesList
	  = new _TDL_Slist ( FALSE /* Do *NOT* delete registry entries
				    * when this list is deleted.       */ );
  }

  return * globalAllDistributedRegistriesList;
}




/*static*/ int4
_TDL_DistributedRegistryEntry::registerAllDistributedRegistryEntries()
{
  int4  totalRegistered = 0;

  for ( _TDL_Snode * snode
	  = _TDL_DistributedRegistryEntry::getAllDistributedRegistriesList()
	      . getFirstNode();
	snode != (_TDL_Snode *) NULL;
	snode  = snode -> getNextNode() )
  {
    _TDL_DistributedRegistryEntry  * distributedRegistryEntry
      = (_TDL_DistributedRegistryEntry *) snode;

	/* Skip ones we've already registered. */
    if ( distributedRegistryEntry -> getHasBeenRegisteredWithTcm() == TRUE )
      continue;

    _TDL_DistributedExceptionRegistryEntry * distributedExceptionRegistryEntry
      = dynamic_cast< _TDL_DistributedExceptionRegistryEntry
                      * >( distributedRegistryEntry );
    if (distributedExceptionRegistryEntry != NULL) {
      if ( TCM_RegisterDistributedException (
	        /* STRING taskName (nodeTypeName) */
	   distributedExceptionRegistryEntry -> getTaskName(),
		/* STRING taskDataFormat */
	   distributedExceptionRegistryEntry -> getDistributedFormatString(),
		/* TCM_Exception::Creator creator */
	   distributedExceptionRegistryEntry -> getExceptionCreator()
	   ) != TCM_Ok ) {
	TDL::getLogStream()
	  << "[_TDL_DistributedRegistryEntry:"
	  << "registerAllDistributedRegistryEntries]  Error:  "
	  << "TCM_RegisterDistributedException("
	  << distributedExceptionRegistryEntry -> getTaskName()
	  << ") Failure." << endl;
      }
      else
	{
	  distributedExceptionRegistryEntry -> setHasBeenRegisteredWithTcm();
	  totalRegistered ++;
	}
    } else {        
      if ( TCM_RegisterDistributedTask (
	        /* STRING taskName (nodeTypeName) */
	   distributedRegistryEntry -> getTaskName(),

		/* TASK_ALLOCATION_FN allocationFn */
  _TDL_DistributedRegistryEntry::genericDistributedRemoteTaskAlloctionFunction,

		/* void *allocationFnClientData */
	   (void *) distributedRegistryEntry,

		/* ACTION_CREATION_FN actionFn */
	   distributedRegistryEntry
	     -> getDistributedCreateRemoteActionFunction(),

		/* void *actionFnClientData */
	   (void *) NULL,

		/* STRING taskDataFormat */
	   distributedRegistryEntry -> getDistributedFormatString(),

		/* STRING theOverloadedTaskNameIndex */
	   distributedRegistryEntry -> getOverloadedTaskNameIndex()

	   ) != TCM_Ok )
	{
	  TDL::getLogStream()
	    << "[_TDL_DistributedRegistryEntry:"
	    << "registerAllDistributedRegistryEntries]  Error:  "
	    << "TCM_RegisterDistributedTask("
	    << distributedRegistryEntry -> getTaskName()
	    << ") Failure." << endl;
	}
      else
	{
	  distributedRegistryEntry -> setHasBeenRegisteredWithTcm();
	  totalRegistered ++;
	}
    }

  } /* FOR ( snode in getAllDistributedRegistriesList ) */


  return totalRegistered;

} /* static int4 registerAllDistributedRegistryEntries ( ... ) */




/*static*/ TCM_Task_Tree_Ref
_TDL_DistributedRegistryEntry::genericDistributedRemoteTaskAlloctionFunction ( 
						      void * theTcmClientData )
{
  _TDL_DistributedRegistryEntry  * distributedRegistryEntry
    = (_TDL_DistributedRegistryEntry *) theTcmClientData;

  if ( theTcmClientData == (void *) NULL )
  {
    TDL::getLogStream()
      << "[_TDL_DistributedRegistryEntry:"
      << "genericDistributedRemoteTaskAlloctionFunction]  "
      << "Irrecoverable Error:  Argument theTcmClientData is NULL."
      << endl;

	/* There's no soft-recovery capability available here... */
    exit ( -1 );
  }

  if (    distributedRegistryEntry -> getDistributedAllocationFunction()
       == (_TDL_DistributedAllocationFunctionType) NULL )
  {
    TDL::getLogStream()
      << "[_TDL_DistributedRegistryEntry:"
      << "genericDistributedRemoteTaskAlloctionFunction]  "
      << "Irrecoverable Error:  "
      << "theTcmClientData -> getDistributedAllocationFunction is NULL."
      << endl;

	/* There's no soft-recovery capability available here... */
    exit ( -1 );
  }


  return
    ( * ( distributedRegistryEntry -> getDistributedAllocationFunction() ) )
    (  STRING(NULL),
       STRING(NULL),
       TRUE /* Is distributed remote-side */
     );
}



/*virtual*/
_TDL_DistributedRegistryEntry::~_TDL_DistributedRegistryEntry()
{
	/* Sometimes objects get mis-used after they have been deleted.
	 * Lets detect that situation gracefully...
	 */
  taskName                      = "deleted-_TDL_DistributedRegistryEntry";
  distributedAllocationFunction = (_TDL_DistributedAllocationFunctionType)NULL;

  distributedCreateRemoteActionFunction
    = (_TDL_DistributedCreateRemoteActionFunctionType) NULL;

  if ( distributedFormatString != STRING(NULL) )
  {
    delete [] distributedFormatString;
    distributedFormatString     = STRING(NULL);
  }

  hasBeenRegisteredWithTcm      = TRUE; /* Don't register this thing. */
}




	/* Abstract iostreamBase-interface method */
/*virtual*/ ostream &
_TDL_DistributedRegistryEntry::printObject (
				  ostream    & theOstream,
				  const char * theIndentString /*= ""*/ ) const
{
  theOstream
    << theIndentString
    << "_TDL_DistributedRegistryEntry  "
    << (void *) this
    << endl

    << theIndentString
    << "  taskName...............................: "
    << getTaskName()
    << endl

    << theIndentString
    << "  distributedAllocationFunction..........: "
    << (void *) getDistributedAllocationFunction()
    << endl

    << theIndentString
    << "  distributedCreateRemoteActionFunction..: "
    << (void *) getDistributedCreateRemoteActionFunction()
    << endl

    << theIndentString
    << "  distributedFormatString................: "
    << getDistributedFormatString()
    << endl

    << theIndentString
    << "  hasBeenRegisteredWithTcm...............: "
    << getHasBeenRegisteredWithTcm()
    << endl

    << endl;

  return theOstream;
}

