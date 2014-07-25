/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

#define _TDL_INTERNAL_
#include <tdl.H>
#include "_TDL_DistributedRegistryEntry.H"


/*
 * Support for: (distributed version):
 *  extern  _TDL_Initialize_Return_Class TDL_Initialize(...);
 */
static BOOLEAN _TDL_HAS_EnableDistributedCommunications_ALREADY_RUN = FALSE;


_TDL_Initialize_Return_Class
TDL_Initialize ( STRING         theAgent,
		 STRING         theCentralHost,
		 /*BOOLEAN*/int theHideTcmMessages,
		 /*BOOLEAN*/int theHideTdlMessages )
{
  _TDL_Initialize_Return_Class returnValue
    = TDL_Initialize ( theHideTcmMessages, theHideTdlMessages );

  if ( _TDL_HAS_EnableDistributedCommunications_ALREADY_RUN == FALSE )
  {
    if (   ( theAgent    == (const char *) NULL )
	|| ( theAgent[0] == (char)         NULL ) )
    {
      TDL::getLogStream()
	<< "[TDL_Initialize]  Error:  "
	<< "theAgent was NULL.  This is NOT permitted." << endl;
    }
    else
    {
      TCM_Return_Type result
	= TCM_EnableDistributedComm ( theAgent, theCentralHost );

      if ( result != TCM_Ok )
      {
	TDL::getLogStream()
	  << "[TDL_Initialize]  Error:  " << "TCM_EnableDistributedComm ( \""
	  << theAgent << "\", \"" << theCentralHost
	  << "\" ) failed with a value of " << int4(result) << "." << endl;
      }
      else
      {
	_TDL_HAS_EnableDistributedCommunications_ALREADY_RUN = TRUE;

	if ( TDL::getIsReporting ( TDL::VERBOSE ) )
	{
	  TDL::getLogStream()
	    << "TCM_EnableDistributedComm ( \"" << theAgent << "\", \""
	    << theCentralHost << "\" )  SUCCEEDED." << endl;
	}
      }
    } /* IF ( theAgent is NULL ) ... ELSE ... */
  } /* if ( _TDL_HAS_EnableDistributedCommunications_ALREADY_RUN == FALSE ) */

  else
  {
    if ( TDL::getIsReporting ( TDL::VERBOSE ) )
    {
      TDL::getLogStream()
	<< "[TDL_Initialize]  Warning:  "
	<< "Unable to re-invoke TCM_EnableDistributedComm with ( \""
	<< theAgent << "\", \"" << theCentralHost << "\" )." << endl;
    }
  }


  int4  numberOfDistributedEntriesRegistered
    = _TDL_DistributedRegistryEntry::registerAllDistributedRegistryEntries();

  if ( TDL::getIsReporting ( TDL::VERBOSE ) )
  {
    TDL::getLogStream()
      << "Number of Distributed Tasks Registered:  "
      << numberOfDistributedEntriesRegistered << endl;
  }


  return returnValue;
}



_TDL_Initialize_Return_Class
_TDL_Initialize_Return_Class::connectAgent (
				     STRING theAgent,
				     STRING theCentralHost /* = NULL */ )
{
  if ( theAgent == STRING(NULL) )
  {
    TDL::getLogStream()
      << "[_TDL_Initialize_Return_Class:connectAgent]  Error:  "
      << "theAgent was NULL.  This is NOT permitted." << endl;
  }
  else
  {
    TCM_Return_Type result
      = TCM_ConnectDistributedAgent ( theAgent, theCentralHost );

    if ( result != TCM_Ok )
    {
      TDL::getLogStream()
	<< "[_TDL_Initialize_Return_Class:connectAgent]  Error:  "
	<< "TCM_ConnectDistributedAgent ( \"" << theAgent << "\", \""
	<< theCentralHost << "\" ) failed with a value of " << int4(result)
	<< "." << endl;
    }
    else
    {
      if ( TDL::getIsReporting ( TDL::VERBOSE ) )
      {
	TDL::getLogStream()
	  << "TCM_ConnectDistributedAgent ( \"" << theAgent << "\", \""
	  << theCentralHost << "\" )  SUCCEEDED." << endl;
      }
    }
  }

  return (* this);
}



/*
 * Located here to resolve distributed linking issues...
 */
TCM_Return_Type
_TDL_INVOKE_TCM_SET_DISTRIBUTED_ACTION (
			const TCM_Task_Tree_Ref & theNode,
			const void *              theArgs,
			STRING                    theOverloadedTaskNameIndex )
{
  return TCM_SetDistributedAction ( theNode,
				    theArgs,
				    theOverloadedTaskNameIndex );
}

