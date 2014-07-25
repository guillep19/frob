/**************************************************************************
 * 
 * PROJECT: Carnegie Mellon Planetary Rover Project
 *
 * FILE: action.cc
 *
 * ABSTRACT: Defines a hierarchy of actions to take when task tree nodes
 *           are "executed".  Includes classes for functional callback,
 *           message passing.  Future classes may include task spawning 
 *           and process forking.
 *
 * EXPORTS:
 *
 * $Revision: 1.8 $
 * $Date: 2009/01/15 17:00:04 $
 * $Author: reids $
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tcm/LICENSE.TXT)
 *
 * REVISION HISTORY:
 *
 * $Log: action.cc,v $
 * Revision 1.8  2009/01/15 17:00:04  reids
 * Released under simplified BSD open source license
 *
 * Revision 1.7  2003/04/17 21:07:56  da0g
 * Added code to free (cleanup) data associated with distributed invocation.
 *
 * Revision 1.6  2002/06/26 16:48:22  reids
 * Made a distinction between the type-name and instance-name of a node.
 *
 * Revision 1.5  1997/12/29 17:06:10  reids
 * Version that has the basic functionality needed to support TDL.
 *
// Revision 1.4  97/12/22  16:52:49  reids
// Basically, added "data" field for CALLBACK_ACTION,
//  and started using "nodeData" for the activation data associated
//  with monitors, and the failure data associated with exceptions.
// 
// Revision 1.3  97/12/18  00:21:37  reids
// Changing ACTION_PTR to a handle, to aid in garbage collection.
// 
// Revision 1.2  97/12/04  17:50:01  reids
// Another fairly stable version (except that monitors do not quite work)
// 
// Revision 1.1  97/11/21  14:06:19  reids
// First release of TCM -- seems to be a stable version
// 
 *
 **************************************************************************/

#include "tcmLogging.h"
#include "action.h"
#include "taskTree.h"

/**************************************************************************
 *
 * CLASS: _Action
 *
 **************************************************************************/

INSTANTIATE_REF_COUNT_FUNCTIONS(_Action);

/*virtual*/ _Action::~_Action()
{
#ifdef TRACE_ACTION_DELETIONS
  tcmMessage("Deleting Action\n");
#endif
	/* Try to cleanup any distributed data. */
  if ( distributedCleanupFunction != NULL )
    distributedCleanupFunction (distributedCleanupRef, distributedCleanupData);
  if ( distributedCleanupRef != (char *) NULL )
    free ( distributedCleanupRef );
  distributedCleanupRef      = (char *) NULL;
  distributedCleanupData     = (void *) NULL;
  distributedCleanupFunction = NULL;
}

#ifdef macintosh // Now defined as a pure virtual function

/**************************************************************************
 *
 * FUNCTION: void execute (Task_Tree_Ref const &node)
 *
 * DESCRIPTION: This should never be called.
 *
 **************************************************************************/

void _Action::execute (Task_Tree_Ref const &node) 
{
  tcmError("Cannot execute a virtual action for node %s",
	   node->instanceName());
}
#endif

/**************************************************************************
 *
 * CLASS: Callback_Action
 *
 **************************************************************************/


/**************************************************************************
 *
 * FUNCTION: void execute (Task_Tree_Ref const &node)
 *
 * DESCRIPTION: Invoke the callback function with a handle to the task
 *              tree node and the node data.
 *
 **************************************************************************/
void Callback_Action::execute (Task_Tree_Ref const &ref) 
{
  (*_callback)(ref, _data);
}

/**************************************************************************
 *
 * CLASS: Ext_Callback_Action
 *
 **************************************************************************/


/**************************************************************************
 *
 * FUNCTION: void execute (Task_Tree_Ref const &node)
 *
 * DESCRIPTION: Invoke the callback function with a handle to the task
 *              tree node, the node data, and the class data.
 *
 **************************************************************************/
void Ext_Callback_Action::execute (Task_Tree_Ref const &ref) 
{
  (*_callback)(ref, _data, _classData);
}
