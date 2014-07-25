/**************************************************************************
 * 
 * PROJECT: Task Control Management.
 *
 * (c) Copyright 1996 Reid Simmons.  All rights reserved.
 *
 * FILE: exception.cc
 *
 * ABSTRACT: Classes for raising and handling exceptions/failures.
 *
 * EXPORTS:
 *
 * $Revision: 1.16 $
 * $Date: 2009/01/15 17:00:04 $
 * $Author: reids $
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tcm/LICENSE.TXT)
 *
 * REVISION HISTORY:
 *
 * $Log: exception.cc,v $
 * Revision 1.16  2009/01/15 17:00:04  reids
 * Released under simplified BSD open source license
 *
 * Revision 1.15  2008/07/11 15:47:02  reids
 * Merged the two previous ways that exceptions were implemented (the original
 * version, and one for TDL).  Extended to handle distributed exceptions.
 * Added the TRACE_UBER flag for even more detailed tracing.
 * Added API functions: TCM_FailureNode and TCM_ExceptionHandlerNode.
 *
 * Revision 1.14  2002/07/11 03:50:25  da0g
 * Addressed String = (char*) vs (const char *) issues.
 *
 * Revision 1.13  2002/06/26 16:48:23  reids
 * Made a distinction between the type-name and instance-name of a node.
 *
 * Revision 1.12  2002/04/02 17:25:28  reids
 * Fixed the problem with "delayTermination" of exception nodes -- now, nodes
 *   that are marked "delay termination" are terminated after they are
 *   achieved only if explicitly requested to be so.
 *
 * Revision 1.11  2002/03/22 02:27:47  da0g
 * Added Exception-Handler-Ordering code.
 *
 * Revision 1.10  2000/07/05 23:05:48  da0g
 * Fixed an infinite-loop bug where the first exception handler would
 * be invoked multiple times when multiple exception handlers were present.
 *
 * Revision 1.9  1999/08/04 14:00:17  reids
 * Distributed version of TCM.  Provides the ability for one process to
 *  add tasks, constraints, exception handlers, etc. with another process.
 *  The distributed version is created by compiling with -DDISTRIBUTED flag,
 *   which can be gotten by "gmaking" with DISTRIBUTED=1.
 *
// Revision 1.8  98/09/15  18:45:20  da0g
// Enhanced exceptions to support multiple-name resolution and Ref_Count (automatically-destroyed) Data.
// 
 * Revision 1.7  1998/03/06 13:12:42  reids
 * Added a way to stop the compiler from complaining about unused function
 *   arguments (apparently, g++ ignores the "#pragma unused" statement).
 *
// Revision 1.6  97/12/30  12:26:53  reids
// Added flag to indicate whether node raised an exception (ie, failed).
// 
// Revision 1.5  97/12/29  17:06:17  reids
// Version that has the basic functionality needed to support TDL.
// 
// Revision 1.4  97/12/22  16:52:52  reids
// Basically, added "data" field for CALLBACK_ACTION,
//  and started using "nodeData" for the activation data associated
//  with monitors, and the failure data associated with exceptions.
// 
// Revision 1.3  97/12/18  00:21:41  reids
// Changing ACTION_PTR to a handle, to aid in garbage collection.
// 
// Revision 1.2  97/12/04  17:50:07  reids
// Another fairly stable version (except that monitors do not quite work)
// 
// Revision 1.1  97/11/21  14:06:29  reids
// First release of TCM -- seems to be a stable version
// 
 *
 **************************************************************************/

#include <stdio.h>
#include "exception.h"
#ifdef macintosh
#include "agenda.h"
#endif

/**************************************************************************
 *
 * CLASS: TCM_Exception
 *
 * Description: A tuple of <name, data>
 *
 **************************************************************************/
BOOLEAN TCM_Exception::matches ( STRING theString ) const
{
  return !strcmp(theString, getExceptionName());
}

TCM_Exception *TCM_Exception::clone ( void ) const
{
  return create_TCM_Exception(getExceptionName(), getExceptionData());
}

// Create a base exception; used mainly for distributed exceptions
TCM_Exception *create_TCM_Exception (STRING name, const void *data)
{
  return new TCM_Exception(name, data);
}

/**************************************************************************
 *
 * CLASS: Exception_Handler
 *
 * Description: A tuple of <type, action, maxInvocations>
 *
 **************************************************************************/

INSTANTIATE_REF_COUNT_FUNCTIONS(Exception_Handler);

// All member functions are defined in exception.h, so far


/**************************************************************************
 *
 * CLASS: Exception_Instance
 *
 * Description: A particular exception handler associated with a given
 *              context node, for handling a specific type of exception.
 *
 **************************************************************************/

void Exception_Instance::addExceptionNode (
			       const Task_Tree_Ref & theParent,
			       const Task_Tree_Ref & theFailureNode )
{
  Task_Tree_Node *exceptionNode;

  /* The failure data is now stored with the exception, which is part
     of Exception_Instance */
  exceptionNode = new Exception_Node ( name,
				       getHandler() -> getAction(),
				       *this,
				       theFailureNode );
  getHandler() -> use();

  exceptionNode -> delayTermination();
  taskTreeInsert ( theParent.operator*(), exceptionNode, TRUE );
}

/**************************************************************************
 *
 * CLASS: Task_Tree_Node
 *
 * Description: Function in Task_Tree_Node class, but defined here
 *              because they all have to do with exceptions.
 *
 **************************************************************************/

// Search list of exceptions backwards, to find most recently added first
void
Task_Tree_Node::findAndApplyExceptionHandler (Task_Tree_Ref parentNode,
					      Task_Tree_Ref failureNode,
					      Exception_Instance *exception)
{
  Exception_Handler       * exceptionHandler = (Exception_Handler *) NULL;
  Const_Exception_Iterator  exceptionIterator ( getExceptions() );

  if ( exception->getHandler().operator*() != (Exception_Handler *) NULL) {
    BOOLEAN found = FALSE;
    for ( ; exceptionIterator && (found == FALSE); exceptionIterator++) {
      found = (exceptionIterator.getObject() == exception->getHandler());
    }
    if (found == FALSE)
      exceptionIterator.reset();
  }

  for (; exceptionIterator && (exceptionHandler == NULL); 
       exceptionIterator++) {
    if ((*exceptionIterator())->available() && 
	exception->getException()->matches((*exceptionIterator())->getName())){
      exceptionHandler = exceptionIterator . getObject() . operator*();
    }
  }

  if ( exceptionHandler != (Exception_Handler *) NULL ) {
    exception->setHandler     ( exceptionHandler );
    exception->setContextNode ( this             );
    exception->addExceptionNode(parentNode, failureNode);
    parentNode->doneHandling();
  } else if (*getParent() != NULL) {
    exception->setHandler(NULL);
    getParent()->findAndApplyExceptionHandler(parentNode, failureNode,
					      exception);
  } else {
    failureNode->noExceptionHandler(exception->getName());
    parentNode->doneHandling();
  }
}

void Task_Tree_Node::noExceptionHandler (STRING failure) const
{
  tcmWarning("No exception handler found for exception type \"%s\"\n",
	     failure);
}

/**************************************************************************
 *
 * CLASS: Exception_Node
 *
 * Description: A specialization of Task_Tree_Goal_Node, containing  
 *              additional information needed to handle an exception.
 *
 **************************************************************************/

void Exception_Node::bypass (void)
{
  Task_Tree_Ref parent = this;
#ifdef TRACE_EXCEPTIONS
  tcmMessage("Bypassing exception \"%s\"\n", name);
#endif

	/* Since raise() will modify the Exception_Instance,
	 * lets create a temporary copy and let that be modified...
	 */
  Exception_Instance  tmpCopyOfOurExceptionInstance ( exceptionInstance );
  tmpCopyOfOurExceptionInstance.raise( parent, failureNode );
  // Don't invoke "doneHandling" until exception handler is actually found
}

void raiseException (TCM_Exception *exception, Task_Tree_Ref const &failureNode)
{
  Exception_Instance exceptionInstance(exception, failureNode);

#ifdef TRACE_EXCEPTIONS
  tcmMessage("Raising exception \"%s\" from %s\n",
	     exception->getExceptionName(), failureNode->instanceName());
#endif
  failureNode->setFailed();
  exceptionInstance.raise(failureNode, failureNode);
  // Don't invoke "doneHandling" until exception handler is actually found
}

/**************************************************************************
 *
 * FUNCTION: void addExceptionHandler(Task_Tree_Ref const &nodeRef, STRING type,
 *		                      const TCM_Action_Ref action,
 *                                    int maxInvocations)
 *
 * DESCRIPTION: Add an exception handler to the node addresses failure "type".
 *              Invoke an instance of the handler at most "maxInvocations" 
 *              times.
 *
 **************************************************************************/
void addExceptionHandler (Task_Tree_Ref const &nodeRef, STRING type, 
			  const TCM_Action_Ref &action,
			  unsigned int maxInvocations,
			  signed int index
			      /* = DEFAULT_EXCEPTION_HANDLER_PRIORITY */ )
{
  Exception_Handler_Ptr handler = new Exception_Handler(type, action,
                                                        maxInvocations,
							index );
  if ( index == DEFAULT_EXCEPTION_HANDLER_PRIORITY )
  {
    nodeRef->getExceptions()->insertFirst(handler);
    return;
  }


  
  for ( Exception_Iterator  iterator ( nodeRef->getExceptions() );
	iterator;
	iterator++ )
  {
    if ( iterator . getObject() . operator*() -> getIndex() >= index )
    {
      iterator--;
      nodeRef -> getExceptions() -> insertAfter ( iterator, handler );
      return;
    }
  }

  nodeRef->getExceptions()->insertLast(handler);
  return;
}

/**************************************************************************
 *
 * FUNCTION: void removeExceptionHandlerTask_Tree_Ref(const &nodeRef, 
 *                                                STRING type,
 *						  const TCM_Action_Ref &action)
 *
 * DESCRIPTION: Remove the give exception handler from the node.
 *
 **************************************************************************/
void removeExceptionHandler (Task_Tree_Ref const &nodeRef, STRING type,
			     const TCM_Action_Ref &action)
{
  UNUSED(nodeRef); UNUSED(type); UNUSED(action);

  tcmWarning("removeExceptionHandler: Not Yet Implemented\n");
}


