/**************************************************************************
 * 
 * PROJECT: Task Control Management.
 *
 * (c) Copyright 1996 Reid Simmons.  All rights reserved.
 *
 * FILE: exception.h
 *
 * ABSTRACT: Classes for raising and handling exceptions/failures.
 *
 * EXPORTS:
 *
 * $Revision: 1.12 $
 * $Date: 2009/01/15 17:00:04 $
 * $Author: reids $
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tcm/LICENSE.TXT)
 *
 * REVISION HISTORY:
 *
 * $Log: exception.h,v $
 * Revision 1.12  2009/01/15 17:00:04  reids
 * Released under simplified BSD open source license
 *
 * Revision 1.11  2008/07/11 15:47:02  reids
 * Merged the two previous ways that exceptions were implemented (the original
 * version, and one for TDL).  Extended to handle distributed exceptions.
 * Added the TRACE_UBER flag for even more detailed tracing.
 * Added API functions: TCM_FailureNode and TCM_ExceptionHandlerNode.
 *
 * Revision 1.10  2002/07/11 03:50:25  da0g
 * Addressed String = (char*) vs (const char *) issues.
 *
 * Revision 1.9  2002/06/26 16:49:17  reids
 * Added casts to satisfy gcc 3.0.
 *
 * Revision 1.8  2002/03/22 02:27:47  da0g
 * Added Exception-Handler-Ordering code.
 *
 * Revision 1.7  1999/08/04 14:00:18  reids
 * Distributed version of TCM.  Provides the ability for one process to
 *  add tasks, constraints, exception handlers, etc. with another process.
 *  The distributed version is created by compiling with -DDISTRIBUTED flag,
 *   which can be gotten by "gmaking" with DISTRIBUTED=1.
 *
 * Revision 1.6  98/09/15  18:45:20  da0g
 * Enhanced exceptions to support multiple-name resolution and Ref_Count (automatically-destroyed) Data.
 * 
 * Revision 1.5  1997/12/29 17:06:17  reids
 * Version that has the basic functionality needed to support TDL.
 *
 * Revision 1.4  97/12/22  16:52:53  reids
 * Basically, added "data" field for CALLBACK_ACTION,
 *  and started using "nodeData" for the activation data associated
 *  with monitors, and the failure data associated with exceptions.
 * 
 * Revision 1.3  97/12/18  00:21:43  reids
 * Changing ACTION_PTR to a handle, to aid in garbage collection.
 * 
 * Revision 1.2  97/12/04  17:50:08  reids
 * Another fairly stable version (except that monitors do not quite work)
 * 
 * Revision 1.1  97/11/21  14:06:30  reids
 * First release of TCM -- seems to be a stable version
 * 
 *
 **************************************************************************/

#ifndef INCexception
#define INCexception

#include "taskTree.h"

// Create a base exception; used mainly for distributed exceptions
extern TCM_Exception *create_TCM_Exception (STRING name, const void *data);

class Exception_Handler : public Named_Object, public Ref_Count
{
 public:
  Exception_Handler() { action = NULL; maxInvocations = numInvocations = 0;
                        index = DEFAULT_EXCEPTION_HANDLER_PRIORITY; }
  Exception_Handler(STRING type,
		    const TCM_Action_Ref &handler,
		    unsigned int maxTimes,
		    signed int theIndex = DEFAULT_EXCEPTION_HANDLER_PRIORITY )
    : Named_Object(type) { action = handler;
                           maxInvocations = maxTimes;
			   index = theIndex;
			   numInvocations = 0; }

  TCM_Action_Ref getAction(void) const { return action; }
  signed int     getIndex (void) const { return index;  }

  void use (void) { numInvocations++; }
  BOOLEAN available (void) { return (numInvocations < maxInvocations); }

 private:
  TCM_Action_Ref action;
  signed   int   index;
  unsigned int maxInvocations;
  unsigned int numInvocations;
};

typedef Exception_Handler *Exception_Handler_Ptr;
typedef Exception_Handler const *Const_Exception_Handler_Ptr;

class Exception_Instance : public Named_Object
{
protected:
  Exception_Handler_Ref handler;
  Task_Tree_Ref         contextNode;
  TCM_Exception_Ref     exception;

 public:
  Exception_Instance()
    : Named_Object  (      ),
      handler       (      ),
      contextNode   (      ),
      exception     ( NULL )
  { }

  Exception_Instance ( TCM_Exception       * theException,
		       const Task_Tree_Ref & theNode )
    : Named_Object  ( theException->getExceptionName() ),
      handler       (                                  ),
      contextNode   ( theNode                          ),
      exception     ( theException                     )
  { }

  Exception_Instance ( const Exception_Instance & theExceptionInstance )
    : Named_Object  ( theExceptionInstance             ),
      handler       ( theExceptionInstance.handler     ),
      contextNode   ( theExceptionInstance.contextNode ),
      exception     ( theExceptionInstance.exception   )
  { }

  ~Exception_Instance() { }

        TCM_Exception_Ref       getException()    const {return exception;    }
  const Exception_Handler_Ref & getHandler()      const {return handler;      }
  const Task_Tree_Ref         & getContextNode()  const {return contextNode;  }

  void  setHandler       (       Exception_Handler * theExceptionHandler )
				   { handler       = theExceptionHandler; }

  void  setContextNode   ( const Task_Tree_Ref     & theContextNode )
				   { contextNode   = theContextNode;      }
  void findNext ();

  void raise ( const Task_Tree_Ref & theParent, 
	       const Task_Tree_Ref & theFailureNode )
	{ contextNode->findAndApplyExceptionHandler(theParent, theFailureNode,
						    this ); }

  void addExceptionNode ( const Task_Tree_Ref & theParent, 
			  const Task_Tree_Ref & theFailureNode );

  const void * getExceptionData () const
    { return (exception.isNull() ? NULL : exception->getExceptionData()); }
};

class Exception_Node : public Task_Tree_Goal_Node
{
 public: 
  const Exception_Instance &getExceptionInstance() const { return exceptionInstance;}
protected:
  Exception_Instance  exceptionInstance;
  Task_Tree_Ref       failureNode;

public:
  Exception_Node ( STRING theNodeName )
    : Task_Tree_Goal_Node   ( theNodeName ),
      exceptionInstance     (             ),
      failureNode           (             )
  { }

  Exception_Node ( STRING                     theNodeName,
		   const TCM_Action_Ref     & theNodeAction,
		   const Exception_Instance & theExceptionInstance,
		   const Task_Tree_Ref      & theFailureNode )
    : Task_Tree_Goal_Node   ( theNodeName, theNodeAction ),
      exceptionInstance     ( theExceptionInstance       ),
      failureNode           ( theFailureNode             )
  { }

  virtual BOOLEAN isException(void) { return TRUE; }

  Task_Tree_Ref getFailureNode() const { return failureNode; }
  Task_Tree_Ref getContextNode() const 
    { return exceptionInstance.getContextNode(); }
  TCM_Exception_Ref getException() const 
    { return exceptionInstance.getException(); }

  void bypass (void);
};

void raiseException (TCM_Exception *exception,
		     Task_Tree_Ref const &failureNode);

void addExceptionHandler (Task_Tree_Ref const &nodeRef, STRING type, 
			  const TCM_Action_Ref & action,
			  unsigned int maxInvocations,
			  signed int index
			               = DEFAULT_EXCEPTION_HANDLER_PRIORITY );

void removeExceptionHandler (Task_Tree_Ref const &nodeRef, STRING type,
			     const TCM_Action_Ref & action);

#endif // INCexception
