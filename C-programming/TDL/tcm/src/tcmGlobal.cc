/*****************************************************************************
 * 
 * PROJECT: Task Control Management.
 *
 * (c) Copyright 1996 Reid Simmons.  All rights reserved.
 *
 * FILE: tcmGlobal.cc
 *
 * ABSTRACT: (Internal) global variables
 *
 * $Source: /afs/cs.cmu.edu/project/TCA/Master/tcaV8/tcm/src/tcmGlobal.cc,v $ 
 * $Revision: 1.5 $
 * $Date: 2009/01/15 17:00:05 $
 * $Author: reids $
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tcm/LICENSE.TXT)
 *
 * REVISION HISTORY:
 *
 * $Log: tcmGlobal.cc,v $
 * Revision 1.5  2009/01/15 17:00:05  reids
 * Released under simplified BSD open source license
 *
 * Revision 1.4  2001/10/23 22:52:59  da0g
 * Added Threading support.  Cached lastchild.
 *
 * Revision 1.3  1999/06/06 13:48:09  reids
 * Changes to TCM 1.3.0:
 *   New list data structures.
 *   New, generalized logging interface.
 *   Fixed firstChild to work if there are no children.
 *   Fixed code to deallocate a node.
 *   In certain circmumstances, don't send termination signal when task
 *     itself terminating.
 *
// Revision 1.2  97/12/04  17:50:25  reids
// Another fairly stable version (except that monitors do not quite work)
// 
// Revision 1.1  97/11/21  14:06:43  reids
// First release of TCM -- seems to be a stable version
// 
 *
 *****************************************************************************/

#include "tcmGlobal.h"

Global_State globalState;

void initializeGlobalState(void)
{
  GET_TCM_GLOBAL(terminalLogger).setLogging(stderr, FALSE);

	/* Allocate the rootNode dynamically off the heap.
	 * It never gets free()'d.  It is intentionally "memory-leaked",
	 * and only gets cleared when the program exits.
	 *
	 * Solves:  Problem wherein static variables were automatically
	 * deleted upon program exit, when certain static variables
	 * required the root-node for their deletion activities,
	 * and the root-node static variable was previously deleted.
	 * (Problem demonstrates itself with respect to threading, as the
	 *  threading-mutex was being deleted, and the deleted root-node
	 *  data-structure could not therefore be reused.)
	 *
	 *   -da0g.
	 */
  GET_TCM_GLOBAL(rootNode) = new Task_Tree_Node("Root Node");

  // Need to artificially increment the refcount, or else the
  // globalRoot gets freed when the program exits!
  GET_TCM_GLOBAL(rootNode)->increment();

  GET_TCM_GLOBAL(rootNode)->transitionTo(Planning_State);
  GET_TCM_GLOBAL(rootNode)->transitionTo(Achieving_State);
}
