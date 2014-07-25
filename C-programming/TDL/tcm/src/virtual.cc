/**************************************************************************
 * 
 * PROJECT: Task Control Management.
 *
 * (c) Copyright 1999 Reid Simmons.  All rights reserved.
 *
 * FILE: virtual.cc
 *
 * ABSTRACT: Defines "virtual" task tree nodes used for distributing a
 *           single task tree amongst multiple processes.  A "virtual" node
 *           has "real" counterpart in another process.  "Virtual" nodes 
 *           know how to send messages to their real counterparts and how
 *           to interpret messages sent to them.
 *
 *   Example:      Process 1      Process 2
 *                   [A]             [A']
 *                   / \               \
 *                 [B] [C']            [C]
 *
 *       In process 1, nodes A and B are real, and C' is virtual;
 *       In process 2, node C is real and node A' is virtual.  Virtual
 *       Virtual node C' corresponds to real node C, and A' corresponds to A.
 *
 * $Revision: 1.13 $
 * $Date: 2009/01/15 17:00:05 $
 * $Author: reids $
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tcm/LICENSE.TXT)
 *
 * REVISION HISTORY:
 *
 * $Log: virtual.cc,v $
 * Revision 1.13  2009/01/15 17:00:05  reids
 * Released under simplified BSD open source license
 *
 * Revision 1.12  2008/07/11 15:47:03  reids
 * Merged the two previous ways that exceptions were implemented (the original
 * version, and one for TDL).  Extended to handle distributed exceptions.
 * Added the TRACE_UBER flag for even more detailed tracing.
 * Added API functions: TCM_FailureNode and TCM_ExceptionHandlerNode.
 *
 * Revision 1.11  2003/07/16 03:10:03  reids
 * Update for gcc 3.2.x
 *
 * Revision 1.10  2002/06/26 16:49:06  reids
 * Enable instance name of node to be set in a distributed fashion.
 *
 * Revision 1.9  2002/03/26 05:19:54  reids
 * Significant bug fixes to handling of distributed nodes to take into account
 *   non-persistent task tree nodes.  Also, cleaning up of the "virtualNodes"
 *   list when task tree nodes complete, and significant change to the way
 *   "when after" constraints are handled.
 *
 * Revision 1.8  2002/02/05 17:45:18  reids
 * Backed out the getLocal function for distributed nodes -- instead,
 *   "getVirtual" creates a new virtual node only if the "host" given as the
 *   virtual address is not the current agent (o/w the node is "local").
 * Fixed several bugs relating to race conditions in the distributed version.
 *
 * Revision 1.7  2002/01/18 14:18:18  reids
 * Added "getLocal" to prevent race condition bugs with deleted virtual nodes
 *
 * Revision 1.6  2001/11/20 19:21:52  reids
 * Moved a few definitions around (forgetVirtual and virtualNodes) so that one
 *  can compile TCM with the -DDISTRIBUTED flag, but then link against it
 *  without having to incorporate any of the distributed files (which, in
 *  turn, need IPC and ipcInterface).
 *
 * Revision 1.5  2001/08/07 23:51:53  da0g
 * Fixed:  Renamed removeRequestedEvents to removeRequestedEvent.
 *
 * Revision 1.4  2001/07/24 12:49:05  reids
 * Enable monitor client data to be sent with all the monitor activation
 *   functions (ActivateAt, ActivateIn, ActivateInAfter).  Fixed bugs in
 *   the way monitor data was being set.
 * Separate templates for virtual nodes from regular templates (compiled in
 *   only when DISTRIBUTED option is used -- makes code cleaner).
 *
 * Revision 1.3  2001/06/11 21:39:24  reids
 * Remove destroyed/cleaned-up nodes from the "virtual nodes" list, so they
 *  can be garbage collected.
 *
 * Revision 1.2  2001/06/11 15:56:10  reids
 * Fixed a problem with distributed nodes being confused about which node is
 *   the root node (basically made it possible to determine whether a virtual
 *   node is the root of some other tree).
 *
 * Revision 1.1  1999/08/04 14:00:20  reids
 * Distributed version of TCM.  Provides the ability for one process to
 *  add tasks, constraints, exception handlers, etc. with another process.
 *  The distributed version is created by compiling with -DDISTRIBUTED flag,
 *   which can be gotten by "gmaking" with DISTRIBUTED=1.
 *
 **************************************************************************/

#include "virtual.h"
#include "distributed.h"
#include "agenda.h"

/* 
 * Move to taskTree.cc, so that do not need to link with virtual.cc
 *   unless one really needs the distributed functionality.
 */
//Task_Tree_Node_List Task_Tree_Node::virtualNodes;

void Virtual_Node::_initialize (void)
{
  // Undo the effects of the Task_Tree_Node initialization
  Task_Tree_Node::removeExpectedEvent(Done_Handling_Signal, this);
}

static void setVirtualAddressIfNeeded (Task_Tree_Ref const &node)
{
  if (!node->validVirtualAddress()) {
    node->setVirtualAddress(thisAgent(), thisAgent(), node->getNodeId());
  }
}

void Virtual_Node::addParent (Task_Tree_Ref const &parentNode,
			      BOOLEAN canBeLastChild, BOOLEAN addConstraints)
{
  setVirtualAddressIfNeeded(parentNode);
  distributedAddParent(this, parentNode, canBeLastChild);
  Task_Tree_Node::addParent(parentNode, canBeLastChild, FALSE);
}

void Virtual_Node::addChild (Task_Tree_Ref const &childNode,
			     BOOLEAN addConstraints)
{
  setVirtualAddressIfNeeded(childNode);
  distributedAddChild(this, childNode);
  //Task_Tree_Node::addChild(childNode, FALSE);
}

void Virtual_Node::removeChild (Task_Tree_Ref const &childNode)
{
  setVirtualAddressIfNeeded(childNode);
  distributedRemoveChild(this, childNode);
}

void Virtual_Node::signal (const Event &event)
{
  setVirtualAddressIfNeeded(event.getNode());
  distributedSignal(this, event);
}

void Virtual_Node::addExpectedEvent (Signal_Enum signal,
				     Task_Tree_Ref const &signallingNode,
				     const void *data)
{
  if (data) tcmError("Virtual_Node::addExpectedEvent: Non-null data\n");
  setVirtualAddressIfNeeded(signallingNode);
  distributedAddExpectedEvent(this, signal, signallingNode, data);
}

void Virtual_Node::addRequestedEvent (State_Enum state, Signal_Enum signal,
				      Task_Tree_Ref const &signalledNode,
				      const void *data)
{
  if (data) tcmError("Virtual_Node::addRequestedEvent: Non-null data\n");
  setVirtualAddressIfNeeded(signalledNode);
  distributedAddRequestedEvent(this, state, signal, signalledNode);
}

// The idea here is to add the signal to the distributed node, but 
//   add the callback and data here.
void Virtual_Node::addRequestedEvent(State_Enum state, 
				     SIMPLE_CALLBACK_FN_TYPE callbackFn, 
				     const void *data)
{
  State_Event event(callbackFn, data, state);

  requestedEvents.insert(event);
  distributedAddRequestedCallback(this, state);
}

void Virtual_Node::invokeCallback (State_Enum state)
{
  State_Event_Iterator listIter(&requestedEvents);

  for (listIter.reset(); listIter; listIter++) {
    if (state == listIter()->getState()) {
#ifdef TRACE_VIRTUAL
      tcmWarning("invokeCallback for %s\n", stateName(state));
#endif
      listIter()->sendSignal(this);
      listIter.removeCurrent();
    }
  }
}

void Virtual_Node::removeExpectedEvent(Signal_Enum signal,
				       Task_Tree_Ref const &signallingNode)
{
  setVirtualAddressIfNeeded(signallingNode);
  distributedRemoveExpectedEvent(this, signal, signallingNode);
}

void Virtual_Node::removeRequestedEvent (Signal_Enum signal,
					  Task_Tree_Ref const &signalledNode)
{
  setVirtualAddressIfNeeded(signalledNode);
  distributedRemoveRequestedEvents(this, signal, signalledNode);
}

/*virtual*/ void Virtual_Node::doneHandling ( void )
{
  distributedDoneHandling(this);
}

BOOLEAN Virtual_Node::terminate (void)
{
  distributedTerminate(this);
  // The "regular" task tree nodes check for various error conditions, but
  // that is too difficult, and hardly worthwhile doing, for distributed nodes.
  return TRUE;
}

BOOLEAN Virtual_Node::deallocate (void)
{
  distributedDeallocate(this);
  // The "regular" task tree nodes check for various error conditions, but
  // that is too difficult, and hardly worthwhile doing, for distributed nodes.
  return TRUE;
}

void Virtual_Node::suspend (void)
{
  distributedSuspend(this);
}

void Virtual_Node::unsuspend (void)
{
  distributedUnsuspend(this);
}

void Virtual_Node::delayTermination (void)
{
  distributedDelayTermination(this);
}

void Virtual_Node::enableTermination (void)
{
  distributedEnableTermination(this);
}

void 
Virtual_Node::addTerminationAction (const TCM_Task_Tree_Ref &terminationRef)
{
  setVirtualAddressIfNeeded(terminationRef);
  distributedAddTermination(this, terminationRef);
}

void Virtual_Node::setInstanceName (STRING newInstanceName)
{
  Task_Tree_Node::setInstanceName(newInstanceName);
  distributedSetInstanceName(this, newInstanceName);
}

void Virtual_Node::displayTree (FILE *stream, int position) const
{
  // You can only display the distributed tree to either stdout or stderr
  distributedDisplayTree((Task_Tree_Node *)this,
			 (stream == stdout ? USE_STDOUT : USE_STDERR),
			 position);
}

void Virtual_Node::findAndApplyExceptionHandler (Task_Tree_Ref parentNode,
						 Task_Tree_Ref failureNode,
						 Exception_Instance *exception)
{ 
  setVirtualAddressIfNeeded(parentNode);
  setVirtualAddressIfNeeded(failureNode);
  distributedFindAndApplyExceptionHandler(this, parentNode, failureNode, 
					  exception);
}

void Virtual_Node::noExceptionHandler (STRING failure) const 
{
  distributedNoExceptionHandler((Task_Tree_Node *)this, failure);
}

void Virtual_Node::setPersistence (BOOLEAN value)
{
  distributedSetPersistence(this, value);
}

/* Need a special function for determining whether it is the root node
 * in order to work correctly for virtual nodes 
 * NOTE: Making the assumption that the first node created in a process (id 0) 
 *       is the root node!!
 */
BOOLEAN Virtual_Node::isRootNode  (void) const
{ 
  return _vaddress.id == 0;
}

/**************************************************************************
 *
 * Functions to map between a virtual address and a virtual node
 *
 * AT SOME POINT, SHOULD IMPLEMENT USING A HASH TABLE!
 *
 **************************************************************************/

/**************************************************************************
 *
 * FUNCTION: void rememberVirtual(void)
 *
 * DESCRIPTION: Memoize the node, so that it can be recalled by its 
 *              virtual address
 *
 **************************************************************************/
void Task_Tree_Node::rememberVirtual (void)
{
  virtualNodes.insertLast(this);
}

/**************************************************************************
 *
 * FUNCTION: Task_Tree_Node_Ptr recallVirtual (const Virtual_Address &vaddress)
 *
 * DESCRIPTION: Find the virtual node associated with the given
 *              virtual address
 *
 **************************************************************************/
Task_Tree_Node_Ptr
Task_Tree_Node::recallVirtual (const Virtual_Address &vaddress)
{
  Const_Task_Tree_Node_Iterator nodeIter(virtualNodes);
  Virtual_Address virtualAddress;

  if (vaddress.host == NULL) return NULL; // Not a real virtual address 

  for (; nodeIter; nodeIter++) {
    virtualAddress = (*nodeIter())->getVirtualAddress();
    if (vaddress.id == virtualAddress.id &&
	streq(vaddress.host, virtualAddress.host) &&
	streq(vaddress.creator, virtualAddress.creator)) {
      return (Task_Tree_Node_Ptr)**nodeIter();
    }
  }
  return NULL;
}

#if 0
/**************************************************************************
 *
 * FUNCTION: void forgetVirtual (const Virtual_Address &vaddress) 
 *
 * DESCRIPTION: Un-memoize the node, freeing up storage
 *
 * NOTE: Moved to taskTree.cc so that one can link TCM programs that do not
 *       use the distributed capabilities, without having to link with IPC.
 **************************************************************************/
void Task_Tree_Node::forgetVirtual (const Virtual_Address &vaddress)
{
  Task_Tree_Node_Iterator nodeIter(virtualNodes);
  Virtual_Address virtualAddress;

  if (vaddress.host == NULL) return; // Not a real virtual address 

  for (; nodeIter; nodeIter++) {
    virtualAddress = (*nodeIter())->getVirtualAddress();
    if (vaddress.id == virtualAddress.id &&
	streq(vaddress.host, virtualAddress.host) &&
	streq(vaddress.creator, virtualAddress.creator)) {
      nodeIter.removeCurrent();
      break;
    }
  }
}
#endif

/**************************************************************************
 *
 * FUNCTION: Task_Tree_Node_Ptr getVirtual (const Virtual_Address &vaddress)
 *
 * DESCRIPTION: Find or create the virtual node associated with the
 *              given virtual address
 *
 **************************************************************************/
Task_Tree_Node_Ptr Task_Tree_Node::getVirtual (const Virtual_Address &vaddress,
					       STRING nodeName)
{
  Task_Tree_Node_Ptr virtualNode = recallVirtual(vaddress);

  if (virtualNode) {
    return virtualNode;
  } else if (streq(vaddress.host, thisAgent())) {
    tcmWarning("getVirtual: No local node for {%d} -- assuming it was "
	       " terminated or completed\n", vaddress.id);
    return NULL;
  } else {
    return new Virtual_Node(nodeName, vaddress);
  }
}

/**************************************************************************
 *
 * FUNCTION: void setVirtualAddress (STRING host, STRING creator, 
 *                                   unsigned int id)
 *
 * DESCRIPTION: Set the virtual address of the node, and memoize the node.
 *
 **************************************************************************/
void  Task_Tree_Node::setVirtualAddress (STRING host, STRING creator, 
					 unsigned int id)
{
  if (host == NULL || strlen(host) == 0 || 
      creator == NULL || strlen(creator) == 0) {
    tcmError("setVirtualAddress: Null host and/or creator for %s\n",
	     instanceName());
  } else if (validVirtualAddress()) {
    tcmError("setVirtualAddress: Address for %s already set\n",
	     instanceName());
  } else {
    _vaddress.host = strdup(host);
    _vaddress.creator = strdup(creator);
    _vaddress.id = id;
    this->id = id;
    rememberVirtual();
  }
}

void Task_Tree_Node::setVirtualAddress (const Virtual_Address &vaddress)
{
  setVirtualAddress(vaddress.host, vaddress.creator, vaddress.id);
}

// Don't terminate unecessarily, that is:
// Don't add the timed event if the signal is start terminating,
//       and the node is in the terminating/terminated state,
//       and the signal state is not terminating/terminated.
// This is special for virtual nodes because it needs node state information.
BOOLEAN Virtual_Node::addEventP(Signal_Enum signal, State_Enum state)
{
  return !((signal) == Start_Terminating_Signal && 
	   getIsTerminating() &&
	   !((state) == Terminating_State || (state) == Terminated_State));
}
