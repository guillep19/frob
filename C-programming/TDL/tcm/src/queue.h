/**************************************************************************
 * 
 * PROJECT: Task Control Management
 *
 * (c) Copyright 1996 Reid Simmons.  All rights reserved.
 *
 * FILE: queue.h
 *
 * ABSTRACT: The queue module provides basic queue creation and manipulation
 * routines, built on top of the list data type (not the most efficient, but
 * will do for now for proof of concept).
 *
 * EXPORTS:
 *
 * $Revision: 1.11 $
 * $Date: 2009/01/15 17:00:05 $
 * $Author: reids $
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tcm/LICENSE.TXT)
 *
 * REVISION HISTORY:
 *
 * $Log: queue.h,v $
 * Revision 1.11  2009/01/15 17:00:05  reids
 * Released under simplified BSD open source license
 *
 * Revision 1.10  2008/07/16 06:15:10  reids
 * Updates for newer (pickier) compilers
 *
 * Revision 1.9  2001/04/04 14:24:21  reids
 * Fixed source of memory leak
 *
 * Revision 1.8  2001/03/26 21:38:56  trey
 * changed list<T> type to be tcmList<T> to avoid conflict with STL lists
 *
 * Revision 1.7  1999/10/21 21:32:32  reids
 * Changed list.cc and list.h to tcmList.cc and tcmList.h to avoid conflicts
 *   with other occurrences of those names in other packages.
 *
 * Revision 1.6  1999/06/06 13:48:08  reids
 * Changes to TCM 1.3.0:
 *   New list data structures.
 *   New, generalized logging interface.
 *   Fixed firstChild to work if there are no children.
 *   Fixed code to deallocate a node.
 *   In certain circmumstances, don't send termination signal when task
 *     itself terminating.
 *
 * Revision 1.5  98/06/02  10:39:38  reids
 * Increased overall efficiency of TCM.
 * 
 * Revision 1.4  98/01/30  14:50:55  reids
 * Updated to compile under gcc 2.7.2 and under Linux.
 * Also, made STRING "const char *" and changed API to take const arguments,
 *   where applicable.
 * 
 * Revision 1.3  97/12/29  17:06:25  reids
 * Version that has the basic functionality needed to support TDL.
 * 
 * Revision 1.2  97/12/04  17:50:14  reids
 * Another fairly stable version (except that monitors do not quite work)
 * 
 * Revision 1.1  97/11/21  14:06:35  reids
 * First release of TCM -- seems to be a stable version
 * 
 *
 **************************************************************************/

#ifndef INCqueue
#define INCqueue

#include "tcmList.h"

template <class T>
class queue: public tcmList<T>
{
 public:
  void push(const T &item) { insertLast(item); }
  void pop(void) { this->removeFirst(); }
  T *front(void) const
    { 
      if (!this->getFirst()) {
	return NULL;
      } else {
	return ((List_Element<T> *)this->getFirst())->getItem();
      }
    }
  BOOLEAN empty(void) const { return this->size() == 0; }
};

template <class T, class K>
class priority_queue: public queue<T>
{
 public:
  priority_queue() : _compFn(NULL) {}
  priority_queue(K compFn) : _compFn(compFn) { }

  T *top(void) const { return this->front(); }
  void push(const T &item)
    { // This is incredibly inefficient!  It works, but needs mongo cleanup.
      Const_List_Iterate<T> listIter(this);
      List_Element<T> *elem = NULL;

      for (listIter.reset(); listIter; listIter++) {
	if (!(_compFn)(item, *listIter())) {
	  break;
	}
	elem = listIter.getCurrent();
      }
      _insertAfter(elem, _allocate(item));
    }

 private:
  const K _compFn;
};

#endif /* INCqueue */
