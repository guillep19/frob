/**************************************************************************
 * 
 * PROJECT: Task Control Management
 *
 * (c) Copyright 1996 Reid Simmons.  All rights reserved.
 *
 * FILE: tcmList.cc
 *
 * ABSTRACT: The list module provides basic list creation and manipulation
 * routines and serves as the base abstract data type for the tcm.
 * The include file list.h provides the top level routines for other modules.
 * Adapted to C++ from the TCA version (original author: Christopher Fedor)
 *
 * EXPORTS:
 *
 * $Revision: 1.6 $
 * $Date: 2009/01/15 17:00:05 $
 * $Author: reids $
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tcm/LICENSE.TXT)
 *
 * REVISION HISTORY:
 *
 * $Log: tcmList.cc,v $
 * Revision 1.6  2009/01/15 17:00:05  reids
 * Released under simplified BSD open source license
 *
 * Revision 1.5  2008/07/16 06:15:10  reids
 * Updates for newer (pickier) compilers
 *
 * Revision 1.4  2002/03/22 02:27:47  da0g
 * Added Exception-Handler-Ordering code.
 *
 * Revision 1.3  2002/01/18 14:03:26  reids
 * Added \n to the tcmError call
 *
 * Revision 1.2  2001/03/26 21:38:56  trey
 * changed list<T> type to be tcmList<T> to avoid conflict with STL lists
 *
 * Revision 1.1  1999/10/21 21:32:33  reids
 * Changed list.cc and list.h to tcmList.cc and tcmList.h to avoid conflicts
 *   with other occurrences of those names in other packages.
 *
 * Revision 1.5  1999/06/06 13:48:08  reids
 * Changes to TCM 1.3.0:
 *   New list data structures.
 *   New, generalized logging interface.
 *   Fixed firstChild to work if there are no children.
 *   Fixed code to deallocate a node.
 *   In certain circmumstances, don't send termination signal when task
 *     itself terminating.
 *
// Revision 1.4  98/06/02  10:39:36  reids
// Increased overall efficiency of TCM.
// 
// Revision 1.3  97/12/29  17:06:20  reids
// Version that has the basic functionality needed to support TDL.
// 
// Revision 1.2  97/12/04  17:50:10  reids
// Another fairly stable version (except that monitors do not quite work)
// 
// Revision 1.1  97/11/21  14:06:31  reids
// First release of TCM -- seems to be a stable version
// 
 *
 **************************************************************************/

#include "tcmList.h"
#include "tcmLogging.h"

/**************************************************************************
 *
 * CLASS: List_Element<T>
 *
 **************************************************************************/

/**************************************************************************
 *
 * FUNCTION: void addBefore(List_Element<T> *otherElement)
 *
 * DESCRIPTION: Add "this" element before the other element.
 *
 **************************************************************************/

template <class T>
void List_Element<T>::addBefore(List_Element<T> *otherElement)
{
  setNext(otherElement);
  if (otherElement) otherElement->setPrevious(this);
}

/**************************************************************************
 *
 * FUNCTION: void addAfter(List_Element<T> *otherElement)
 *
 * DESCRIPTION: Add "this" element after the other element.
 *
 **************************************************************************/

template <class T>
void List_Element<T>::addAfter(List_Element<T> *otherElement)
{
  setPrevious(otherElement);
  if (otherElement) otherElement->setNext(this);
}

/**************************************************************************
 *
 * CLASS: tcmList<T>
 *
 * DESCRIPTION: The "list" class uses List_Element, which has prev/next 
 *              links and type-specific data.
 *
 **************************************************************************/

/**************************************************************************
 *
 * FUNCTION: ~tcmList()
 *
 * DESCRIPTION: Destructor for the tcmList class
 *
 **************************************************************************/

template <class T>
tcmList<T>::~tcmList(void)
{
  List_Element<T> *tmp;

  while (first) {
    tmp = first->getNext();
    delete first;
    first = tmp;
  }
}

/**************************************************************************
 *
 * FUNCTION: void _insertFirst(List_Element<T> *listElement)
 *
 * DESCRIPTION: Insert the element at the front of the list
 *              Do nothing if the element is NULL.
 *
 **************************************************************************/

template <class T>
void tcmList<T>::_insertFirst(List_Element<T> *listElement)
{
  if (listElement) {
    if (!first) {
      last = listElement;
    } else {
      listElement->addBefore(first);
    }

    first = listElement;
    length++;
  }
}

/**************************************************************************
 *
 * FUNCTION: void _insertLast(List_Element<T> *listElement)
 *
 * DESCRIPTION: Insert the element at the end of the list.
 *              Do nothing if the element is NULL.
 *
 **************************************************************************/

template <class T>
void tcmList<T>::_insertLast(List_Element<T> *listElement)
{
  if (listElement) {
    if (!last) {
      first = listElement;
    } else {
      listElement->addAfter(last);
    }

    last = listElement;
    length++;
  }
}

/**************************************************************************
 *
 * FUNCTION: void insertAfter(const List_Iterate<T> & listIteration,
 *                            const T &item)
 *
 * DESCRIPTION: Insert the element after the current location specified in
 *              listIteration.
 *
 **************************************************************************/

template <class T>
void tcmList<T>::insertAfter(const List_Iterate<T> & listIteration,
			     const T & item )
{
  _insertAfter ( listIteration . getCurrent(),
		 _allocate(item) );
}

/**************************************************************************
 *
 * FUNCTION: void _insertAfter(List_Element<T> *afterElement,
 *                             List_Element<T> *listElement)
 *
 * DESCRIPTION: Insert the element after the given element in the list.
 *              Do nothing if the listElement is NULL.
 *              If the afterElement is NULL, insert at the beginning.
 *
 **************************************************************************/

template <class T>
void tcmList<T>::_insertAfter(List_Element<T> *afterElement,
			   List_Element<T> *listElement)
{
  if (listElement) {
    if (!afterElement) {
      _insertFirst(listElement);
    } else if (afterElement == last) {
      _insertLast(listElement);
    } else {
      listElement->addBefore(afterElement->getNext());
      listElement->addAfter(afterElement);
      length++;
    }
  }
}

/**************************************************************************
 *
 * FUNCTION: List_Element<T> *_removeFirst(void)
 *
 * DESCRIPTION: Remove the first element on the list.
 *              Return that element (or NULL if the list is empty).
 *
 **************************************************************************/

template <class T>
List_Element<T> *tcmList<T>::_removeFirst(void)
{
  List_Element<T> *listElement, *nextElement;

  if (!first) {
    listElement = NULL;
  } else {
    listElement = first;
    nextElement = first->getNext();
    if (!nextElement) {
      last = NULL;
    } else {
      nextElement->addAfter(NULL);
    }
    first = nextElement;
    length--;
  }
  return listElement;
}

/**************************************************************************
 *
 * FUNCTION: List_Element<T> *_removeLast(void)
 *
 * DESCRIPTION: Remove the last element on the list.
 *              Return that element (or NULL if the list is empty).
 *
 **************************************************************************/

template <class T>
List_Element<T> * tcmList<T>::_removeLast(void)
{
  List_Element<T> *listElement, *prevElement;

  if (!last) {
    listElement = NULL;
  } else {
    listElement = last;
    prevElement = last->getPrevious();
    if (!prevElement) {
      first = NULL;
    } else {
      prevElement->addBefore(NULL);
    }
    last = prevElement;
    length--;
  }
  return listElement;
}

/**************************************************************************
 *
 * FUNCTION: List_Element<T> *removeElement(List_Element<T> *listElement)
 *
 * DESCRIPTION: Remove the given element from the list.
 *              Return that element.
 *
 * NOTES: Assumes that the element is actually part of the list.
 *        Use with care!!
 *
 **************************************************************************/

template <class T>
List_Element<T> *tcmList<T>::_removeElement(List_Element<T> *listElement)
{
  List_Element<T> *prevElement, *nextElement;

  if (listElement) {
    prevElement = listElement->getPrevious();
    nextElement = listElement->getNext();
    if (prevElement) prevElement->addBefore(nextElement);
    if (nextElement) nextElement->addAfter(prevElement);
    if (listElement == first) first = nextElement;
    if (listElement == last) last = prevElement;
    length--;
  }
  return listElement;
}

/**************************************************************************
 *
 * FUNCTION: void _delete(List_Element<T> *listElement)
 *
 * DESCRIPTION: "delete" the list element by sticking it on the freelist
 *
 **************************************************************************/

template <class T>
void tcmList<T>::_delete(List_Element<T> *listElement)
{
  if (!listElement) {
    tcmError("List empty or element not found in list\n");
  } else {
    listElement->reset(nullItem);
    _freeList._insertFirst(listElement);
  }
}

/**************************************************************************
 *
 * FUNCTION:   List_Element<T> *_allocate(const T &item)
 *
 * DESCRIPTION: Allocate a new List_Element containing data "item"
 *              Either take from free list or malloc a new element.
 *
 **************************************************************************/

template <class T>
List_Element<T> *tcmList<T>::_allocate(const T &item)
{
  if (_freeList.size() > 0) {
    List_Element<T> *element = _freeList._removeFirst();
    element->reset(item);
    return element;
  } else
    return new List_Element<T>(item);
}

/**************************************************************************
 *
 * FUNCTION:   List_Element<T> *_findItem(const T &item)
 *
 * DESCRIPTION: Return the element of the list whose data matches "item"
 *
 **************************************************************************/

template <class T>
List_Element<T> *tcmList<T>::_findItem(const T &item) const
{
  List_Element<T> *listElement = getFirst();

  while (listElement && !(listElement->getItem()->operator==(item))) {
    listElement = listElement->getNext();
  }
  return listElement;
}

/**************************************************************************
 *
 * FUNCTION: void clear(void)
 *
 * DESCRIPTION: Remove all elements from the list.
 *
 **************************************************************************/

template <class T> void tcmList<T>::clear (void)
{
  while (length > 0) delete _removeLast();
}

/**************************************************************************
 *
 * CLASS: List_Iterate<T>
 *
 * DESCRIPTION: An iterator for list class objects.
 *              Currently does not handle case where element is removed
 *              from list during iteration.
 *
 **************************************************************************/

/**************************************************************************
 *
 * FUNCTION: void removeCurrent(void)
 *
 * DESCRIPTION: Remove the current element pointed to by the iterator (if any)
 *              Delete the element.
 *
 **************************************************************************/

template <class T>
void List_Iterate<T>::removeCurrent(void)
{
  if (_current) {
    List_Element<T> *deleted = _current;
    List_Element<T> *next = _current->getNext();

    _list->_removeElement(_current);
    _current = next;
    _justDeleted = TRUE;
    _list->_delete(deleted);
  }
}

