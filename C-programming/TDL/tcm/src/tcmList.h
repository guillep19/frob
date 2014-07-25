/**************************************************************************
 * 
 * PROJECT: Task Control Management
 *
 * (c) Copyright 1996 Reid Simmons.  All rights reserved.
 *
 * FILE: list.h
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
 * $Log: tcmList.h,v $
 * Revision 1.6  2009/01/15 17:00:05  reids
 * Released under simplified BSD open source license
 *
 * Revision 1.5  2008/07/16 06:15:10  reids
 * Updates for newer (pickier) compilers
 *
 * Revision 1.4  2003/04/17 21:10:52  da0g
 * Added a non-operator method for obtaining a pointer value.
 *
 * Revision 1.3  2002/03/22 02:27:47  da0g
 * Added Exception-Handler-Ordering code.
 *
 * Revision 1.2  2001/03/26 21:38:56  trey
 * changed list<T> type to be tcmList<T> to avoid conflict with STL lists
 *
 * Revision 1.1  1999/10/21 21:32:33  reids
 * Changed list.cc and list.h to tcmList.cc and tcmList.h to avoid conflicts
 *   with other occurrences of those names in other packages.
 *
 * Revision 1.8  1999/06/06 13:48:08  reids
 * Changes to TCM 1.3.0:
 *   New list data structures.
 *   New, generalized logging interface.
 *   Fixed firstChild to work if there are no children.
 *   Fixed code to deallocate a node.
 *   In certain circmumstances, don't send termination signal when task
 *     itself terminating.
 *
 * Revision 1.7  1999/02/14 01:11:48  da0g
 * Updated to work with g++ 2.8.1 and egcs.
 *
 * Revision 1.6  98/09/15  18:45:21  da0g
 * Enhanced exceptions to support multiple-name resolution and Ref_Count (automatically-destroyed) Data.
 * 
 * Revision 1.5  1998/06/02 10:39:37  reids
 * Increased overall efficiency of TCM.
 *
 * Revision 1.4  98/01/30  14:50:50  reids
 * Updated to compile under gcc 2.7.2 and under Linux.
 * Also, made STRING "const char *" and changed API to take const arguments,
 *   where applicable.
 * 
 * Revision 1.3  97/12/29  17:06:21  reids
 * Version that has the basic functionality needed to support TDL.
 * 
 * Revision 1.2  97/12/04  17:50:11  reids
 * Another fairly stable version (except that monitors do not quite work)
 * 
 * Revision 1.1  97/11/21  14:06:32  reids
 * First release of TCM -- seems to be a stable version
 * 
 *
 **************************************************************************/

#ifndef INClist
#define INClist

#include <stdio.h>
#include "tcmBasics.h"

	/* Class pre-declarations. */
template <class T> class List_Iterate;
template <class T> class tcmList;

template <class T> class List_Element
{
  friend class tcmList<T>;

 public:
  List_Element(const T &data) : item(data) { next = previous = NULL; }
  List_Element *getNext(void) const { return next;}
  List_Element *getPrevious(void) const { return previous;}
  T *getItem(void) { return &item;}

  void reset(const T &theItem) { next = previous = NULL; item = theItem; }

 protected: 
  void addBefore(List_Element *otherElement);
  void addAfter(List_Element *otherElement);

 private:
  List_Element *next, *previous;
  T item;

  void setNext(List_Element *nextElement) { next = nextElement;};
  void setPrevious(List_Element *prevElement) { previous = prevElement;};
};

template <class T>
class tcmList
{
  friend class List_Iterate<T>;

 public:
  tcmList() { length = 0; first = last = NULL; };
  ~tcmList();

  List_Element<T> *getFirst(void) const { return first; }
  List_Element<T> *getLast(void) const { return last; }

  int getLength(void) const { return length; }
  unsigned int size(void) const { return length; }
  void clear (void);

  void insertFirst(const T &item) { _insertFirst(_allocate(item)); }
  void insertLast(const T &item) { _insertLast(_allocate(item)); }
  void insertAfter(const T &afterItem, const T &item);
  void insertAfter(const List_Iterate<T> & listIteration, const T &item);

  void removeFirst(void) { _delete(_removeFirst()); }
  void removeLast(void) { _delete(_removeLast()); }
  void removeItem(const T &item)
    { _delete(_removeElement(_findItem(item))); }

  BOOLEAN member(const T &item) const { return _findItem(item) != NULL; }

 protected: 
  void _insertFirst(List_Element<T> *listElement);
  void _insertLast(List_Element<T> *listElement);
  void _insertAfter(List_Element<T> *afterElement, 
		    List_Element<T> *listElement);

  List_Element<T> *_removeFirst(void);
  List_Element<T> *_removeLast(void);
  List_Element<T> *_removeElement(List_Element<T> *listElement);

  void _delete(List_Element<T> *listElement);
  List_Element<T> *_allocate(const T &item);
  List_Element<T> *_findItem(const T &item) const;

 private:
  unsigned int length;
  List_Element<T> *first, *last;

  static tcmList<T> _freeList;
  static T nullItem;
};

// The "const" class does not have the "removeCurrent" function.

template <class T> class Const_List_Iterate
{
 public:
  Const_List_Iterate() { _list = NULL; _current = NULL; }
  Const_List_Iterate(const tcmList<T> &theList) { set(&theList); }
  Const_List_Iterate(const tcmList<T> *listPtr) { set(listPtr); }

  List_Element<T> *getCurrent(void) const { return _current; }

  void reset(void) { _current = _list->getFirst(); }
  void resetLast(void) { _current = _list->getLast(); }
  void operator ++(int) { next(); }
  void operator --(int) { previous(); }
  operator void *() const { return _current; }

  T *operator ()() const { return getCurrent()->getItem();  }
  T * getPointer() const { return   (this -> operator()()); }
  T & getObject()  const { return * (this -> operator()()); }

 protected:
  tcmList<T> *_list;
  List_Element<T> *_current;

  void next(void) { if (_current) _current = _current->getNext(); }
  void previous(void) { if (_current) _current = _current->getPrevious(); }
  void set(const tcmList<T> *listPtr) { _list = (tcmList<T> *)listPtr; reset(); }
};

// This does not handle having items removed from the list during iteration
// Use "removeCurrent" instead.

template <class T> class List_Iterate : public Const_List_Iterate<T>
{
 using Const_List_Iterate<T>::_list;
 using Const_List_Iterate<T>::_current;

 public:
  List_Iterate() { _list = NULL; _current = NULL; _justDeleted = FALSE; }
  List_Iterate(tcmList<T> &theList)
    : Const_List_Iterate<T>(theList) { _justDeleted = FALSE; }
  List_Iterate(tcmList<T> *listPtr)
    : Const_List_Iterate<T>(listPtr) { _justDeleted = FALSE; }

  void removeCurrent(void);

  void operator ++(int) 
    { if (!_justDeleted) this->next();
      _justDeleted = FALSE; 
    } 
  void operator --(int) 
    { if (!_justDeleted) this->previous();
      _justDeleted = FALSE; 
    } 
  
 private:
  BOOLEAN _justDeleted;
};

#endif /* INClist */
