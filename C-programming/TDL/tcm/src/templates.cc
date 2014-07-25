/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tcm/LICENSE.TXT)
 */
#include <event.h>
#include <agenda.h>
#include <exception.h>
#include <taskTree.h>

// Since all the list functions are template functions, need to include
// the CC file here, so the right template functions can be generated.
#include "tcmList.cc"

template class tcmHandle<Task_Tree_Node>;
template class tcmHandle<_Action>;
template class tcmHandle<Ref_Count>;

template <class T> tcmList<T> tcmList<T>::_freeList;
template <class T> T tcmList<T>::nullItem;

template class tcmList<Event>;
template class tcmList<State_Event>;
template class tcmList<Task_Tree_Ref>;

template class List_Element<Event>;
template class Const_List_Iterate<Event>;
template class List_Iterate<Event>;

template class List_Element<State_Event>;
template class Const_List_Iterate<State_Event>;
template class List_Iterate<State_Event>;

template class List_Element<Task_Tree_Ref>;
template class List_Iterate<Task_Tree_Ref>;
template class Const_List_Iterate<Task_Tree_Ref>;
template Task_Tree_Ref makeHandle(Task_Tree_Node *);

template class tcmList<Pending_Event>;
template class List_Element<Pending_Event>;
template class queue<Pending_Event>;

template class queue<Task_Tree_Ref>;

template class List_Element<Timed_Event>;
template class Const_List_Iterate<Timed_Event>;
template class List_Iterate<Timed_Event>;
template class tcmList<Timed_Event>;
template class queue<Timed_Event>;
template class priority_queue<Timed_Event, Timed_Event_Comp>;

template class List_Element<External_Event>;
template class tcmList<External_Event>;
template class Const_List_Iterate<External_Event>;

template class tcmHandle<Exception_Handler>;
template class List_Element<Exception_Handler_Ref>;
template class tcmList<Exception_Handler_Ref>;
template class List_Iterate<Exception_Handler_Ref>;
template class Const_List_Iterate<Exception_Handler_Ref>;
