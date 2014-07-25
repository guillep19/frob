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
#include "distributed.h"

template <class T> tcmList<T> tcmList<T>::_freeList;
template <class T> T tcmList<T>::nullItem;

template class List_Element<Distributed_Task>;
template class tcmList<Distributed_Task>;

template class List_Element<Agent_Connection>;
template class tcmList<Agent_Connection>;

/*
 * gcc complains about not using <hash_map>, but that is not actually 
 * allowable under my current compiler (3.2.2).  This turns off the warnings.
 */
#define _CPP_BACKWARD_BACKWARD_WARNING_H
#define _BACKWARD_BACKWARD_WARNING_H
#include <hash_map.h>
template class hash_map<STRING, TCM_Exception::Creator, hash<STRING>, eqstr>;
