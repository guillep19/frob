/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

#define _TDL_INTERNAL_
#include "tdl.H"

#include "_TDL_BaseAction.H"

/*virtual*/ _TDL_BaseAction::~_TDL_BaseAction()
{
  tcmTaskTreeNodePointer = (Task_Tree_Node *) NULL;
}

