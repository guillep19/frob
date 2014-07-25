/*
 * This is used as part of the generated RESUME functionality, so we can
 * keep track of what the current TCM Task is...  It's necessary for the
 * _TDL_INTERNAL_RESUME macro to work properly.
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

#ifndef _TDL_BaseAction_H
#define _TDL_BaseAction_H

#include <tcm.h>
#include <tdldef.H>

class _TDL_BaseAction : public _Action
{
protected:
	/* Note:  Intentionally not using a TCM_Task_Tree_Ref here to avoid *
	 * looping constructs that would inhibit garbage collection.        */
  Task_Tree_Node * tcmTaskTreeNodePointer;

public:
  _TDL_BaseAction()
    : tcmTaskTreeNodePointer ( (Task_Tree_Node *) NULL )
  {}

  virtual ~_TDL_BaseAction();

  Task_Tree_Node  * _TDL_getTcmTaskTreeNode() const
    { return tcmTaskTreeNodePointer; }

  _TDL_BaseAction * _TDL_setTcmTaskTreeNodeAndVerifyActionsMatch (
				   Task_Tree_Node * theTcmTaskTreeNodePointer,
				   STRING           theStaticActionClassName,
				   const char *     theLocation )
    {
      tcmTaskTreeNodePointer = theTcmTaskTreeNodePointer;
      TDL::verifyResumeMatchesAction ( theTcmTaskTreeNodePointer,
				       theStaticActionClassName,
				       theLocation );
      return this;
    }

  void _TDL_clearTcmTaskTreeNode()
    { tcmTaskTreeNodePointer = ((Task_Tree_Node *)NULL); }
};

#endif /* _TDL_BaseAction_H */

