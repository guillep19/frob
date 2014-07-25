/**************************************************************************
 * 
 * PROJECT: Task Control Management.
 *
 * (c) Copyright 1996 Reid Simmons.  All rights reserved.
 *
 * FILE: action.h
 *
 * ABSTRACT: Defines a hierarchy of actions to take when task tree nodes
 *           are "executed".  Includes classes for functional callback,
 *           message passing.  Future classes may include task spawning 
 *           and process forking.
 *
 * EXPORTS:
 *
 * $Revision: 1.7 $
 * $Date: 2009/01/15 17:00:04 $
 * $Author: reids $
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tcm/LICENSE.TXT)
 *
 * REVISION HISTORY:
 *
 * $Log: action.h,v $
 * Revision 1.7  2009/01/15 17:00:04  reids
 * Released under simplified BSD open source license
 *
 * Revision 1.6  1998/12/16 01:58:45  reids
 * Added an == operator definition for actions.
 *
 * Revision 1.5  98/01/30  14:50:49  reids
 * Updated to compile under gcc 2.7.2 and under Linux.
 * Also, made STRING "const char *" and changed API to take const arguments,
 *   where applicable.
 * 
 * Revision 1.4  97/12/22  16:52:51  reids
 * Basically, added "data" field for CALLBACK_ACTION,
 *  and started using "nodeData" for the activation data associated
 *  with monitors, and the failure data associated with exceptions.
 * 
 * Revision 1.3  97/12/18  00:21:40  reids
 * Changing ACTION_PTR to a handle, to aid in garbage collection.
 * 
 * Revision 1.2  97/12/04  17:50:02  reids
 * Another fairly stable version (except that monitors do not quite work)
 * 
 * Revision 1.1  97/11/21  14:06:20  reids
 * First release of TCM -- seems to be a stable version
 * 
 *
 **************************************************************************/

#ifndef INCaction
#define INCaction

#include "tcmBasics.h"
#include "tcm.h"
#include "tcmPriv.h"

typedef class Callback_Action *CALLBACK_ACTION_PTR;

class Callback_Action : public _Action
{
 public:
  Callback_Action() { _callback = NULL; }
  Callback_Action(const CALLBACK_FN_TYPE callbackFn,
		  const void *data = NULL) 
    { _callback = callbackFn; _data = data; }

  void execute (Task_Tree_Ref const &node);

  virtual BOOLEAN operator==(const Callback_Action &otherAction) const
    { return (_callback == otherAction._callback &&
	      _data == otherAction._data); }

 protected:
  CALLBACK_FN_TYPE _callback;
  const void *_data;
};

typedef class Ext_Callback_Action *EXT_CALLBACK_ACTION_PTR;

class Ext_Callback_Action : public Callback_Action
{
 public:
  Ext_Callback_Action() : Callback_Action() { _classData = NULL; }
  Ext_Callback_Action(const CALLBACK_FN_TYPE callbackFn,
		      const void *data = NULL, const void *classData = NULL)
    : Callback_Action(callbackFn, data) { _classData = classData;}

  void execute (Task_Tree_Ref const &node);

  virtual BOOLEAN operator==(const Ext_Callback_Action &otherAction) const
    { return (_callback == otherAction._callback &&
	      _data == otherAction._data &&
	      _classData == otherAction._classData); }

 private:
  EXT_CALLBACK_FN_TYPE _callback;
  const void *_classData;
};

#if 0
class Message_Action : public _Action
{
 public:
  Message_Action() { msgName = NULL; msgData = NULL;}
  Message_Action(STRING name, const void *data = NULL)
    { msgName = name; msgData = data;}

  void execute (Task_Tree_Ref const &node);

 private:
  STRING msgName;
  const void *msgData;
};
#endif

#endif /* INCaction */
