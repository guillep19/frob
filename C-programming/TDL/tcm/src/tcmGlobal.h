/*****************************************************************************
 * 
 * PROJECT: Task Control Management.
 *
 * (c) Copyright 1996 Reid Simmons.  All rights reserved.
 *
 * FILE: tcmGlobal.h
 *
 * ABSTRACT: (Internal) global variables
 *
 * $Source: /afs/cs.cmu.edu/project/TCA/Master/tcaV8/tcm/src/tcmGlobal.h,v $ 
 * $Revision: 1.3 $
 * $Date: 2009/01/15 17:00:05 $
 * $Author: reids $
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tcm/LICENSE.TXT)
 *
 * REVISION HISTORY:
 *
 * $Log: tcmGlobal.h,v $
 * Revision 1.3  2009/01/15 17:00:05  reids
 * Released under simplified BSD open source license
 *
 * Revision 1.2  1999/06/06 13:48:09  reids
 * Changes to TCM 1.3.0:
 *   New list data structures.
 *   New, generalized logging interface.
 *   Fixed firstChild to work if there are no children.
 *   Fixed code to deallocate a node.
 *   In certain circmumstances, don't send termination signal when task
 *     itself terminating.
 *
 * Revision 1.1  97/11/21  14:06:44  reids
 * First release of TCM -- seems to be a stable version
 * 
 *
 *****************************************************************************/

#ifndef INCtcmGlobal
#define INCtcmGlobal

#include "agenda.h"
#include "taskTree.h"
#include "tcmLogging.h"
#include "tcmLogInterface.h"

#define GET_TCM_GLOBAL(x) (globalState.x)

class Global_State 
{
 public:
  Agenda agenda;
  Task_Tree_Ref rootNode;
  Logger terminalLogger, fileLogger;
  Logging_Fns loggingFns;
};

extern Global_State globalState;

void initializeGlobalState(void);

#endif /* INCtcmGlobal */
