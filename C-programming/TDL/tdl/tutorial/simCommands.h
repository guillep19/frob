/*****************************************************************************
 * PROJECT: TDL Tutorial
 *
 * (c) Copyright 2001 Reid Simmons. All rights reserved.
 *
 * FILE: simCommands.h
 *
 * DESCRIPTION:	TDL connection to simulator.
 *
 * $Source: /afs/cs.cmu.edu/project/TCA/Master/tcaV8/tutorial/simulator/simCommands,v $ 
 * $Revision: 1.6 $
 * $Date: 1996/08/05 16:10:22 $
 * $Author: rich $
 *
 * REVISION HISTORY:
 *
 * $Log: simCommands,v $
 *****************************************************************************/

#ifndef INC_SIMCOMMANDS
#define INC_SIMCOMMANDS

#include <tdl.H>
#include <ipc.h>

#define isSuccess(ref) (!strcmp(IPC_msgInstanceName(ref), SUCCESS_MSG))

#define sendSimCommand(cmdName, dataPtr, handler, tcmRef) \
  IPC_queryNotifyData(cmdName, (void *)dataPtr, (HANDLER_TYPE)handler, \
                      (void *)new TCM_Task_Tree_Ref(tcmRef))

#endif // INC_SIMCOMMANDS
