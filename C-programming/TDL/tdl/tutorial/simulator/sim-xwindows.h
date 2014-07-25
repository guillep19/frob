/*****************************************************************************
 * PROJECT: TDL Tutorial
 *
 * (c) Copyright 2001 Reid Simmons. All rights reserved.
 * (c) Copyright 1994 Richard Goodwin & Reid Simmons. All rights reserved.
 *
 * FILE: sim-xwindows.h
 *
 * ABSTRACT:
 *
 * Main header file for the simulator.
 *
 * $Source: /afs/cs.cmu.edu/project/TCA/Master/tcaV8/tutorial/simulator/sim-xwindows.h,v $ 
 * $Revision: 1.4 $
 * $Date: 1996/08/05 16:10:21 $
 * $Author: rich $
 *
 * REVISION HISTORY:
 *
 * $Log: sim-xwindows.h,v $
 *****************************************************************************/

#ifndef INCLUDED_SIMX
#define INCLUDED_SIMX

#include "ezx.h"

extern Widget sonarW, bitmapW;
extern Widget toplevelW, outerW, viewportW, teleportW, quitW;
extern Widget sonarbuttonW, timelabelW, elapsedtimelabelW;
extern Widget xposW, yposW, angleW;
extern XtAppContext app_context;

void init_simx(char *fname);

void Quit(Widget widget, XtPointer closure, XtPointer callData);
void ToggleSonar(Widget w, XtPointer closure,  XtPointer callData);
void TeleportRobot(Widget w, XtPointer closure, XtPointer callData);

#endif /* INCLUDED_SIMX */
