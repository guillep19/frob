/*****************************************************************************
 * PROJECT: TDL Tutorial
 *
 * (c) Copyright 2001 Reid Simmons. All rights reserved.
 * (c) Copyright 1994 Richard Goodwin & Reid Simmons. All rights reserved.
 *
 * FILE: updates.h
 *
 * ABSTRACT: This file contains the constants that defines the interface 
 *           to the update routines.
 *
 * $Source: /afs/cs.cmu.edu/project/TCA/Master/tcaV8/tutorial/simulator/updates.h,v $ 
 * $Revision: 1.3 $
 * $Date: 1996/08/05 16:10:26 $
 * $Author: rich $
 *
 * REVISION HISTORY:
 *
 * $Log: updates.h,v $
 *****************************************************************************/

#ifndef INCLUDED_UPDATES
#define INCLUDED_UPDATES

#define CTRL_CONT 1
#define CTRL_STEP 0

#define NEXT_PAUSE 1
#define NEXT_CONT  0

extern unsigned long timeScale;		/* in msec */
extern unsigned long currentTime;
extern unsigned long timeIncrement;
extern int refreshRate;
extern int continuallyUpdateLasers;

void updatePosition(unsigned long tMsec);
void forceUpdate(void);
unsigned long getTime(void);		/* get current time in msecs */
void resetTime(void);
void refreshSimDisplay(int force);
void simUpdate(void);

#endif /* INCLUDED_UPDATES */
