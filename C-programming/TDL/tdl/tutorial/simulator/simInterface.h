/*****************************************************************
 *
 * File: simInterface.h
 *
 * Abstract: Interface between simulator and IPC.
 *
 *****************************************************************/

#ifndef SIM_INTERFACE_H
#define SIM_INTERFACE_H

#define DEF_TRANS_VELOCITY  15.0
#define DEF_ROT_VELOCITY    30.0

extern void simStartBoundedMove (CMS distance);
extern void simStartGuardedMove (CMS distance, CMS stopRange);
extern void simStartTurn (DEGREES turn);
extern void simStop (void);
extern void simSetVelocity(double transVelocity, double rotVelocity);
extern void simVelocity(double *transVelocity, double *rotVelocity);
extern void simTalk(char *message);
extern void sonarScan(CMS sonarData[24]);
extern int simInMotion(void);

#endif /* SIM_INTERFACE_H */
