/*****************************************************************************
 * PROJECT: TDL Tutorial
 *
 * (c) Copyright 2001 Reid Simmons. All rights reserved.
 * (c) Copyright 1994 Richard Goodwin & Reid Simmons. All rights reserved.
 *
 * FILE: draw_env.h
 *
 * ABSTRACT:
 *
 * Interface to the environment drawing routines.
 *
 * $Source: /afs/cs.cmu.edu/project/TCA/Master/tcaV8/tutorial/simulator/draw_env.h,v $ 
 * $Revision: 1.4 $
 * $Date: 1996/08/05 16:10:16 $
 * $Author: rich $
 *
 * REVISION HISTORY:
 *
 * $Log: draw_env.h,v $
 *****************************************************************************/

#ifndef INCLUDED_DRAW_ENV
#define INCLUDED_DRAW_ENV

#include "ezx.h"

#define DRAW_LINE(window, x1, y1, x2, y2)\
EZX_DrawLine\
(window, ((int)(x1 * scale)), bm_h-((int)(y1 * scale)),\
 ((int)(x2 * scale)), bm_h-((int)(y2 * scale)))

#define DRAW_CIRCLE(window, xc, yc, radius)\
EZX_DrawCircle\
(window, ((int)(xc * scale)), bm_h-((int)(yc * scale)),\
 ((int)(radius * scale)))

#define SONAR_ON 1
#define SONAR_OFF 0

extern char env_name[DEFAULT_LINE_LENGTH];
extern double scale;
extern int displayp;
/* if true, then robot leaves a "slime" trail */
extern int DISPLAY_TRAIL; 

extern Drawable w_env;
extern Pixmap bitmap;
extern int bm_w, bm_h;
extern int useColor;

extern int mapWidth, mapHeight;

extern int robotColour;
extern int backColour;
extern int obstColour;
extern int roomColour;
extern int corrColour;
extern int doorColour;
extern int textColour;
extern int fovColour;

extern char *textFont;
extern char *buttonFont;

extern int sonarState;
extern int sonarWwidth;
extern int posWwidth;
extern int posWheight;
extern int rotation_mode;

void DrawMapOnBitmap(void);
void DisplayBitmap(int x, int y, int w, int h);
void RedrawMap(Widget w, XExposeEvent *ev, String *parm, Cardinal *n);

void plot_robot(int modus);
void plot_object(int i);

void RedrawWholeMap(void);
void RedrawSonar(Widget widget, XExposeEvent *ev, String *parm, 
		 Cardinal *n);
void RedrawPosition(Widget widget, XExposeEvent *ev,
		    String *parm, Cardinal *n);
void ResetTime(void);
void UpdateElapsedTime(void);
void RedrawTime(void);

#endif /* INCLUDED_DRAW_ENV */
