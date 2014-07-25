/*****************************************************************************
 * PROJECT: TDL Tutorial
 *
 * (c) Copyright 2001 Reid Simmons. All rights reserved.
 * (c) Copyright 1994 Richard Goodwin. All rights reserved.
 *
 * FILE: billboard.h
 *
 * ABSTRACT: Simple graphics package for TDL tutorial. Draws stuff.
 *
 * The following functions are implemented:
 *              BB_init
 *              BB_clear
 *		BB_fill
 *		BB_drawSquare
 *		BB_drawCircle
 *		BB_drawLine
 *		BB_setColour
 *		BB_getColour
 *
 * Very little error checking is done, X may crash if you use 
 * unreasonable parameter values.
 *
 * $Source: /afs/cs.cmu.edu/project/TCA/Master/tcaV8/tutorial/simulator/billBoard.h,v $ 
 * $Revision: 1.6 $
 * $Date: 1996/08/05 16:10:22 $
 * $Author: rich $
 *
 * REVISION HISTORY:
 *
 * $Log: billBoard.h,v $
 *****************************************************************************/

#ifndef INC_BILLBOARD
#define INC_BILLBOARD

#define BB_WIDTH  300
#define BB_HEIGHT 300

#define BILLBOARD_NAME "billboard"

extern XtAppContext app_context;

void BB_init (void);
void BB_clear (void);
void BB_fill (const char *colour);
void BB_drawSquare (int top, int left, int size);
void BB_drawCircle (int xc, int yc, int radius, int filled);
void BB_drawLine (int x0, int y0, int x1, int y1);
void BB_setColour (const char *colour);
const char*BB_getColour (void);

#endif /* INC_BILLBOARD */
