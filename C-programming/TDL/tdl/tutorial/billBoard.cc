/*****************************************************************************
 * PROJECT: TDL Tutorial
 *
 * (c) Copyright 2001 Reid Simmons. All rights reserved.
 * (c) Copyright 1994 Richard Goodwin. All rights reserved.
 *
 * FILE: billboard.c
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
 * $Source: /afs/cs.cmu.edu/project/TCA/Master/tcaV8/tutorial/simulator/billBoard.c,v $ 
 * $Revision: 1.6 $
 * $Date: 1996/08/05 16:10:22 $
 * $Author: rich $
 *
 * REVISION HISTORY:
 *
 * $Log: billBoard.c,v $
 *****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "ezx.h"
#include "billBoard.h"

/* Global Variables */
Window theWindow;
XtAppContext app_context;
int theColour = C_RED;

/* Utility routines. */
static void setColour(const char *colour)
{
  if (strstr(colour,"red")) {
    theColour = C_RED;
  } else if (strstr(colour,"white")) {
    theColour = C_WHITE;
  } else if (strstr(colour,"blue")) {
    theColour = C_BLUE;
  } else if (strstr(colour,"green")) {
    theColour = C_LAWNGREEN;
  } else if (strstr(colour,"yellow")) {
    theColour = C_YELLOW;
  } else {
    printf("Unknown Colour\n");
  }
  fflush(stdout);
  EZX_SetColor(theColour);
}

static char *getColour(void)
{
  if (theColour == C_RED) {
    return "red";
  } else if (theColour == C_WHITE) {
    return "white";
  } else if (theColour == C_BLUE) {
    return "blue";
  } else if (theColour == C_LAWNGREEN) {
    return "green";
  } else if (theColour == C_YELLOW) {
    return "yellow";
  } else if (theColour == C_WHITE) {
    return "white";
  } else {
    return "unknown";
  }
}

void BB_clear (void)
{
  EZX_ClearWindow(theWindow); 
}

void BB_fill (const char *colour)
{
  setColour(colour);
  EZX_FillRectangle(theWindow, 0, 0, BB_WIDTH, BB_HEIGHT);
  EZX_Flush();
}

void BB_drawSquare (int top, int left, int size)
{
  EZX_FillRectangle(theWindow, left, top, size, size);
  EZX_Flush();
}

void BB_drawCircle (int xc, int yc, int radius, int filled)
{
  if (filled)
    EZX_FillCircle(theWindow, xc, yc, radius);
  else
    EZX_DrawCircle(theWindow, xc, yc, radius);
  EZX_Flush();
}

void BB_drawLine (int x0, int y0, int x1, int y1)
{
  EZX_DrawLine(theWindow, x0, y0, x1, y1);
  EZX_Flush();
}

void BB_setColour (const char *colour)
{
  setColour(colour);
}

const char *BB_getColour (void)
{
  return getColour();
}

void BB_init (void)
{
  /* Initialize X11. */
  EZX_InitGraphics(getenv("DISPLAY"), BILLBOARD_NAME);

  /* Create the window */
  theWindow = EZX_MakeWindow(BILLBOARD_NAME, BB_WIDTH, BB_HEIGHT, "+20+20");
  //app_context = XtWidgetToApplicationContext(theWindow);
  app_context = XtCreateApplicationContext();

  /* Set the initial color */
  EZX_SetColor(theColour);
}

