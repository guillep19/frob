/*****************************************************************************
 * PROJECT: TDL Tutorial
 *
 * (c) Copyright 2001 Reid Simmons. All rights reserved.
 * (c) Copyright 1993 Richard Goodwin & Joseph O'Sullivan. All rights reserved.
 *
 * FILE: ezx.h
 *
 * ABSTRACT: Interface to the modified version of ezx11 in ezx.c.
 *
 * $Source: /afs/cs.cmu.edu/project/TCA/Master/tcaV8/utils/ezx.h,v $ 
 * $Revision: 1.7 $
 * $Date: 96/08/05 16:15:09 $
 * $Author: rich $
 *
 * REVISION HISTORY:
 * $Log:	ezx.h,v $
 *****************************************************************************/

#ifndef EZX_H
#define EZX_H

#if defined(__STDC__)
#define FUNCPROTO 0xF
#define XTFUNCPROTO 0xF
#else
#undef FUNCPROTO
#undef XTFUNCPROTO
#endif

#include <X11/Xlib.h>
#include <X11/Intrinsic.h>
#include <X11/X.h>

#include <X11/Xatom.h>
#include <X11/IntrinsicP.h>
#include <X11/Core.h>
#include <X11/CoreP.h>
#include <X11/StringDefs.h>

#include <X11/Xaw/Command.h>
#include <X11/Xaw/Form.h>

#include <X11/Xaw/Label.h>
#include <X11/Xaw/Viewport.h>

#include <X11/Shell.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Cardinals.h>
#include <X11/Xaw/Dialog.h>


/* colors */

typedef enum  {
  C_GREY, C_RED, C_PINK, C_ORANGE, C_YELLOW, C_GOLD,
  C_LAWNGREEN, C_BLUE, C_VIOLET,C_PURPLE, C_BROWN,
  /* Sebastians favourite colours. */
  C_MEDIUMPURPLE3,C_CYAN,C_KHAKI4,C_TURQUOISE4,
  C_WHITE,C_BLACK
} theColours;

typedef enum {
  C_GREY0=C_BLACK, C_GREY5, C_GREY10, C_GREY15, C_GREY20,
  C_GREY25, C_GREY30, C_GREY35, C_GREY40, C_GREY45, C_GREY50,
  C_GREY55, C_GREY60, C_GREY65, C_GREY70, C_GREY75, C_GREY80,
  C_GREY85, C_GREY90, C_GREY95, C_GREY100
} theGreys;

typedef enum {
  darkgreen=C_GREY100+1, darkolivegreen, green4,
  forestgreen, darkolivegreen4, springgreen4, palegreen4, seagreen,
  darkseagreen4, lightseagreen, mediumseagreen, darkseagreen,
  green3, limegreen, darkolivegreen3, springgreen3,
  palegreen3, seagreen3, darkseagreen3, green2, darkolivegreen2,
  springgreen2, palegreen2, seagreen2, darkseagreen2,
  mediumspringgreen, palegreen, lawngreen, green, green1,
  greenyellow,  springgreen, springgreen1,
  palegreen1, seagreen1, darkseagreen1, darkolivegreen1
} theGreens;

typedef enum {
  midnightblue=darkolivegreen1+1, navyblue, blue4,
  slateblue4, darkslateblue, royalblue4, dodgerblue4, steelblue4,
  deepskyblue4, skyblue4, lightskyblue4, lightsteelblue4, lightblue4,
  cadetblue4, cadetblue, steelblue, mediumblue, blue3, slateblue3,
  slateblue, royalblue3, dodgerblue3, steelblue3, deepskyblue3,
  skyblue3, lightsteelblue3, lightskyblue3, lightblue3, cadetblue3,
  lightsteelblue, royalblue, blueviolet, lightblue, powderblue, skyblue,
  cornflowerblue, blue2, slateblue2, mediumslateblue, royalblue2,
  dodgerblue2, steelblue2, deepskyblue2, skyblue2, lightsteelblue2,
  lightskyblue2, lightblue2, cadetblue2, lightskyblue, blue, blue1,
  slateblue1, lightslateblue, royalblue1, dodgerblue, dodgerblue1,
  steelblue1, deepskyblue, deepskyblue1, skyblue1, lightsteelblue1,
  lightskyblue1, lightblue1, cadetblue1, aliceblue
} theBlues;

typedef enum {
  yellow4=C_GREY100+1, lightyellow4, yellow3,
  lightyellow3, yellow2, lightyellow2,
  lightgoldenrodyellow, yellow, yellow1,
  lightyellow, lightyellow1 	} theYellows;

#define MIN_GREY C_GREY0
#define MAX_GREY C_GREY100
#define NUM_GREY (MAX_GREY - MIN_GREY +1)

#define MIN_YELLOW yellow4
#define MAX_YELLOW lightyellow1
#define NUM_YELLOW (MAX_YELLOW - MIN_YELLOW +1)

#define MAXCOLORS	MAX_YELLOW+1

extern GC               theGC;
extern int	    	theScreen;		/* Screen number */
extern Display	       *theDisplay;		/* X server connection */
extern int	     	theDepth;		/* 1 if monochrome */
extern Colormap	        theColormap;
extern unsigned long	theBlackPixel;
extern unsigned long	theWhitePixel;
extern XFontStruct	*theFont;

extern unsigned long	thePixels[MAXCOLORS];
extern char	*theColorNames[];

#ifdef __cplusplus
extern "C" {
#endif

void EZX_InitDefaultColors(void);

void EZX_initGC(Widget w);

void EZX_InitGraphics(char *display, char *program);

void EZX_SetColor(int color);	      /* set foreground color */

void EZX_SetLineWidth(int line_width);

void EZX_waitForMap(void);

Window EZX_MakeWindow(char *title, unsigned int width, unsigned int height,
		      char *position /* eg, "+4-4" */);

void EZX_ClearWindow(Window w);

void EZX_EndWindow(Window w);

void EZX_DrawCircle(Drawable w, int x, int y, int r);

void EZX_DrawPoint(Drawable w, int x, int y);

void EZX_DrawGrey(Drawable w, int x, int y, int g);

void EZX_DrawLine(Drawable w, int Ax, int Ay, int Bx, int By);

void EZX_ClearRectangle(Drawable w, int left, int top, int width, int height);

void EZX_DrawRectangle(Drawable w, int x, int y, 
		       unsigned int width, unsigned int height);

void EZX_FillPolygon(Drawable w, int npoints, XPoint *points);

void EZX_FillCircle(Drawable w, int x, int y, int r);

void EZX_FillRectangle(Drawable w, int x, int y,
		       unsigned int width, unsigned int height);

void EZX_Flush(void);

void EZX_UseFont(GC theNewGC, char fontname[]);

void EZX_FreeFont(void);

void EZX_DrawString(Drawable w, int x, int y, char *string);

void EZX_DrawText(Drawable w, int x, int y, char *string);

void EZX_DrawTextAt(Drawable w, int x, int y, char *string, char style);

void EZX_DrawStringAt(Drawable w, int x, int y, char style, char *string);

void EZX_FormatAt(Drawable w, int x, int y, int background_filled,
		  int underlined, char style, char *string);

int EZX_blackWhite(void);

Pixmap EZX_MakePixmap(Window w);

void EZX_CopyPixmapToWindow(Pixmap p, Window w);

int EZX_GetFontHeight(void);

int EZX_GetTextWidth(char string[]);

#ifndef CURSORX_LOADED
#include "cursorx.h"
#endif

void EZX_DestroyPopupPrompt(Widget widget, XtPointer client_data,
			    XtPointer call_data);

void EZX_CenterPopup(Widget button, Widget popup);

#ifdef __cplusplus
}
#endif

#endif /* EZX_H */
