/*
 *  EZX11.h - Header File for Multiple Window Graphics Interface to X11 (R3)
 *
 *  Created by gth.
 *  Modified by ljl (Aug 1, 1990).
 */

#ifndef CURSORX_LOADED
#define CURSORX_LOADED

#ifndef EZX_H
#include "ezx.h"
#endif

#define	LEFT_BUTTON	Button1	/* button detail */
#define MIDDLE_BUTTON	Button2
#define RIGHT_BUTTON	Button3
#define OTHER_BUTTON	Button4

/* cursor routines */

#ifdef __cplusplus
extern "C" {
#endif

/* get cursor position when button pressed */
int EZX_GetCursor(int *xp, int *yp);
int EZX_GetCursorw(int *xp, int *yp, Window *win);

/* set flag when button pressed */
int EZX_TestCursor(Window w);

/* get cursor position when button pressed */
int EZX_TestGetCursor(Window w, int *xp, int *yp);
int EZX_TestGetCursorw(int *xp, int *yp, Window *win);

/* get cursor positions when button pressed and released */
int EZX_GetCursors( int	*xp, int *yp, int *xr, int *yr);
int EZX_GetCursorsw(int *xp, int *yp, int *xr, int *yr, Window *win);

void EZX_bell(void);

int EZX_block_and_wait(struct timeval *timeout);

int EZX_WaitForButtonIn(XtAppContext app_context,
			Widget widget, int *xp, int *yp);

#ifdef __cplusplus
}
#endif

#endif /* CURSORX_LOADED */
