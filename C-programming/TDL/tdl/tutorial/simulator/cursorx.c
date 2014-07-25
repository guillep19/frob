/*
 *  cursorx.c
 *
 *  Created by Goang-Tay Hsu (gth).
 *  Modified by Long-Ji Lin (ljl) at Aug 1, 1990.
 *
 */

#include <errno.h>

#include "common.h"
#include "cursorx.h"
#include "ezx.h"

static XButtonEvent theButtonEvent;
static XEvent theEvent;
static XMotionEvent theMotionEvent;

/* For holding the button event */


void EZX_bell(void)
{
  putc(7,stderr);
}

/* get cursor position when button pressed */

int EZX_GetCursor(int *xp, int *yp)
{
  return EZX_GetCursorw(xp,yp,NULL);
}

int EZX_GetCursorw(int *xp, int *yp, Window *win)
{
  do {
    XNextEvent(theDisplay, &theEvent);
    theButtonEvent=theEvent.xbutton;
  } while (theButtonEvent.type != ButtonPress);
  
  if (win!=NULL)  *win  = theButtonEvent.window;
  *xp = theButtonEvent.x;
  *yp = theButtonEvent.y;
  
  if (theButtonEvent.button == Button1) return(LEFT_BUTTON);
  else if (theButtonEvent.button == Button2) return(MIDDLE_BUTTON);
  else if (theButtonEvent.button == Button3) return(RIGHT_BUTTON);
  else return(OTHER_BUTTON);
}

/* get cursor position when button pressed */
int EZX_TestGetCursorw(int *xp, int *yp, Window *win)
{
  *xp = -1;
  *yp = -1;
  
  if (XCheckMaskEvent(theDisplay, ButtonPressMask, &theEvent)){
    
    theButtonEvent=theEvent.xbutton;
    if (win != NULL)
      *win = theButtonEvent.window; /* this is the window the button
				     * was pressed in */
    *xp = theButtonEvent.x;
    *yp = theButtonEvent.y;
    
    if (theButtonEvent.button == Button1) return(LEFT_BUTTON);
    else if (theButtonEvent.button == Button2) return(MIDDLE_BUTTON);
    else if (theButtonEvent.button == Button3) return(RIGHT_BUTTON);
    else return(OTHER_BUTTON);
  }
  
  else if (XCheckTypedEvent(theDisplay, MotionNotify, &theEvent)){
    
    do{
      theMotionEvent=theEvent.xmotion;
      if (win != NULL)
	*win = theMotionEvent.window; /* this is the window pointer
				       * was moved in */
      *xp = theMotionEvent.x;
      *yp = theMotionEvent.y;
    }
    while(XCheckTypedEvent(theDisplay, MotionNotify, &theEvent));
    
    return -1;
  }
  
  else{				/* ...nothing found here */
    *xp = -1;
    *yp = -1;
    return -1;
  }
}



/* get cursor position when button pressed */
int EZX_TestGetCursor(Window w, int *xp, int *yp)
{
#ifndef linux
  Window win=NULL;
#else
  Window win=0;
#endif
  int ret_value;
  
  ret_value = EZX_TestGetCursorw(xp, yp, &win);
  if (win != w)
    *xp = *yp = ret_value = -1;
  return  ret_value;
}





/* set flag when button pressed */
int EZX_TestCursor(Window w)
{
  
  
  XCheckMaskEvent(theDisplay, ButtonPressMask, &theEvent);
  
  if (w==(Window)NULL)   
    return (theEvent.xbutton.type == ButtonPress);
  else
    return ((theEvent.xbutton.type == ButtonPress) &&
	    (theEvent.xbutton.window==w));
}


/* get cursor positions when button pressed and released */
int EZX_GetCursors(int *xp, int *yp, int *xr, int *yr)
{
  return EZX_GetCursorsw(xp, yp, xr, yr, NULL);
}

int EZX_GetCursorsw(int *xp, int *yp, int *xr, int *yr, Window *win)
{
  do {
    XNextEvent(theDisplay, &theEvent);
    theButtonEvent=theEvent.xbutton;
  } while (theButtonEvent.type != ButtonPress);
  
  if (win!=NULL) *win=theButtonEvent.window;
  *xp = theButtonEvent.x;
  *yp = theButtonEvent.y;
  
  do {
    XNextEvent(theDisplay, &theEvent);
    theButtonEvent=theEvent.xbutton;
  } while (theButtonEvent.type != ButtonRelease);
  
  *xr = theButtonEvent.x;
  *yr = theButtonEvent.y;
  
  if (theButtonEvent.button == Button1) return(LEFT_BUTTON);
  else if (theButtonEvent.button == Button2) return(MIDDLE_BUTTON);
  else if (theButtonEvent.button == Button3) return(RIGHT_BUTTON);
  else return(OTHER_BUTTON);
}


int EZX_block_and_wait(struct timeval *timeout)
{
  int stat, xfd;
  fd_set readMask;
  
  xfd = XConnectionNumber(theDisplay);
  
  FD_ZERO(&readMask);
  
  FD_SET(xfd,&readMask);
  
  do {
    stat = select(FD_SETSIZE, &readMask, NULL, NULL, timeout);
  } while (stat < 0 && errno == EINTR);
  
  if (stat > 0)
    {
      if (FD_ISSET(xfd,&readMask))
	return 1;
      else
	return 2;
    }
  else return 0;
}


/*****************************************************************************
 *
 * FUNCTION: int EZX_WaitForButtonIn(Widget widget, int *xp, int *yp)
 *
 * DESCRIPTION: Wait until a button is pressed in the given widget.  
 *              Sets the x & y co-ordinates (relative to the widget) and
 *              returns which button was pushed.
 *
 * INPUTS:
 *
 * OUTPUTS: 
 *
 * HISTORY:
 *
 *****************************************************************************/


/* This dummy procedure is used for for a parameter in an  X Toolkit procedure 
 *  in TeleportRobot to prevent the system from crashing if I used NULL
 * as the procedure parameter instead 
 */
static void do_nothing(void)
{}

int EZX_WaitForButtonIn(XtAppContext app_context,
			Widget widget, int *xp, int *yp)
{
  XEvent event;
  int button_was_pressed = FALSE;

  XtAddEventHandler(widget, ButtonPressMask, FALSE,
		    (XtEventHandler) do_nothing, NULL); 
  
  do {
    XtAppNextEvent(app_context, &event);
    if (event.type == ButtonPress) {
      button_was_pressed = TRUE;   
      theButtonEvent=event.xbutton;
    }
  } while (!button_was_pressed);
  
  /* Disable being able to press the mouse button over the bitmap */
  XtRemoveEventHandler(widget, ButtonPressMask, FALSE,
		       (XtEventHandler) do_nothing, NULL); 
  *xp = theButtonEvent.x;
  *yp = theButtonEvent.y;
  
  if (theButtonEvent.button == Button1) return(LEFT_BUTTON);
  else if (theButtonEvent.button == Button2) return(MIDDLE_BUTTON);
  else if (theButtonEvent.button == Button3) return(RIGHT_BUTTON);
  else return(OTHER_BUTTON);
}
