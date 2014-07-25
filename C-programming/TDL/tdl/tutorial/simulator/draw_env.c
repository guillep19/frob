/*****************************************************************************
 * PROJECT: TDL Tutorial
 *
 * (c) Copyright 2001 Reid Simmons. All rights reserved.
 * (c) Copyright 1994 Richard Goodwin & Reid Simmons. All rights reserved.
 *
 * FILE: draw_env.c
 *
 * ABSTRACT:
 * 
 * This file provides the routines to draw the environment in an X11 window,
 * including the sonar readings and the time buttons.
 *
 * $Source: /afs/cs.cmu.edu/project/TCA/Master/tcaV8/tutorial/simulator/draw_env.c,v $ 
 * $Revision: 1.3 $
 * $Date: 1995/04/07 05:09:45 $
 * $Author: rich $
 *
 * REVISION HISTORY:
 *
 * $Log: draw_env.c,v $
 *****************************************************************************/

#include <math.h>

#include "common.h"
#include "ezx.h"
#include "sim-xwindows.h"
#include "sensors.h"

#include "draw_env.h"
#include "environment.h"
#include "files.h"
#include "action.h"
#include "updates.h"

/*****************************************************************************
 * Global constants
 *****************************************************************************/

/*****************************************************************************
 * Global variables
 *****************************************************************************/

char env_name[DEFAULT_LINE_LENGTH] = "NoName";
double scale = 1.0;
int displayp = TRUE;
int DISPLAY_TRAIL=TRUE; 
/* if true, then robot leaves a "slime" trail */

Drawable w_env=(Drawable) NULL;
Pixmap bitmap;
int bm_w=0, bm_h=0;
int useColor=TRUE;

int mapWidth = 300, mapHeight = 300;

int robotColour = C_LAWNGREEN;
int backColour  = C_BLACK;
int obstColour  = C_BLUE;
int roomColour  = C_WHITE;
int corrColour	= C_YELLOW;
int doorColour  = C_ORANGE;
int textColour  = C_RED;
int fovColour   = C_VIOLET;

char *textFont = "9x15";
char *buttonFont = "9x15";

int sonarState = SONAR_ON;
int sonarWwidth = 200;
int posWwidth = 120;
int posWheight = 25;
int rotation_mode=0;

/*****************************************************************************
 * Private variables
 *****************************************************************************/

static int last_robot_x=-1, last_robot_y=-1;

static long timeZero = 0;

/*
 * Most routines rely on this being called first.
 */
static void initBitmap(void)
{
  if( bitmap == 0 ) {		/* first time */
    /* initialize useColor */
    useColor = !EZX_blackWhite();
    
    if (!useColor) { 
      robotColour = C_BLACK;
      backColour = C_BLACK;
      obstColour = C_BLACK;
      textColour = C_BLACK;
      roomColour = C_WHITE;
      corrColour = C_WHITE;
      doorColour = C_WHITE;
      fovColour = C_BLACK;
    }
    w_env = XtWindow(bitmapW);
    bm_w = ((int) (((double) env_x) * scale + 0.5)); 
    bm_h = ((int) (((double) env_y) * scale + 0.5));
    
    bitmap = XCreatePixmap(XtDisplay(bitmapW),XtWindow(bitmapW), bm_w, bm_h,
			   XDisplayPlanes(XtDisplay(bitmapW),
					  XDefaultScreen(XtDisplay(bitmapW))));
    DrawMapOnBitmap();
  }
}

void plot_object(int i)
{
  char buff[10];
  
  if (displayp == 1){
    if (objs[i].type == O_ROOM){ /* room */
      EZX_SetColor(((objs[i].status == ROOM_STATUS) ? 
		    roomColour : corrColour));
      EZX_FillRectangle(bitmap, (int) (objs[i].x1 * scale),
			bm_h-(int) (objs[i].y2 * scale),
			(int) ((objs[i].x2-objs[i].x1) * scale),
			(int) ((objs[i].y2-objs[i].y1) * scale));
      EZX_DrawRectangle(bitmap, (int) (objs[i].x1 * scale),
			bm_h-(int) (objs[i].y2 * scale),
			(int) ((objs[i].x2-objs[i].x1) * scale),
			(int) ((objs[i].y2-objs[i].y1) * scale));

      /* put numbers in the rooms. */
      
      EZX_SetColor(textColour);
      EZX_UseFont(theGC, textFont);
      sprintf(buff,"%d", i);
#if 0
      EZX_DrawString(bitmap, (int)((objs[i].x1+objs[i].x2)/2.0 * scale), 
		   bm_h-(int)((objs[i].y1+objs[i].y2)/2.0 * scale),
		   buff);
#endif
    } else if (objs[i].type == O_CLOSE_DOOR || objs[i].type == O_OPEN_DOOR) { 
      /* door */
      if (useColor) {
	EZX_SetColor(doorColour);
	EZX_FillRectangle(bitmap, (int) (objs[i].x1 * scale),
			  bm_h-(int) (objs[i].y2 * scale),
			  (int) ((objs[i].x2-objs[i].x1) * scale),
			  (int) ((objs[i].y2-objs[i].y1) * scale));
      } else {
	EZX_SetColor(backColour);
      }
      EZX_DrawRectangle(bitmap, (int) (objs[i].x1 * scale),
			bm_h-(int) (objs[i].y2 * scale),
			(int) ((objs[i].x2-objs[i].x1) * scale),
			(int)((objs[i].y2-objs[i].y1) * scale));
      EZX_SetColor(backColour);
      if (objs[i].y2-objs[i].y1 > objs[i].x2-objs[i].x1){
	/* door vertical */
	if (objs[i].type == O_CLOSE_DOOR) /* door closed */
	  DRAW_LINE(bitmap, (objs[i].x1+objs[i].x2)/2.0, objs[i].y1,
		    (objs[i].x1+objs[i].x2)/2.0, objs[i].y2);
      } else { /* door horizontal */
	if (objs[i].type == O_CLOSE_DOOR) /* door closed */
	  DRAW_LINE(bitmap, objs[i].x1, (objs[i].y1+objs[i].y2)/2.0,
		    objs[i].x2, (objs[i].y1+objs[i].y2)/2.0);
      }
    } else if (objs[i].type == O_START){ /* start */
      EZX_SetColor(textColour);
      DRAW_CIRCLE(bitmap, objs[i].x1, objs[i].y1, robot_r);
      EZX_DrawCircle(bitmap, (int) (objs[i].x1 * scale), 
		     bm_h-(int) (objs[i].y1 * scale), 
		     (int) (robot_r * scale));
    } else if (objs[i].type == O_GOAL && objs[i].status == 0) {
      /* goal, not eaten */
      EZX_SetColor(textColour);
      EZX_FillCircle(bitmap, (int) (objs[i].x1 * scale),
		     bm_h-(int) (objs[i].y1 * scale), 
		     (int) (robot_r * scale * 2.0));
    } else if (objs[i].type == O_RECTANGLE) { /* rect. obstacle */
      if (objs[i].status == 1) EZX_SetColor(roomColour);
      else EZX_SetColor(obstColour);
      EZX_FillRectangle(bitmap, (int) (objs[i].x1 * scale),
			bm_h-(int) (objs[i].y2 * scale),
			(int) ((objs[i].x2-objs[i].x1) * scale),
			(int) ((objs[i].y2-objs[i].y1) * scale));
      if (useColor && objs[i].status != 1) {
	EZX_SetColor(backColour);
	EZX_DrawRectangle(bitmap, (int) (objs[i].x1 * scale),
			  bm_h-(int) (objs[i].y2 * scale),
			  (int) ((objs[i].x2-objs[i].x1) * scale),
			  (int) ((objs[i].y2-objs[i].y1) * scale));
      }
    } else if (objs[i].type == O_ROUND){ /* round obst. */
      if (objs[i].status == 1) EZX_SetColor(roomColour);
      else EZX_SetColor(obstColour);
      EZX_FillCircle(bitmap, (int) (objs[i].x1 * scale), 
		     bm_h-(int) (objs[i].y1 * scale), 
		     (int) (objs[i].diameter * scale));
      EZX_SetColor(backColour);
      DRAW_CIRCLE(bitmap, objs[i].x1, objs[i].y1, objs[i].diameter/2.0);
      
      /* Draw the object number (for the robot competition) */
      if (objs[i].prev_status != 0) { /* prev_status holds the object number */
	char text[10];
	sprintf(text, "%d", objs[i].prev_status);
	EZX_DrawText(bitmap, (int)((objs[i].x1 + objs[i].diameter/2.0) * scale),
	             bm_h-(int)((objs[i].y1 + objs[i].diameter/2.0) * scale), text);
      }
    }
  }
}

/*
 * modus = 0: clear plot robot
 *         1: plot robot
 *         2: replot robot - in the new position.
 */
void plot_robot(int modus)
{
  int r = (int)(robot_r*scale);
  double x, y;
  
  if (displayp == 0)
    return;
  switch (modus) {
  case 0:
    { /* erase the old robot image */
      DisplayBitmap(last_robot_x-r-1,bm_h-last_robot_y-r-1, 2*r+2,2*r+2);
      break;
    }
  case 2:
    {
      if (((scale * robot_x) == last_robot_x) &&
	  ((scale * robot_x) == last_robot_x)) {
	/* the robot has not moved a whole pixel, don't redraw */
	return;
      } else { /* the robot has moved and must be redrawn */
	DisplayBitmap(last_robot_x-r-1,bm_h-last_robot_y-r-1, 2*r+2,2*r+2);
      }
    }
  case 1:
    {
      last_robot_x = robot_x*scale, last_robot_y = robot_y*scale;
      if (DISPLAY_TRAIL) {
/*
	EZX_SetColor(doorColour);
	EZX_DrawCircle(bitmap, last_robot_x, bm_h-last_robot_y, r+3);
*/
	EZX_SetColor(corrColour);
	EZX_FillCircle(bitmap, last_robot_x, bm_h-last_robot_y, r);
      }
      EZX_SetColor(robotColour);
      EZX_FillCircle(w_env, last_robot_x, bm_h-last_robot_y, r);
      x = sin(robot_o) * r; 
      y = cos(robot_o) * r; 
      
      EZX_SetColor(roomColour);
      EZX_DrawLine(w_env, last_robot_x, bm_h-last_robot_y, 
		   ((int) (last_robot_x + x)), bm_h-((int) (last_robot_y + y)));
      /* torso direction */
      x = sin((double)(robot_o+torsoDir)) * r/2; 
      y = cos((double)(robot_o+torsoDir)) * r/2; 
      EZX_SetColor(textColour);
      EZX_DrawLine(w_env, last_robot_x, bm_h-last_robot_y, 
		   ((int) (last_robot_x + x)), bm_h-((int) (last_robot_y + y)));
    }
  }
}

void DrawMapOnBitmap(void)
{
  int i;
  EZX_SetColor(backColour);
  EZX_FillRectangle(bitmap, 0,0, bm_w, bm_h);
  
  for (i = 0; i < n_obj; i++) 
    if (objs[i].type >= 0)	/* orig == 0 */
      plot_object(i);
}

void DisplayBitmap(int x, int y, int w, int h)
{
  XCopyArea(XtDisplay(bitmapW),bitmap,XtWindow(bitmapW),
	    theGC, x, y, w, h, x, y);
}

static void drawMap(int x, int y, int w, int h)
{
  if (displayp) {
    initBitmap();
    DisplayBitmap(x, y, w, h);
    
    plot_robot(1);
    EZX_Flush();
  }
}

void RedrawMap(Widget w, XExposeEvent *ev, String *parm, Cardinal *n)
{
  drawMap(ev->x,ev->y,ev->width,ev->height);
}

void RedrawWholeMap(void)
{
  drawMap(0,0,mapWidth,mapHeight);
}

void RedrawSonar(Widget widget, XExposeEvent *ev,
		 String *parm, Cardinal *n)
{
  int i;
  int rw_x = sonarWwidth/2, rw_y = sonarWwidth/2;
  double angle=0.0;
  double sc;
  Drawable w = XtWindow(widget);
  int display_robot_r;
  
  display_robot_r = (int) 3 * robot_r;
  
  sc = (double)sonarWwidth/(sensors_distance*2);
  if ( sonarState == SONAR_ON ) {
    EZX_SetColor(C_WHITE);
    EZX_FillRectangle(w, 0, 0, sonarWwidth, sonarWwidth);
  }
  if( sonarState == SONAR_ON ) {
    /*	EZX_SetColor(C_BLUE);	  */
    EZX_SetColor(C_BLACK);
    for (i = 0; i < n_sensors; i++){
      angle = ((double) i) / ((double) (n_sensors)) * TWO_PI;
      if( rotation_mode == 0 )
	angle += robot_o;
      EZX_DrawLine(w, rw_x, rw_y,
		   rw_x + ((int) (sin(angle) * sc * sensor[i])),
		   rw_y - ((int) (cos(angle) * sc * sensor[i])));
    }
  }

  if ( sonarState == SONAR_ON ) {
    EZX_SetColor(C_BLACK);
    EZX_FillCircle(w, rw_x, rw_y, ((int) (display_robot_r*sc)));
    EZX_SetColor(C_WHITE);
    EZX_DrawCircle(w, rw_x, rw_y, ((int) (display_robot_r*sc)));
    if (rotation_mode == 1) {
      EZX_DrawLine(w, rw_x, rw_y, 
		   rw_x, rw_y - ((int) (display_robot_r*sc)));
      EZX_SetColor(C_RED);
      EZX_DrawLine(w, rw_x, rw_y, 
		   rw_x+((int)(display_robot_r/2*sc*sin(torsoDir))),
		   rw_y-((int)(display_robot_r/2*sc*cos(torsoDir))));
    } else if (rotation_mode == 0) {
      EZX_DrawLine(w, rw_x, rw_y, 
		   rw_x+ ((int) (display_robot_r*sc*sin(robot_o))),
		   rw_y - ((int) (display_robot_r*sc*cos(robot_o))));
      EZX_SetColor(C_RED);
      EZX_DrawLine(w, rw_x, rw_y, 
		   rw_x+ ((int) (display_robot_r/2*sc*sin(robot_o+torsoDir))),
		   rw_y - ((int) (display_robot_r/2*sc*cos(robot_o+torsoDir))));
    }
  }
}

void RedrawTime(void)
{
  static int firstTime=1;
  static char oldStr[40];
  char str[40];
  
  if(firstTime) oldStr[0]='\0';
  
  if( displayp == 1 ) {
    sprintf( str, "%02ld:%02ld:%02ld",
	    currentTime/3600000, (currentTime/60000)%60, 
	    (currentTime/1000)%60 );
    if (strcmp(str,oldStr) != 0)  {
      EZX_SetColor(C_BLACK);
      EZX_UseFont(theGC,buttonFont);
      EZX_DrawText(XtWindow(timelabelW),0,EZX_GetFontHeight(),str);
      EZX_Flush();
      strcpy(oldStr,str);
    }
  }
}

void ResetTime(void)
{
  timeZero = getTime()/1000;
  currentTime = 0;
}

void UpdateElapsedTime(void)
{
  unsigned long now;
  static unsigned long lastTime = 0;
  char str[40];
  
  now = getTime()/1000 - timeZero;
  
  if (displayp == 1 && (now != lastTime)) {
    sprintf( str, "%02d:%02d:%02d",
	    (int)(now/3600), (int)(now/60)%60, (int)now%60 );
    EZX_SetColor(C_BLACK);
    EZX_UseFont(theGC,buttonFont);
    EZX_DrawText(XtWindow(elapsedtimelabelW),0,EZX_GetFontHeight(),str);
    EZX_Flush();
  }
  lastTime = now;
}

void RedrawPosition(Widget widget, XExposeEvent *ev,
		    String *parm, Cardinal *n)
{
  char str[40];
  int currentX, currentY, currentAngle;
  
  if (displayp == 1) {
    
    currentX = (int)(0.5+robot_x);
    sprintf(str, "X: %d ", currentX);
    EZX_SetColor(C_BLACK);
    EZX_UseFont(theGC,buttonFont);
    EZX_DrawText(XtWindow(xposW),0,EZX_GetFontHeight(),str);
    EZX_Flush();
    
    currentY = (int)(0.5+robot_y);
    sprintf(str, "Y: %d ", currentY);
    EZX_SetColor(C_BLACK);
    EZX_UseFont(theGC,buttonFont);
    EZX_DrawText(XtWindow(yposW),0,EZX_GetFontHeight(),str);
    EZX_Flush();

    currentAngle = (int)(0.5 + 10*RAD_TO_DEG(robot_o)) % 3600;
    sprintf(str, "Angle: %.1f  ", currentAngle/10.0);
    EZX_SetColor(C_BLACK);
    EZX_UseFont(theGC,buttonFont );
    EZX_DrawText(XtWindow(angleW),0,EZX_GetFontHeight(),str);
    EZX_Flush();
  }
}
