/*****************************************************************************
 * PROJECT: TDL Tutorial
 *
 * (c) Copyright 2001 Reid Simmons. All rights reserved.
 * (c) Copyright 1994 Richard Goodwin & Reid Simmons. All rights reserved.
 *
 * FILE: sim-xwindows.c
 *
 * ABSTRACT: Simulator xwindows stuff.
 *
 * $Source: /afs/cs.cmu.edu/project/TCA/Master/tcaV8/tutorial/simulator/sim-xwindows.c,v $ 
 * $Revision: 1.5 $
 * $Date: 1996/02/14 22:16:06 $
 * $Author: rich $
 *
 * REVISION HISTORY:
 *
 * $Log: sim-xwindows.c,v $
 *****************************************************************************/

#include "common.h"
#include "ezx.h"
#include "sim-xwindows.h"
#include "updates.h"
#include "draw_env.h"
#include "environment.h"
#include "sensors.h"
#include "files.h"
#include "action.h"

/*****************************************************************************
 * Global constants
 *****************************************************************************/

#define MAX_VHEIGHT 800
#define MAX_VWIDTH  550

/*****************************************************************************
 * Global variables
 *****************************************************************************/

Widget sonarW, bitmapW;
Widget toplevelW, outerW, viewportW, teleportW, orientW, quitW;
Widget sonarbuttonW, timelabelW, elapsedtimelabelW;
Widget xposW, yposW, angleW;
XtAppContext app_context;

/*****************************************************************************
 * Private variables
 *****************************************************************************/

static String fallback_resources[] = { 
  "*geometry: +1+1",
  "*font: 9x15",
  "*boldFont: 12x24",
  "*input: True",
  "*allowShellResize: True",
  "*Dialog*value: ",
  "*Dialog*label.resizable: TRUE",
  "*orientDialog*value.translations: #override \\n <Key>Return: SetOrientationOK()",
  "*orientDialog.label: Robot Orientation (degrees)",
  NULL,
};

void Quit(Widget widget, XtPointer closure, XtPointer callData)
{
  XtDestroyWidget((Widget)closure);
  exit(0);
}

static String sonarLabels[2] = {
  "Display Sonar     ",
  "Sonar Display Off",
};

void ToggleSonar(Widget w, XtPointer closure,  XtPointer callData)
{
  sonarState = (sonarState+1)%2;
  XtVaSetValues( w, XtNlabel, sonarLabels[sonarState], NULL );
  switch(sonarState) {
  case SONAR_ON:
    sensors();
    RedrawSonar( sonarW, NULL,NULL,NULL );
    break;
  case SONAR_OFF:
    EZX_SetColor(C_WHITE);
    EZX_FillRectangle(XtWindow(sonarW), 0, 0, sonarWwidth, sonarWwidth);
    RedrawSonar( sonarW, NULL,NULL,NULL );
    break;
  }
}

static void setOrientation(Widget w, XtPointer client_data,
			   XtPointer call_data)
{  
  Widget dialog = (Widget) client_data;
  String amount = (String) XawDialogGetValueString(dialog);

  robot_o = DEG_TO_RAD(atof(amount));
  hyperJump(robot_x, robot_y, robot_o);

  EZX_DestroyPopupPrompt(NULL, (XtPointer) dialog, (XtPointer)NULL);
}

static void setOrientationOK(Widget widget, XEvent *event, String *params,
			     Cardinal *num_params)
{  
  setOrientation(widget, (XtPointer)XtParent(widget), (XtPointer)NULL);
}

static void setOrientationDialog(Widget button, XtPointer client_data, 
				 XtPointer callData)
{
  Arg  args[5];
  Widget popup, dialog;
  Position x, y;
  Dimension width, height;
  Cardinal n;

  /*
   * This will position the upper goLeft hand corner of the popup at the
   * center of the widget which invoked this callback, which will also
   * become the parent of the popup.
   */

  n = 0;
  XtSetArg(args[0], XtNwidth, &width); n++;
  XtSetArg(args[1], XtNheight, &height); n++;
  XtGetValues(button, args, n);
  XtTranslateCoords(button, (Position)(width/2), (Position)(height/2), &x, &y);
  
  n = 0;
  XtSetArg(args[n], XtNx, x);    n++;
  XtSetArg(args[n], XtNy, y);    n++;
  
  popup = XtCreatePopupShell("prompt", transientShellWidgetClass, 
			     button, args, n);
  dialog = XtCreateManagedWidget("orientDialog", dialogWidgetClass, popup,
				 NULL, 0);
  
  XawDialogAddButton(dialog, "ok", setOrientation, (XtPointer) dialog);
  XawDialogAddButton(dialog, "cancel", EZX_DestroyPopupPrompt,
		     (XtPointer)dialog);
  
  XtPopup(popup, XtGrabNone);
}

/* This dummy procedure is used for for a parameter in an  X Toolkit procedure 
 *  in TeleportRobot to prevent the system from crashing if I used NULL
 * as the procedure parameter instead 
 */

static void do_nothing(void) 
{}

void TeleportRobot(Widget w, XtPointer closure, XtPointer callData)
{ 
  XEvent event;
  int button_was_pressed = FALSE;
  int robot_would_be_in_free_space = FALSE;
  int robot_would_be_inside_an_obstacle = FALSE;
  double teleport_robot_x, teleport_robot_y, teleport_robot_o=0.0;
  double rbb_x1, rbb_x2, rbb_y1, rbb_y2;
  double obb_x1, obb_x2, obb_y1, obb_y2, obstacle_radius;
  int i;
  
  /* Allow for mouse button to be pressed over the bitmap */
  XtAddEventHandler(bitmapW, ButtonPressMask, FALSE,
		    (XtEventHandler) do_nothing, NULL); 
  
  do {
    XtAppNextEvent(app_context, &event);
      
    if (event.type == ButtonPress) {
      button_was_pressed = TRUE;   
	  
      teleport_robot_x = event.xbutton.x / scale;
      teleport_robot_y = (mapHeight - event.xbutton.y) / scale;
      teleport_robot_o = robot_o;
	  
      /* set the robot bounding box of the would-be teleported robot */
      rbb_x1 = teleport_robot_x - robot_r;
      rbb_x2 = teleport_robot_x + robot_r;
      rbb_y1 = teleport_robot_y - robot_r;
      rbb_y2 = teleport_robot_y + robot_r;
	  
      /* test to see if teleported robot would be in free space */
      for (i = 0; i < n_space; i++) {
	if (teleport_robot_x >= space[i].x1 && teleport_robot_x <= space[i].x2
	    && teleport_robot_y >= space[i].y1
	    && teleport_robot_y <= space[i].y2)
	  robot_would_be_in_free_space = TRUE;
      }

      /* test to see if teleported robot would be interfering */
      /* with an obstacle, I checked for all possible overlapping */
      /* cases of robot with rectangular obstacle */
      for (i = 0; i < n_obj; i++) {
	if (objs[i].type == O_RECTANGLE && objs[i].status == 0 && 
	    /* rectangle obstacle */
	    ((rbb_x1 >= objs[i].x1 && rbb_x1 <= objs[i].x2 &&
	      rbb_y1 >= objs[i].y1 && rbb_y1 <= objs[i].y2) ||
	     (rbb_x2 >= objs[i].x1 && rbb_x2 <= objs[i].x2 &&
	      rbb_y1 >= objs[i].y1 && rbb_y1 <= objs[i].y2) ||
	     (rbb_x1 >= objs[i].x1 && rbb_x1 <= objs[i].x2 &&
	      rbb_y2 >= objs[i].y1 && rbb_y2 <= objs[i].y2) ||
	     (rbb_x2 >= objs[i].x1 && rbb_x2 <= objs[i].x2 &&
	      rbb_y2 >= objs[i].y1 && rbb_y2 <= objs[i].y2) ||
	     (objs[i].x1 >= rbb_x1 && objs[i].x1 <= rbb_x2 &&
	      objs[i].x2 >= rbb_x1 && objs[i].x2 <= rbb_x2 &&
	      objs[i].y1 <= rbb_y2 && objs[i].y2 >= rbb_y1) ||
	     (objs[i].y1 >= rbb_y1 && objs[i].y1 <= rbb_y2 &&
	      objs[i].y2 >= rbb_y1 && objs[i].y2 <= rbb_y2 &&
	      objs[i].x1 <= rbb_x2 && objs[i].x2 >=rbb_x1)))
	  robot_would_be_inside_an_obstacle = TRUE;
	      
	/* do a similar test as above for a round obstacle, just */
	/* set the bounding box coordinates for the round */
	/* obstacle first */
	if (objs[i].type == O_ROUND && objs[i].status == 0) {
	  /* circle obstacle */
	  /* set round obstacle bounding box */
	  obstacle_radius = objs[i].diameter / 2;
	  obb_x1 = objs[i].x1 - obstacle_radius;
	  obb_x2 = objs[i].x1 + obstacle_radius;
	  obb_y1 = objs[i].y1 - obstacle_radius;
	  obb_y2 = objs[i].y1 + obstacle_radius;
		  
	  if ((rbb_x1 >= obb_x1 && rbb_x1 <= obb_x2 &&
	       rbb_y1 >= obb_y1 && rbb_y1 <= obb_y2) ||
	      (rbb_x2 >= obb_x1 && rbb_x2 <= obb_x2 &&
	       rbb_y1 >= obb_y1 && rbb_y1 <= obb_y2) ||
	      (rbb_x1 >= obb_x1 && rbb_x1 <= obb_x2 &&
	       rbb_y2 >= obb_y1 && rbb_y2 <= obb_y2) ||
	      (rbb_x2 >= obb_x1 && rbb_x2 <= obb_x2 &&
	       rbb_y2 >= obb_y1 && rbb_y2 <= obb_y2) ||
	      (obb_x1 >= rbb_x1 && obb_x1 <= rbb_x2 &&
	       obb_x2 >= rbb_x1 && obb_x2 <= rbb_x2 &&
	       obb_y1 <= rbb_y2 && obb_y2 >= rbb_y1) ||
	      (obb_y1 >= rbb_y1 && obb_y1 <= rbb_y2 &&
	       obb_y2 >= rbb_y1 && obb_y2 <= rbb_y2 &&
	       obb_x1 <= rbb_x2 && obb_x2 >=rbb_x1))
	    robot_would_be_inside_an_obstacle = TRUE;
	}
      }   
	  
      /* If robot can be teleported to the desired position, */
      /* then move it to that position, otherwise print the */
      /* appropriate error messages */
      if (robot_would_be_in_free_space && !robot_would_be_inside_an_obstacle) {
	hyperJump(teleport_robot_x, teleport_robot_y, teleport_robot_o);
      } else {	       
	if (!robot_would_be_in_free_space)
	  fprintf(stderr,
		  "Robot cannot be teleported there. It would not be in free space. \n");
	if (robot_would_be_inside_an_obstacle)
	  fprintf(stderr,
		  "Robot cannot be teleported there. It would be inside an obstacle. \n");
      }
    }
  } while (!button_was_pressed);
  
  /* Disable being able to press the mouse button over the bitmap */
  XtRemoveEventHandler(bitmapW, ButtonPressMask, FALSE,
		       (XtEventHandler) do_nothing, NULL);
}

void init_simx(char *fname)
{
  int argc=0;
  String map_trans = "<Expose>: RedrawMap()";
  String sonar_trans = "<Expose>: RedrawSonar()";
  String position_trans = "<Expose>: RedrawPosition()";
  static XtActionsRec window_actions[] = 
    {{"RedrawMap", (XtActionProc) RedrawMap},
       {"RedrawPosition", (XtActionProc) RedrawPosition},
       {"RedrawSonar", (XtActionProc) RedrawSonar},
       {"SetOrientationOK", (XtActionProc) setOrientationOK}
   };

  toplevelW = XtAppInitialize(&app_context, "Simulator", NULL, 0,
			      &argc, NULL, fallback_resources, NULL, 0);
  
  init_environment(fname);
  
  mapWidth = env_x * scale + 0.5;
  mapHeight = env_y * scale + 0.5;
  
  outerW = XtVaCreateManagedWidget( "form", formWidgetClass, toplevelW, NULL);
  
  EZX_initGC(toplevelW);
  
  /* map */
  viewportW = XtVaCreateManagedWidget( "Viewport", viewportWidgetClass, outerW,
				      XtNallowVert, True,
				      XtNallowHoriz, True,
				      XtNwidth, MIN(mapWidth,MAX_VWIDTH),
				      XtNheight, MIN(mapHeight,MAX_VHEIGHT),
				      XtNright, XawChainRight,
				      XtNleft, XawChainLeft,
				      XtNtop, XawChainTop,
				      XtNbottom, XawChainBottom,
				      NULL);
  
  bitmapW = XtVaCreateManagedWidget("bitmap", widgetClass, viewportW,
				    XtNtranslations, 
				    XtParseTranslationTable(map_trans),
				    XtNwidth, mapWidth,
				    XtNheight, mapHeight,
				    NULL); 
  
  /* time label */
  timelabelW = XtVaCreateManagedWidget("time",labelWidgetClass,outerW,
				       XtNlabel, "        ",
				       XtNfromHoriz, viewportW,
				       XtNleft, XawChainRight,
				       XtNright, XawChainRight,
				       XtNtop, XawChainTop,
				       XtNbottom, XawChainTop,
				       NULL);
  /* elapsed time label */
  elapsedtimelabelW = XtVaCreateManagedWidget("etime",
					      commandWidgetClass, outerW,
					      XtNlabel, "        ",
					      XtNfromHoriz, timelabelW,
					      XtNleft, XawChainRight,
					      XtNright, XawChainRight,
					      XtNtop, XawChainTop,
					      XtNbottom, XawChainTop,
					      NULL);
  XtAddCallback( elapsedtimelabelW, XtNcallback, (XtCallbackProc) ResetTime, 
		(XtPointer) toplevelW);
  
  /* Transport button */
  teleportW = XtVaCreateManagedWidget("Teleport", commandWidgetClass, outerW,
				      XtNfromVert, timelabelW,
				      XtNfromHoriz, viewportW,
				      XtNleft, XawChainRight,
				      XtNright, XawChainRight,
				      XtNtop, XawChainTop,
				      XtNbottom, XawChainTop,
				      NULL);
  XtAddCallback( teleportW, XtNcallback, TeleportRobot, (XtPointer) toplevelW);
  
  /* Orientation button */
  orientW = XtVaCreateManagedWidget("Orientation", commandWidgetClass, outerW,
				    XtNfromHoriz, teleportW,
				    XtNfromVert, timelabelW,
				    XtNleft, XawChainRight,
				    XtNright, XawChainRight,
				    XtNtop, XawChainTop,
				    XtNbottom, XawChainTop,
				    NULL);
  XtAddCallback(orientW,XtNcallback,setOrientationDialog,(XtPointer)toplevelW);

  /* sonar display on off */
  sonarbuttonW = XtVaCreateManagedWidget(sonarLabels[sonarState],
					 commandWidgetClass, outerW,
					 XtNfromVert, teleportW,
					 XtNfromHoriz, viewportW,
					 XtNleft, XawChainRight,
					 XtNright, XawChainRight,
					 XtNtop, XawChainTop,
					 XtNbottom, XawChainTop,
					 NULL);
  XtAddCallback( sonarbuttonW, XtNcallback, ToggleSonar, (XtPointer)0 );
  
  /* sonar window */
  sonarW = XtVaCreateManagedWidget("Sonar", widgetClass, outerW,
				   XtNtranslations, 
				   XtParseTranslationTable(sonar_trans),
				   XtNfromVert, sonarbuttonW,
				   XtNfromHoriz, viewportW,
				   XtNleft, XawChainRight,
				   XtNright, XawChainRight,
				   XtNtop, XawChainTop,
				   XtNbottom, XawChainTop,
				   XtNwidth, sonarWwidth,
				   XtNheight, sonarWwidth,
				   NULL);
  
  xposW = XtVaCreateManagedWidget("xpos",widgetClass,outerW,
				  XtNtranslations, 
				  XtParseTranslationTable(position_trans),
/*				  XtNlabel, "X: 0000",*/
/*				  XtNlabel, "       ",*/
				  XtNfromVert, sonarW,
				  XtNfromHoriz, viewportW,
				  XtNleft, XawChainRight,
				  XtNright, XawChainRight,
				  XtNtop, XawChainTop,
				  XtNbottom, XawChainTop,
				  XtNwidth, posWwidth,
				  XtNheight, posWheight,
				  NULL);
  
  yposW = XtVaCreateManagedWidget("ypos",widgetClass,outerW,
				  XtNtranslations, 
				  XtParseTranslationTable(position_trans),
				  XtNfromVert, xposW,
				  XtNfromHoriz, viewportW,
				  XtNleft, XawChainRight,
				  XtNright, XawChainRight,
				  XtNtop, XawChainTop,
				  XtNbottom, XawChainTop,
				  XtNwidth, posWwidth,
				  XtNheight, posWheight,
				  NULL);
  
  angleW = XtVaCreateManagedWidget("angle",widgetClass,outerW,
				   XtNtranslations, 
				   XtParseTranslationTable(position_trans),
				   XtNfromVert, yposW,
				   XtNfromHoriz, viewportW,
				   XtNleft, XawChainRight,
				   XtNright, XawChainRight,
				   XtNtop, XawChainTop,
				   XtNbottom, XawChainTop,
				   XtNwidth, posWwidth,
				   XtNheight, posWheight,
				   NULL);
  
  XtAppAddActions(app_context, window_actions, XtNumber(window_actions));
  
  /* quit button */
  quitW = XtVaCreateManagedWidget( "quit", commandWidgetClass, outerW,
				  XtNfromVert, angleW,
				  XtNfromHoriz, viewportW,
				  XtNleft, XawChainRight,
				  XtNright, XawChainRight,
				  XtNtop, XawChainTop,
				  XtNbottom, XawChainTop,
				  NULL);
  XtAddCallback( quitW, XtNcallback, Quit, (XtPointer) toplevelW);
  
  XtRealizeWidget(toplevelW);
}
