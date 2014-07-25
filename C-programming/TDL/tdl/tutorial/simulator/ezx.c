/*****************************************************************************
 * PROJECT: TDL Tutorial
 *
 * (c) Copyright 2001 Reid Simmons. All rights reserved.
 * (c) Copyright 1993 Richard Goodwin & Joseph O'Sullivan. All rights reserved.
 *
 * FILE: ezx.c
 *
 * ABSTRACT: This file contains a a set of routines for using X11.  
 *           It is derived from EZX done by Long-Ji Lin.
 *
 * $Source: /afs/cs.cmu.edu/project/TCA/Master/tcaV8/utils/ezx.c,v $ 
 * $Revision: 1.6 $
 * $Date: 1996/02/10 16:54:17 $
 * $Author: rich $
 *
 * REVISION HISTORY:
 *
 * $Log: ezx.c,v $
 *****************************************************************************/

#include "common.h"
#include "cursorx.h"
#include "ezx.h"

/*****************************************************************************
 * Global constants
 *****************************************************************************/

#define EZX_MAX_FONTS 10

/*****************************************************************************
 * Global Types
 *****************************************************************************/

typedef struct {
  XFontStruct *font;
  char *name;
} aFont_struct;

/*****************************************************************************
 * Global variables
 *****************************************************************************/

aFont_struct     theFonts[EZX_MAX_FONTS] =
{
  {NULL,NULL},
  {NULL,NULL},
  {NULL,NULL},
  {NULL,NULL},
  {NULL,NULL},
  {NULL,NULL},
  {NULL,NULL},
  {NULL,NULL},
  {NULL,NULL},
  {NULL,NULL}};

static XGCValues    WGCValues;		/* Struct for creating GC */
GC              theGC;
int	    	theScreen;		/* Screen number */
Display	       *theDisplay=NULL;	/* X server connection */
int	     	theDepth;		/* 1 if monochrome */
Colormap	theColormap;
unsigned long	theBlackPixel;
unsigned long	theWhitePixel;
XFontStruct	*theFont=NULL;

/* stuff added from EZX11 */
static int	    WFirstTime=True;	/* Flag for first time creation */
static long	    WPositionHint = USPosition; /* Default is user specified */
static int	    WScreen;			/* Screen number */
static unsigned long WBorder, WBackground, WForeground;	     /* Pixel values */
static unsigned long WBorderWidth = 2;		/* Border width */
static XSizeHints   WSizeHints;		/* Size hints for window manager */  
static XSetWindowAttributes WAttributes;/* Temporary Set Window Attribute */
XFontStruct *WFont=NULL;		/* Font descriptor */
#define FONT_NAME	"9x15bold"

static XWMHints WWMHints = {	/* Let the WM know how to handle this window */
  (InputHint|StateHint),		/* flags */
  False,				/* input */
  NormalState,			/* initial_state */
  0,				/* icon pixmap */
  0,				/* icon window */
  0, 0,				/* icon location */
  0,				/* icon mask */
  0,				/* window group */
};

unsigned long	thePixels[MAXCOLORS];

char	*theColorNames[MAXCOLORS] = {
  "grey",
  "red", 
  "pink", 
  "orange", 
  "yellow", 
  "gold", 
  "lawngreen", 
  "blue", 
  "violet", 
  "purple",
  "brown",
  /* Sebastian's color's */
  "mediumpurple3",
  "cyan",
  "khaki4",
  "turquoise4",
  "white", 
  /* The Greys */
  "black", 
  "grey5",
  "grey10", "grey15", 
  "grey20", "grey25", 
  "grey30", "grey35", 
  "grey40", "grey45", 
  "grey50", "grey55", 
  "grey60", "grey65", 
  "grey70", "grey75", 
  "grey80", "grey85", 
  "grey90", "grey95", "grey100",
  /* the yellows */
  "yellow4",
  "lightyellow4",
  "yellow3",
  "lightyellow3",
  "yellow2",
  "lightyellow2",
  "lightgoldenrodyellow",
  "yellow",
  "yellow1",
  "lightyellow",
  "lightyellow1"
};

void EZX_InitDefaultColors(void)
{
  XColor	theRGBColor, theHardwareColor;
  int		theStatus;
  int		i;
  static int colorsDone=FALSE;
  
  if (!colorsDone) {
    if (theDepth==1) {
      /* monochrome system */
      thePixels[C_WHITE] = theWhitePixel;
      thePixels[C_BLACK] = theBlackPixel;
    } else {
      for(i=0;i<MAXCOLORS;++i) {
	theStatus = XLookupColor(theDisplay, theColormap, theColorNames[i],
				 &theRGBColor, &theHardwareColor);
	if (theStatus != 0) {
	  theStatus = XAllocColor(theDisplay, theColormap,
				  &theHardwareColor);
	  if (theStatus != 0)
	    thePixels[i] = theHardwareColor.pixel;
	  else
	    thePixels[i] = theBlackPixel;
	}
      }
    }
    WGCValues.foreground = theBlackPixel;
    WGCValues.background = theWhitePixel;
    theGC = XDefaultGC( theDisplay, theScreen );
    colorsDone=TRUE;
  }
}

static void initEZX(char *program)
{
  static int initted = FALSE;
  
  if (!initted) {
    initted = TRUE;
    
    theScreen     = XDefaultScreen( theDisplay );
    theDepth      = XDisplayPlanes( theDisplay, theScreen );
    theWhitePixel = XWhitePixel( theDisplay, theScreen );
    theBlackPixel = XBlackPixel( theDisplay, theScreen );
    theColormap   = XDefaultColormap( theDisplay, theScreen );
    if ( (WFont = XLoadQueryFont( theDisplay, FONT_NAME )) == NULL ) {
      fprintf(stderr, "%s: could not open font %s.", program, FONT_NAME);
      exit(1);
    }
    theFont = WFont;
    theFonts[0].font = WFont;
    theFonts[0].name = FONT_NAME;
    
    EZX_InitDefaultColors();
  }
}

void EZX_initGC(Widget w)
{
  if (theDisplay == NULL)
    theDisplay = XtDisplay(w);
  initEZX(NULL);
}

void EZX_InitGraphics(char *display, char *program)
{
  if (! WFirstTime) return;
  
  if ( display == NULL )
    display = getenv("DISPLAY");
  if (theDisplay == NULL) {
    if ( (theDisplay = XOpenDisplay(display)) == NULL ) {
      fprintf(stderr, "\n%s: could not open display %s.\n",
	      program, XDisplayName(display));
      exit(1);
    }
  }
  initEZX(program);
  WFirstTime = False;
}

void EZX_SetColor(int color)			/* set foreground color */
{
  static int last_color = -1;
  if (color >= MAXCOLORS || color < 0) {
    printf("Wrong color: %d\n", color);
    return;
  }
  if (color != last_color) {
    XSetForeground(theDisplay, theGC, thePixels[color]);
    last_color = color;
  }
}

void EZX_SetLineWidth(int line_width)
{
  WGCValues.line_width = line_width;
  XChangeGC(theDisplay, theGC, (GCLineWidth), &WGCValues);
}

void EZX_waitForMap(void)
{
  XEvent rep;
  
  while(1) {
    XNextEvent(theDisplay, &rep);
    if (rep.type == MapNotify)
      break;
  }
}

Window EZX_MakeWindow(char *title, unsigned int width,
		      unsigned int height,
		      char *position)			/* eg, "+4-4" */
{
  int  left=0, top=0;
  Window window;
  char geometry[32];
  static int	    WFirstWindow=True;	/* Flag for first window creation */
  
  /* Geometry */
  sprintf(geometry, "=%dx%d", width, height);
  if (position) {
    strcat(geometry, position);
    XParseGeometry(geometry, &left, &top, &width, &height );
  } else {
    /* user specifies position at run-time */
    WPositionHint = PPosition;
  }
  /*.. Select colors for the border, background, and foreground ..*/
  WScreen     = XDefaultScreen( theDisplay );
  
  WBackground = XWhitePixel( theDisplay, WScreen );
  WForeground = XBlackPixel( theDisplay, WScreen );
  WBorder	= XBlackPixel( theDisplay, WScreen );
  /*
     WBackground = XBlackPixel( theDisplay, WScreen );
     WForeground = XWhitePixel( theDisplay, WScreen );
     WBorder	= XWhitePixel( theDisplay, WScreen );
     */
  
  /*.. Providing the window with an Initial Position & Size ..*/
  WSizeHints.flags = (WPositionHint | USSize); /* User spec'd Size */
  WSizeHints.x = left;
  WSizeHints.y = top;
  WSizeHints.width = width;
  WSizeHints.height = height;
  
  window = XCreateSimpleWindow( theDisplay,
			       XDefaultRootWindow( theDisplay ),
			       WSizeHints.x, WSizeHints.y,
			       WSizeHints.width, WSizeHints.height,
			       WBorderWidth, WBorder, WBackground );
  if( window == (Window) NULL)
    {
      fprintf(stderr, "\n XCreateSimpleWindow failed (NULL returned)");
      exit(1);
    }
  {
    int argc;  char *argv[7];
    argv[0] = title;
    argv[1] = "-g";
    argv[2] = geometry;
    argv[3] = "-b";
    argv[4] = "Always";
    argv[5] = "-c";
    argc = 6;
    if (! position) {
      argv[6] = "-p";
      ++argc;
    }
    XSetStandardProperties( theDisplay, window, title, title, None,
			   0,0, /*argv, argc,*/ &WSizeHints  );
  }
  XSetWMHints( theDisplay, window, &WWMHints );
  
  /*..  Set the Window Property so that Backing Store is maintained ..*/
  
  WAttributes.backing_store	= Always;
  WAttributes.event_mask	=  ButtonPressMask | ButtonReleaseMask|
    StructureNotifyMask;
  
  XChangeWindowAttributes( theDisplay, window, CWBackingStore | CWEventMask,
			  &WAttributes );
  
  /*..  Create the Graphics Contents, for only the first one ..*/
  
  if (WFirstWindow) {
    WGCValues.font	 = WFont->fid;
    WGCValues.foreground = WForeground;
    WGCValues.background = WBackground;
    theGC = XCreateGC( theDisplay, window,
		      (GCFont | GCForeground | GCBackground), &WGCValues );
    WFirstWindow = False;
  }
  
  EZX_initGC((Widget) window);
  
  XMapWindow( theDisplay, window );  /* Make it visible */
  
  EZX_waitForMap();
  
  XFlush( theDisplay );
  
  return( window );
}

void EZX_ClearWindow(
		     Window w
		     )
{
  XClearWindow( theDisplay, w );
  XFlush( theDisplay );
}

void EZX_EndWindow(Window w)
{
  XFlush( theDisplay );
  XDestroyWindow( theDisplay, w );
}

void EZX_DrawCircle(Drawable w, int x, int y, int r)
{
  XDrawArc( theDisplay, w, theGC, x - r, y - r, r<<1, r<<1, 0, 360*64);
}

void EZX_DrawPoint(Drawable w, int x, int y)
{
  XDrawPoint(theDisplay, w, theGC, x, y);
}

void EZX_DrawGrey(Drawable w, int x, int y, int g)
{
  if (g>=NUM_GREY)
    g = MAX_GREY;
  else if (g<0)
    g = MIN_GREY;
  else
    g=g+MIN_GREY;
  
  EZX_SetColor(g);
  XDrawPoint(theDisplay, w, theGC, x, y);
  /* EZX_SetColor(C_BLACK); */
}

void EZX_DrawLine(Drawable w, int Ax, int Ay, int Bx, int By)
{
  XDrawLine(theDisplay, w, theGC, Ax, Ay, Bx, By);
}

void EZX_ClearRectangle(Drawable w, int left, int top, int width, int height)
{
  XClearArea(theDisplay, w, left, top, width, height, False);
}

void EZX_DrawRectangle(Drawable w, int x, int y,
		       unsigned int width, unsigned int height)
{
  XDrawRectangle( theDisplay, w, theGC, x, y, width, height );
}

void EZX_FillCircle(Drawable w, int x, int y, int r)
{
  /*
     domingo: don't know why this it changes the radius
     int  r2 = (int) (r / 1.41421356 + 0.5);
     */
  int r2 = r;
  unsigned int wh = 2 * r2;
  XFillArc( theDisplay, w, theGC, x - r2, y - r2, wh, wh, 0, 360*64);
}

void EZX_FillRectangle(Drawable w, int x, int y,
		       unsigned int width, unsigned int height)
{
  XFillRectangle( theDisplay, w, theGC, x, y, width, height );
}

void EZX_FillPolygon(Drawable w, int npoints, XPoint *points)
{
  XFillPolygon(theDisplay, w, theGC, points, npoints,
	       Convex, CoordModeOrigin);
}

void EZX_Flush(void)
{
  XFlush( theDisplay );
}

void EZX_UseFont(GC theNewGC, char fontname[])
{
  int i;
  
  for(i=0; i < EZX_MAX_FONTS; i++) {
    if (theFonts[i].name == NULL)
      break;
    if (strcmp(theFonts[i].name,fontname) == 0)
      {
	theFont = theFonts[i].font;
	XSetFont(theDisplay, theNewGC, theFont->fid);
	return;
      }
  }
  /* font not found */
  if (i < EZX_MAX_FONTS) {
    theFont = XLoadQueryFont(theDisplay, fontname);
    if (theFont != 0) {
      XSetFont(theDisplay, theNewGC, theFont->fid);
      theFonts[i].font = theFont;
      theFonts[i].name = fontname;
    }
  } else {
    fprintf(stderr,"Max Fonts exceeded. Can't load font %s\n",fontname);
  }
}

void EZX_FreeFont(void)
{
  XFreeFont(theDisplay, theFont);
}

void EZX_DrawString(Drawable w, int x, int y, char *string)
{
  XDrawString(theDisplay, w, theGC, x, y, string, strlen(string));
}

void EZX_DrawText(Drawable w, int x, int y, char *string)
{
  XDrawImageString(theDisplay, w, theGC, x, y, string, strlen(string));
}

static void format_spec(int x, int y, char string[], char style,
			int *newx, int *newy, int *width)
{
  *width = XTextWidth(theFont, string, strlen(string));
  *newx  = x;
  *newy  = y;
  switch( style ) {
  case 'l':	/* Left Alignment */
  case 'L':
    break;
  case 'R':	/* Right Alignment */
  case 'r':
    /*	    *newx = x + WSizeHints.width - *width;  */
    *newx = x - *width;
    break;
  case 'c':	/* Centered */
  case 'C':
    /*	    *newx = x + (WSizeHints.width - *width) / 2; */
    *newx = x - *width / 2;
    /*	    *newy = y - EZX_GetFontHeight()/2; */
    break;
  default:	/* use left alignment as default */
    format_spec(x,y, string, 'L', newx,newy,width);
  }
}


void EZX_DrawTextAt(Drawable w, int x, int y, char *string, char style)
{
  int	width,newx,newy;
  
  format_spec(x,y, string, style, &newx, &newy, &width);
  EZX_DrawText(w, newx, newy, string );
}


void EZX_DrawStringAt(Drawable w, int x, int y, char style, char *string)
{
  int	width,newx,newy;
  
  format_spec(x,y, string, style, &newx, &newy, &width);
  EZX_DrawString(w, newx, newy, string );
}


void EZX_FormatAt(Drawable w, int x, int y, int background_filled,
		  int underlined, char style, char *string)
{
  int	width,newx,newy;
  
  format_spec(x,y, string, style, &newx, &newy, &width);
  if( background_filled )
    EZX_DrawText(w, newx, newy, string );
  else
    EZX_DrawString(w, newx, newy, string );
  
  if( underlined )
    EZX_DrawLine(w, newx, newy+2, x+width, newy+2 );
}


int  EZX_GetFontHeight(void)
{
  return( theFont->ascent + theFont->descent );
}


int EZX_GetTextWidth(char string[])
{
  return( XTextWidth(theFont, string, strlen(string)) );
}

int EZX_blackWhite(void)
{
  int screen_num;
  
  screen_num = DefaultScreen(theDisplay);
  return ((DefaultDepth(theDisplay, screen_num) == 1) ? 1:0);
}

/* EZX_MakePixmap: Makes a Pixmap with the same dimensions as the given Window.
 *                 EZX drawing functions can operate on Pixmap's.
 * EZX_CopyPixmapToWindow: Copies content of given Pixmap to given Window.
 *   Useful for displaying results without flicker, by first drawing
 *     to a background (Pixmap) buffer.
 *   Good for visualizing graphical differences.           -hsw 93/05/21
 */
Pixmap EZX_MakePixmap(Window w)
{
  Window root;
  int x,y;
  unsigned int width, height, border_width, depth;
  XGetGeometry(theDisplay, w, &root, &x, &y, &width, &height, &border_width,
	       &depth);
  return XCreatePixmap(theDisplay,w,width,height,
		       XDisplayPlanes(theDisplay,
				      XDefaultScreen(theDisplay)));
}

void EZX_CopyPixmapToWindow(Pixmap p, Window w)
{
  Window root;
  int x, y;
  unsigned int width, height, border_width, depth;
  XGetGeometry(theDisplay, p, &root, &x, &y, &width, &height, &border_width,
	       &depth);
  XCopyArea(theDisplay,p,w,theGC,0,0,width,height,0,0);
}

/* Function Name: EZX_DestroyPopupPrompt
 * Description: Destroys the popup dialog widget.
 * Arguments: w - *** UNUSED ***.
 *                 client_data - the dialog widget.  This widget is a direct
 *                               child of the popup shell to destroy.
 *                 call_data - *** UNUSED **.
 * Returns: none.
 */

void EZX_DestroyPopupPrompt(Widget widget, XtPointer client_data,
			    XtPointer call_data)
{
  Widget popup = XtParent( (Widget) client_data);
  XtDestroyWidget(popup);
}

void EZX_CenterPopup(Widget button, Widget popup) 
{
  Arg args[2];
  Position x, y;
  Dimension width, height, popupWidth, popupHeight;
  
  /*
   * This will center the popup at the center of the widget which invoked
   * this callback, which will also.
   */
  
  XtSetArg(args[0], XtNwidth, &width);
  XtSetArg(args[1], XtNheight, &height);
  XtGetValues(button, args, 2);
  XtTranslateCoords(button, (Position)(width/2), (Position)(height/2), &x, &y);
  
  XtSetArg(args[0], XtNwidth, &popupWidth);
  XtSetArg(args[1], XtNheight, &popupHeight);
  XtGetValues(popup, args, 2);
  
  XtSetArg(args[0], XtNx, x-popupWidth/2);
  XtSetArg(args[1], XtNy, y-popupHeight/2);
  XtSetValues(popup, args, 2);

}
