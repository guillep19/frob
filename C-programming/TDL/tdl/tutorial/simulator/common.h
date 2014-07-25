/*****************************************************************************
 * PROJECT: TDL Tutorial
 *
 * (c) Copyright 2001 Reid Simmons. All rights reserved.
 * (c) Copyright 1994 Richard Goodwin & Reid Simmons. All rights reserved.
 *
 * FILE: common.h
 *
 * ABSTRACT:
 * 
 * This file provides some common type and macro definitions.
 * Also provides ANSI C definitions for standard library routines.
 *
 * $Source: /afs/cs.cmu.edu/project/TCA/Master/tcaV8/tutorial/simulator/common.h,v $ 
 * $Revision: 1.7 $
 * $Date: 97/06/17 12:48:35 $
 * $Author: reids $
 *
 * REVISION HISTORY:
 * $Log:	common.h,v $
 *****************************************************************************/

#ifndef COMMON_LOADED
#define COMMON_LOADED

#include <stdio.h>
#include <stdlib.h>

#ifdef __GNUC__
#define INLINE __inline__
#else
#define INLINE
#endif

#define EXTERN extern
#ifndef PRIVATE
#define PRIVATE static
#endif
#define PUBLIC

#ifndef TRUE
#define TRUE 1
#endif
#ifndef FALSE
#define FALSE 0
#endif

typedef double DEGREES;
typedef double RADIANS;
typedef double METERS;
typedef double CMS;
typedef double FEET;

#define DEGREES_NAME   "degrees" /* for IPC usage */
#define DEGREES_FORMAT "double" /* for IPC usage */

#define CMS_NAME       "cms" /* for IPC usage */
#define CMS_FORMAT     "double" /* for IPC usage */

#ifndef ABS
#define ABS(x)	  ((x) >= 0 ? (x) : -(x))
#endif
#ifndef FABS
#define FABS(x)	  ((x) >= 0.0 ? (x) : -(x))
#endif
#ifndef MAX
#define MAX(x,y)  ((x) > (y) ? (x) : (y))
#endif
#ifndef MIN
#define MIN(x,y)  ((x) > (y) ? (y) : (x))
#endif
#ifndef SQR
#define SQR(x)    ((x) * (x))
#endif

#define NEAR(x1,x2,eps) (ABS((x1)-(x2))<=(eps))

#define IRINT(x)  ((int) rint(x))

#ifndef PI
#define PI 3.1415926535897932384626433
#endif

#define SQRT2 1.4142135
#define TWO_PI (2 * PI)

#define RAD_TO_DEG(r)   ((r) * 180.0 / PI)
#define DEG_TO_RAD(d)   ((d) * PI / 180.0)

#define FT2M (12.0/39.37)
#define FEET_TO_METERS(ft) ((ft)*FT2M)
#define METERS_TO_FEET(ms) ((ms)/FT2M)

#define INCHES_TO_CMS(in) (100.0*FEET_TO_METERS((in)/12.0))
#define CMS_TO_INCHES(cm) (12.0*METERS_TO_FEET((cm)/100.0))

#define DEFAULT_LINE_LENGTH 80

#define RAND() (random() / (double) INT_MAX)

#define ALMOST_ZERO  0.000000001

#endif /* COMMON_LOADED */
