/*****************************************************************************
 * PROJECT: TDL Tutorial
 *
 * (c) Copyright 2001 Reid Simmons. All rights reserved.
 * (c) Copyright 1994 Richard Goodwin & Reid Simmons. All rights reserved.
 *
 * FILE: files.c
 *
 * ABSTRACT: This file provides the routines to read the parameter file.
 *
 * $Source: /afs/cs.cmu.edu/project/TCA/Master/tcaV8/tutorial/simulator/files.c,v $ 
 * $Revision: 1.3 $
 * $Date: 1995/04/07 05:09:52 $
 * $Author: rich $
 *
 * REVISION HISTORY:
 *
 * $Log: files.c,v $
 *****************************************************************************/

#include "common.h"
#include "draw_env.h"
#include "updates.h"
#include "files.h"
#include "action.h"
#include "sensors.h"
#include "environment.h"

/*****************************************************************************
 * Global constants
 *****************************************************************************/

/*****************************************************************************
 * Global variables
 *****************************************************************************/

struct _object objs[MAX_OBJ];
int n_obj = 0;
int id_obj = 0;
int pause_sim = 0;
double units_per_cm = 1.0;

/*****************************************************************************
 * Private variables
 *****************************************************************************/

struct _object *add_room(double x1, double y1, double x2, double y2,
			 char *name, int status)
{
  objs[n_obj].x1 = x1;
  objs[n_obj].y1 = y1;
  objs[n_obj].x2 = x2;
  objs[n_obj].y2 = y2;
  objs[n_obj].type = O_ROOM;
  objs[n_obj].objID = id_obj++; /* The id of initial objects is the index */
  objs[n_obj].status = status;
  objs[n_obj].room_name = strcpy((char *)calloc(strlen(name),sizeof(char)),
				 name);
  if (objs[n_obj].x1 >= objs[n_obj].x2 || objs[n_obj].y1 >= objs[n_obj].y2)
    {
      fprintf(stderr,"\n### Error invalid room offsets # obj %d", n_obj);
      return (struct _object *) NULL;
    }
  else 
    n_obj++;
  return &(objs[n_obj-1]);
}


struct _object *add_room_size(double x1, double y1, double width,
			      double height,
			      int status)
{
  char *noname=NULL;
  return add_room(x1, y1, x1+width, y1+height, noname, status);
}

struct _object *add_door(double x1, double y1, double x2, double y2, int open)
{
  objs[n_obj].x1 = x1;
  objs[n_obj].y1 = y1;
  objs[n_obj].x2 = x2;
  objs[n_obj].y2 = y2;
  if (open)
    objs[n_obj].type = O_OPEN_DOOR;
  else
    objs[n_obj].type = O_CLOSE_DOOR;
  
  objs[n_obj].objID = id_obj++; /* The id of initial objects is the index */
  if (objs[n_obj].x1 >= objs[n_obj].x2 || objs[n_obj].y1 >= objs[n_obj].y2)
    {
      fprintf(stderr,"\n### Error invalid door offsets # obj #%d", n_obj);
      return (struct _object *) NULL;
    }
  else 
    n_obj++;
  return &(objs[n_obj-1]);
}


struct _object *add_door_size(double x1, double y1, double width, int open)
{
  /* fix to find the orientation given the lower left corner */
  /* this is a hack for map entry from speech */
  return add_door(x1, y1, x1+width, y1, open);
}

struct _object *add_obst(int status, double x1, double y1,
			 double x2, double y2)
{
  int in_room, i;
  
  objs[n_obj].status = status;
  objs[n_obj].x1 = x1;
  objs[n_obj].y1 = y1;
  objs[n_obj].x2 = x2;
  objs[n_obj].y2 = y2;
  objs[n_obj].type = O_RECTANGLE;
  objs[n_obj].objID = id_obj++; /* The id of initial objects is the index */
  in_room = FALSE;
  for (i = 0; i < n_obj; i++){
    if (objs[n_obj].x1 >= objs[i].x1
	&& objs[n_obj].x2 <= objs[i].x2
	&& objs[n_obj].y1 >= objs[i].y1
	&& objs[n_obj].y2 <= objs[i].y2){
      in_room = TRUE;
    }
  }
  if (in_room == FALSE ||
      objs[n_obj].x1 >= objs[n_obj].x2 || 
      objs[n_obj].y1 >= objs[n_obj].y2) {
    
    fprintf(stderr,"\n### Error in #%d: obst not in room/door", n_obj);
    return (struct _object *) NULL;
  }
  else	
    n_obj++;
  return &(objs[n_obj-1]);
}


struct _object *add_obst_size(int status, 
			      double x1, double y1, double width, double height)
{
  return add_obst(status, x1, y1, x1+width, y1+height);
}

struct _object *add_round(int status, double x1, double y1, double diameter)
{
  int in_room, i;
  
  objs[n_obj].prev_status = status;
  objs[n_obj].status = 0;
  objs[n_obj].x1 = x1;
  objs[n_obj].y1 = y1;
  objs[n_obj].diameter = diameter;
  objs[n_obj].type = O_ROUND;
  objs[n_obj].objID = id_obj++; /* The id of initial objects is the index */
  in_room = FALSE;
  for (i = 0; i < n_obj; i++){
    if (objs[n_obj].x1-objs[n_obj].diameter >= objs[i].x1
	&& objs[n_obj].x1+objs[n_obj].diameter <= objs[i].x2
	&& objs[n_obj].y1-objs[n_obj].diameter >= objs[i].y1
	&& objs[n_obj].y1+objs[n_obj].diameter <= objs[i].y2){
      in_room = TRUE;
    }
  }
  if (in_room == FALSE ||objs[n_obj].diameter <= 0.0) {
    fprintf(stderr,"\n### Error in #%d: round object not in room/door", n_obj);
    return (struct _object *) NULL;
  }
  else	
    n_obj++;
  return &(objs[n_obj-1]);
}


struct _object *delete_obj(void)
{ /* delete the last object added */
  if (n_obj > 0) {
    n_obj--;
    return &objs[n_obj+1];
  }
  return NULL;
}

struct _object *redo_obj(void)
{ /* delete the last object added */
  if (n_obj < MAX_OBJ) {
    n_obj++;
    return &objs[n_obj-1];
  }
  return NULL;
}

void read_environment(file_name)
     char *file_name;
{
  FILE *fp;
  
  static char command[DEFAULT_LINE_LENGTH];
  static int  int_param1;
  static double  double_param1, double_param2, double_param3, double_param4;
  static char str_param1[DEFAULT_LINE_LENGTH];
  static char line_str[DEFAULT_LINE_LENGTH];
  
  int exit_flag = 0;
  int nlines = 0;
  int i;
  int status;
  
  fprintf(stderr,"Reading %s...", file_name);
  if( (fp = fopen(file_name, "r")) == NULL ) {
    fprintf( stderr, "cannot open %s\n", file_name );
    exit( 1 );
  }
  while( !exit_flag && fscanf(fp, "%s", command) == 1) {
    nlines++;
    
    if( strcmp(command, "end") == 0 ) {
      exit_flag = 1;
    }
    
    /*---------------------------------------------------------------*/
    
    /* room <low-x> <high-x> <low-y> <high-y> <room_name> */
    
    else if (!strcmp(command, "room") || !strcmp(command, "corridor") ||
	     !strcmp(command, "area")) {
      fgets(line_str,DEFAULT_LINE_LENGTH,fp);
      i=sscanf(line_str, "%lf %lf %lf %lf %s", &double_param1, &double_param2,
	       &double_param3, &double_param4, str_param1);
      status = (!strcmp(command, "corridor") ? CORRIDOR_STATUS
		: !strcmp(command, "area") ? AREA_STATUS : ROOM_STATUS);
      if (add_room(double_param1, double_param3, double_param2, double_param4,
		   ((i==4)?"name_not_given":str_param1), status)
	  == NULL)
	fprintf(stderr,"\n### Error while reading %s in #%d: obj #%d",
		file_name, nlines, n_obj);
    }

    /*---------------------------------------------------------------*/

    /* door <low-x> <high-x> <low-y> <high-y>
       <close/open>                                            */
    
    else if (!strcmp(command, "door")){
      fscanf(fp, "%lf %lf %lf %lf %s", &double_param1, &double_param2,
	     &double_param3, &double_param4, str_param1);
      if (add_door(double_param1, double_param3, double_param2, double_param4,
		   (!strcmp(str_param1, "open"))) == NULL)
	fprintf(stderr,"\n### Error while reading %s in #%d: obj #%d",
		file_name, nlines, n_obj);
    }
    
    
    /*---------------------------------------------------------------*/
    
    /* obst   <shape> <status>
       
       shape==rectangle   <low-x> <high-x> <low-y> <high-y>
       shape==circle      <mid-x> <mid-y> <diameter>                 */

    else if (!strcmp(command, "obst")){
      
      fscanf(fp, "%s %d %lf %lf %lf %lf", str_param1, &int_param1,
	     &double_param1, &double_param2, &double_param3, &double_param4);
      if (!strcmp(str_param1, "rect")){
	if (add_obst(int_param1, double_param1, double_param3, 
		     double_param2, double_param4) == NULL)
	  fprintf(stderr,
		  "\n### Error while reading %s in #%d: obst not in room/door",
		  file_name, nlines);
      }
      else if (!strcmp(str_param1, "circle")){

	/* For the robot competition: the "prev_status" is the object number */
	if (add_round(int_param1, double_param1, double_param2, 
		      double_param3) == NULL)
	  fprintf(stderr,
		  "\n### Error while reading %s in #%d: obst not in room/door",
		  file_name, nlines);
      }
      else
	fprintf(stderr,
		"\n### Error while reading %s in #%d: obj #%d: unknown shape",
		file_name, nlines, n_obj);
    }
    
    /*---------------------------------------------------------------*/
    
    /* environment  <name> <scale-factor>                            */
    
    else if (!strcmp(command, "environment")){
      fscanf(fp, "%s %lf",env_name, &scale);
    }
    
    /*---------------------------------------------------------------*/
    /* robot  <position_x>   <position_y> <orientation> <radius> <stepsize> */
    
    else if (!strcmp(command, "robot")){
      int in_room = FALSE;
      double deg;
      
      fscanf(fp, "%lf %lf %lf %lf", &robot_x, &robot_y, &deg, &robot_r);
      for (; deg >=  360; deg -= 360);
      for (; deg <   0  ; deg += 360);
      robot_o = ((double) deg / 180) * PI;
      for (i = 0; i < n_obj; i++){
	if (robot_x >= objs[i].x1+robot_r
	    && robot_x <= objs[i].x2-robot_r
	    && robot_y >= objs[i].y1+robot_r
	    && robot_y <= objs[i].y2-robot_r){
	  in_room = TRUE;
	  actual_object = i;
	  actual_freespace = -1;
	  break;
	}
      }
      if (in_room == FALSE)
	fprintf(stderr,"\n### Error while reading %s in #%d: robot not in room/door",
		file_name, nlines);
    }
    
    /*---------------------------------------------------------------*/
    /* sensors  <#number> <measurements_per_sensor> <distance> <range>
       <rotation_mode>                                      */
    
    else if (!strcmp(command, "sensors")){
      fscanf(fp, "%d %d %lf %lf %d", &n_sensors, &n_sensors_fact,
	     &sensors_distance, &sensors_range, &rotation_mode);
      sensors_range = (sensors_range / 180.0) * PI;
    }
    
    else if (!strcmp(command, "units_per_cm")){
      double f;
      fscanf(fp, "%lf", &f);
      units_per_cm = f;
    }
    
    else if (!strcmp(command, "units_per_ft")){
      double f;
      fscanf(fp, "%lf", &f);
      units_per_cm = f/CMPERFOOT;
    }
    
    /*---------------------------------------------------------------*/
    /* nodisplay                                                       */
    
    else if (!strcmp(command, "nodisplay")){
      displayp = 0;
    }
    
    /*---------------------------------------------------------------*/
    
    
    /*---------------------------------------------------------------*/
    /* constants                                          */
    
    else if (!strcmp(command, "sensor_noise_level")){
      fscanf(fp, "%lf", &sensor_noise_level);
    }
    else if (!strcmp(command, "sensor_zero")){
      fscanf(fp, "%lf", &sensor_zero);
      sensor_zero = (sensor_zero / 180.0) * PI;
    }
    else if (!strcmp(command, "forward_noise_level")){
      fscanf(fp, "%lf", &forward_noise_level);
    }
    else if (!strcmp(command, "turning_noise_level")){
      fscanf(fp, "%lf", &turning_noise_level);
      /*turning_noise_level = (turning_noise_level / 180) * PI;*/
    }
    else if (!strcmp(command, "time_scale")){
      fscanf(fp, "%ld", &timeScale);
    }
    else if (!strcmp(command, "time_increment")){
      double ftimeIncrement;
      fscanf(fp, "%lf", &ftimeIncrement);
      timeIncrement = (int) (1000 * ftimeIncrement);
    }
    else if (!strcmp(command, "refresh_rate")) {
      fscanf(fp, "%d", &refreshRate);
    }

    /*---------------------------------------------------------------*/
    
    /* #                                                             */
    
    else if (!strcmp(command, "#")){
      fgets( str_param1, DEFAULT_LINE_LENGTH, fp );
    }
    
    /*---------------------------------------------------------------*/
    
    else /* unknown command */ {
      fprintf(stderr,
	      "\n### Error while reading %s unknown command #%d: %s  (exit)",
	      file_name, nlines, command);
      exit_flag = 1;
    }
    if (fscanf(fp, "\n") == 1) nlines++;
  }
  
  fprintf(stderr,"..finished.\n");
  fclose(fp);
}
