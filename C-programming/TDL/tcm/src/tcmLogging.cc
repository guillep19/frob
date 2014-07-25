/*****************************************************************************
 * 
 * PROJECT: Task Control Management.
 *
 * (c) Copyright 1996 Reid Simmons.  All rights reserved.
 *
 * FILE: tcmLogging.cc
 *
 * ABSTRACT: Handle logging of messages and errors for the TCM library.
 *
 * $Source: /afs/cs.cmu.edu/project/TCA/Master/tcaV8/tcm/src/tcmLogging.cc,v $ 
 * $Revision: 1.10 $
 * $Date: 2009/05/04 19:44:49 $
 * $Author: reids $
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tcm/LICENSE.TXT)
 *
 * REVISION HISTORY:
 *
 * $Log: tcmLogging.cc,v $
 * Revision 1.10  2009/05/04 19:44:49  reids
 * Changed to using snprintf to avoid corrupting the stack on overflow
 *
 * Revision 1.9  2009/01/15 17:00:05  reids
 * Released under simplified BSD open source license
 *
 * Revision 1.8  2003/07/30 09:10:34  da0g
 * Endless loop detection now uses the unsuppressible tcmWarning()
 *   instead of tcmMessage().
 * Increased tcmWarning() buffer size.
 *
 * Revision 1.7  2002/07/11 03:49:25  da0g
 * Added NULL_CHAR macro.
 *
 * Revision 1.6  2002/06/26 16:50:41  reids
 * Fixed an array-bounds error found by Insure.
 *
 * Revision 1.5  2000/07/05 23:17:51  da0g
 * Modified MSecs to be a 8-byte integer, thereby fixing the
 * time-wrapping bug that occurs every 7 weeks or so.
 *
 * Revision 1.4  1999/08/04 13:55:45  reids
 * Reduced the number of system calls needed to log information -- should
 *   make the logging much more efficient.
 *
// Revision 1.3  98/06/02  10:39:44  reids
// Increased overall efficiency of TCM.
// 
// Revision 1.2  97/12/04  17:50:28  reids
// Another fairly stable version (except that monitors do not quite work)
// 
// Revision 1.1  97/11/21  14:06:46  reids
// First release of TCM -- seems to be a stable version
// 
 *
 *****************************************************************************/

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <time.h>

#include "tcmLogging.h"
#include "tcmGlobal.h"

#define MAX_CHARS 1023
static char line[MAX_CHARS];

// Return TRUE if the criterion applies to any logging device 
#define TCM_LOG_P(criterion) { \
  BOOLEAN doneBoth = FALSE; Logger logger=GET_TCM_GLOBAL(terminalLogger); \
  for (; !doneBoth; logger=GET_TCM_GLOBAL(fileLogger), doneBoth=TRUE) \
    if (criterion) return TRUE; \
  return FALSE; }

// Return TRUE if messages are being logged to any device 
BOOLEAN tcmLogMessagesP (void)
{
  TCM_LOG_P(logger.file() && logger.message() && !logger.ignoreNow());
}

// Return TRUE if status messages are being logged to any device 
BOOLEAN tcmLogStatusP (void)
{
  TCM_LOG_P(logger.file() && logger.status() && !logger.ignoreNow());
}

// Return TRUE if time is being logged to any device 
BOOLEAN tcmLogTimeP (void)
{
  TCM_LOG_P(logger.file() && logger.time() && !logger.ignoreNow());
}

void tcmMessage(const char *description, ...)
{
  Logger logger;
  BOOLEAN doneBoth = FALSE;
  va_list args;
    
  for (logger=GET_TCM_GLOBAL(terminalLogger);
       !doneBoth; logger=GET_TCM_GLOBAL(fileLogger), doneBoth=TRUE) {
    if (logger.file() && logger.message() && !logger.ignoreNow()) {
      va_start(args, description);
      vfprintf(logger.file(), description, args);
      va_end(args);
      if (logger.flush()) fflush(logger.file());
    }
  }
}

void tcmStatus (const char *description, ...)
{
  Logger logger;
  BOOLEAN doneBoth = FALSE;
  va_list args;
    
  for (logger=GET_TCM_GLOBAL(terminalLogger);
       !doneBoth; logger=GET_TCM_GLOBAL(fileLogger), doneBoth=TRUE) {
    if (logger.file() && logger.status() && !logger.ignoreNow()) {
      va_start(args, description);
      vfprintf(logger.file(), description, args);
      va_end(args);
      if (logger.flush()) fflush(logger.file());
    }
  }
}

void tcmLog (const char *description, ...)
{
  Logger logger;
  BOOLEAN doneBoth = FALSE;
  va_list args;
    
  for (logger=GET_TCM_GLOBAL(terminalLogger);
       !doneBoth; logger=GET_TCM_GLOBAL(fileLogger), doneBoth=TRUE) {
    if (logger.file()) {
      va_start(args, description);
      vfprintf(logger.file(), description, args);
      va_end(args);
      if (logger.flush()) fflush(logger.file());
    }
  }
}

void tcmWarning (const char *description, ...)
{
  Logger logger;
  BOOLEAN doneBoth = FALSE;
  va_list args;
    
  // Do it this way to reduce the number of (expensive) system calls.
  snprintf(line, MAX_CHARS, "WARNING: %s", description);
  for (logger=GET_TCM_GLOBAL(terminalLogger);
       !doneBoth; logger=GET_TCM_GLOBAL(fileLogger), doneBoth=TRUE) {
    if (logger.file()) {
      va_start(args, description);
      vfprintf(logger.file(), line, args);
      va_end(args);
      if (logger.flush()) fflush(logger.file());
    }
  }
}

void tcmError (const char *description, ...)
{
  Logger logger;
  BOOLEAN doneBoth = FALSE;
  va_list args;
    
  // Do it this way to reduce the number of (expensive) system calls.
  snprintf(line, MAX_CHARS, "ERROR: %s", description);
  for (logger=GET_TCM_GLOBAL(terminalLogger);
       !doneBoth; logger=GET_TCM_GLOBAL(fileLogger), doneBoth=TRUE) {
    if (logger.file()) {
      va_start(args, description);
      vfprintf(logger.file(), line, args);
      va_end(args);
      if (logger.flush()) fflush(logger.file());
    }
  }
  tcmEndFileLogging();
  exit(1);
}


#if VXWORKS
void printTimeFromTicks(FILE *fp, int indent)
{
  int  hour, min, sec, hund, tmp;
  unsigned long msec;

  msec = timeInMsecs();
  
  tmp = msec/10;
  hund = (tmp)%100;
  tmp /= 100;
  sec =  (tmp)%60;
  tmp /= 60;
  min =  (tmp)%60;
  tmp /= 60;
  hour = tmp;
  
  fprintf(fp, "%*d:%02d:%02d.%02d", indent+2,hour,min,sec,hund);
}
#endif

#ifdef _WINSOCK_
void printSystemTime(FILE *fp, int indent)
{
  SYSTEMTIME theTime;
  GetSystemTime(&theTime);
  fprintf(fp, "%*d:%02d:%02d.%02d", indent+2,
	  theTime.wHour,
	  theTime.wMinute,
	  theTime.wSecond,
	  theTime.wMilliseconds*10);
}
#endif

void tcmLogTime (int indent)
{
  Logger logger;
  BOOLEAN doneBoth = FALSE;

  if (tcmLogTimeP()) {
#if !defined(VXWORKS) && !defined(_WINSOCK_)
    MSecs msecs = timeInMsecs();
    time_t secs = (time_t)(msecs/1000);
    int hundredths = (msecs%1000)/10;
    struct tm *localTime;
#endif

    for (logger=GET_TCM_GLOBAL(terminalLogger);
	 !doneBoth; logger=GET_TCM_GLOBAL(fileLogger), doneBoth=TRUE) {
      if (logger.file() && logger.time() && !logger.ignoreNow()) {
#if !defined(VXWORKS) && !defined(_WINSOCK_)
	localTime = localtime(&secs);
	fprintf(logger.file(), "%*d:%02d:%02d.%02d", indent+2,
		localTime->tm_hour, localTime->tm_min,
		localTime->tm_sec, hundredths);
#elif defined(VXWORKS)
	printTimeFromTicks(logger.file(), indent);
#elif defined(_WINSOCK_)
	printSystemTime(logger.file(), indent);
#endif
	if (logger.flush()) fflush(logger.file());
      }
    }
  }
}

void tcmLogId (int id, LOG_STATUS_ENUM logStatus)
{
  Logger logger;
  BOOLEAN doneBoth = FALSE;
  
  for (logger=GET_TCM_GLOBAL(terminalLogger);
       !doneBoth; logger=GET_TCM_GLOBAL(fileLogger), doneBoth=TRUE) {
    if (logger.file() && logger.id() && !logger.ignoreNow() &&
	(logStatus == LOGGING_STATUS ? logger.status() :
	 logStatus == LOGGING_MESSAGE ? logger.message() : TRUE)) {
      fprintf(logger.file(), " {%d}", id);
      if (logger.flush()) fflush(logger.file());
    }
  }
}

void tcmLogParentId (int parentId, LOG_STATUS_ENUM logStatus)
{
  Logger logger;
  BOOLEAN doneBoth = FALSE;
  
  for (logger=GET_TCM_GLOBAL(terminalLogger);
       !doneBoth; logger=GET_TCM_GLOBAL(fileLogger), doneBoth=TRUE) {
    if (logger.file() && logger.parentId() && !logger.ignoreNow() &&
	(logStatus == LOGGING_STATUS ? logger.status() :
	 logStatus == LOGGING_MESSAGE ? logger.message() : TRUE)) {
      fprintf(logger.file(), " {%d}", parentId);
      if (logger.flush()) fflush(logger.file());
    }
  }
}

void tcmStartIgnoreLogging (void)
{
  Logger logger;
  BOOLEAN doneBoth = FALSE;
  
  for (logger=GET_TCM_GLOBAL(terminalLogger);
       !doneBoth; logger=GET_TCM_GLOBAL(fileLogger), doneBoth=TRUE) {
    if (logger.ignore()) {
      logger.setIgnoreLoggingNow(TRUE);
    }
  }
}

void tcmEndIgnoreLogging (void)
{
  Logger logger;
  BOOLEAN doneBoth = FALSE;
  
  for (logger=GET_TCM_GLOBAL(terminalLogger);
       !doneBoth; logger=GET_TCM_GLOBAL(fileLogger), doneBoth=TRUE) {
    logger.setIgnoreLoggingNow(FALSE);
  }
}

void tcmStartTerminalLogging (void)
{
  Logger logger;
  
  logger = GET_TCM_GLOBAL(terminalLogger);

  if (logger.file()) {
    fprintf(logger.file(), "Task Control Management %d.%d.%d (%s)\n",
	    TCM_VERSION_MAJOR, TCM_VERSION_MINOR, TCM_VERSION_MICRO,
	    TCM_VERSION_DATE);
  }
}

#define LINE_LENGTH 80
#define COMMENT_LENGTH LINE_LENGTH-3

static void add_comments_to_log_file (FILE *logFile)
{ 
  char comment_string[COMMENT_LENGTH+1];
  static char astericks[] = "********************************************************************************";
  static char comments[] =  "*                                   Comments                                   *";
  static char spacer[] =    "*                                                                              *";
  int last_line_p, has_comments_p = FALSE;
  
  printf("Enter any comments in the log file (end with blank line)\n");
  fflush(stdout);
  
  do {
    fgets(comment_string, COMMENT_LENGTH, stdin);
    last_line_p = (comment_string[0] == '\n'); /* the "newline" character */
    if (last_line_p) {
      if (has_comments_p)
	fprintf(logFile, "%s\n%s\n\n", spacer, astericks);
    }
    else {
      if (!has_comments_p) {
	fprintf(logFile, "\n%s\n%s\n%s\n", astericks, comments, spacer);
	has_comments_p = TRUE;
      }
      /* Replace "newline" character */
      comment_string[strlen(comment_string)-1] = NULL_CHAR;
      fprintf(logFile, "* %-*s*\n", LINE_LENGTH-3, comment_string);
    }
  }
  while (!last_line_p);
}

void tcmStartFileLogging (const char *fileName)
{
  Logger logger;
#ifndef VXWORKS
  time_t secs = timeInMsecs()/1000;
#endif
  
  logger=GET_TCM_GLOBAL(fileLogger);
  if (logger.message() || logger.status() || logger.time()) {
    if (!logger.file()) {
      logger.setLogging(fopen(fileName, "w"), TRUE);
      if (!logger.file()) {
	tcmError("Cannot open file %s\n", fileName);
      }
    }
    printf("Logging to %s\n", fileName);
#ifndef VXWORKS
    fprintf(logger.file(), "Logging Task Control Server %d.%d.%d (%s) on %s",
	    TCM_VERSION_MAJOR, TCM_VERSION_MINOR, TCM_VERSION_MICRO,
	    TCM_VERSION_DATE, ctime(&secs));
    add_comments_to_log_file(logger.file());
#else
    fprintf(logger.file(), "Logging Task Control Server %d.%d.%d (%s)",
	    TCM_VERSION_MAJOR, TCM_VERSION_MINOR, TCM_VERSION_MICRO,
	    TCM_VERSION_DATE);
#endif
  }
}

void tcmEndFileLogging (void)
{
  Logger logger;

  logger=GET_TCM_GLOBAL(fileLogger);
  if (logger.file()) {
#ifndef VXWORKS
    add_comments_to_log_file(logger.file());
#endif
    fclose(logger.file());
    logger.setLogging(NULL, FALSE);
    logger.setMessageLogging(FALSE);
    logger.setStatusLogging(FALSE);
    logger.setTimeLogging(FALSE);
  }
}

/*****************************************************************
 * The following functions write (concatenate) to a string.
 * This can save system calls if the line of text is composed of 
 *   multiple calls).
 *
 * NOTE: Assumes that "string" is big enough to hold the text!
 ****************************************************************/

void tcmStrMessage(Logger logger, char *string, const char *description, ...)
{
  va_list args;
    
  if (logger.message() && !logger.ignoreNow()) {
    va_start(args, description);
    vsnprintf(line, MAX_CHARS, description, args);
    va_end(args);
    strcat(string, line);
  }
}

void tcmStrStatus (Logger logger, char *string, const char *description, ...)
{
  va_list args;
    
  if (logger.status() && !logger.ignoreNow()) {
    va_start(args, description);
    vsnprintf(line, MAX_CHARS, description, args);
    va_end(args);
    strcat(string, line);
  }
}

void tcmStrLog (Logger logger, char *string, const char *description, ...)
{
  va_list args;
    
  va_start(args, description);
  vsnprintf(line, MAX_CHARS, description, args);
  va_end(args);
  strcat(string, line);
}

void tcmStrLogTime (Logger logger, char *string, int indent)
{

  if (logger.time() && !logger.ignoreNow()) {
#if !defined(VXWORKS) && !defined(_WINSOCK_)
    MSecs msecs = timeInMsecs();
    time_t secs = (time_t)(msecs/1000);
    int hundredths = (msecs%1000)/10;
    struct tm *localTime;
#endif

#if !defined(VXWORKS) && !defined(_WINSOCK_)
    localTime = localtime(&secs);
    snprintf(line, MAX_CHARS, "%*d:%02d:%02d.%02d", indent+2,
	     localTime->tm_hour, localTime->tm_min,
	     localTime->tm_sec, hundredths);
#elif defined(VXWORKS)
    tcmError("Need to implement tcmStrLogTime for VXWORKS!");
#elif defined(_WINSOCK_)
    tcmError("Need to implement tcmStrLogTime for Windows!");
#endif
    strcat(string, line);
  }
}

void tcmStrLogId (Logger logger, char *string,
		  int id, LOG_STATUS_ENUM logStatus)
{
  if (logger.id() && !logger.ignoreNow() &&
      (logStatus == LOGGING_STATUS ? logger.status() :
       logStatus == LOGGING_MESSAGE ? logger.message() : TRUE)) {
    snprintf(line, MAX_CHARS, " {%d}", id);
    strcat(string, line);
  }
}

void tcmStrLogParentId (Logger logger, char *string,
			int parentId, LOG_STATUS_ENUM logStatus)
{
  if (logger.parentId() && !logger.ignoreNow() &&
      (logStatus == LOGGING_STATUS ? logger.status() :
       logStatus == LOGGING_MESSAGE ? logger.message() : TRUE)) {
    snprintf(line, MAX_CHARS, " {%d}", parentId);
    strcat(string, line);
  }
}
