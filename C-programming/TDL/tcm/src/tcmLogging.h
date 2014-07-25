/*****************************************************************************
 * 
 * PROJECT: Task Control Management.
 *
 * (c) Copyright 1996 Reid Simmons.  All rights reserved.
 *
 * FILE: tcmLogging.h
 *
 * ABSTRACT: Handle logging of messages and errors for the TCM library.
 *
 * Functions:
 *   The following five functions take a variable number of arguments,
 *   just like printf.
 *
 *   tcmMessage (decription, ...<args>) :
 *     Logs to a device (terminal or file) if _Log_Message is not 0;
 *
 *   tcmStatus (decription, ...<args>) :
 *     Logs to a device (terminal or file) if _Log_Status is not 0;
 *
 *   tcmLog (decription, ...<args>) :
 *     Unconditionally logs onto all open devices (terminal or log file).
 *
 *   tcmWarning (decription, ...<args>) :
 *     Unconditionally logs a warning message onto all open devices 
 *     (terminal or log file).
 *
 *   tcmError (decription, ...<args>) :
 *     Unconditionally logs an error message onto all open devices
 *     (terminal or log file), and exits.
 *
 *   tcmLogTime (indent) :
 *     Logs the time (hh:mm:sec.milli), indenting "indent" spaces.
 *     Logs onto a device (terminal or log file) if_Log_Time is not 0;
 *
 *  tcmLogId (int id, LOG_STATUS_ENUM logStatus) :
 *     Log the id of the node;
 *     logStatus indicates what type of message this is part of
 *
 *  tcmLogParentId (int parentId, LOG_STATUS_ENUM logStatus) :
 *     Log the id of the parent of the node;
 *     logStatus indicates what type of message this is part of
 *
 *   tcmStartIgnoreLogging () :
 *     Starting now, don't log messages or status reports to devices (terminal
 *     or log file) if _Log_Ignore is TRUE.
 *
 *   tcmEndIgnoreLogging () :
 *     Reinstate logging messages and status reports
 *
 *   tcmStartTerminalLogging () :
 *     Prints out the TCM header and version number.
 *
 *   tcmStartFileLogging () :
 *     If either File_Log_Messages or File_Log_Status are non-zero,
 *     opens the file given by Log_File_Name and prints out the TCM
 *     header and version number.
 *     Sets signal functions to trap errors and close log file
 *     Gives user option to enter initial comments to file.
 *
 *   tcmEndFileLogging () :
 *     Gives user option to enter final comments to file.
 *     Close the Log_File (if open).
 *
 *   THE FOLLOWING FUNCTIONS WRITE (CONCATENATE) TO A STRING (THIS CAN SAVE
 *     SYSTEM CALLS IF THE LINE OF TEXT IS COMPOSED OF MULTIPLE CALLS).
 *
 *   tcmStrMessage (logger, string, decription, ...<args>) :
 *     Concatenates the text to the string if _Log_Message is not 0;
 *
 *   tcmStrStatus (logger, string, decription, ...<args>) :
 *     Concatenates the text to the string if _Log_Status is not 0;
 *
 *   tcmStrLog (logger, string, decription, ...<args>) :
 *     Unconditionally concatenates the text to the string.
 *
 *   tcmStrLogTime (logger, string, indent) :
 *     Logs the time (hh:mm:sec.milli), indenting "indent" spaces.
 *     Concatenates the time to the string if_Log_Time is not 0;
 *
 *  tcmStrLogId (logger, string, int id, LOG_STATUS_ENUM logStatus) :
 *     Log the id of the node;
 *     logStatus indicates what type of message this is part of
 *
 *  tcmStrLogParentId(logger, string, int parentId, LOG_STATUS_ENUM logStatus):
 *     Log the id of the parent of the node;
 *     logStatus indicates what type of message this is part of
 *
 *
 * $Source: /afs/cs.cmu.edu/project/TCA/Master/tcaV8/tcm/src/tcmLogging.h,v $ 
 * $Revision: 1.5 $
 * $Date: 2009/01/15 17:00:05 $
 * $Author: reids $
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tcm/LICENSE.TXT)
 *
 * REVISION HISTORY:
 *
 * $Log: tcmLogging.h,v $
 * Revision 1.5  2009/01/15 17:00:05  reids
 * Released under simplified BSD open source license
 *
 * Revision 1.4  1999/08/04 13:55:44  reids
 * Reduced the number of system calls needed to log information -- should
 *   make the logging much more efficient.
 *
 * Revision 1.3  98/06/02  10:39:45  reids
 * Increased overall efficiency of TCM.
 * 
 * Revision 1.2  97/12/04  17:50:29  reids
 * Another fairly stable version (except that monitors do not quite work)
 * 
 * Revision 1.1  97/11/21  14:06:47  reids
 * First release of TCM -- seems to be a stable version
 * 
 *
 *****************************************************************************/

#ifndef INCtcmLogging
#define INCtcmLogging

#include <stdio.h>
#include "tcmBasics.h"

// What type of message is this a part of
typedef enum { LOGGING_ALWAYS, LOGGING_MESSAGE,
	       LOGGING_STATUS} LOG_STATUS_ENUM;

class Logger
{
 public:
  Logger()
    {
      _file = NULL;
      _message = _status = TRUE;
      _id = _parentId = _flush = _ignore = TRUE;
      _time = _ignoreNow = FALSE;
    }

  void setLogging (FILE *file, BOOLEAN flush) { _file = file; _flush = flush; }
  void setMessageLogging (BOOLEAN message) { _message = message; }
  void setStatusLogging (BOOLEAN status) { _status = status; }
  void setTimeLogging (BOOLEAN time) { _time = time; }
  void setIdLogging (BOOLEAN id) { _id = id; }
  void setParentIdLogging (BOOLEAN parentId) { _parentId = parentId; }
  void setIgnoreLogging (BOOLEAN ignore) { _ignore = ignore; }
  void setIgnoreLoggingNow (BOOLEAN ignoreNow) { _ignoreNow = ignoreNow; }

  FILE *file (void) const { return _file; }
  BOOLEAN flush (void) const { return _flush; }
  BOOLEAN message (void) const { return _message; }
  BOOLEAN status (void) const { return _status; }
  BOOLEAN time (void) const { return _time; }
  BOOLEAN id (void) const { return _id; }
  BOOLEAN parentId (void) const { return _parentId; }
  BOOLEAN ignore (void) const { return _ignore; }
  BOOLEAN ignoreNow (void) const { return _ignoreNow; }

 private:
  FILE *_file;
  BOOLEAN _flush, _message, _status, _time, _id, _parentId;
  BOOLEAN _ignore, _ignoreNow;
};

/* Return TRUE if messages/status/time is being logged to any device */
BOOLEAN tcmLogMessagesP (void);
BOOLEAN tcmLogStatusP (void);
BOOLEAN tcmLogTimeP (void);

/* These functions take a variable number of arguments, just like printf */
void tcmMessage (const char *description, ...);
void tcmStatus (const char *description, ...);
void tcmLog (const char *description, ...);
void tcmWarning (const char *description, ...);
void tcmError (const char *description, ...);

void tcmLogTime (int indent);
void tcmLogId (int id, LOG_STATUS_ENUM logStatus);
void tcmLogParentId (int parentId, LOG_STATUS_ENUM logStatus);

void tcmStartIgnoreLogging (void);
void tcmEndIgnoreLogging (void);
void tcmStartTerminalLogging (void);
void tcmStartFileLogging (const char *fileName);
void tcmEndFileLogging (void);

void tcmStrMessage(Logger logger, char *string, const char *description, ...);
void tcmStrStatus (Logger logger, char *string, const char *description, ...);
void tcmStrLog (Logger logger, char *string, const char *description, ...);
void tcmStrLogTime (Logger logger, char *string, int indent);
void tcmStrLogId (Logger logger, char *string,
		  int id, LOG_STATUS_ENUM logStatus);
void tcmStrLogParentId (Logger logger, char *string,
			int parentId, LOG_STATUS_ENUM logStatus);

#endif /* INCtcmLogging */
