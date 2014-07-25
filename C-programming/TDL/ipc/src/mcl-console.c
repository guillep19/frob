//
// File:           mcl-console.c++
//
// Summary:        Console I/O stubs for printing to the listener from C
//
//                 ************************************************************
//                 Copyright (c) 1998 by Tobias Kunze. All Rights Reserved.
//
//                 This library is free software; you can redistribute it
//                 and/or modify it under the terms of the GNU Library
//                 General Public License as published by the Free Software
//                 Foundation; either version 2 of the License, or (at your
//                 option) any later version.
// 
//                 This library is distributed in the hope that it will be
//                 useful, but WITHOUT ANY WARRANTY; without even the
//                 implied warranty of MERCHANTABILITY or FITNESS FOR A
//                 PARTICULAR PURPOSE.  See the GNU Library General Public
//                 License for more details.
//
//                 You should have received a copy of the GNU Library
//                 General Public License along with this library; if not,
//                 write to the Free Software Foundation, Inc., 59 Temple
//                 Place - Suite 330, Boston, MA 02111-1307, USA.
//
//                 Suggestions, comments and bug reports are welcome.  Address
//                 email to tkunze@ccrma.stanford.edu.
//                 ************************************************************
//
// Author:         Tobias Kunze
// E-Mail:         tkunze@ccrma.stanford.edu
// Org:            
//
// Orig-Date:      12-Jul-97 at 20:50:11
// Last-Mod:       17-Dec-97 at 17:30:48 by Tobias Kunze
//
// "$Revision: 2.1 $"
//
// Description:    The last four functions are largely stolen from CW's
//                 SIOUX replacement example code
//
// Bugs:           
//
// Changes:
//    02-Nov-97    tk    Revision 1.1
//
//    Jan-10-98    Reid Simmons: Adapted for my own uses (and added the "event handler" stuff,
//                               which is adapted from the CodeWarrior SIOUX.c file).

#include <MacWindows.h>
#include <ToolUtils.h>
#include <DiskInit.h>
#include <Events.h>
#include <console.h>
#include <Sioux.h>

//
// Constants
// ---------

static const int FD_INVALID = -1;

static const int CONSOLE_NO_ERR = 0;
static const int CONSOLE_ERR = -1;

static char STDIN_NAME[] = "stdin";
static char STDOUT_NAME[] = "stdout";
static char STDERR_NAME[] = "stderr";

static const int STDIN_FD = 0;		// &__files[0]
static const int STDOUT_FD = 1;		// &__files[1]
static const int STDERR_FD = 2;		// &__files[2]


//
// Globals
// -------

static int gCurFD;

typedef void (*Printer_Fn_Type)(char *, long);
typedef void (*Event_Handler_Type)(EventRecord *);

//
// Prototypes
// ----------

static void PrintToLispStub(char* buffer, long n);
static void EventHandlerStub(EventRecord *event);

#pragma export on
void mclSetupLispPrinter(Printer_Fn_Type printer);
void mclSetupEventHandler(Event_Handler_Type eventHandler);
#pragma export off

Printer_Fn_Type  LispPrinter = &PrintToLispStub;
Event_Handler_Type EventHandler = &EventHandlerStub;

//
// PrintToLispStub
// ---------------
// Dummy function.  Initial target of (*LispPrinter).
//
// Arguments:
//   <ignored>
//
// Result:
//   <void>

void PrintToLispStub(char* buffer, long n)
{
#pragma unused (buffer, n)
}

void EventHandlerStub(EventRecord *event)
{
#pragma unused (event)
}

//
// mclSetupLispPrinter
// -------------------
// Used to set the function pointer LispPrinter to the actual LISP callback
// that prints the buffer.
//
// Arguments:
//   printer		printing function to store in LispPrinter
//
// Result:
//   <void>

void mclSetupLispPrinter(Printer_Fn_Type printer)
{
  LispPrinter = printer;
}

void mclSetupEventHandler(Event_Handler_Type eventHandler)
{
  EventHandler = eventHandler;
}

//
// The following four functions provide the UI for the console package.
// Users wishing to replace SIOUX with their own console package need
// only provide the four functions below in a library.


//
// InstallConsole
// --------------
// Installs the Console package, this function will be called right before
// any read or write to one of the standard streams. 
//
// Arguements:
//   fd			stream to read/write to/from
//
// Result:
//   0			success
//   <short>		failure

short InstallConsole(short fd) {
  switch (fd) {
  case STDIN_FD : 
    gCurFD = STDIN_FD;
    break;
  case STDOUT_FD :
    gCurFD = STDOUT_FD;
    break;
  case STDERR_FD :
    gCurFD = STDERR_FD;
    break;
  default:
    return CONSOLE_ERR;
  }
  return CONSOLE_NO_ERR;
}


//
// RemoveConsole
// -------------
// Removes the console package.  It is called after all other streams are
// closed and exit functions (installed by either atexit or _atexit) have
// been called.  Since there is no way to recover from an error, this
// function doesn't need to return any. 
//
// Arguments:
//   <void>
//
// Result:
//   <void>

void RemoveConsole(void) {
  gCurFD = FD_INVALID;
}


//
// WriteCharsToConsole
// -------------------
// Writes a stream of output to the Console window.  This function is called
// by write. 
//
// Arguments:
//   buffer		buffer to be written
//   n			length of the buffer to be written
//
// Result:
//   -1			failure
//   <short>		number of characters written to the stream

long WriteCharsToConsole(char *buffer, long n)
{
  (*LispPrinter)(buffer, n);
  return n;
}


//
// ReadCharsFromConsole
// --------------------
// Reads from the Console into a buffer.  This function is called by read.
//
// Arguments:
//   buffer		buffer to fill with input
//   n			maximal length of buffer
//
// Result:
//   -1			failure
//   <short>		number of characters written to the stream

long ReadCharsFromConsole(char *buffer, long n) {
#pragma unused (buffer, n)
  // don't need input
  return 0;
}

//
// *__ttyname
// ----------
// Return the name of the current terminal (only valid terminals are the
// standard stream (ie stdin, stdout, stderr). 
//
// Arguments:
//   fildes		stream to query
//
// Result:
//   NULL		stream is not valid
//   <char*>		pointer to static global data containing a C string

extern char *__ttyname(long fildes) {
  switch (fildes) {
  case STDIN_FD :
    return STDIN_NAME;
  case STDOUT_FD :
    return STDOUT_NAME;
  case STDERR_FD :
    return STDERR_NAME;
  default :
    return (char*)NULL;
  }
}

short SIOUXHandleOneEvent(EventRecord *userEvent)
{
  if (userEvent != NULL && userEvent->what != nullEvent) {
    (*EventHandler)(userEvent);
    return true;
  } else {
    return false;
  }
}

//
// -*- EOF -*-

