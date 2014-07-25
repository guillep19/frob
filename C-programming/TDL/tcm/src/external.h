/**************************************************************************
 * 
 * PROJECT: Task Control Management.
 *
 * (c) Copyright 1996 Reid Simmons.  All rights reserved.
 *
 * FILE: external.h
 *
 * ABSTRACT: Defines functions for dealing with external events.
 *
 * EXPORTS:
 *
 * $Revision: 1.4 $
 * $Date: 2009/01/15 17:00:04 $
 * $Author: reids $
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tcm/LICENSE.TXT)
 *
 * REVISION HISTORY:
 *
 * $Log: external.h,v $
 * Revision 1.4  2009/01/15 17:00:04  reids
 * Released under simplified BSD open source license
 *
 * Revision 1.3  2001/03/26 21:38:56  trey
 * changed list<T> type to be tcmList<T> to avoid conflict with STL lists
 *
 * Revision 1.2  1998/10/30 11:16:32  da0g
 * Added ExternalEventBypassHandler.
 *
 * Revision 1.1  1997/12/29 17:06:19  reids
 * Version that has the basic functionality needed to support TDL.
 *
 **************************************************************************/

#ifndef INCexternal
#define INCexternal

#include "tcmBasics.h"
#include "event.h"

class External_Event
{
 public:
   External_Event () { _sd = -1; _callback = NULL; _callbackData = NULL; }
   External_Event (int sd) { _sd = sd; _callback = NULL; _callbackData = NULL; }
   External_Event (int sd, SD_CALLBACK_FN_TYPE callback, const void *callbackData)
   	{ _sd = sd; _callback = callback; _callbackData = callbackData; }
   
   BOOLEAN operator== (External_Event const &externalEvent) 
     { return externalEvent._sd == _sd; }
   
   void invoke (void) const { (*_callback)(_sd, _callbackData); }
   
 private:
  int _sd;
  SD_CALLBACK_FN_TYPE _callback;
  const void *_callbackData;
};

typedef tcmList<External_Event> External_Event_List;
typedef Const_List_Iterate<External_Event> Const_External_Event_Iterate;

class External_Events
{
 public:
  External_Events()
    : _maxConnection            ( 0 ),
      externalEventBypassHandler( EXTERNAL_EVENT_BYPASS_HANDLER_TYPE ( NULL ) )
  { FD_ZERO ( & _eventMask ); }
  
  const fd_set &eventMask (void) const { return _eventMask; }
  void     dispatchEvents (struct timeval *timeout);
  BOOLEAN _dispatchEvents (struct timeval *timeout);
  void addExternalEvent (int sd, SD_CALLBACK_FN_TYPE callback, 
			 const void *clientData);
  void removeExternalEvent (int sd);

  EXTERNAL_EVENT_BYPASS_HANDLER_TYPE getBypassHandler() const
					 { return externalEventBypassHandler; }


  void setBypassHandler( EXTERNAL_EVENT_BYPASS_HANDLER_TYPE theBypassHandler )
			     { externalEventBypassHandler = theBypassHandler; }

  BOOLEAN getHasBypassHandler() const
		     { return    getBypassHandler()
			      != EXTERNAL_EVENT_BYPASS_HANDLER_TYPE ( NULL ); }

  BOOLEAN getHasDispatchableEvents() const { return ( _eventList.size() > 0 );}

  BOOLEAN hasEvents (void) const { return (   getHasDispatchableEvents()
					   || getHasBypassHandler()      ); }


 private:
   External_Event *_find (int sd) const;
   BOOLEAN _remove (int sd);

 private:
  fd_set                              _eventMask;
  int                                 _maxConnection;
  External_Event_List                 _eventList;
  EXTERNAL_EVENT_BYPASS_HANDLER_TYPE  externalEventBypassHandler;
};

#endif /* INCexternal */
