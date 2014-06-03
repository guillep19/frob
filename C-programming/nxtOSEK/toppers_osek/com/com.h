/************************************************************************
  (C)Copyright Naohiko Shimizu/IP ARCH, Inc. 2012.
  All rights are reserved.

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.


  Contact information:
  Naohiko Shimizu, Ph.D
  email: nshimizu@ip-arch.jp
  URL: http://www.ip-arch.jp/
************************************************************************/
#ifndef _OSEK_COM
#define _OSEK_COM

#include <osek.h>

#define MAX_COMAPPMODE	16
#define MAX_MSG	256

#define E_COM_ID	((StatusType)1)
#define E_COM_LENGTH	((StatusType)2)
#define E_COM_LIMIT	((StatusType)3)
#define E_COM_NOMSG	((StatusType)4)

#define E_COM_INVALID	((StatusType)255)

typedef	UINT16	MessageIdentifier;
typedef	void*	ApplicationDataRef;
typedef UINT16	COMLengthType;
typedef	COMLengthType*	LengthRef;
typedef	enum {COM_FALSE,COM_TRUE}	FlagValue;
typedef	UINT16	COMApplicationModeType;
typedef enum {COM_SHUTDOWN_IMMEDIATE}	COMShutdownModeType;
typedef	FlagValue	CalloutReturnType;
typedef	enum {
	COMServiceid_StartCOM,
	COMServiceid_StopCOM,
	COMServiceid_GetCOMApplicationMode,
	COMServiceid_InitMessage,
	COMServiceid_StartPeriodic,
	COMServiceid_StopPeriodic,
	COMServiceid_ReadFlag,
	COMServiceid_ResetFlag,
	COMServiceid_SendMessage,
	COMServiceid_RecieveMessage,
	COMServiceid_SendDynamicMessage,
	COMServiceid_ReceiveDynamicMessage,
	COMServiceid_SendZeroMessage,
	COMServiceid_GetMessageStatus,
	COMServiceid_COMErrorGetServiceid,
	COMServiceid_COMErrorHook,
	COMServiceid_COMCallout,
	COMServiceid_StartCOMExtension
	} COMServiceIdType;


extern StatusType StartCOM(COMApplicationModeType Mode);
extern StatusType StartCOMExtension(void);
extern StatusType StopCOM(COMShutdownModeType Mode);
extern COMApplicationModeType GetCOMApplicationMode(void);
extern StatusType InitMessage(MessageIdentifier Message, ApplicationDataRef Dataref);
extern StatusType StartPeriodic(void);
extern StatusType StopPeriodic(void);
extern StatusType SendMessage(MessageIdentifier Message, ApplicationDataRef Dataref);
extern StatusType RecieveMessage(MessageIdentifier Message, ApplicationDataRef Dataref);
extern StatusType SendDynamicMessage(MessageIdentifier Message, ApplicationDataRef Dataref, LengthRef LengthRef);
extern StatusType ReceiveDynamicMessage(MessageIdentifier Message, ApplicationDataRef Dataref, LengthRef LengthRef);
extern StatusType SendZeroMessage(MessageIdentifier Message);
extern StatusType GetMessageStatus(MessageIdentifier Message);
extern COMServiceIdType COMErrorGetServiceid(void);
extern void COMErrorHook(StatusType Error);
extern StatusType StartCOMExtension(void);
#endif
