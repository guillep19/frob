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
#include <osek_com.h>
#include <com.h>
#include <com_ext.h>
#include "com_cfg.h"
#define MsgType(X)	(X>>8)
#define MsgNum(X)	(X&0xff)

static int COMStarted = FALSE;

StatusType SendMessage(MessageIdentifier Message, ApplicationDataRef Data) {
	MessageIdentifier *tmp,rcv;
	if(COMStarted==FALSE) return E_COM_NOMSG;
	switch(MsgType(Message)) {
	case 0x40:
		tmp=sndlist_snd_in_norm[MsgNum(Message)];
		SuspendAllInterrupts();
		while(*tmp != MSG_NULL) {
			rcv = *tmp;
			switch(MsgType(rcv)) {
				case 0x04:
					if(filter_rcv_unq_norm[MsgNum(rcv)]()==E_OK) {
						copyfunc_rcv_unq_norm[MsgNum(rcv)](msgbuf_rcv_unq_norm[MsgNum(rcv)],Data);
					}
			}
			tmp++;
		}
		ResumeAllInterrupts();
		tmp=sndlist_snd_in_norm[MsgNum(Message)];
		while(*tmp != MSG_NULL) {
			rcv = *tmp;
			switch(MsgType(rcv)) {
				case 0x04:
					if(filter_rcv_unq_norm[MsgNum(rcv)]()==E_OK) {
						notification1_rcv_unq_norm[MsgNum(rcv)]();
					}
			}
			tmp++;
		}
		return E_OK;
	default:
		return E_COM_NOMSG;
	}
}
StatusType RecieveMessage(MessageIdentifier Message, ApplicationDataRef Data) {
	if(COMStarted==FALSE) return E_COM_NOMSG;
	switch(MsgType(Message)) {
	case 0x04:
		SuspendAllInterrupts();
		copyfunc_rcv_unq_norm[MsgNum(Message)](Data,msgbuf_rcv_unq_norm[MsgNum(Message)]);
		ResumeAllInterrupts();
		return E_OK;
	default:
		return E_COM_NOMSG;
	}
}
static COMApplicationModeType CurrentCOMAppMode;
StatusType StartCOM(COMApplicationModeType Mode) {
	CurrentCOMAppMode = Mode;
	init_flags();
	unque_init();
	COMStarted = TRUE;
#ifdef USE_COMSTARTCOMEXTENSION
	return StartCOMExtention();
#else
	return E_OK;
#endif
}

StatusType StopCOM(COMShutdownModeType Mode) {
	COMStarted=FALSE;
	if(Mode == COM_SHUTDOWN_IMMEDIATE)
		return E_OK;
	return E_COM_NOMSG;
}

COMApplicationModeType GetCOMApplicationMode(void) {
	return CurrentCOMAppMode;
}

StatusType InitMessage(MessageIdentifier Message, ApplicationDataRef Data) {
	switch(MsgType(Message)) {
	case 0x40:
		return E_OK;
	case 0x04:
		SuspendAllInterrupts();
		copyfunc_rcv_unq_norm[MsgNum(Message)](msgbuf_rcv_unq_norm[MsgNum(Message)],Data);
		ResumeAllInterrupts();
		return E_OK;
		break;
	default:
		return E_COM_NOMSG;
	}
}
