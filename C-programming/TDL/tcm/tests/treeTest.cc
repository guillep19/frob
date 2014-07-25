/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tcm/LICENSE.TXT)
 */
#include <stdio.h>
#include <stdlib.h>
#include "tcm.h"

#define PRINT_OUT
// For doing the "external" test
#include <fcntl.h>
#include <unistd.h>
//extern "C" int read (int, void *, unsigned int);
//extern "C" int write (int, const void *, unsigned int);
#include <netdb.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <sys/socket.h>
/* defines that should be in <sys/socket.h> */
//extern "C" int close (int);
#if !defined(linux) && !defined(__svr4__) && !defined(__sgi)
extern "C" int accept (int, struct sockaddr *, socklen_t *);
#if (!defined(__GNUC__) || (__GNUC__ < 4))
extern "C" int bind (int, struct sockaddr *, int);
extern "C" int setsockopt (int s, int level, int optname,
			   const char *optval, int optlen);
#endif
extern "C" int connect (int s, const struct sockaddr *name, int namelen);
extern "C" int getsockopt (int, int, int, void *optval, int *);
extern "C" int listen (int, int);
extern "C" int socket (int, int, int);
#endif
//extern "C" int gethostname (char *name, unsigned int namelen);

#define BACKLOG 5
#define PROTOCOL_NAME "tcp"
#define TCP_SOCKET_BUFFER (16*1024)
#define HOST_NAME_SIZE   32
#define PORT_NUM 4567

#define DOING_TEST(x) (whichTest == (x))

#define BASIC_TEST      0
#define COMPLETION_TEST 1

#define TIMING_TEST1 11
#define TIMING_TEST2 12
#define TIMING_TEST3 13

#define SUSPEND_TEST1 21
#define SUSPEND_TEST2 22
#define SUSPEND_TEST3 23
#define SUSPEND_TEST4 24

#define EXTERNAL_TEST1 31

#define SIGNAL_TEST1 41
#define SIGNAL_TEST2 42

#define EXCEPTION_TEST1 51
#define EXCEPTION_TEST2 52
#define EXCEPTION_TEST3 53

#define MONITOR_TEST1 61
#define MONITOR_TEST2 62
#define MONITOR_TEST3 63
#define MONITOR_TEST4 64
#define MONITOR_TEST5 65
#define MONITOR_TEST6 66
#define MONITOR_TEST7 67

#define TERMINATE_TEST 100
#define TERMINATE_TEST2 TERMINATE_TEST+2
#define TERMINATE_TEST3 TERMINATE_TEST+3
#define TERMINATE_TEST4 TERMINATE_TEST+4
#define TERMINATE_TEST5 TERMINATE_TEST+5
#define TERMINATE_TEST6 TERMINATE_TEST+6
#define TERMINATE_TEST7 TERMINATE_TEST+7
#define TERMINATE_TEST8 TERMINATE_TEST+8
#define TERMINATE_TEST9 TERMINATE_TEST+9
#define TERMINATE_TEST10 TERMINATE_TEST+10
#define TERMINATE_TEST11 TERMINATE_TEST+11
#define TERMINATE_TEST12 TERMINATE_TEST+12
#define TERMINATE_TEST13 TERMINATE_TEST+13
#define TERMINATE_TEST14 TERMINATE_TEST+14
#define TERMINATE_TEST15 TERMINATE_TEST+15
#define TERMINATE_TEST16 TERMINATE_TEST+16
#define TERMINATE_TEST17 TERMINATE_TEST+17
#define TERMINATE_AT_TEST         201
#define TERMINATE_IN_TEST         202
#define TERMINATE_AT_AFTER_TEST1  203
#define TERMINATE_AT_AFTER_TEST1a 204
#define TERMINATE_AT_AFTER_TEST2  205
#define TERMINATE_AT_AFTER_TEST2a 206
#define TERMINATE_ON_TERMINATION1 207
#define TERMINATE_ON_TERMINATION2 208
#define TERMINATE_ON_TERMINATION3 209
#define TERMINATE_AT_TEST2        210
#define TERMINATE_AT_AFTER_TEST3  211

#define DEALLOCATE_TEST1 301
#define DEALLOCATE_TEST2 302

int whichTest = BASIC_TEST;

// Forward references
static void AHnd (const TCM_Task_Tree_Ref &node, const void *data);
static void BHnd (const TCM_Task_Tree_Ref &node, const void *data);
static void CHnd (const TCM_Task_Tree_Ref &node, const void *data);
static void DHnd (const TCM_Task_Tree_Ref &node, const void *data);
static void EHnd (const TCM_Task_Tree_Ref &node, const void *data);

int writeSd = -1;
int readSd = -1;

STRING exceptionName = "Test Failure";
STRING exceptionName2 = "Test Failure2";

static void Excep1Hnd (TCM_Task_Tree_Ref const &node, const void *data);
static void Excep2Hnd (TCM_Task_Tree_Ref const &node, const void *data);
static void Excep3Hnd (TCM_Task_Tree_Ref const &node, const void *data);

class Exception2 : public TCM_Exception
{
public:
  Exception2(const void *data) : TCM_Exception(exceptionName2, data) {}

  virtual BOOLEAN matches ( STRING theString ) const {
    return (!strcmp(theString, exceptionName) ||
	    !strcmp(theString, exceptionName2));
  }
  TCM_Exception *clone ( void ) const {
    return new Exception2(getExceptionData());
  }
};

static TCM_Task_Tree_Ref monitorRef;

static void timingTest1 (const TCM_Task_Tree_Ref &node,
			 const TCM_Task_Tree_Ref &nodeB)
{
  TCM_Task_Tree_Ref Delay;
  Delay = TCM_CreateDelayCommand(node, seconds(10));
  TCM_DelayUntil(TCM_StartOf(TCM_AchievingOf(nodeB)),
		 TCM_EndOf(TCM_AchievingOf(Delay)));

  Delay = TCM_CreateDelayCommand(node, seconds(5));
  TCM_DelayUntil(TCM_StartOf(TCM_AchievingOf(nodeB)),
		 TCM_EndOf(TCM_AchievingOf(Delay)));

  Delay = TCM_CreateDelayCommand(node, seconds(15));
  TCM_DelayUntil(TCM_StartOf(TCM_AchievingOf(nodeB)),
		 TCM_EndOf(TCM_AchievingOf(Delay)));
}

static void timingTest2 (const TCM_Task_Tree_Ref &nodeB)
{
  TCM_DelayFor(TCM_StartOf(TCM_AchievingOf(nodeB)), seconds(10));
  TCM_DelayFor(TCM_StartOf(TCM_AchievingOf(nodeB)), seconds(5));
  TCM_DelayFor(TCM_StartOf(TCM_AchievingOf(nodeB)), seconds(15));
}

static void monitorAct (const TCM_Task_Tree_Ref &node, const void *data)
{
#ifdef PRINT_OUT
  fprintf(stderr, "Invoking monitorAct %ld %ld %d %d\n", 
	  (size_t)data, (size_t)TCM_ActivationData(node),
	  TCM_NumActivations(node), TCM_NumTriggers(node));
#else
  UNUSED(data);
#endif
  TCM_Trigger(node);
  TCM_CreateCommandNode(node, "EMon", NULL, EHnd);
  TCM_Success(node);
}

static void timerCallback (const void *data)
{
  static int count = 0;

  count++;
#ifdef PRINT_OUT
  fprintf(stderr, "Invoking timerCallback %ld\n", (size_t)data);
#endif
  if (count < 3)
    TCM_InvokeAfter(seconds(1.5), timerCallback, data);
}

static void monitorTest1 (const TCM_Task_Tree_Ref &node,
			  const TCM_Task_Tree_Ref &nodeB,
			  const TCM_Task_Tree_Ref &nodeC,
			  const TCM_Task_Tree_Ref &nodeD)
{
  if (DOING_TEST(MONITOR_TEST1) || DOING_TEST(MONITOR_TEST7))
    monitorRef = TCM_CreateMonitorNode(node, "monitor", (void *)2, monitorAct);
  else if (DOING_TEST(MONITOR_TEST3) || DOING_TEST(MONITOR_TEST4)) 
    monitorRef = TCM_CreatePollingMonitorNode(node, "monitor", (void *)2,
					      monitorAct, seconds(5.0), 3, 3,
					      NULL, SEQ_ACH, TRUE);
  else if (DOING_TEST(MONITOR_TEST5)) 
    monitorRef = TCM_CreateMonitorNode(node, "monitor", (void *)2,
				       monitorAct, 3, 2);
				 
  if (DOING_TEST(MONITOR_TEST7)) {
    TCM_ActivateInAfter(monitorRef, seconds(20.0),
			TCM_EndOf(TCM_AchievingOf(nodeB)), (void *)11);
    TCM_ActivateInAfter(monitorRef, seconds(10.0),
			TCM_EndOf(TCM_PlanningOf(nodeC)), (void *)12);
    TCM_ActivateIn(monitorRef, seconds(5.0), (void *)13);
    TCM_DelayFor(TCM_StartOf(TCM_AchievingOf(nodeD)), seconds(15.0));
  } else {
    TCM_ActivateAt(monitorRef, TCM_EndOf(TCM_AchievingOf(nodeB)), (void *)11);
    TCM_ActivateAt(monitorRef, TCM_EndOf(TCM_PlanningOf(nodeC)), (void *)12);
  }
  TCM_TerminateAt(monitorRef, TCM_EndOf(TCM_AchievingOf(nodeD)));
}

static void monitorTest2 (const TCM_Task_Tree_Ref &node,
			  const TCM_Task_Tree_Ref &nodeB,
			  const TCM_Task_Tree_Ref &nodeC,
			  const TCM_Task_Tree_Ref &nodeD)
{
  TCM_Task_Tree_Ref monitorRef = 
    TCM_CreatePollingMonitorNode(node, "monitor", (void *)1, monitorAct, 
				 seconds(5.0), 3, 3, NULL, SEQ_ACH);
				 
  TCM_DelayUntil(TCM_StartOf(TCM_AchievingOf(monitorRef)),
		 TCM_StartOf(TCM_PlanningOf(nodeC)));
  TCM_ActivateAt(monitorRef, TCM_EndOf(TCM_AchievingOf(nodeB)), (void *)11);
  TCM_ActivateAt(monitorRef, TCM_EndOf(TCM_PlanningOf(nodeC)), (void *)12);
  TCM_TerminateAt(monitorRef, TCM_EndOf(TCM_AchievingOf(nodeD)));

  timingTest2(nodeB);

  TCM_Activate(monitorRef, (void *)-1);
}

static void externHnd (int sd, const void *data)
{
#ifdef PRINT_OUT
  fprintf(stderr, "Invoking externHnd %d (%ld)\n", sd, (size_t)data);

  char message[100];
  fprintf(stderr, "   Reading: %d, %ld, %s\n",
	  sd, read(sd, message, 100), message);
#else
  UNUSED(sd); UNUSED(data);
#endif	
}

static int openSocket (int port)
{
  int sd, reuse;
  struct sockaddr_in server;
  
  bzero((char *)&server, sizeof(struct sockaddr_in));
  
  server.sin_port = htons(port);
  
  server.sin_family = AF_INET;
  server.sin_addr.s_addr = INADDR_ANY;
  
  if ((sd = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP)) < 0) {
    perror("socket fail");
  }

  if ((setsockopt(sd, SOL_SOCKET, SO_REUSEADDR,
		  (char *)&reuse, sizeof(int))) < 0) {
    perror("setsockopt fail");
    return -1;
  }
  
  { int value = TCP_SOCKET_BUFFER;
    if ((setsockopt(sd, IPPROTO_TCP, TCP_NODELAY,
		    (char *)&value, sizeof(int))) < 0)
      return FALSE;
    setsockopt(sd, SOL_SOCKET, SO_SNDBUF, (char *)&value, sizeof(int));
    setsockopt(sd, SOL_SOCKET, SO_RCVBUF, (char *)&value, sizeof(int));
  }
  
  if ((bind(sd, (struct sockaddr *)&server, 
	    sizeof(struct sockaddr_in))) < 0) {
    return FALSE;
  }
  
  if (listen(sd, BACKLOG) < 0) {
    perror( "listen fail");
    return -1;
  }

  return sd;
}

static int connectSocket (int port)
{
  int sd;
  struct hostent *hp;
  struct sockaddr_in server;
  char thisHost[HOST_NAME_SIZE];

  bzero((char *)&server, sizeof(struct sockaddr_in));
  
  server.sin_family = AF_INET;
  server.sin_port = htons((u_short)port);

  gethostname(thisHost, HOST_NAME_SIZE);
  hp = gethostbyname(thisHost);
  memcpy((char *)&server.sin_addr, (char *)hp->h_addr, hp->h_length);
  
  if ((sd = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP)) < 0) {
    return -1;
  }

  int item = 1;
  if ((setsockopt(sd, SOL_SOCKET, SO_REUSEADDR,
		  (char *)&item, sizeof(int))) < 0) {
    close(sd);
    return -1;
  }
  
  int value = TCP_SOCKET_BUFFER;
  if ((setsockopt(sd, IPPROTO_TCP, TCP_NODELAY,
		  (char *)&value, sizeof(int))) < 0) {
    close(sd);
    return -1;
  }
    
  setsockopt(sd, SOL_SOCKET, SO_SNDBUF, (char *)&value, sizeof(int));
  setsockopt(sd, SOL_SOCKET, SO_RCVBUF, (char *)&value, sizeof(int));
  
  if (connect(sd, (struct sockaddr *)&server, sizeof(server)) < 0) {
    close(sd);
    return -1;
  }
  return sd;
}

static void printMsg (const void *data)
{ 
#ifdef PRINT_OUT
  fprintf(stderr, "INVOCATION: %s\n", (const char *)data);
#else
  UNUSED(data);
#endif
}

static void AHnd (const TCM_Task_Tree_Ref &node, const void *data)
{
#ifdef PRINT_OUT
  fprintf(stderr, "Invoking AHnd %ld\n", (size_t)data);
#else
  UNUSED(data);
#endif
  if (DOING_TEST(SUSPEND_TEST1) || DOING_TEST(SUSPEND_TEST2) ||
  	  DOING_TEST(SUSPEND_TEST3) || DOING_TEST(SUSPEND_TEST4))
  	TCM_SuspendNode(node);

  if (DOING_TEST(TERMINATE_TEST3)) TCM_TerminateNode(node);

  TCM_Task_Tree_Ref B = TCM_CreateGoalNode(node, "B", NULL, BHnd);
  TCM_Task_Tree_Ref C = 
    (DOING_TEST(DEALLOCATE_TEST2) ? TCM_AllocateGoalNode("C")
     : TCM_CreateGoalNode(node, "C", NULL, CHnd));

  if (DOING_TEST(TIMING_TEST3)) {
    TCM_DelayForAfter(TCM_StartOf(TCM_AchievingOf(C)), seconds(4.0),
		      TCM_EndOf(TCM_AchievingOf(B)));
    TCM_InvokeWhen(TCM_EndOf(TCM_AchievingOf(B)), timerCallback,
		   (const void *)567);
  } else {
    TCM_DelayUntil(TCM_StartOf(TCM_AchievingOf(C)),
		   TCM_EndOf(TCM_AchievingOf(B)));
  }

  if (DOING_TEST(SIGNAL_TEST1)) {
    TCM_DelayUntilSignal(TCM_StartOf(TCM_PlanningOf(C)));
  }

  if (DOING_TEST(TERMINATE_TEST13)) TCM_TerminateNode(B);
  if (DOING_TEST(TERMINATE_TEST14)) TCM_TerminateNode(C);
  if (DOING_TEST(DEALLOCATE_TEST2)) TCM_DeallocateNode(C);

  TCM_Task_Tree_Ref D = TCM_CreateGoalNode(node, "D", NULL, DHnd);
  // Need to add node first, so it has a parent and "last child" defined
  TCM_AddConstraint(D, SEQ_ACH + DELAY_PLANNING);

  if (DOING_TEST(SIGNAL_TEST1)) {
    TCM_SetPersistence(B);
    TCM_SetPersistence(C);
    TCM_SetPersistence(D);
  }

  if (DOING_TEST(MONITOR_TEST1) || DOING_TEST(MONITOR_TEST3) ||
      DOING_TEST(MONITOR_TEST4) || DOING_TEST(MONITOR_TEST5) ||
      DOING_TEST(MONITOR_TEST7)) 
    monitorTest1(node, B, C, D);
  if (DOING_TEST(MONITOR_TEST2)) 
    monitorTest2(node, B, C, D);

  if (DOING_TEST(TERMINATE_TEST14)) TCM_TerminateNode(D);

  if (DOING_TEST(TIMING_TEST1)) timingTest1(node, B);
  if (DOING_TEST(TIMING_TEST2) || DOING_TEST(TERMINATE_IN_TEST)) timingTest2(B);

  if (DOING_TEST(TERMINATE_TEST4)) TCM_TerminateNode(node);

  if (DOING_TEST(SUSPEND_TEST1) || DOING_TEST(SUSPEND_TEST2))
    TCM_SuspendNode(node);

  if (DOING_TEST(TERMINATE_AT_TEST) || DOING_TEST(TERMINATE_AT_TEST2))
    TCM_TerminateAt(D, TCM_EndOf(TCM_AchievingOf(B)));
  else if (DOING_TEST(TERMINATE_AT_AFTER_TEST1) ||
	   DOING_TEST(TERMINATE_AT_AFTER_TEST1a) ||
	   DOING_TEST(TERMINATE_AT_AFTER_TEST3)) {
    TCM_InvokeAfter(seconds(5), printMsg, "5 seconds have passed");
    TCM_TerminateInAfter(D, seconds(3), TCM_EndOf(TCM_AchievingOf(B)));
  } else if (DOING_TEST(TERMINATE_AT_AFTER_TEST2) ||
	     DOING_TEST(TERMINATE_AT_AFTER_TEST2a)) {
    TCM_InvokeAfter(seconds(5), printMsg, "5 seconds have passed");
    TCM_TerminateInAfter(D, seconds(10), TCM_EndOf(TCM_AchievingOf(B)));
  }
  if (DOING_TEST(TERMINATE_IN_TEST) || DOING_TEST(TERMINATE_AT_AFTER_TEST1) ||
      DOING_TEST(TERMINATE_AT_AFTER_TEST1a) ||
      DOING_TEST(TERMINATE_AT_AFTER_TEST2) ||
      DOING_TEST(TERMINATE_AT_AFTER_TEST2a) || DOING_TEST(COMPLETION_TEST))
    TCM_TerminateIn(D, seconds(8));

  if (DOING_TEST(TERMINATE_ON_TERMINATION1) || 
      DOING_TEST(TERMINATE_ON_TERMINATION2) ||
      DOING_TEST(TERMINATE_ON_TERMINATION3)) {
    TCM_OnTermination(B, "onTermination-C", NULL, CHnd);
  }

  if (DOING_TEST(TERMINATE_ON_TERMINATION2)) {
    TCM_TerminateNode(node);
  }

  TCM_Success(node);
}

static void BHnd (const TCM_Task_Tree_Ref &node, const void *data)
{
#ifdef PRINT_OUT
  fprintf(stderr, "Invoking BHnd %ld\n", (size_t)data);
#else
  UNUSED(data);
#endif

  TCM_AddExceptionHandler(node, (DOING_TEST(EXCEPTION_TEST3)
				 ? exceptionName2 : exceptionName),
			  Excep1Hnd, 1);

  if (DOING_TEST(DEALLOCATE_TEST1)) {
    TCM_Task_Tree_Ref E1 = TCM_CreateCommandNode(node, "E1", NULL, EHnd);
    TCM_Task_Tree_Ref E2 = TCM_AllocateCommandNode("E2");
    // NOTE: Do E2 *before* E1 !!!
    TCM_DelayUntil(TCM_StartOf(TCM_AchievingOf(E1)),
		   TCM_EndOf(TCM_AchievingOf(E2)));
    TCM_DeallocateNode(E2);

    TCM_Success(node);
    return;
  }

  if (DOING_TEST(TERMINATE_TEST5) || 
      DOING_TEST(TERMINATE_AT_TEST2) || DOING_TEST(TERMINATE_AT_AFTER_TEST3))
    TCM_TerminateNode(node);

  TCM_Task_Tree_Ref E1 = TCM_CreateCommandNode(node, "E1", NULL, EHnd);
  TCM_Task_Tree_Ref E2 = TCM_CreateCommandNode(node, "E2", NULL, EHnd);
  if (DOING_TEST(TERMINATE_TEST16)) TCM_TerminateNode(E2);
  if (DOING_TEST(TERMINATE_TEST17)) TCM_TerminateNode(E1);
  // NOTE: Do E2 *before* E1 !!!
  TCM_DelayUntil(TCM_StartOf(TCM_AchievingOf(E1)),
		 TCM_EndOf(TCM_AchievingOf(E2)));

  if (DOING_TEST(TERMINATE_TEST15)) TCM_TerminateNode(E2);
  if (DOING_TEST(SIGNAL_TEST2)) {
    TCM_InvokeWhen(TCM_EndOf(TCM_AchievingOf(E1)),
		   printMsg, "End Achieving E1");
    TCM_InvokeWhen(TCM_StartOf(TCM_AchievingOf(E2)),
		   printMsg, "Start Achieving E2");
    TCM_InvokeWhen(TCM_StartOf(TCM_PlanningOf(node)),
		   printMsg, "Start Planning B");
    TCM_InvokeWhen(TCM_EndOf(TCM_PlanningOf(node)),
		   printMsg, "End Planning B");
  }

  TCM_Success(node);

  if (DOING_TEST(TERMINATE_TEST6) || DOING_TEST(TERMINATE_ON_TERMINATION1))
    TCM_TerminateNode(node);
}

static void CHnd (const TCM_Task_Tree_Ref &node, const void *data)
{
#ifdef PRINT_OUT
  fprintf(stderr, "Invoking CHnd %ld\n", (size_t)data);
#else
  UNUSED(data);
#endif
  if (DOING_TEST(TERMINATE_TEST7)) TCM_TerminateNode(node);

  TCM_Task_Tree_Ref E3 = TCM_CreateCommandNode(node, "E3", NULL, EHnd);

  if (DOING_TEST(TERMINATE_TEST8)) TCM_TerminateNode(node);

  TCM_Success(node);
}

static void DHnd (const TCM_Task_Tree_Ref &node, const void *data)
{
#ifdef PRINT_OUT
  fprintf(stderr, "Invoking DHnd %ld\n", (size_t)data);
#else
  UNUSED(data);
#endif
  if (DOING_TEST(TERMINATE_TEST9)) TCM_TerminateNode(node);

  if (DOING_TEST(TERMINATE_AT_AFTER_TEST1) ||
      DOING_TEST(TERMINATE_AT_AFTER_TEST2) ||
      DOING_TEST(TERMINATE_AT_AFTER_TEST3))
    TCM_WaitFor(seconds(10));

  if (DOING_TEST(MONITOR_TEST4))
    TCM_DelayFor(TCM_EndOf(TCM_AchievingOf(node)), seconds(1));

  TCM_Success(node);

  if (DOING_TEST(TERMINATE_TEST10)) TCM_TerminateNode(node);
}

static void EHnd (const TCM_Task_Tree_Ref &node, const void *data)
{
#ifdef PRINT_OUT
  fprintf(stderr, "Invoking EHnd %ld\n", (size_t)data);
#else
  UNUSED(data);
#endif
  if (DOING_TEST(EXTERNAL_TEST1)) {
    if (streq("E3", TCM_NodeName(node))) {
      TCM_RemoveExternalEvent(readSd);
    }
#ifdef PRINT_OUT
    fprintf(stderr, "  Writing to %d: %ld, %s\n", writeSd, 
	    write(writeSd, TCM_NodeName(node), strlen(TCM_NodeName(node))+1),
	    TCM_NodeName(node));
#endif
  }

  if (DOING_TEST(SIGNAL_TEST1)) {
    TCM_DisplayTree(stderr, TCM_RootNode());
    if (streq("E2", TCM_NodeName(node))) {
      TCM_Signal(TCM_StartOf(TCM_PlanningOf(TCM_NextChild(TCM_Parent(node)))));
    }
  }

  if (DOING_TEST(TERMINATE_TEST11)) TCM_TerminateNode(node);

  if (DOING_TEST(EXCEPTION_TEST1) || DOING_TEST(EXCEPTION_TEST2) || 
      DOING_TEST(EXCEPTION_TEST3)) {
    if (streq("E1", TCM_NodeName(node))) {
      if (DOING_TEST(EXCEPTION_TEST3)) {
	TCM_Failure(node, new Exception2((void *)2));
      } else {
	TCM_Failure(node, exceptionName, (void *)-1);
      }
    } else
      TCM_Success(node);
  } else 
    TCM_Success(node);

  if (DOING_TEST(MONITOR_TEST1) || DOING_TEST(MONITOR_TEST7)) 
    if (streq("E3", TCM_NodeName(node)))
      TCM_Activate(monitorRef, (void *)-2);

  if (DOING_TEST(TERMINATE_TEST12)) TCM_TerminateNode(node);
}

static void Excep1Hnd (const TCM_Task_Tree_Ref &node, const void *data)
{
  UNUSED(data);
#ifdef PRINT_OUT
  fprintf(stderr, "Excep1Hnd: Handling exception %s (%ld)\n", 
	  TCM_NodeName(node), (size_t)TCM_FailureData(node));
#endif
  if (DOING_TEST(EXCEPTION_TEST1) || DOING_TEST(EXCEPTION_TEST3))
    TCM_Bypass(node);
  else
    TCM_Failure(node, exceptionName, (void *)1);
}

static void Excep2Hnd (const TCM_Task_Tree_Ref &node, const void *data)
{
  UNUSED(data);
#ifdef PRINT_OUT
  fprintf(stderr, "Excep2Hnd: Handling exception %s (%ld)\n", 
	  TCM_NodeName(node), (size_t)TCM_FailureData(node));
#endif
  if (DOING_TEST(EXCEPTION_TEST1) || DOING_TEST(EXCEPTION_TEST3))
    TCM_Bypass(node);
  else
    TCM_Failure(node, exceptionName, (void *)2);
}

static void Excep3Hnd (const TCM_Task_Tree_Ref &node, const void *data)
{
  UNUSED(data);
#ifdef PRINT_OUT
  fprintf(stderr, "Excep3Hnd: Handling exception %s (%ld)\n", 
	  TCM_NodeName(node), (size_t)TCM_FailureData(node));
#endif
  if (DOING_TEST(EXCEPTION_TEST1) || DOING_TEST(EXCEPTION_TEST3))
    TCM_Bypass(node);
  else
    TCM_Failure(node, exceptionName, (void *)3);
}

#ifdef macintosh
#include <console.h>
#include <SIOUX.h>

/* The right thing to do is to include GUSI.h, but that 
   includes other files that conflict with some of my declarations */
extern "C" void GUSIwithInternetSockets();
extern "C" void GUSIwithUnixSockets();
extern "C" void GUSIwithSIOUXSockets();
extern "C" void GUSISetup(void (*socketfamily)());
extern "C" void GUSIDefaultSetup();

static void initGUSI(void)
{
  SIOUXSettings.autocloseonquit = FALSE;
  SIOUXSettings.showstatusline = FALSE;
  SIOUXSettings.asktosaveonclose = TRUE;

  GUSIDefaultSetup();
  //GUSISetup(GUSIwithInternetSockets);
  //GUSISetup(GUSIwithUnixSockets);
  // Handle stdin
  //GUSISetup(GUSIwithSIOUXSockets);
}
#endif

int main (int argc, char **argv)
{
#ifdef macintosh
  //initGUSI();
  argc = ccommand(&argv);
#endif
  if (argc > 1) {
    whichTest = atoi(argv[1]);
  }

  TCM_Initialize();

#ifdef PRINT_OUT
  TCM_SetTerminalLoggingOptions(Log_Msg | Log_Status | Log_Id | Log_ParentId);
  // TCM_SetTerminalLoggingOptions(Log_Time | TCM_TerminalLoggingOptions());
#else
  TCM_SetTerminalLoggingOptions(0);
#endif
  
  if (DOING_TEST(EXTERNAL_TEST1)) {
    int listenSd = openSocket(PORT_NUM);
    writeSd = connectSocket(PORT_NUM);
    readSd = accept(listenSd, (struct sockaddr *)NULL, (socklen_t *)NULL);
    TCM_AddExternalEvent(readSd, externHnd, (void *)42);
  }

  if (DOING_TEST(MONITOR_TEST6)) {
    TCM_InvokeAfter(seconds(2.5), timerCallback, (void *)123);
  }

  TCM_Task_Tree_Ref A = TCM_CreateGoalNode(TCM_RootNode(), "A", NULL, AHnd);

  if (DOING_TEST(EXCEPTION_TEST1) || DOING_TEST(EXCEPTION_TEST2) ||
      DOING_TEST(EXCEPTION_TEST3)) {
    // Handler closest to the failure is found first, and if several are 
    // on the same node, last added is first found -- should be invoked 1, 2, 3
    TCM_AddExceptionHandler(A, exceptionName, Excep3Hnd, 3);
    TCM_AddExceptionHandler(A, exceptionName, Excep2Hnd, 2);
  }

  if (DOING_TEST(TERMINATE_TEST2)) TCM_TerminateNode(A);
  
  if (DOING_TEST(TIMING_TEST1) || DOING_TEST(TIMING_TEST2) ||
      DOING_TEST(TERMINATE_IN_TEST) || DOING_TEST(MONITOR_TEST2) ||
      DOING_TEST(DEALLOCATE_TEST1) || DOING_TEST(DEALLOCATE_TEST1))
    TCM_WaitUntil(TCM_EndOf(TCM_AchievingOf(A)));
  else
    TCM_ProcessAgenda();

  if (DOING_TEST(SIGNAL_TEST1)) {
    TCM_DisplayTree(stderr, TCM_RootNode());
  }
  
  if (DOING_TEST(SUSPEND_TEST1)) {
    TCM_UnsuspendNode(A);
    TCM_ProcessAgenda();
  } else if (DOING_TEST(SUSPEND_TEST3)) {
    TCM_UnsuspendNode(TCM_ChildNamed(A, "B"));
    TCM_ProcessAgenda();
    TCM_UnsuspendNode(TCM_ChildNamed(A, "D"));
    TCM_ProcessAgenda();
    TCM_UnsuspendNode(TCM_ChildNamed(A, "C"));
    TCM_ProcessAgenda();
  } else if (DOING_TEST(SUSPEND_TEST4)) {
    TCM_UnsuspendNode(TCM_ChildNamed(A, "D"));
    TCM_TerminateNode(TCM_ChildNamed(A, "B"));
    TCM_TerminateNode(TCM_ChildNamed(A, "C"));
    TCM_ProcessAgenda();
  }

  // This is TERMINATE_TEST1
  // It is used for all other termination tests, as well.
  TCM_TerminateNode(A);
  TCM_ProcessAgenda();

  if (DOING_TEST(SIGNAL_TEST1)) {
    TCM_DisplayTree(stderr, TCM_RootNode());
  }
  return 0;
}

