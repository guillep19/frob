/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tcm/LICENSE.TXT)
 */
/*
   Creates and executes the task tree :   
                             A
                           / | \
                          B  C  D
                        / \  |
                       E1 E2 E3
   where agent A defines tasks A, C and E, and 
   agent B defines tasks B and D.
*/ 

#include <stdio.h>
#include <stdlib.h>
#include "tcm.h"

#include "distrTests.h"

#define THIS_AGENT  AGENT_A
#define OTHER_AGENT AGENT_B

// For doing the "external" test
#include <fcntl.h>
#include <netdb.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <sys/socket.h>
/* defines that should be in <sys/socket.h> */
extern "C" int close (int);
#if !defined(linux) && !defined(__svr4__) && !defined(__sgi)
extern "C" int accept (int, struct sockaddr *, int *);
extern "C" int bind (int, struct sockaddr *, int);
extern "C" int connect (int s, const struct sockaddr *name, int namelen);
extern "C" int getsockopt (int, int, int, void *optval, int *);
extern "C" int setsockopt (int s, int level, int optname,
			   const char *optval, int optlen);
extern "C" int listen (int, int);
extern "C" int socket (int, int, int);
#endif

#define BACKLOG 5
#define PROTOCOL_NAME "tcp"
#define TCP_SOCKET_BUFFER (16*1024)
#define HOST_NAME_SIZE   32
#define PORT_NUM 4567

int whichTest = BASIC_TEST;

// Forward references
static void CHnd (const TCM_Task_Tree_Ref &node, const void *data);
static void EHnd (const TCM_Task_Tree_Ref &node, const void *data);

int writeSd = -1;
int readSd = -1;

//static void Excep1Hnd (TCM_Task_Tree_Ref const &node, const void *data);
static void Excep2Hnd (TCM_Task_Tree_Ref const &node, const void *data);
static void Excep3Hnd (TCM_Task_Tree_Ref const &node, const void *data);

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
  fprintf(stderr, "Invoking monitorAct %d %d %d %d\n", 
	  (int)data, (int)TCM_ActivationData(node),
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
  fprintf(stderr, "Invoking timerCallback %d\n", (int)data);
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
			TCM_EndOf(TCM_AchievingOf(nodeB))/*, (void *)11*/);
    TCM_ActivateInAfter(monitorRef, seconds(10.0),
			TCM_EndOf(TCM_PlanningOf(nodeC)), (void *)12);
    TCM_ActivateIn(monitorRef, seconds(5.0), (void *)13);
    TCM_DelayFor(TCM_StartOf(TCM_AchievingOf(nodeD)), seconds(15.0));
  } else {
    TCM_ActivateAt(monitorRef, TCM_EndOf(TCM_AchievingOf(nodeB))/*, (void *)11*/);
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
  TCM_ActivateAt(monitorRef, TCM_EndOf(TCM_AchievingOf(nodeB))/*, (void *)11*/);
  TCM_ActivateAt(monitorRef, TCM_EndOf(TCM_PlanningOf(nodeC)), (void *)12);
  TCM_TerminateAt(monitorRef, TCM_EndOf(TCM_AchievingOf(nodeD)));

  timingTest2(nodeB);

  TCM_Activate(monitorRef, (void *)-1);
}

static void externHnd (int sd, const void *data)
{
#ifdef PRINT_OUT
  fprintf(stderr, "Invoking externHnd %d (%d)\n", sd, (int)data);

  char message[100];
  fprintf(stderr, "   Reading: %d, %d, %s\n",
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
  fprintf(stderr, "Invoking AHnd %d\n", (int)data);
#else
  UNUSED(data);
#endif
  if (DOING_TEST(SUSPEND_TEST1) || DOING_TEST(SUSPEND_TEST2) ||
  	  DOING_TEST(SUSPEND_TEST3) || DOING_TEST(SUSPEND_TEST4))
  	TCM_SuspendNode(node);

  if (DOING_TEST(TERMINATE_TEST3)) TCM_TerminateNode(node);

  TCM_Task_Tree_Ref B = TCM_CreateDistributedNode(OTHER_AGENT, node, "B", NULL);
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

  TCM_Task_Tree_Ref D = TCM_CreateDistributedNode(OTHER_AGENT, node, "D", NULL);
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

static void CHnd (const TCM_Task_Tree_Ref &node, const void *data)
{
#ifdef PRINT_OUT
  fprintf(stderr, "Invoking CHnd %d\n", (int)data);
#else
  UNUSED(data);
#endif
  if (DOING_TEST(TERMINATE_TEST7)) TCM_TerminateNode(node);

  TCM_Task_Tree_Ref E3 = TCM_CreateCommandNode(node, "E3", NULL, EHnd);

  if (DOING_TEST(TERMINATE_TEST8)) TCM_TerminateNode(node);

  TCM_Success(node);
}

static void EHnd (const TCM_Task_Tree_Ref &node, const void *data)
{
#ifdef PRINT_OUT
  fprintf(stderr, "Invoking EHnd %d\n", (int)data);
#else
  UNUSED(data);
#endif
  if (DOING_TEST(EXTERNAL_TEST1)) {
    if (streq("E3", TCM_NodeName(node))) {
      TCM_RemoveExternalEvent(readSd);
    }
#ifdef PRINT_OUT
    fprintf(stderr, "  Writing to %d: %d, %s\n", writeSd, 
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
      // Need to have a pointer to a static variable (so the value doesn't
      //  go away when the procedure exits.
      static int datum = -1;
      if (DOING_TEST(EXCEPTION_TEST3)) {
	datum = 2;
	TCM_Failure(node, new Exception2(&datum));
      } else {
	TCM_Failure(node, EXCEPTION_NAME, &datum);
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
/****************************************************************
 *  DISTRIBUTED ALLOCATION AND ACTION FUNCTIONS
 ****************************************************************/

class E_Action : public _Action
{
 public: 
  E_Action ( TCM_Task_Tree_Ref & task, void *args) {}

  virtual void execute ( const TCM_Task_Tree_Ref & task ) { EHnd(task, NULL); }
};

static TCM_Task_Tree_Ref allocateE1 (void *dummy)
{
  return TCM_AllocateCommandNode("E1");
}

static TCM_Task_Tree_Ref allocateE2 (void *dummy)
{
  return TCM_AllocateCommandNode("E2");
}

static TCM_Task_Tree_Ref allocateEMon (void *dummy)
{
  return TCM_AllocateCommandNode("EMon");
}

static TCM_Action_Ref actionE (TCM_Task_Tree_Ref &E, void *args,
			       void *dummy)
{
  return new E_Action(E, args);
}
/****************************************************************/

static void Excep2Hnd (const TCM_Task_Tree_Ref &node, const void *data)
{
  UNUSED(data);
#ifdef PRINT_OUT
  fprintf(stderr, "Excep2Hnd: Handling exception %s (%d)\n", 
	  TCM_NodeName(node), *(int *)TCM_FailureData(node));
#endif
  if (DOING_TEST(EXCEPTION_TEST1) || DOING_TEST(EXCEPTION_TEST3))
    TCM_Bypass(node);
  else {
    // Need to have a pointer to a static variable (so the value doesn't
    //  go away when the procedure exits.
    static int datum = 2;
    TCM_Failure(node, EXCEPTION_NAME, &datum);
  }
}

static void Excep3Hnd (const TCM_Task_Tree_Ref &node, const void *data)
{
  UNUSED(data);
#ifdef PRINT_OUT
  fprintf(stderr, "Excep3Hnd: Handling exception %s (%d)\n", 
	  TCM_NodeName(node), *(int *)TCM_FailureData(node));
#endif
  if (DOING_TEST(EXCEPTION_TEST1) || DOING_TEST(EXCEPTION_TEST3))
    TCM_Bypass(node);
  else {
    // Need to have a pointer to a static variable (so the value doesn't
    //  go away when the procedure exits.
    static int datum = 3;
    TCM_Failure(node, EXCEPTION_NAME, &datum);
  }
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
  
  //IPC_setVerbosity(IPC_Silent);
  TCM_EnableDistributedComm(THIS_AGENT, CENTRAL_HOST);
  TCM_ConnectDistributedAgent(OTHER_AGENT, CENTRAL_HOST);

  TCM_RegisterDistributedTask("E1", allocateE1, NULL, actionE, NULL, NULL);
  TCM_RegisterDistributedTask("E2", allocateE2, NULL, actionE, NULL, NULL);
  TCM_RegisterDistributedTask("EMon", allocateEMon, NULL, actionE, NULL, NULL);
  
  if (DOING_TEST(EXTERNAL_TEST1)) {
    int listenSd = openSocket(PORT_NUM);
    writeSd = connectSocket(PORT_NUM);
    readSd = accept(listenSd, (struct sockaddr *)NULL, (unsigned int *)NULL);
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
    TCM_AddExceptionHandler(A, EXCEPTION_NAME, Excep3Hnd, 3);
    TCM_AddExceptionHandler(A, EXCEPTION_NAME, Excep2Hnd, 2);
    TCM_RegisterDistributedException(EXCEPTION_NAME, EXCEPTION_FORMAT);
    TCM_RegisterDistributedException(EXCEPTION2_NAME, EXCEPTION2_FORMAT,
				     Exception2::creator);
  }

  if (DOING_TEST(TERMINATE_TEST2)) TCM_TerminateNode(A);
  
  if (DOING_TEST(TIMING_TEST1) || DOING_TEST(TIMING_TEST2) ||
      DOING_TEST(TERMINATE_IN_TEST) || DOING_TEST(MONITOR_TEST2) ||
      DOING_TEST(DEALLOCATE_TEST1) || DOING_TEST(DEALLOCATE_TEST1))
    TCM_WaitUntil(TCM_EndOf(TCM_AchievingOf(A)));
  else if (DOING_TEST(SUSPEND_TEST1) || DOING_TEST(SUSPEND_TEST2) ||
	   DOING_TEST(SUSPEND_TEST3) || DOING_TEST(SUSPEND_TEST4))
    TCM_ProcessAgenda(FALSE,INFINITE_TIME,TRUE);
  else
    //TCM_ProcessAgenda(FALSE,INFINITE_TIME,TRUE);
    TCM_WaitUntil(TCM_EndOf(TCM_AchievingOf(A)));

  if (DOING_TEST(SIGNAL_TEST1)) {
    TCM_DisplayTree(stderr, TCM_RootNode());
  }
  
  if (DOING_TEST(SUSPEND_TEST1)) {
    TCM_UnsuspendNode(A);
    //TCM_ProcessAgenda(FALSE,INFINITE_TIME,TRUE);
    TCM_WaitUntil(TCM_EndOf(TCM_AchievingOf(A)));
  } else if (DOING_TEST(SUSPEND_TEST3)) {
    TCM_UnsuspendNode(TCM_ChildNamed(A, "B"));
    TCM_ProcessAgenda(FALSE,INFINITE_TIME,TRUE);
    TCM_UnsuspendNode(TCM_ChildNamed(A, "D"));
    TCM_ProcessAgenda(FALSE,INFINITE_TIME,TRUE);
    TCM_UnsuspendNode(TCM_ChildNamed(A, "C"));
    //TCM_ProcessAgenda(FALSE,INFINITE_TIME,TRUE);
    TCM_WaitUntil(TCM_EndOf(TCM_AchievingOf(A)));
  } else if (DOING_TEST(SUSPEND_TEST4)) {
    TCM_UnsuspendNode(TCM_ChildNamed(A, "D"));
    TCM_TerminateNode(TCM_ChildNamed(A, "B"));
    TCM_TerminateNode(TCM_ChildNamed(A, "C"));
    //TCM_ProcessAgenda(FALSE,INFINITE_TIME,TRUE);
    TCM_WaitUntil(TCM_EndOf(TCM_AchievingOf(A))); 
  } else if (DOING_TEST(MONITOR_TEST6)) {
    TCM_ProcessAgenda(TRUE, 1000*10);
  }

  // This is TERMINATE_TEST1
  // It is used for all other termination tests, as well.
  TCM_TerminateNode(A);
  TCM_ProcessAgenda(TRUE, 1000);

  if (DOING_TEST(SIGNAL_TEST1)) {
    TCM_DisplayTree(stderr, TCM_RootNode());
  }
  return 0;
}
