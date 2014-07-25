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

#define THIS_AGENT  AGENT_B
#define OTHER_AGENT AGENT_A

int whichTest = BASIC_TEST;

// Forward references
static void BHnd (const TCM_Task_Tree_Ref &node, const void *data);
static void DHnd (const TCM_Task_Tree_Ref &node, const void *data);

static void printMsg (const void *data)
{ 
#ifdef PRINT_OUT
  fprintf(stderr, "INVOCATION: %s\n", (const char *)data);
#else
  UNUSED(data);
#endif
}

static void Excep1Hnd (const TCM_Task_Tree_Ref &node, const void *data)
{
  UNUSED(data);
#ifdef PRINT_OUT
  fprintf(stderr, "Excep1Hnd: Handling exception %s (%d)\n", 
	  TCM_NodeName(node), *(int *)TCM_FailureData(node));
#endif
  if (DOING_TEST(EXCEPTION_TEST1) || DOING_TEST(EXCEPTION_TEST3))
    TCM_Bypass(node);
  else {
    // Need to have a pointer to a static variable (so the value doesn't
    //  go away when the procedure exits.
    static int datum = 1;
    TCM_Failure(node, EXCEPTION_NAME, &datum);
  }
}

static void BHnd (const TCM_Task_Tree_Ref &node, const void *data)
{
#ifdef PRINT_OUT
  fprintf(stderr, "Invoking BHnd %d\n", (int)data);
#else
  UNUSED(data);
#endif

  TCM_AddExceptionHandler(node, (DOING_TEST(EXCEPTION_TEST3)
				 ? EXCEPTION2_NAME : EXCEPTION_NAME),
			  Excep1Hnd, 1);

  if (DOING_TEST(DEALLOCATE_TEST1)) {
    TCM_Task_Tree_Ref E1 = TCM_CreateDistributedNode(OTHER_AGENT, node, "E1",
						     NULL);
    TCM_Task_Tree_Ref E2 = TCM_AllocateDistributedNode(OTHER_AGENT, "E2");
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

  TCM_Task_Tree_Ref E1 = TCM_CreateDistributedNode(OTHER_AGENT, node, "E1",
						   NULL);
  TCM_Task_Tree_Ref E2 = TCM_CreateDistributedNode(OTHER_AGENT, node, "E2",
						   NULL);
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

static void DHnd (const TCM_Task_Tree_Ref &node, const void *data)
{
#ifdef PRINT_OUT
  fprintf(stderr, "Invoking DHnd %d\n", (int)data);
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

/****************************************************************
 *  DISTRIBUTED ALLOCATION AND ACTION FUNCTIONS
 ****************************************************************/

class B_Action : public _Action
{
 public: 
  B_Action ( TCM_Task_Tree_Ref & task, void *args) {}

  virtual void execute ( const TCM_Task_Tree_Ref & task ) { BHnd(task, NULL); }
};

static TCM_Task_Tree_Ref allocateB (void *dummy)
{
  return TCM_AllocateGoalNode("B");
}

static TCM_Action_Ref actionB (TCM_Task_Tree_Ref &B, void *args,
			       void *dummy)
{
  return new B_Action(B, args);
}

class D_Action : public _Action
{
 public: 
  D_Action ( TCM_Task_Tree_Ref & task, void *args) {}

  virtual void execute ( const TCM_Task_Tree_Ref & task ) { DHnd(task, NULL); }
};

static TCM_Task_Tree_Ref allocateD (void *dummy)
{
  return TCM_AllocateGoalNode("D");
}

static TCM_Action_Ref actionD (TCM_Task_Tree_Ref &D, void *args,
			       void *dummy)
{
  return new D_Action(D, args);
}

/****************************************************************/

int main (int argc, char **argv)
{
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

  TCM_RegisterDistributedTask("B", allocateB, NULL, actionB, NULL, NULL);
  TCM_RegisterDistributedTask("D", allocateD, NULL, actionD, NULL, NULL);
  if (DOING_TEST(EXCEPTION_TEST1) || DOING_TEST(EXCEPTION_TEST2) ||
      DOING_TEST(EXCEPTION_TEST3)) {
    TCM_RegisterDistributedException(EXCEPTION_NAME, EXCEPTION_FORMAT);
    TCM_RegisterDistributedException(EXCEPTION2_NAME, EXCEPTION2_FORMAT,
				     Exception2::creator);
  }

  TCM_ProcessAgendaInfiniteBlock();

  return 0;
}
