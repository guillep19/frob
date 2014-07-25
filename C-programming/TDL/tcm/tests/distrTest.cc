/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tcm/LICENSE.TXT)
 */
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/wait.h>

#include "tcm.h"
#include "distrTests.h"

#ifndef CENTRAL_BIN_DIR
#ifdef USE_TCA
#define CENTRAL_BIN_DIR HOME_DIR "/code/tca/bin"
#else
#define CENTRAL_BIN_DIR HOME_DIR "/code/ipc/bin/Linux-2.4"
#endif
#endif /* *NOT* ! CENTRAL_BIN_DIR */

#ifndef TCM_TESTS_DIR
#define TCM_TESTS_DIR   HOME_DIR "/code/tcm/tests"
#endif /* *NOT* ! TCM_TESTS_DIR */

#define PS_LINE_LEN (132)

#define PS_NAME_LEN (8)
#define PS_PID_LEN  (6)
#define PS_CPU_LEN  (5)
#define PS_MEM_LEN  (5)
#define PS_SS_LEN   (5)
/* Slight variablilty between Linux and Sunos */
#if defined(linux)
#ifdef REDHAT_6
#define PS_RSS_TTY_LEN (16)
#else
#define PS_RSS_TTY_LEN (12)
#endif
#else
#define PS_RSS_TTY_LEN (9)
#endif
#define PS_STAT_LEN (4)

#define PS_PID_START  PS_NAME_LEN
#define PS_STAT_START (PS_NAME_LEN + PS_PID_LEN + PS_CPU_LEN + PS_MEM_LEN + PS_SS_LEN + PS_RSS_TTY_LEN)

static int forkProcess (STRING dir, STRING name, char **args, char **env)
{
  int id[2], processId;

  if (pipe(id) == -1) {
    fprintf(stderr, "ERROR: PIPE FAILURE for %s\n", name);
    exit(-1);
  }

  switch (processId = fork()) {
  case -1:
    close(id[0]); close(id[1]);
    fprintf(stderr, "ERROR: FORK FAILURE for %s\n", name);
    exit(-1);
  case 0:			/* This is the child process executing */
    {
      dup2(id[0],fileno(stdin));

      if (chdir(dir) == -1) {
	fprintf(stderr, "ERROR: Cannot chdir to %s\n", dir);
	exit(-1);
      }
      execve(name, args, env);
      exit(0);
    }
  default:		/* This is the parent process executing */
    ;
  }
  return processId;
}

static int forkCentral (STRING port, char**env)
{
  char *args[4], portArg[20];

  args[0] = (char *)"central";
  sprintf(portArg, "-p%s", port);
  args[1] = portArg;
  //#define VERBOSE_IPC
#ifdef VERBOSE_IPC
  args[2] = (char *)"-lmdsi";
#else
#ifdef USE_TCA
  args[2] = (char *)"-lx";
#else
  args[2] = (char *)"-s";
#endif
#endif
  args[3] = NULL;
  
  return forkProcess(CENTRAL_BIN_DIR, args[0], args, env);
}

static int forkDistr (STRING name, int testNum, char**env)
{
  char *args[3], testArg[10];

  args[0] = (char *)name;
  sprintf(testArg, "%d", testNum);
  args[1] = testArg;
  args[2] = NULL;

  return forkProcess(TCM_TESTS_DIR, name, args, env);
}

/* This is taken from Joseph O'Sullivan's supervise program */
static char *psGetLine(char *buff, int max_chars, FILE *fp)
{
  int i, c;
  char *str;
  
  buff[0] = '\0';
  str = buff;
  for( i = 0; i < max_chars; i++){
    c = getc(fp);
    if (c == '\n' || c == EOF) {
      *buff++ = '\0';
      return (c == EOF ? NULL : str);
    } else 
      *buff++ = c;
  }
  return str;
}

/* This is based on code in Joseph O'Sullivan's supervise program */
static void waitWhileExecuting(int numPids, int *pids)
{
  static char command[120], line[PS_LINE_LEN+1];
  FILE *fd;
  int i, pid, numFound = 0;
  char state;

  sprintf(command, "ps auwx | egrep %s", getenv("USER"));
  line[0] = '\0';

  do {
    sleep(1);
    if ((fd = popen(command, "r")) == NULL) {
      fprintf(stderr, "WARNING: Unable to fork popen.\n");
    } else if (psGetLine(line, PS_LINE_LEN, fd) == NULL) {
      fprintf(stderr, "WARNING: ps returned empty.\n");
    } else {
      numFound = 0;
      while (numFound < numPids && !feof(fd) &&
	     psGetLine(line, PS_LINE_LEN, fd)) {
	pid = atoi(&line[PS_PID_START]);
	state = line[PS_STAT_START];
	if (state != 'Z') { // Not a zombie
	  for (i=0; i<numPids; i++) {
	    if (pid == pids[i]) {
	      numFound++;
	      break;
	    }
	  }
	}
      }
    }
    //fprintf(stderr, "FOUND %d PIDS\n", numFound);
  } while (numFound == numPids);
}

int main (int argc, char **argv, char **env)
{
  int central, distrA, distrB, whichTest=0;

  if (argc > 1) {
    whichTest = atoi(argv[1]);
  }

  central = forkCentral(CENTRAL_HOST, env);
  distrB  = forkDistr("distrTestB", whichTest, env);
#define SLEEP_TIME 2
#if 1
  sleep(SLEEP_TIME);
#else
  { 
    struct timeval timeout = {SLEEP_TIME,0};
    select(0, NULL, NULL, NULL, &timeout);
  }
#endif
  distrA  = forkDistr("distrTestA", whichTest, env);

  int pids[3] = {central, distrA, distrB };
  if ( 0 )
    waitWhileExecuting(3, pids);
  else
    wait ( (int*)NULL );
  
  // Clean-up
  for (int i=0; i<3; i++)
  {
    kill(pids[i], 9);
    usleep(10000);
  }
}
