/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tcm/LICENSE.TXT)
 */
#ifndef INC_DISTR_TESTS
#define INC_DISTR_TESTS

#define AGENT_A "agentA"
#define AGENT_B "agentB"

#define CENTRAL_HOST "localhost"

#define EXCEPTION_NAME   "Test Failure"
#define EXCEPTION_FORMAT "int"

#define EXCEPTION2_NAME   "Test Failure2"
#define EXCEPTION2_FORMAT "int"

#define PRINT_OUT

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

class Exception2 : public TCM_Exception
{
public:
  Exception2(const void *data) : TCM_Exception(EXCEPTION2_NAME, data) {}

  virtual BOOLEAN matches ( STRING theString ) const {
    return (!strcmp(theString, EXCEPTION_NAME) ||
	    !strcmp(theString, EXCEPTION2_NAME));
  }
  TCM_Exception *clone ( void ) const
    { return new Exception2(getExceptionData()); }
  static TCM_Exception *creator (STRING name, const void *data)
    { return new Exception2(data); }
};

#endif // INC_DISTR_TESTS
