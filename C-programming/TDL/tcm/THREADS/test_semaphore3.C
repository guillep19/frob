
static const char * copyrightAndLicense
	= "Copyright (c) 2001, by David B. Apfelbaum.\n"
	  "All rights reserved.\n"
	  "\n"
	  "Standard BSD Open-Source License:\n"
	  "\n"
	  "Redistribution and use in source and binary forms,\n"
	  "with or without modification, are permitted provided\n"
	  "that the following conditions are met:\n"
	  "\n"
	  " + Redistributions of source code must retain the\n"
	  "   above copyright notice, this list of conditions\n"
	  "   and the following disclaimer.\n"
	  "\n"
	  " + Redistributions in binary form must reproduce the\n"
	  "   above copyright notice, this list of conditions\n"
	  "   and the following disclaimer in the documentation\n"
	  "   and/or other materials provided with the\n"
	  "   distribution. \n"
	  "\n"
	  " + Neither the name of David B. Apfelbaum nor the\n"
	  "   names of any contributors may be used to endorse\n"
	  "   or promote products derived from this software\n"
	  "   without specific prior written permission. \n"
	  "\n"
	  "THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS\n"
	  "AND CONTRIBUTORS \"AS IS\" AND ANY EXPRESS OR IMPLIED\n"
	  "WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE\n"
	  "IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR\n"
	  "A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT\n"
	  "SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR ANY\n"
	  "DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR\n"
	  "CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,\n"
	  "PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF\n"
	  "USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)\n"
	  "HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,\n"
	  "WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT\n"
	  "(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY\n"
	  "WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED\n"
	  "OF THE POSSIBILITY OF SUCH DAMAGE.\n";
static void copyrightAndLicense_markUsed()
	{ (void)&copyrightAndLicense_markUsed; (void)&copyrightAndLicense; }



#include "Semaphore.H"
#include "Thread.H"

#define POUT(X)  cout << # X " = " << (X) << endl


	/* mutex is used Solely to serialize text output statements... */
Mutex      mutex ( Mutex::ERROR_CHECK );

Semaphore  semaphore;

BOOLEAN    allowWakeupsThatHaveAlreadyHappened = FALSE;


void
threadFunction()
{
  status_t  result;

  mutex.lock();
  cout << "Thread:  started\n";
  cout << "Thread:  waiting on semaphore\n\n\n";
  mutex.unlock();

  result = semaphore . waitForSignal(allowWakeupsThatHaveAlreadyHappened);

  mutex.lock();
  cout << "Thread:  waitForSignal() returned:  " << result << endl;
  cout << "Thread:  ending.\n";
  POUT ( semaphore );
  mutex.unlock();
}


int
main()
{
  Thread  threadOne, threadTwo;

  threadOne . setThreadFunction ( & threadFunction );
  threadTwo . setThreadFunction ( & threadFunction );

  cout << "Main() running.\n";

  POUT ( semaphore );

  semaphore . clearWakeupAlreadyReceived();
  semaphore . clearWakeupAlreadyReceived();



	/* THREAD ONE STARTING SHORTLY */

  cout << "Starting thread One...\n";

  allowWakeupsThatHaveAlreadyHappened = FALSE;
  threadOne . start();

  sleep ( 1 );

  mutex.lock();
  POUT ( semaphore . wakeupOtherThread(FALSE) );
  POUT ( semaphore . wakeupOtherThread(FALSE) );
  POUT ( semaphore );
  mutex.unlock();

  threadOne . waitForThreadToStop();

	/* THREAD ONE END */



  semaphore . clearWakeupAlreadyReceived();
  semaphore . clearWakeupAlreadyReceived();
  POUT ( semaphore );
  

  POUT ( semaphore . wakeupOtherThread(FALSE) );
  POUT ( semaphore );



	/* THREAD TWO STARTING SHORTLY */

  cout << "Starting thread Two...\n";

  allowWakeupsThatHaveAlreadyHappened = TRUE;
  threadTwo . start();
  threadTwo . waitForThreadToStop();

	/* THREAD TWO END */


  cout << "Main() ending...\n";
}

