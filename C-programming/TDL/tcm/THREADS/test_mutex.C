
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



#include "Thread.H"
#include "Mutex.H"

#define POUT(X)    cout << # X " = " << (X);
#define POUTNL(X)  cout << # X " = " << (X) << endl

Mutex  fastMutex      ( Mutex::FAST        );
Mutex  recursiveMutex ( Mutex::RECURSIVE   );
Mutex  errorMutex     ( Mutex::ERROR_CHECK );

void
test_fast()
{
  cout << "FastMutex Thread Running" << endl;
  POUTNL ( fastMutex.lock() );
  sleep ( 2 );
  cout << "FastMutex Thread Ending after unlock" << endl;
  POUTNL ( fastMutex.unlock() );
}

void
test_recursive()
{
  cout << "recursiveMutex Thread Running" << endl;
  POUTNL ( recursiveMutex.lock() );
  sleep ( 2 );
  cout << "recursiveMutex Thread Ending after unlock" << endl;
  POUTNL ( recursiveMutex.unlock() );
}

void
test_error()
{
  cout << "errorMutex Thread Running" << endl;
  POUTNL ( errorMutex.lock() );
  sleep ( 2 );
  cout << "errorMutex Thread Ending after unlock" << endl;
  POUTNL ( errorMutex.unlock() );
}



int
main()
{
  Thread  fastThread, recursiveThread, errorThread;
  status_t  status;

  POUTNL ( fastThread      . setThreadFunction ( test_fast      ) );
  POUTNL ( recursiveThread . setThreadFunction ( test_recursive ) );
  POUTNL ( errorThread     . setThreadFunction ( test_error     ) );

  POUT   ( fastMutex );
  POUTNL ( fastMutex . lock() );
  POUT   ( fastMutex );
  POUTNL ( fastMutex . unlock() );
  POUT   ( fastMutex );

  cout << endl << endl;

  POUT   ( recursiveMutex );
  POUTNL ( recursiveMutex . lock() );
  cout << "Recursive Mutexes only register as being locked if they "
       << "are locked by a different Thread." << endl;
  POUT   ( recursiveMutex );
  POUTNL ( recursiveMutex . lock() );
  POUTNL ( recursiveMutex . unlock() );
  POUTNL ( recursiveMutex . unlock() );
  POUT   ( recursiveMutex );

  cout << endl << endl;

  POUT   ( errorMutex );
  POUTNL ( errorMutex . lock() );
  POUT   ( errorMutex );
  POUTNL ( errorMutex . lock() );
  POUTNL ( errorMutex . unlock() );
  POUT   ( errorMutex );


  cout << endl << endl;
  fastThread . start();
  sleep ( 1 );
  POUT   ( fastMutex );
  status = fastMutex . lock();
  Thread::yield(); /* Try to let the unlock message print first. */
  cout << "fastMutex . lock() = " << status << endl;
  POUT   ( fastMutex );
  POUTNL ( fastMutex . unlock() );
  POUT   ( fastMutex );
  POUTNL ( fastThread . waitForThreadToStop() );

  cout << endl << endl;
  recursiveThread . start();
  sleep ( 1 );
  POUT   ( recursiveMutex );
  status = recursiveMutex . lock();
  Thread::yield(); /* Try to let the unlock message print first. */
  cout << "recursiveMutex . lock() = " << status << endl;
  cout << "Recursive Mutexes only register as being locked if they "
       << "are locked by a different Thread." << endl;
  POUT   ( recursiveMutex );
  POUTNL ( recursiveMutex . lock() );
  POUTNL ( recursiveMutex . unlock() );
  POUTNL ( recursiveMutex . unlock() );
  POUT   ( recursiveMutex );
  POUTNL ( recursiveThread . waitForThreadToStop() );


  cout << endl << endl;
  errorThread . start();
  sleep ( 1 );
  POUT   ( errorMutex );
  status = errorMutex . lock();
  Thread::yield(); /* Try to let the unlock message print first. */
  cout << "errorMutex . lock() = " << status << endl;
  POUT   ( errorMutex );
  POUTNL ( errorMutex . lock() );
  POUTNL ( errorMutex . unlock() );
  POUT   ( errorMutex );
  POUTNL ( errorThread . waitForThreadToStop() );
}
