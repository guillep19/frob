/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

#include <tdl.H>
#include <time.h>

#define pout(X)   cout << # X " = "   << (X) << endl
#define poutx(X)  cout << # X " = 0x" << (X) << endl

int
main( int argc, char *argv[] )
{
  MARKUSED ( argv );

	/* If we don't TCM_Initialize(), we get a core dump when we exit. */
  TCM_Initialize();

  MSecs  msecs;
  time_t timeT;

  pout (msecs = _TDL_translateAbsoluteTimeToMSecs ( 12, 11, 10, 0.987, TRUE ));
  pout ( timeT = msecs );
  pout ( ctime ( & timeT ) );

  pout ( msecs /= 1000 );
  pout ( timeT = msecs );
  pout ( ctime ( & timeT ) );

  pout ( msecs = timeInMsecs() );
  pout ( timeT = msecs / 1000 );
  pout ( ctime ( & timeT ) );

  pout ( sizeof ( MSecs  ) );
  pout ( sizeof ( time_t ) );

  if ( argc < 2 )
    return 0;

  u_int8 uint8_currentTime = time(NULL);

  pout ( uint8_currentTime );
  cout << hex;
  cout << "                                        "
       << "uint8_currentTime = 0x" << uint8_currentTime << endl;
  poutx ( uint8_currentTime *= 1000 );
  cout << "                                        "
       << "uint8_currentTime = 0x" << uint8_currentTime << endl;
  poutx ( uint8_currentTime /= 1000 );
  cout << "                                        "
       << "uint8_currentTime = 0x" << uint8_currentTime << endl;
  pout ( timeT = uint8_currentTime );
  pout ( ctime ( & timeT ) );
  cout << endl;

  u_int4 uint4_currentTime = time(NULL);

  pout ( uint4_currentTime );
  cout << hex;
  cout << "                                        "
       << "uint4_currentTime = 0x" << uint4_currentTime << endl;
  poutx ( uint4_currentTime *= 1000 );
  cout << "                                        "
       << "uint4_currentTime = 0x" << uint4_currentTime << endl;
  poutx ( uint4_currentTime /= 1000 );
  cout << "                                        "
       << "uint4_currentTime = 0x" << uint4_currentTime << endl;
  pout ( timeT = uint4_currentTime );
  pout ( ctime ( & timeT ) );
  cout << endl;


  time_t        currentTime = time(NULL);
  unsigned int  baseTime, days, hours, minutes, seconds;

  baseTime  = currentTime * 1000;
  baseTime /= 1000;
  cout << endl << endl << hex
       << "Time       = 0x" << currentTime          << endl
       << "Time*1000  = 0x" << (currentTime * 1000) << endl
       << "Time*/1000 = 0x" << baseTime             << endl
       << dec
       << "Time*/1000 =   " << baseTime             << endl << endl;

  days    = (baseTime / (24 * 60 * 60));
  hours   = (baseTime % (24 * 60 * 60)) / 3600;
  minutes = (baseTime % (     60 * 60)) / 60;
  seconds = (baseTime % (          60));

  cout << "Last wrapped:  " << endl
       << "   Days:  " << days << endl
       << "   Time:  " << hours << " : " << minutes << " : " << seconds
       << endl << endl;


  baseTime = (0xffffffff / 1000) - baseTime;
  days     = (baseTime / (24 * 60 * 60));
  hours    = (baseTime % (24 * 60 * 60)) / 3600;
  minutes  = (baseTime % (     60 * 60)) / 60;
  seconds  = (baseTime % (          60));

  cout << "Next wrap:  " << endl
       << "   Days:  " << days << endl
       << "   Time:  " << hours << " : " << minutes << " : " << seconds
       << endl << endl;

}
