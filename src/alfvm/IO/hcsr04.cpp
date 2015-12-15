#include "hcsr04.h"
#include "mbed.h"
#include "FrobDefinitions.h"
/*
*HCSR04.cpp
*/
HCSR04::HCSR04(PinName t, PinName e) : trig(t), echo(e) {}

long HCSR04::echo_duration() {
  timer.reset();  //reset timer
  trig = 0;   // trigger low 
  wait_us(2); //  wait 
  trig = 1;   //  trigger high
  wait_us(10);
  trig = 0;  // trigger low
  while(!echo); // start pulseIN
  timer.start();
  while(echo);
  timer.stop();
  return timer.read_us(); 
}

//return distance in cm
long HCSR04::distance() {
  duration = echo_duration();
  distance_cm = (duration/2)/29.1;
  return distance_cm;
}

//returns distance in cm, max distance is 256.
WORD HCSR04::read() {
  long dist = distance();
  if (dist > 255) dist = 255;
  return (WORD) dist;
}
