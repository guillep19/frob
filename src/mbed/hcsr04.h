/* File: HCSR04.h
 * Author: Antonio Buonanno  
 * Hardware: Ultrasonic Range HC-SR04,  
 * 
 * This work derived from Arduino library, 
 *
 * Desc: driver for HCSR04 Ultrasonic Range Finder.  The returned range
 *       is in units of meters.
 *
*/
 
/* EXAMPLE
#include "mbed.h"
#include "hcsr04.h"
 
  //D12 TRIGGER D11 ECHO
  HCSR04 sensor(D12, D11); 
  int main() {
    while(1) {
     long distance = sensor.distance();   
      printf("distanza  %d  \n",distance);
      wait(1.0); // 1 sec  
        
    }
}
*/
#ifndef hcsr04_H
#define hcsr04_H
#include "mbed.h"
 
class HCSR04 {
  public:
    HCSR04(PinName t, PinName e);
    long echo_duration();
    long distance();
 
    private:
        DigitalOut trig;
        DigitalIn echo;
        Timer timer;
        long duration,distance_cm;
};
 
#endif
