/* File: HCSR04.h
 * Author: Guillermo Pacheco
 * Hardware: Ultrasonic Range HC-SR04,  
 * 
 * Work derived from Antonio Buonanno, also
 * derived from Arduino library.
 *
 * Desc: driver for HCSR04 Ultrasonic Range Finder. The returned range
 *       is in centimeters.
 *
 */
 
/* Example
  #include "mbed.h"
  #include "hcsr04.h"
 
  HCSR04 sensor(D12, D11); //12 trigger, 11 echo.
  int main() {
    while(1) {
      long distance = sensor.distance();   
      printf("distance=%d\n",distance);
      wait(1.0); // 1 sec  
    }
  }
*/
#ifndef hcsr04_H
#define hcsr04_H
#include "mbed.h"
 
class HCSR04 {
  private:
    DigitalOut trig;
    DigitalIn echo;
    Timer timer;
    long duration,distance_cm;
  public:
    HCSR04(PinName t, PinName e);
    long echo_duration();
    long distance();
};
 
#endif
