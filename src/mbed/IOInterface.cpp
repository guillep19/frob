#include "IOInterface.h"
#include "mbed.h"
#include "hcsr04.h"

//Serial pc(USBTX, USBRX); // tx, rx

#define FROB_LED 1
DigitalOut led(LED1);

#define FROB_TIME 1
Timer t;

#define FROB_DISTANCE_SENSOR 1
HCSR04 distance_sensor(p7, p8);

void initialize_iointerface() {
  t.start();
}

void write_output(WORD index, WORD value) {
  if (index == FROB_LED) {
    led = value;
  }
}

void read_input(WORD index) {
  if (index == FROB_TIME) {
    float taken_time = t.read();
    //pc.printf("<TIME> %f \n", taken_time);
  } else if (index == FROB_DISTANCE_SENSOR) {
    long dist = distance_sensor.distance();
    //pc.printf("<DIST> %d \n", distance);
  }
}
