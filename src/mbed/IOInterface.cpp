#include "IOInterface.h"
#include "mbed.h"
#include "hcsr04.h"

//Serial pc(USBTX, USBRX); // tx, rx

#define FROB_LED 1
DigitalOut led(LED1);
DigitalOut led_externo(p11);

#define FROB_TIME 0
Timer t;

#define FROB_DISTANCE_SENSOR 1
HCSR04 distance_sensor(p7, p8); //trig, echo

#define FROB_LIGHT_SENSOR 2
I2C iic(p28, p27); //sda scl

void initialize_iointerface() {
  t.start();
  led = 1;
}

void write_output(WORD index, WORD value) {
  if (index == FROB_LED) {
    led_externo = value;
  }
}

WORD read_input(WORD index) {
  WORD value = 0;
  if (index == FROB_TIME) {
    //Retorna tiempo en decimas de segundos
    float taken_time = t.read();
    //pc.printf("<TIME> %f \n", taken_time);
    value = (WORD) taken_time * 10;
  } else if (index == FROB_DISTANCE_SENSOR) {
    long dist = distance_sensor.distance();
    //pc.printf("<DIST> %d \n", distance);
    value = (WORD) dist;
  } else if (index == FROB_LIGHT_SENSOR) {
    //value = (WORD) iic.read();
    value = 19;
  }
  return value;
}
