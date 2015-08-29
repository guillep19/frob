#include "IOInterface.h"
#include "mbed.h"
#include "hcsr04.h"

//Serial pc(USBTX, USBRX); // tx, rx

#define FROB_LED1 0
DigitalOut led(LED1);
#define FROB_ENGINE_L 1
PWMEngine engine_left(p24);
#define FROB_ENGINE_R 2
PWMEngine engine_right(p25);

#define FROB_TIME 0
Timer t;
#define FROB_DISTANCE_SENSOR 1
HCSR04 distance_sensor(p14, p15); //trig, echo
#define FROB_LIGHT_SENSOR_L 2
I2C iic(p28, p27); //sda scl
#define FROB_LIGHT_SENSOR_R 3
I2C iic(p28, p27); //sda scl

void initialize_iointerface() {
  t.start();
}

void write_output(WORD index, WORD value) {
  switch (index) {
    case FROB_LED1:
      led = value; break;
    case FROB_ENGINE_L:
      max = 256
      min = 0
    case FROB_ENGINE_R:

  } else if (index == FROB_MOTOR_IZQ) {
    float val = value * 0.1;
    while (val > 1) val-=1;
    motor_izq = val;
  } else if (index == FROB_MOTOR_DER) {
    float val = value * 0.1;
    while (val > 1) val-=1;
    motor_der = val;
  }
}

WORD read_input(WORD index) {
  WORD value = 0;
  if (index == FROB_TIME) {
    value = t.read();
  } else if (index == FROB_DISTANCE_SENSOR) {
    value = distance_sensor.read();
  } else if (index == FROB_LIGHT_SENSOR) {
    value = light//value = (WORD) iic.read();
    value = 19;
  }
  return value;
}
