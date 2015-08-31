#include "IOInterface.h"
#include "mbed.h"
#include "hcsr04.h"
#include "pwmEngine.h"
#include "frobTimer.h"
#include "bh1750.h"

#define FROB_LED1 0
DigitalOut led(LED1);
#define FROB_ENGINE_L 1
PWMEngine engine_left(p24);
#define FROB_ENGINE_R 2
PWMEngine engine_right(p25);

#define FROB_TIME 0
FrobTimer t;
#define FROB_DISTANCE_SENSOR 1
HCSR04 distance_sensor(p14, p15); //trig, echo
#define FROB_LIGHT_SENSOR_R 2 //sda scl (add=vcc)
//LightSensor light_sensor_r(p28, p27, BH1750_V_CHIP_ADDR);
#define FROB_LIGHT_SENSOR_L 3 //sda scl (add=gnd)
BH1750 light_sensor_l(p28, p27, BH1750_G_CHIP_ADDR);

void write_output(WORD index, WORD value) {
  switch (index) {
    case FROB_LED1:
      led = value; break;
    case FROB_ENGINE_L:
      engine_left.write(value); break;
    case FROB_ENGINE_R:
      engine_right.write(value); break;
    default:
      break;
  }
}

WORD read_input(WORD index) {
  WORD value = 0;
  switch (index) {
    case FROB_TIME:
      value = t.read(); break;
    case FROB_DISTANCE_SENSOR:
      value = distance_sensor.read(); break;
    case FROB_LIGHT_SENSOR_R:
      //value = light_sensor_r.read(); break;
    case FROB_LIGHT_SENSOR_L:
      value = light_sensor_l.read(); break;
    default:
      break;
  }
  return value;
}
