#include "IOInterface.h"
#include <stdio.h>

#define FROB_LED1 0
#define FROB_ENGINE_L 1
#define FROB_ENGINE_R 2

#define FROB_TIME 0
#define FROB_DISTANCE_SENSOR 1
#define FROB_LIGHT_SENSOR_R 2
#define FROB_LIGHT_SENSOR_L 3

void write_output(WORD index, WORD value) {
  switch (index) {
    case FROB_LED1:
      printf("write(led, %d)", value); break;
    case FROB_ENGINE_L:
      printf("write(engine_left, %d)", value); break;
    case FROB_ENGINE_R:
      printf("write(engine_right, %d)", value); break;
    default:
      break;
  }
}

WORD read_input(WORD index) {
  WORD value = 0;
  switch (index) {
    case FROB_TIME:
      value = 18; break;
    case FROB_DISTANCE_SENSOR:
      value = 20; break;
    case FROB_LIGHT_SENSOR_R:
      value = 10; break;
    case FROB_LIGHT_SENSOR_L:
      value = 35; break;
    default:
      break;
  }
  return value;
}
