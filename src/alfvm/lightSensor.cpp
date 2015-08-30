
#include "lightSensor.h"
#include "mbed.h"
#include "FrobDefinitions.h"

LightSensor::LightSensor(PinName sda, PinName scl,
                         int addr, char mode): iic(sda, scl) {
  addr = addr;
  /* initialize */
  buffer[0] = mode;
  buffer[1] = 0;
  iic.write(addr, buffer, 1, false);
  wait_ms(10);
}

WORD LightSensor::read() {
  int status = iic.read(addr, buffer, 2, false);
  if (status != 0) {
    return 255;
  }
  WORD value = (buffer[0] << 8) | buffer[1];
  return value;
}
