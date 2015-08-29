
#include "lightSensor.h"
#include "mbed.h"
#include "FrobDefinitions.h"

LightSensor::LightSensor(PinName sda, PinName scl, WORD addr) iic(sda, scl) {
  addr = addr;
}

LightSensor::read() {
  WORD value = iic.read(addr);
  return value;
}
