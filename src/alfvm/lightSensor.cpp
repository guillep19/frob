
#include "lightSensor.h"
#include "mbed.h"
#include "FrobDefinitions.h"
#include "serial.h"

LightSensor::LightSensor(PinName sda, PinName scl,
                         int addr, char mode): iic(sda, scl) {
  addr = addr;
  /* initialize */
  buffer[0] = 0;
  buffer[1] = 0;
  iic.frequency(100000);
  buffer[0] = BH1750_POWER_ON; //power on
  int status = iic.write(addr, buffer, 1, false);
  wait_ms(240);
}

WORD LightSensor::read() {
  int status = iic.read(addr, buffer, 2);
  float result = 0;
  result = ((buffer[0]<<8)|buffer[1])/1.2;
  pc.printf("//////////////////////////////Status: %d, Light intensity: %.4f Lux\r\n", status, result);
  //if (status != 0) {
  //  return 255;
  //}
  WORD value = (buffer[0] << 8) | buffer[1];
  return value;
}
