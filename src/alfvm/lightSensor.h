
#ifndef lightSensor_H
#define lightSensor_H

#include "mbed.h"
#include "FrobDefinitions.h"

class LightSensor {
  private:
    I2C iic;
    WORD addr;
  public:
    LightSensor(PinName sda, PinName scl, WORD addr);
    WORD read();
}

#endif /* lightSensor_H */

