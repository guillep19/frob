
#ifndef lightSensor_H
#define lightSensor_H

//BH1750

#define LIGHTSENSOR_ADDR_VCC 0x5C
#define LIGHTSENSOR_ADDR_GND 0x23
// No active state
#define BH1750_POWER_DOWN 0x00
// Wating for measurment command
#define BH1750_POWER_ON 0x01
// Reset data register value - not accepted in POWER_DOWN mode
#define BH1750_RESET 0x07
// Start measurement at 1lx resolution. Measurement time is approx 120ms.
#define BH1750_CONTINUOUS_HIGH_RES_MODE  0x10
// Start measurement at 0.5lx resolution. Measurement time is approx 120ms.
#define BH1750_CONTINUOUS_HIGH_RES_MODE_2  0x11
// Start measurement at 4lx resolution. Measurement time is approx 16ms.
#define BH1750_CONTINUOUS_LOW_RES_MODE  0x13

#include "mbed.h"
#include "FrobDefinitions.h"

class LightSensor {
  private:
    I2C iic;
    int addr;
    char buffer[2];
  public:
    LightSensor(PinName sda, PinName scl,
                int addr, char mode);
    WORD read();
};

#endif /* lightSensor_H */
 
