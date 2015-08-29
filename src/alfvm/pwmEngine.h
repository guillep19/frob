

#ifndef pwmEngine_H
#define pwmEngine_H
#include "mbed.h"
#include "FrobDefinitions.h"

class PWMEngine {
  private:
    PwmOut engine;
  public:
    PWMEngine(PinName e);
    //0 = Stopped, 256 = Max speed.
    void write(WORD speed);
};

#endif /* pwmEngine_H */

