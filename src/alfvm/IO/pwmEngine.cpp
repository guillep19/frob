
#include "pwmEngine.h"
#include "mbed.h"
#include "FrobDefinitions.h"

PWMEngine::PWMEngine(PinName e) : engine(e) {}

void PWMEngine::write(WORD speed) {
  //if (speed > 0) {
  //  engine = 0.8f;
  //} else {
  //  engine = 0.0f;
  //}

  engine = ((float)speed)/256.0;
  //float period = 0.020;
  //engine.period(period);
  //engine.pulsewidth(() * period);
}

