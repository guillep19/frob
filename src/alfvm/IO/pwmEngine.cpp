
#include "pwmEngine.h"
#include "mbed.h"
#include "FrobDefinitions.h"

PWMEngine::PWMEngine(PinName e) : engine(e) {}

void PWMEngine::write(WORD speed) {
  engine = speed/256.0;
}

