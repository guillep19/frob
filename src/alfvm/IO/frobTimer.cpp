
#include "frobTimer.h"
#include "mbed.h"
#include "FrobDefinitions.h"

FrobTimer::FrobTimer() {
  t.start();
}

WORD FrobTimer::read() {
  float time = t.read();
  return (WORD) time * 10;
}
