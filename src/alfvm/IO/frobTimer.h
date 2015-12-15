
#ifndef frobTimer_H
#define frobTimer_H

#include "mbed.h"
#include "FrobDefinitions.h"

class FrobTimer {
  private:
    Timer t;
  public:
    FrobTimer();
    WORD read(); //Return time in ds.
};

#endif /* frobTimer_H */
