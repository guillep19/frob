#ifndef IOINTERFACE_H
#define IOINTERFACE_H

#include "FrobDefinitions.h"

void initialize_iointerface();

void write_output(WORD index, WORD value);

WORD read_input(WORD index);

#endif /* IOINTERFACE_H */
