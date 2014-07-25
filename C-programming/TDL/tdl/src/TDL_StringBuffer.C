/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

#include "TDL_StringBuffer.H"

/*virtual*/
TDL_StringBuffer::~TDL_StringBuffer()
{
#if !__GNUC_PREREQ(3,3)
    /* Allow internal string to be free'd when we are destroyed. */
  unfreeze();
#endif
}
