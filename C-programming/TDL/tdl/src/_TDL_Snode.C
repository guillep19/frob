/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

#include "_TDL_Snode.H"

/*virtual*/
_TDL_Snode::~_TDL_Snode()
{
  setNextNode ( (_TDL_Snode *) NULL );
}


