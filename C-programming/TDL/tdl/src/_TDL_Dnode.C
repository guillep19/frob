/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

#include "_TDL_Dnode.H"

/*virtual*/
_TDL_Dnode::~_TDL_Dnode()
{
  setNextNode     ( (_TDL_Dnode *) NULL );
  setPreviousNode ( (_TDL_Dnode *) NULL );
}



