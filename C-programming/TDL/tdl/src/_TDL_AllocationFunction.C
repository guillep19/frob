/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

/* Note:  This will include tcm.h, _TDL_InterfaceToTCM_2.H,
 *        _TDL_AllocationFunction.H, and tdldef.H
 *        in the correct order.
 */

#define _TDL_INTERNAL_
#include "tdl.H"

/*static*/ const char *   _TDL_AllocationFunction::TYPES_STRINGS
		        [ _TDL_AllocationFunction::NUMBER_OF_TYPES_STRINGS ]
  = { "LOCAL_NONDISTRIBUTED_ONLY",
      "EITHER_LOCAL_OR_DISTRIBUTED",
      "DISTRIBUTED_ONLY",
      "DELAYED_ALLOCATION",
      "NULL",
      "UNKNOWN",
      "INVALID_VALUE" };


	/* This resolves the header-file interdependency between
	 * _TDL_AllocationFunction.H and tdldef.H.
	 */
/*static*/ ostream &
_TDL_AllocationFunction::getLogStream()
{
  return TDL::getLogStream();
}

