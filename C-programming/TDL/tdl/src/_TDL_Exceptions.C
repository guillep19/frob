/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

#include <_TDL_Exceptions.H>

/*virtual*/
_TDL_BaseException::~_TDL_BaseException()
{ }


/*static*/ STRING
_TDL_BaseException::getStaticExceptionName()
{
  return "_TDL_BaseException";
}


/*virtual*/ STRING
_TDL_BaseException::getExceptionName() const
{
  return _TDL_BaseException::getStaticExceptionName();
}


/*static*/ BOOLEAN
_TDL_BaseException::exceptionMatches ( STRING theString )
{
  return StringEqual ( _TDL_BaseException::getStaticExceptionName(),
		       theString );
}


/*virtual*/
_TDL_BaseHandler::~_TDL_BaseHandler()
{ }


	/* Override & Define this method here. */
/*virtual*/ u_int4
_TDL_BaseHandler::_TDL_getMaximumActivates() const
{
  return _TDL_MaximumActivates;
}
