/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

#ifndef _TDL_Exceptions_H
#define _TDL_Exceptions_H

#define _TDL_INTERNAL_
#include "tdl.H"

	/* Base class for all TDL Exceptions */
class _TDL_BaseException : public _TDL_BaseException_TCMInterfaceBaseClass
{
public:
  static  STRING TCM_getStaticName() { return "_TDL_BaseException";}
  virtual STRING TCM_getActionName() { return TCM_getStaticName(); }

public:
  _TDL_BaseException() {}
  virtual ~_TDL_BaseException();

  static  STRING  getStaticExceptionName();
  virtual STRING  getExceptionName() const;
  static  BOOLEAN exceptionMatches ( STRING theString );
};


inline _TDL_BaseException *
_TDL_CreateException__TDL_BaseException()
{
  return new _TDL_BaseException();
}



	/* Base class for all TDL Exception Handlers */
class _TDL_BaseHandler : public _TDL_BaseHandler_TCMInterfaceBaseClass
{
public:
  static  STRING TCM_getStaticName() { return "_TDL_BaseHandler";  }
  virtual STRING TCM_getActionName() { return TCM_getStaticName(); }

public:
  const u_int4 _TDL_MaximumActivates;

  _TDL_BaseHandler()
    : _TDL_MaximumActivates ( _TDL_MAXIMUM_ACTIVATES )
  {}

  _TDL_BaseHandler ( u_int4 theMaximumActivates )
    : _TDL_MaximumActivates ( theMaximumActivates )
  {}

  _TDL_BaseHandler ( const _TDL_BaseHandler & theBaseHandler )
    : _TDL_BaseHandler_TCMInterfaceBaseClass(),
      _TDL_MaximumActivates ( theBaseHandler._TDL_getMaximumActivates() )
  {}

  virtual ~_TDL_BaseHandler();

	/* Override & Define this method here. */
  virtual u_int4 _TDL_getMaximumActivates() const;

/* Inherited methods:
 *  virtual void execute (TCM_Task_Tree_Ref const & _TDL_ENCLOSING_TASK) //= 0;
 *                                                    TCM_PURE_VIRTUAL_METHOD ;
 *  virtual STRING _TDL_getHandledExceptionName() const //= 0;
 *                                                    TCM_PURE_VIRTUAL_METHOD ;
 */

};

#endif /* _TDL_Exceptions_H */

