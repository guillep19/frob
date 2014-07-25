/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

#ifndef _TDL_Snode_H
#define _TDL_Snode_H

#define _TDL_INTERNAL_
#include "tdl.H"


class _TDL_Snode 
#ifndef DISABLE_ISTREAMBASE
		 : public virtual iostreamBase
#endif /* ! DISABLE_ISTREAMBASE */
{
protected:
  _TDL_Snode  * nextNode;

public:
  _TDL_Snode() : nextNode ( (_TDL_Snode *) NULL )  {}

  virtual ~_TDL_Snode();

  _TDL_Snode * getNextNode () const { return nextNode; }
  void         setNextNode ( _TDL_Snode * theNextNode )
						  { nextNode = theNextNode; }

	/* Abstract iostreamBase-interface method */
  virtual ostream & printObject (
	     ostream    & theOstream,
	     const char * theIndentString = "") const TCM_PURE_VIRTUAL_METHOD ;


	/* These operations are disabled (prohibited) & unimplemented */
private:
  _TDL_Snode (              const _TDL_Snode & theObjectToCopy );
  _TDL_Snode & operator = ( const _TDL_Snode & theObjectToCopy );
};


#endif /* _TDL_Snode_H */
