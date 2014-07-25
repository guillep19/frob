/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

#ifndef _TDL_Dnode_H
#define _TDL_Dnode_H

#define _TDL_INTERNAL_
#include "tdl.H"


/*
 * _TDL_Dnode is simply a node of a doubly-linked-list.  The natural 
 * inclination is to merely extend the _TDL_Snode class to include a
 * "previous" link.  However, we don't want _TDL_Dnodes stored in a
 * _TDL_Slist.  (The "previous" link would be improperly set.)  
 *
 * (And, to complicate matters, certain derived classes will need to
 *  inhert from both Snode & Dnode.)
 */

class _TDL_Dnode
#ifndef DISABLE_ISTREAMBASE
		 : public virtual iostreamBase
#endif /* ! DISABLE_ISTREAMBASE */
{
protected:
  _TDL_Dnode  * previousNode;
  _TDL_Dnode  * nextNode;

public:
  _TDL_Dnode()
    : previousNode ( (_TDL_Dnode *) NULL ),
      nextNode     ( (_TDL_Dnode *) NULL )
  {}

  virtual ~_TDL_Dnode();

  _TDL_Dnode * getPreviousNode () const { return previousNode; }
  _TDL_Dnode * getNextNode ()     const { return nextNode; }

  void         setPreviousNode ( _TDL_Dnode * thePreviousNode )
					    { previousNode = thePreviousNode; }

  void         setNextNode     ( _TDL_Dnode * theNextNode )
					    { nextNode = theNextNode;         }

	/* Abstract iostreamBase-interface method */
  virtual ostream & printObject (
	     ostream    & theOstream,
	     const char * theIndentString = "") const TCM_PURE_VIRTUAL_METHOD ;


	/* These operations are disabled (prohibited) & unimplemented */
private:
  _TDL_Dnode (              const _TDL_Dnode & theObjectToCopy );
  _TDL_Dnode & operator = ( const _TDL_Dnode & theObjectToCopy );
};


#endif /* _TDL_Dnode_H */
