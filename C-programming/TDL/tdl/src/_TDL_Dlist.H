/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

#ifndef _TDL_Dlist_H
#define _TDL_Dlist_H

#define _TDL_INTERNAL_
#include "tdl.H"
#include "_TDL_Dnode.H"


/*
 * _TDL_Dlist is simply a doubly-linked-list.  The natural inclination
 * is to merely extend the _TDL_Slist class to deal with the "previous"
 * link.  However, we want to keep _TDL_Dnode's and _TDL_Snode's distinct.
 * And so, for lack of a better solution, for the time being we are
 * duplicating some of the _TDL_Slist functionality here.
 */

class _TDL_Dlist :
#ifndef DISABLE_ISTREAMBASE
		   public virtual iostreamBase,
#endif /* ! DISABLE_ISTREAMBASE */
		   public _TDL_Dnode
{
public:
  enum DIRECTION
  {
    FORWARD  = TRUE,
    BACKWARD = FALSE
  };

protected:
  BOOLEAN      deletesContainedObjects;
  _TDL_Dnode * ___firstNode;
  _TDL_Dnode * ___lastNode;

public:
		/* Default is *NOT* to delete the "contained" objects */
  _TDL_Dlist ( BOOLEAN theDeletesContainedObjects = FALSE )
    : deletesContainedObjects ( theDeletesContainedObjects ),
      ___firstNode            ( (_TDL_Dnode *) NULL        ),
      ___lastNode             ( (_TDL_Dnode *) NULL        )
  {}

	/* Note:  Deletes all elements if deletesContainedObjects == TRUE */
  virtual ~_TDL_Dlist();

	/* Note:  Deletes all elements if deletesContainedObjects == TRUE */
  void     emptyAllElementsInList();

	/* ASSUMES that theDnode can be deleted  *
	 * when this Dlist is deleted!!!         *
	 *  (if deletesContainedObjects == TRUE) */
  status_t appendNode  ( _TDL_Dnode * theDnode );
  status_t prependNode ( _TDL_Dnode * theDnode );

	/* Note:  Does NOT delete theDnode. *
	 * Returns NULL for failure.        */
  _TDL_Dnode * removeNode ( _TDL_Dnode *           theDnode,
			    _TDL_Dlist::DIRECTION  theSearchDiretion
						      = _TDL_Dlist::BACKWARD );

  BOOLEAN  contains ( const _TDL_Dnode *     theDnode,
		      _TDL_Dlist::DIRECTION  theSearchDiretion
						= _TDL_Dlist::BACKWARD ) const;

  u_int4   count() const;

  BOOLEAN  isEmpty() const
	       { return getFirstNode() == (_TDL_Dnode *) NULL ? TRUE : FALSE; }

  _TDL_Dnode * getFirstNode() const { return ___firstNode; }
  _TDL_Dnode * getLastNode () const { return ___lastNode;  }
  _TDL_Dnode * getNodeAtIndex ( u_int4 theIndex ) const;

  BOOLEAN getDeletesContainedObjects() const
				    { return deletesContainedObjects; }

  void setDeletesContainedObjects ( BOOLEAN theDeletesContainedObjects )
		      { deletesContainedObjects = theDeletesContainedObjects; }

  void repairList();

	/* Stack-imitation operations */
  _TDL_Dnode * pop        (                            );
  status_t     push       ( _TDL_Dnode * theNodeToPush )
				       { return appendNode ( theNodeToPush ); }
  _TDL_Dnode * topOfStack (                            ) const
				       { return getLastNode();                }


	/* iostreamBase interface */
  virtual ostream & printObject ( ostream    & theOstream,
				  const char * theIndentString = "") const;


protected:
  void setFirstNode ( _TDL_Dnode * theDnode ) { ___firstNode = theDnode; }
  void setLastNode  ( _TDL_Dnode * theDnode ) { ___lastNode  = theDnode; }


	/* These operations are disabled (prohibited) & unimplemented */
private:
  _TDL_Dlist (              const _TDL_Dlist & theObjectToCopy );
  _TDL_Dlist & operator = ( const _TDL_Dlist & theObjectToCopy );
};

#endif /* _TDL_Dlist_H */
