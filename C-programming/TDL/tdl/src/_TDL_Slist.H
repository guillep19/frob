/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

#ifndef _TDL_Slist_H
#define _TDL_Slist_H

#define _TDL_INTERNAL_
#include "tdl.H"
#include "_TDL_Snode.H"


/*
 * _TDL_Slist is simply a singly-linked-list.
 */

class _TDL_Slist :
#ifndef DISABLE_ISTREAMBASE
		   public virtual iostreamBase,
#endif /* ! DISABLE_ISTREAMBASE */
		   public _TDL_Snode
{
protected:
  BOOLEAN      deletesContainedObjects;
  _TDL_Snode * ___firstNode;
  _TDL_Snode * ___lastNode;

public:
		/* Default is *TO* delete the "contained" objects */
  _TDL_Slist ( BOOLEAN theDeletesContainedObjects = TRUE )
    : deletesContainedObjects ( theDeletesContainedObjects ),
      ___firstNode            ( (_TDL_Snode *) NULL        ),
      ___lastNode             ( (_TDL_Snode *) NULL        )
  {}

	/* Note:  Deletes all elements if deletesContainedObjects == TRUE */
  virtual ~_TDL_Slist();

	/* Note:  Deletes all elements if deletesContainedObjects == TRUE */
  void     emptyAllElementsInList();

	/* ASSUMES that theSnode can be deleted  *
	 * when this Slist is deleted!!!         *
	 *  (if deletesContainedObjects == TRUE) */
  status_t appendNode  ( _TDL_Snode * theSnode );
  status_t prependNode ( _TDL_Snode * theSnode );

	/* Note:  Does NOT delete theSnode. *
	 * Returns NULL for failure.        */
  _TDL_Snode * removeNode ( _TDL_Snode * theSnode );


  BOOLEAN  contains ( const _TDL_Snode * theSnode ) const;

  u_int4   count() const;

  BOOLEAN  isEmpty() const
	       { return getFirstNode() == (_TDL_Snode *) NULL ? TRUE : FALSE; }

  _TDL_Snode * getFirstNode() const { return ___firstNode; }
  _TDL_Snode * getLastNode () const { return ___lastNode;  }
  _TDL_Snode * getNodeAtIndex ( u_int4 theIndex ) const;

  BOOLEAN getDeletesContainedObjects() const
				    { return deletesContainedObjects; }

  void setDeletesContainedObjects ( BOOLEAN theDeletesContainedObjects )
		      { deletesContainedObjects = theDeletesContainedObjects; }

  void repairList();

	/* Stack-imitation operations */
  _TDL_Snode * pop        (                            );
  status_t     push       ( _TDL_Snode * theNodeToPush )
				       { return appendNode ( theNodeToPush ); }
  _TDL_Snode * topOfStack (                            ) const
				       { return getLastNode();                }


	/* iostreamBase interface */
  virtual ostream & printObject ( ostream    & theOstream,
				  const char * theIndentString = "") const;

protected:
  void setFirstNode ( _TDL_Snode * theSnode ) { ___firstNode = theSnode; }
  void setLastNode  ( _TDL_Snode * theSnode ) { ___lastNode  = theSnode; }


	/* These operations are disabled (prohibited) & unimplemented */
private:
  _TDL_Slist (              const _TDL_Slist & theObjectToCopy );
  _TDL_Slist & operator = ( const _TDL_Slist & theObjectToCopy );
};

#endif /* _TDL_Slist_H */
