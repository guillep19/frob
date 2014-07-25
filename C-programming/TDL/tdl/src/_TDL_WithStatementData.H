/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

#ifndef _TDL_WithStatementData_H
#define _TDL_WithStatementData_H

#define _TDL_INTERNAL_
#include "tdl.H"
#include "_TDL_Snode.H"
#include "_TDL_WithStatement.H"

/*
 * _TDL_WithStatementData represents the actual data associated with
 * a specific TDL WITH statement. In the event that the TDL WITH
 * statement occurs inside an iterative loop, an instance of this
 * class will represent one (and only one) single occurrence of that
 * iteration.  (More _TDL_WithStatementData objects will be created
 * for the other iterations.)
 *
 * (See _TDL_WithStatement.H for the Association Relationship.)
 */

class _TDL_WithStatementTreeNode;

class _TDL_WithStatementData : 
#ifndef DISABLE_ISTREAMBASE
		          public virtual iostreamBase,
#endif /* ! DISABLE_ISTREAMBASE */
			  public _TDL_TreeNodeBranch
{
protected:
  _TDL_WithStatement &  withStatement;
  _TDL_Slist            withStatementTreeNodeSlist;


public:
  _TDL_WithStatementData ( _TDL_WithStatement & theWithStatement )
    :  _TDL_TreeNodeBranch        (                                     ),
       withStatement              ( theWithStatement                    ),
       withStatementTreeNodeSlist ( TRUE /* Delete contained objects */ )
  {}

  virtual ~_TDL_WithStatementData();

  const _TDL_Slist & getWithStatementTreeNodeSlist() const
					 { return withStatementTreeNodeSlist; }

  _TDL_WithStatementTreeNode * createAndAttachNewWithStatementTreeNode();

  status_t removeWithStatementTreeNode (
	       _TDL_WithStatementTreeNode * theWithStatementTreeNodeToRemove );

  _TDL_WithStatement &   getWithStatement() const { return withStatement;  }



        /*************************************************/
	/* _TDL_TreeNode / _TDL_TreeNodeBranch interface */
        /*************************************************/

	/* Names are really useful for identifying (subclassed) *
	 * _TDL_TreeNode instances during debugging/testing...  */
  virtual const char * getName() const;
  virtual BOOLEAN      hasName ( const char * theName ) const;


	/* Common workhorse method to destroy _TDL_SpawnStatementData's *
         * referred to by this subtree.                                 */
  virtual status_t doDestroy ( BOOLEAN   theCanStillBeReferenced,
			       BOOLEAN   theDestroyIfUnused,
			       BOOLEAN & theFoundSpawnStatementData,
			       int4      theDepth                    );

	/* Lets allow safe-downcasts... (Basic RTTI) */
  virtual operator const _TDL_WithStatementData * () const;

	/* iostreamBase interface. */
  virtual ostream & printObject ( ostream    & theOstream,
				  const char * theIndentString = "") const;




  int operator == (const _TDL_WithStatementData & theWithStatementData) const
				  { return (& theWithStatementData) == this; }
  int operator != (const _TDL_WithStatementData & theWithStatementData) const
				  { return (& theWithStatementData) != this; }


protected:
  _TDL_Slist & getWithStatementTreeNodeSlistNonConst()
					{ return withStatementTreeNodeSlist; }


	/* These operations are disabled (prohibited) & unimplemented */
private:
  _TDL_WithStatementData (  const _TDL_WithStatementData & theObjectToCopy );
  _TDL_WithStatementData & operator = (
			     const _TDL_WithStatementData & theObjectToCopy );
};

#endif /* _TDL_WithStatementData_H */

