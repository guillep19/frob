/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

#ifndef _TDL_WithStatementTreeNode_H
#define _TDL_WithStatementTreeNode_H

#define _TDL_INTERNAL_
#include "tdl.H"
#include "_TDL_Snode.H"
#include "_TDL_TreeNode.H"
#include "_TDL_WithStatementData.H"

/*
 * _TDL_WithStatementTreeNode represents a TreeNode that corresponds
 * to a specific _TDL_WithStatementData object.
 *
 * (See _TDL_WithStatement.H for the Association Relationship.)
 */


class _TDL_WithStatementTreeNode :
#ifndef DISABLE_ISTREAMBASE
		          public virtual iostreamBase,
#endif /* ! DISABLE_ISTREAMBASE */
			  public _TDL_Snode,
			  public _TDL_TreeNode
{
protected:
  _TDL_WithStatementData &  withStatementData;


public:
  _TDL_WithStatementTreeNode (_TDL_WithStatementData & theWithStatementData)
    :  _TDL_Snode        (                       ),
       _TDL_TreeNode     (                       ),
       withStatementData ( theWithStatementData )
  {}

  virtual ~_TDL_WithStatementTreeNode();


  _TDL_WithStatementData & getWithStatementData() const
						 { return withStatementData; }

  _TDL_WithStatement     & getWithStatement()     const
		      { return getWithStatementData() . getWithStatement(); }



        /***************************/
	/* _TDL_TreeNode interface */
        /***************************/


        /* Allow automatic conversion to TCM_Task_Tree_Ref.
	 *
	 * (For the first _TDL_SpawnStatementTreeNode that we contain.)
	 * (May also transparently allocate() that first
	 *  _TDL_SpawnStatementData, performing lazy allocation.)
	 *
         * In the event that we contain no _TDL_SpawnStatementTreeNode's,
         * we return the [empty] _TDL_TreeNode::EMPTY_TCM_TASK_TREE_REF.
         */
  virtual operator const TCM_Task_Tree_Ref & ();


	/* Names are really useful for identifying (subclassed) *
	 * _TDL_TreeNode instances during debugging/testing...  */
  virtual const char * getName() const;
  virtual BOOLEAN      hasName ( const char * theName ) const;


	/* Returns our corresponding children... */
  virtual const _TDL_Dlist * getChildren() const;


	/* Performs a depth-first search to discover whether or not this *
	 * object contains any _TDL_SpawnStatementTreeNode objects...    */
  virtual const _TDL_SpawnStatementTreeNode *
				      firstSpawnStatementTreeNodeConst() const;

	/* Common workhorse method to destroy _TDL_WithStatementData's *
         * referred to by this subtree.                                 */
  virtual status_t doDestroy ( BOOLEAN   theCanStillBeReferenced,
			       BOOLEAN   theDestroyIfUnused,
			       BOOLEAN & theFoundWithStatementData,
			       int4      theDepth                    );

	/* Lets allow safe-downcasts... (Basic RTTI) */
  virtual operator const _TDL_WithStatementTreeNode * () const;


	/* iostreamBase interface */
  virtual ostream & printObject ( ostream    & theOstream,
				  const char * theIndentString = "") const;



  int operator == (
	  const _TDL_WithStatementTreeNode & theWithStatementTreeNode ) const
			      { return (& theWithStatementTreeNode) == this; }
  int operator != (
	  const _TDL_WithStatementTreeNode & theWithStatementTreeNode ) const
			      { return (& theWithStatementTreeNode) != this; }


  int operator == (const _TDL_WithStatementData & theWithStatementData) const
		   { return getWithStatementData() == theWithStatementData; }
  int operator != (const _TDL_WithStatementData & theWithStatementData) const
		   { return getWithStatementData() != theWithStatementData; }


	/* These operations are disabled (prohibited) & unimplemented */
private:
  _TDL_WithStatementTreeNode (
			 const _TDL_WithStatementTreeNode & theObjectToCopy );
  _TDL_WithStatementTreeNode & operator = (
			 const _TDL_WithStatementTreeNode & theObjectToCopy );

};

#endif /* _TDL_WithStatementTreeNode_H */
