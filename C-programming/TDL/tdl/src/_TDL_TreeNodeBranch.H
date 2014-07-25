/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

#ifndef _TDL_TreeNodeBranch_H
#define _TDL_TreeNodeBranch_H

#define _TDL_INTERNAL_
#include "tdl.H"
#include "_TDL_Snode.H"
#include "_TDL_Dlist.H"
#include "_TDL_TreeNode.H"
#include "_TDL_ConstraintsList.H"


/*
 * _TDL_TreeNodeBranch is an abstract base class representing
 * _TDL_TreeNode's that contain children or offspring _TDL_TreeNode's.
 *
 * For convenience in dealing with arrays of SPAWN's, _TDL_TreeNodeBranch'es
 * also contain a list of WITH-Statement-style constraints via the
 * _TDL_ConstraintsList base class.
 */

class _TDL_TreeNodeBranch : public _TDL_TreeNode,
			    public _TDL_Snode,
			    public _TDL_ConstraintsList
{
protected:
  static u_int4      TREE_NODE_BRANCH_COUNT; /* = 0 */

protected:
  _TDL_Dlist         childrenDlist;
  const u_int4       branchNumber;  /* Used for debugging and to implement */
  const char *       branchName;    /* getName() if it's not overriden.    */


public:
  _TDL_TreeNodeBranch ( )
    :  _TDL_TreeNode        (                                                ),
       _TDL_Snode           (                                                ),
       _TDL_ConstraintsList (                                                ),
       childrenDlist        ( FALSE /*Do *NOT* delete the children objects*/ ),
       branchNumber         ( _TDL_TreeNodeBranch::TREE_NODE_BRANCH_COUNT ++ ),
       branchName           ( (const char *) NULL                            )
  {}

  _TDL_TreeNodeBranch ( const char * theBranchName )
    :  _TDL_TreeNode        (                                                ),
       _TDL_Snode           (                                                ),
       _TDL_ConstraintsList (                                                ),
       childrenDlist        ( FALSE /*Do *NOT* delete the children objects*/ ),
       branchNumber         ( _TDL_TreeNodeBranch::TREE_NODE_BRANCH_COUNT ++ ),
       branchName           ( NewString ( theBranchName )                    )
  {}

  virtual ~_TDL_TreeNodeBranch();

  u_int4   getBranchNumber() const { return branchNumber; }


  status_t        addChild ( _TDL_TreeNode * theChild )
		    { return getChildrenNonConst() . appendNode ( theChild ); }

  _TDL_TreeNode * removeChild ( _TDL_TreeNode * theChild )
		    { return (_TDL_TreeNode *)
			     getChildrenNonConst() . removeNode ( theChild ); }

  _TDL_TreeNode * getChildWithArrayIndex ( int4 theArrayIndex ) const;



       /*
	* This method returns the (TDL-defined) "PREVIOUS" node/branch.
	* (It is used to find the value of "PREVIOUS" for Constraints...)
	*
	* If theTreeNode is NULL, assume the new-child has yet to be added,
	*     and start searching for previous-child with the last-child.
	* If theTreeNode is NOT NULL, start searching for previous-child
	*     with the child BEFORE theTreeNode.
	* If theTreeNode is NOT a child, assume that it has yet to be added,
	*    and start searching for previous-child with the last-child.
	*
	* Previous-child, the returned _TDL_TreeNode, must be or contain
	* at least one _TDL_SpawnStatementTreeElement object.  That is to say,
	* empty _TDL_WithStatementTreeElement are skipped over.
	*/
  _TDL_TreeNode * getPreviousChild ( const _TDL_TreeNode * theTreeNode ) const;



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


	/**** This really should be overriden in subclasses. ****/
	/* Names are really useful for identifying (subclassed) *
	 * _TDL_TreeNode instances during debugging/testing...  */
  virtual /*mutable*/ const char * getName() const;


	/* Returns our children list. */
  virtual const _TDL_Dlist *  getChildren() const;
  

	/* Performs a depth-first search to discover whether or not this *
	 * object contains any _TDL_SpawnStatementTreeNode objects...    */
  virtual const _TDL_SpawnStatementTreeNode *
				      firstSpawnStatementTreeNodeConst() const;


	/* Common workhorse method to destroy _TDL_SpawnStatementData's *
         * referred to by this subtree.                                 */
  virtual status_t doDestroy ( BOOLEAN   theCanStillBeReferenced,
			       BOOLEAN   theDestroyIfUnused,
			       BOOLEAN & theFoundSpawnStatementData,
			       int4      theDepth                    );

	/* Lets allow safe-downcasts... (Basic RTTI) */
  virtual operator const _TDL_TreeNodeBranch * () const;


	/* iostreamBase interface. */
  virtual ostream & printObject ( ostream    & theOstream,
				  const char * theIndentString = "") const;

	/* Convenience method */
  ostream & printArrayIndexes ( ostream & theOstream ) const;



protected:
	/* Note:  getChildren()         returns a pointer.                *
         *        getChildrenNonConst() returns a reference to an object! */
  _TDL_Dlist & getChildrenNonConst() { return childrenDlist; }


	/* These operations are disabled (prohibited) & unimplemented */
private:
  _TDL_TreeNodeBranch (          const _TDL_TreeNodeBranch & theObjectToCopy );
  _TDL_TreeNodeBranch & operator = (
				 const _TDL_TreeNodeBranch & theObjectToCopy );
};

#endif /* _TDL_TreeNodeBranch_H */

