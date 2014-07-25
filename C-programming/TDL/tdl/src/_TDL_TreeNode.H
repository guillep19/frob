/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

#ifndef _TDL_TreeNode_H
#define _TDL_TreeNode_H

#define _TDL_INTERNAL_
#include "tdl.H"
#include "_TDL_Dnode.H"
#include "_TDL_Dlist.H"


/*
 * _TDL_TreeNode is an abstract base class for elements of the TDL-CODE-TREE.
 * (Not to be confused with the TCM Task Tree.)
 *
 * It is used to represent and maintain the nested list formed by SPAWN
 * and WITH Statements.
 *
 * It consists a tree-structure implemented with pointers.  The children
 * of a particular node are stored as a doublely linked list.
 */

class _TDL_TreeNodeBranch;
class _TDL_SpawnStatementTreeNode;
class _TDL_WithStatementTreeNode;
class _TDL_WithStatementData;


class _TDL_TreeNode : public _TDL_Dnode
{
public:
  enum CONSTANTS
  {
    MINIMUM_ARRAY_INDEX = -9000,  /* Note '>' test in hasArrayIndex()*/
    TOPMOST_ARRAY_INDEX = -9001,  /* Note '>' test in hasArrayIndex()*/
    NO_ARRAY_INDEX      = -9002,  /* Note '>' test in hasArrayIndex()*/
    ERROR_ARRAY_INDEX   = -9003   /* Note '>' test in hasArrayIndex()*/
  };

public:
  static const TCM_Task_Tree_Ref  EMPTY_TCM_TASK_TREE_REF;

protected:
  _TDL_TreeNodeBranch  *  parentTreeNodeBranch;
  int4                    arrayIndex;
  BOOLEAN                 isDestroyedSubtree; /* Cached data for subclasses */

public:
  _TDL_TreeNode ( )
    : _TDL_Dnode           (                               ),
      parentTreeNodeBranch ( (_TDL_TreeNodeBranch *) NULL  ),
      arrayIndex           ( _TDL_TreeNode::NO_ARRAY_INDEX ),
      isDestroyedSubtree   ( FALSE                         )
  {}

  virtual ~_TDL_TreeNode();

  _TDL_TreeNodeBranch * getParent () const { return parentTreeNodeBranch; }

  void setParent ( _TDL_TreeNodeBranch * theParentTreeNodeBranch )
			    { parentTreeNodeBranch = theParentTreeNodeBranch; }


  BOOLEAN hasArrayIndex() const { return arrayIndex > NO_ARRAY_INDEX; }
  int4    getArrayIndex() const { return arrayIndex; }
  void    setArrayIndex( int4 theArrayIndex ) { arrayIndex = theArrayIndex; }

  BOOLEAN getIsDestroyedSubtree() const { return isDestroyedSubtree; }
  void    setIsDestroyedSubtree()       { isDestroyedSubtree = TRUE; }


  _TDL_TreeNode * getPreviousSibling() const
		 { return (_TDL_TreeNode *) _TDL_Dnode::getPreviousNode(); }
  _TDL_TreeNode * getLeftSibling()     const
		 { return (_TDL_TreeNode *) _TDL_Dnode::getPreviousNode(); }

  _TDL_TreeNode * getNextSibling()     const
		 { return (_TDL_TreeNode *) _TDL_Dnode::getNextNode(); }
  _TDL_TreeNode * getRightSibling()    const
		 { return (_TDL_TreeNode *) _TDL_Dnode::getNextNode(); }


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
  virtual const char * getName() const TCM_PURE_VIRTUAL_METHOD ;
  virtual BOOLEAN      hasName ( const char * theName ) const;


	/* Returns our nonexistent children...  Overriden in subclass. */
  virtual const _TDL_Dlist * getChildren() const;
  BOOLEAN hasChildren() const
			 { return getChildren() != (const _TDL_Dlist *) NULL; }



	/* Performs a depth-first search to discover whether or not this *
	 * object contains any _TDL_SpawnStatementTreeNode objects...    */
  virtual const _TDL_SpawnStatementTreeNode *
				      firstSpawnStatementTreeNodeConst() const;

	/* Defaults to over-riding const'ness on whatever *
         * firstSpawnStatementTreeNodeConst returns.      */
  virtual _TDL_SpawnStatementTreeNode * firstSpawnStatementTreeNode();

  BOOLEAN containsSpawnStatementTreeNode() const
		      { return    firstSpawnStatementTreeNodeConst()
			       != (const _TDL_SpawnStatementTreeNode *) NULL; }



	/* Destroys all _TDL_SpawnStatementData's referred to *
         * by this subtree                                    */
  status_t destroy ( BOOLEAN theCanStillBeReferenced = FALSE )
		       { BOOLEAN  foundSpawnStatementData = FALSE;
			 return doDestroy ( theCanStillBeReferenced, FALSE,
					    foundSpawnStatementData, 0     ); }

	/* Destroys all unused _TDL_SpawnStatementData's referred to *
         * by this subtree.                                          */
  status_t destroyIfUnused ( BOOLEAN theCanStillBeReferenced = FALSE )
		       { BOOLEAN  foundSpawnStatementData = FALSE;
		         return doDestroy ( theCanStillBeReferenced, TRUE,
					    foundSpawnStatementData, 0     ); }

	/* Common workhorse method to destroy _TDL_SpawnStatementData's *
         * referred to by this subtree.                                 */
  virtual status_t doDestroy (
	      BOOLEAN   theCanStillBeReferenced,
	      BOOLEAN   theDestroyIfUnused,
	      BOOLEAN & theFoundSpawnStatementData,
	      int4      theDepth                    ) TCM_PURE_VIRTUAL_METHOD ;



	/* Lets allow safe-downcasts... (Basic RTTI) */
  virtual operator const _TDL_TreeNodeBranch         * () const;
  virtual operator const _TDL_SpawnStatementTreeNode * () const;
  virtual operator const _TDL_WithStatementTreeNode  * () const;
  virtual operator const _TDL_WithStatementData      * () const;

	/* These versions of safe-downcasting might be clearer... */
  const _TDL_TreeNodeBranch         * getTreeNodeBranchConst()         const
		{ return (const _TDL_TreeNodeBranch         *) (*this); }
  const _TDL_SpawnStatementTreeNode * getSpawnStatementTreeNodeConst() const
		{ return (const _TDL_SpawnStatementTreeNode *) (*this); }
  const _TDL_WithStatementTreeNode  * getWithStatementTreeNodeConst()  const
		{ return (const _TDL_WithStatementTreeNode  *) (*this); }
  const _TDL_WithStatementData      * getWithStatementDataConst()      const
		{ return (const _TDL_WithStatementData      *) (*this); }

	/* And non-const versions...  (These default to just casting away *
         * const'ness after calling the const versions.)                  */
  virtual operator _TDL_TreeNodeBranch         * ();
  virtual operator _TDL_SpawnStatementTreeNode * ();
  virtual operator _TDL_WithStatementTreeNode  * ();
  virtual operator _TDL_WithStatementData      * ();

	/* These versions of safe-downcasting might be clearer... */
  _TDL_TreeNodeBranch         * getTreeNodeBranch()
			 { return (_TDL_TreeNodeBranch         *) (*this); }
  _TDL_SpawnStatementTreeNode * getSpawnStatementTreeNode()
			 { return (_TDL_SpawnStatementTreeNode *) (*this); }
  _TDL_WithStatementTreeNode  * getWithStatementTreeNode()
			 { return (_TDL_WithStatementTreeNode  *) (*this); }
  _TDL_WithStatementData      * getWithStatementData()
			 { return (_TDL_WithStatementData      *) (*this); }


	/* Convenience tests... */
  BOOLEAN isTreeNodeBranch() const
    { return    getTreeNodeBranchConst()
	     != (const _TDL_TreeNodeBranch         *) NULL ? TRUE : FALSE; }

  BOOLEAN isSpawnStatementTreeNode() const 
    { return    getSpawnStatementTreeNodeConst()
	     != (const _TDL_SpawnStatementTreeNode *) NULL ? TRUE : FALSE; }

  BOOLEAN isWithStatementTreeNode()  const 
    { return    getWithStatementTreeNodeConst()
	     != (const _TDL_WithStatementTreeNode  *) NULL ? TRUE : FALSE; }

  BOOLEAN isWithStatementData()  const 
    { return    getWithStatementDataConst()
	     != (const _TDL_WithStatementData      *) NULL ? TRUE : FALSE; }



	/* Abstract method.  iostreamBase interface. *
	 * Inherited from _TDL_Dnode.                */
  virtual ostream & printObject ( ostream    & theOstream,
				  const char * theIndentString = "") const;


	/* These operations are disabled (prohibited) & unimplemented */
private:
  _TDL_TreeNode (              const _TDL_TreeNode & theObjectToCopy );
  _TDL_TreeNode & operator = ( const _TDL_TreeNode & theObjectToCopy );
};

#endif /* _TDL_TreeNode_H */

