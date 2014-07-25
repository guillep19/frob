/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

#include "_TDL_TreeNode.H"
#include "_TDL_TreeNodeBranch.H"


/*static*/
const TCM_Task_Tree_Ref  _TDL_TreeNode::EMPTY_TCM_TASK_TREE_REF;



/*virtual*/
_TDL_TreeNode::~_TDL_TreeNode()
{
  setParent ( (_TDL_TreeNodeBranch *) NULL );
}


        /* Allow automatic conversion to TCM_Task_Tree_Ref.
	 *
	 * (For the first _TDL_SpawnStatementTreeNode that we contain.)
	 * (May also transparently allocate() that first
	 *  _TDL_SpawnStatementData, performing lazy allocation.)
	 *
         * In the event that we contain no _TDL_SpawnStatementTreeNode's,
         * we return the [empty] _TDL_TreeNode::EMPTY_TCM_TASK_TREE_REF.
         */
/*virtual*/
_TDL_TreeNode::operator const TCM_Task_Tree_Ref & ()
{
  return _TDL_TreeNode::EMPTY_TCM_TASK_TREE_REF;
}


	/* Names are really useful for identifying (subclassed) *
	 * _TDL_TreeNode instances during debugging/testing...  */
/*virtual*/ BOOLEAN
_TDL_TreeNode::hasName ( const char * theName ) const
{
  return StringEqual ( getName(), theName );
}



	/* Returns our nonexistent children...  Overriden in subclass. */
/*virtual*/ const _TDL_Dlist *
_TDL_TreeNode::getChildren() const
{
  return (const _TDL_Dlist *) NULL;
}

	/* Performs a depth-first search to discover whether or not this *
	 * object contains any _TDL_SpawnStatementTreeNode objects...    */
/*virtual*/ const _TDL_SpawnStatementTreeNode *
_TDL_TreeNode::firstSpawnStatementTreeNodeConst() const
{
  return (const _TDL_SpawnStatementTreeNode *) NULL;
}

/*virtual*/ _TDL_SpawnStatementTreeNode *
_TDL_TreeNode::firstSpawnStatementTreeNode()
{
  return (_TDL_SpawnStatementTreeNode *) firstSpawnStatementTreeNodeConst();
}



	/* Lets allow safe-downcasts... (Basic RTTI) */
/*virtual*/
_TDL_TreeNode::operator const _TDL_TreeNodeBranch * () const
		      { return (const _TDL_TreeNodeBranch *) NULL; }
/*virtual*/
_TDL_TreeNode::operator const _TDL_SpawnStatementTreeNode * () const
		      { return (const _TDL_SpawnStatementTreeNode *) NULL; }
/*virtual*/
_TDL_TreeNode::operator const _TDL_WithStatementTreeNode  * () const
		      { return (const _TDL_WithStatementTreeNode  *) NULL; }
/*virtual*/
_TDL_TreeNode::operator const _TDL_WithStatementData      * () const
		      { return (const _TDL_WithStatementData      *) NULL; }


	/* And non-const versions...  (These default to just casting away *
         * const'ness after calling the const versions.)                  */
/*virtual*/
_TDL_TreeNode::operator _TDL_TreeNodeBranch * ()
{
  return (_TDL_TreeNodeBranch *) getTreeNodeBranchConst();
}

/*virtual*/
_TDL_TreeNode::operator _TDL_SpawnStatementTreeNode * ()
{
  return (_TDL_SpawnStatementTreeNode *) getSpawnStatementTreeNodeConst();
}

/*virtual*/
_TDL_TreeNode::operator _TDL_WithStatementTreeNode * ()
{
  return (_TDL_WithStatementTreeNode *) getWithStatementTreeNodeConst();
}

/*virtual*/
_TDL_TreeNode::operator _TDL_WithStatementData * ()
{
  return (_TDL_WithStatementData *) getWithStatementDataConst();
}



	/* iostreamBase interface */
/*virtual*/ ostream &
_TDL_TreeNode::printObject ( ostream    & theOstream,
			     const char * theIndentString /*=""*/ ) const
{
  theOstream << theIndentString << " arrayIndex..: " << getArrayIndex() << endl
	     << theIndentString << " name........: \""
	     << getName() << "\"" << endl
	     << theIndentString << " parent......: " << ((void *) getParent());

  if ( getParent() != (_TDL_TreeNodeBranch *) NULL )
    theOstream << "  ( " << getParent() -> getName() << " )";

  theOstream << endl;

  return theOstream;
}


