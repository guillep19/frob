/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

#include "_TDL_WithStatementTreeNode.H"

/*virtual*/
_TDL_WithStatementTreeNode::~_TDL_WithStatementTreeNode()
{ }

        /***************************/
	/* _TDL_TreeNode interface */
        /***************************/


	/* Allow automatic conversion to TCM_Task_Tree_Ref.
	 * (May also transparently allocate() that first
	 *  _TDL_WithStatementData, performing lazy allocation.)
	 */
/*virtual*/
_TDL_WithStatementTreeNode::operator const TCM_Task_Tree_Ref & ()
{
  return getWithStatementData() . operator const TCM_Task_Tree_Ref & ();
}


	/* Names are really useful for identifying (subclassed) *
	 * _TDL_TreeNode instances during debugging/testing...  */
/*virtual*/ const char *
_TDL_WithStatementTreeNode::getName() const
{
  return getWithStatement() . getName();
}

/*virtual*/ BOOLEAN
_TDL_WithStatementTreeNode::hasName ( const char * theName ) const
{
  return getWithStatement() . hasName ( theName );
}


	/* Returns our corresponding children... */
/*virtual*/ const _TDL_Dlist *
_TDL_WithStatementTreeNode::getChildren() const
{
  return getWithStatementData() . getChildren();
}


	/* Performs a depth-first search to discover whether or not this *
	 * object contains any _TDL_SpawnStatementTreeNode objects...    */
/*virtual*/ const _TDL_SpawnStatementTreeNode *
_TDL_WithStatementTreeNode::firstSpawnStatementTreeNodeConst() const
{
  return getWithStatementData() . firstSpawnStatementTreeNodeConst();
}


	/* Common workhorse method to destroy _TDL_WithStatementData's *
         * referred to by this subtree.                                 */
/*virtual*/ status_t
_TDL_WithStatementTreeNode::doDestroy ( BOOLEAN   theCanStillBeReferenced,
					BOOLEAN   theDestroyIfUnused,
					BOOLEAN & theFoundWithStatementData,
					int4      theDepth                   )
{
	/* Assume that, if we reach this point, that we are doing something
	 * like destroy'ing a WITH-Statement subtree.  So just destroy our
	 * corresponding _TDL_WithStatementData object.
	 *
	 * Note: Proper destruction of WithStatement's should be done by
	 * invoking doDestroy() upon the corresponding _TDL_WithStatement,
	 * and providing an iteration-indexes argument.
	 */

	/* Note that we have been destroyed */
  setIsDestroyedSubtree();

  return getWithStatementData() . doDestroy ( theCanStillBeReferenced,
					      theDestroyIfUnused,
					      theFoundWithStatementData,
					      theDepth );
}


/*virtual*/
_TDL_WithStatementTreeNode::operator const _TDL_WithStatementTreeNode *() const
{
  return this;
}


	/* iostreamBase interface */
/*virtual*/ ostream &
_TDL_WithStatementTreeNode::printObject (
				   ostream    & theOstream,
				   const char * theIndentString /*=""*/ ) const
{
  char  subIndentString [ iostreamBase::MAXIMUM_INDENT ];
  iostreamBase::createSubIndentString ( subIndentString, theIndentString );

  theOstream << theIndentString << "-Begin- _TDL_WithStatementTreeNode  "
	     << ((void *)this)  << endl;

  _TDL_TreeNode::printObject ( theOstream, theIndentString );

  theOstream << theIndentString << " Associated with:  " << endl;

  getWithStatementData() . printObject ( theOstream, subIndentString );

  theOstream << theIndentString << "--End-- _TDL_WithStatementTreeNode  "
	     << ((void *)this)  << endl;

  return theOstream;
}

