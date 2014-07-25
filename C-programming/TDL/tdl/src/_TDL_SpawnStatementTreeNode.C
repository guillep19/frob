/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

#include "_TDL_SpawnStatementTreeNode.H"

/*virtual*/
_TDL_SpawnStatementTreeNode::~_TDL_SpawnStatementTreeNode()
{ }

        /***************************/
	/* _TDL_TreeNode interface */
        /***************************/


	/* Allow automatic conversion to TCM_Task_Tree_Ref.
	 * (May also transparently allocate() that first
	 *  _TDL_SpawnStatementData, performing lazy allocation.)
	 */
/*virtual*/
_TDL_SpawnStatementTreeNode::operator const TCM_Task_Tree_Ref & ()
{
  return getSpawnStatementData() . operator const TCM_Task_Tree_Ref & ();
}


	/* Names are really useful for identifying (subclassed) *
	 * _TDL_TreeNode instances during debugging/testing...  */
/*virtual*/ const char *
_TDL_SpawnStatementTreeNode::getName() const
{
  return getSpawnStatement() . getName();
}

/*virtual*/ BOOLEAN
_TDL_SpawnStatementTreeNode::hasName ( const char * theName ) const
{
  return getSpawnStatement() . hasName ( theName );
}


	/* Performs a depth-first search to discover whether or not this *
	 * object contains any _TDL_SpawnStatementTreeNode objects...    */
/*virtual*/ const _TDL_SpawnStatementTreeNode *
_TDL_SpawnStatementTreeNode::firstSpawnStatementTreeNodeConst() const
{
  return this;
}


	/* Common workhorse method to destroy _TDL_SpawnStatementData's *
         * referred to by this subtree.                                 */
/*virtual*/ status_t
_TDL_SpawnStatementTreeNode::doDestroy ( BOOLEAN   theCanStillBeReferenced,
					 BOOLEAN   theDestroyIfUnused,
					 BOOLEAN & theFoundSpawnStatementData,
					 int4      theDepth                   )
{
	/* Assume that, if we reach this point, that we are doing something
	 * like destroy'ing a WITH-Statement subtree.  So just destroy our
	 * corresponding _TDL_SpawnStatementData object.
	 *
	 * Note: Proper destruction of SpawnStatement's should be done by
	 * invoking doDestroy() upon the corresponding _TDL_SpawnStatement,
	 * and providing an iteration-indexes argument.
	 */

  _TDL_MARKUSED ( theDepth );

	/* Note that we have been destroyed */
  setIsDestroyedSubtree();

  theFoundSpawnStatementData = TRUE;
  return getSpawnStatementData() . doDestroy ( theCanStillBeReferenced,
					       theDestroyIfUnused      );
}


/*virtual*/
_TDL_SpawnStatementTreeNode::operator const _TDL_SpawnStatementTreeNode * ()
									  const
{
  return this;
}


	/* iostreamBase interface */
/*virtual*/ ostream &
_TDL_SpawnStatementTreeNode::printObject (
				   ostream    & theOstream,
				   const char * theIndentString /*=""*/ ) const
{
  char  subIndentString [ iostreamBase::MAXIMUM_INDENT ];
  iostreamBase::createSubIndentString ( subIndentString, theIndentString );

  theOstream << theIndentString << "-Begin- _TDL_SpawnStatementTreeNode  "
	     << ((void *)this)  << endl;

  _TDL_TreeNode::printObject ( theOstream, theIndentString );

  theOstream << theIndentString << " Associated with:  " << endl;

  getSpawnStatementData() . printObject ( theOstream, subIndentString );

  theOstream << theIndentString << "--End-- _TDL_SpawnStatementTreeNode  "
	     << ((void *)this)  << endl;

  return theOstream;
}



	/* Convenience method */
ostream &
_TDL_SpawnStatementTreeNode::printArrayIndexes ( ostream & theOstream ) const
{
  const _TDL_SpawnStatementTreeNode * targetTreeNode;

	/* Top-of-Tree case. */
  if ( getArrayIndex() == _TDL_TreeNode::TOPMOST_ARRAY_INDEX )
  {
    theOstream << "[.]";
    return theOstream;
  }

	/* If we have an array index */
  if ( hasArrayIndex() == TRUE )
  {
    theOstream << "[";
    if ( getParent() != (_TDL_TreeNodeBranch *) NULL )
      getParent() -> printArrayIndexes ( theOstream );
    theOstream  << getArrayIndex() << "]";
    return theOstream;
  }


      /* Walk up the tree, applying pre-insert With-Do/Iteration constraints */
  for ( targetTreeNode  = (const _TDL_SpawnStatementTreeNode *)
			     ( getSpawnStatementData()
			         .  getSpawnStatementTreeNodeSlist()
			         .  getFirstNode() );
	targetTreeNode != (const _TDL_SpawnStatementTreeNode *) NULL;
	targetTreeNode  = (const _TDL_SpawnStatementTreeNode *)
			     ( targetTreeNode -> _TDL_Snode::getNextNode() )
       )
  {
    if ( targetTreeNode -> hasArrayIndex() == TRUE )
      return targetTreeNode -> printArrayIndexes ( theOstream );
  }

	/* IF nothing has an array index, we are obviously a singleton case. */
  return theOstream;
}

