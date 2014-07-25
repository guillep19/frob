/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

#include "_TDL_TreeNodeBranch.H"
#include "_TDL_SpawnStatementTreeNode.H"


/*static*/ u_int4 _TDL_TreeNodeBranch::TREE_NODE_BRANCH_COUNT = 0;


/*virtual*/
_TDL_TreeNodeBranch::~_TDL_TreeNodeBranch()
{
	/* Lets clean up after ourselves. */
  if ( branchName != (const char *)NULL )
  {
    delete [] (char *)branchName;
    branchName = (const char *)NULL;
  }
}


_TDL_TreeNode *
_TDL_TreeNodeBranch::getChildWithArrayIndex ( int4 theArrayIndex ) const
{
  _TDL_Dnode * child;

  for ( child  = getChildren() -> getFirstNode();
	child != (_TDL_Dnode *) NULL;
	child  = child -> getNextNode() )
  {
    if ( theArrayIndex == ((_TDL_TreeNode *) child) -> getArrayIndex() )
      return (_TDL_TreeNode *) child;
  }

  return (_TDL_TreeNode *) NULL;
}



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
_TDL_TreeNode *
_TDL_TreeNodeBranch::getPreviousChild (const _TDL_TreeNode * theTreeNode) const
{
  _TDL_Dnode * child;

    /* If theTreeNode is NULL, start previous-child search at last-child */
  if ( theTreeNode == (_TDL_TreeNode *) NULL )
  {
    child = getChildren() -> getLastNode();
  }
  else
  {
	/* If theTreeNode is a child... */
    if ( getChildren() -> contains(theTreeNode, _TDL_Dlist::BACKWARD) == TRUE )
    {
	  /* Start searching at the node before the theTreeNode child */
      child = theTreeNode -> getPreviousNode();
    }
    else  /* If theTreeNode is ***NOT*** a child... */
    {
	/* Assume that theTreeNode has not yet been inserted...       */
	/* And start searching for the previous-child at the last-child. */
      child  = getChildren() -> getLastNode();

      if ( TDL::getIsReporting ( TDL::TDL_DEBUG | TDL::VERBOSE ) )
      {
	TDL::getLogStream()
	  << "[_TDL_TreeNodeBranch::getPreviousChild]  Warning:  "
	  << "theTreeNode is not a child.  Assuming lastchild."
	  << endl;
      }
    }
  }


	/* Find the previous child */
  for ( ;
	child != (_TDL_Dnode *) NULL;
	child  = child -> getPreviousNode()
       )
  {
    if ( ((_TDL_TreeNode *) child) -> containsSpawnStatementTreeNode() == TRUE)
    {
      if ( TDL::getIsReporting ( TDL::TDL_DEBUG | TDL::VERBOSE ) )
      {
	TDL::getLogStream()
	  << "[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  "
	  << "Returning Previous of:  \""
	  << ((_TDL_TreeNode *) child) -> getName() << "\""
	  << endl;
      }
      return (_TDL_TreeNode *) child;
    }
  } /* FOR ( ; child != NULL;  child = child -> getPreviousNode() ) */


  if ( TDL::getIsReporting ( TDL::TDL_DEBUG | TDL::VERBOSE ) )
  {
    TDL::getLogStream()
      << "[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  "
      << "Returning Previous of:  \"NULL\""
      << endl;
  }
  return (_TDL_TreeNode *) NULL;
}



        /******************************/
	/* _TDL_TreeNode interface */
        /******************************/

/* Allow automatic conversion to TCM_Task_Tree_Ref for the
 * first _TDL_SpawnStatementTreeNode that we contain.
 * (May also transparently allocate() that first
 *  _TDL_SpawnStatementData, performing lazy allocation.)
 */
/*virtual*/
_TDL_TreeNodeBranch::operator const TCM_Task_Tree_Ref & ()
{
  _TDL_SpawnStatementTreeNode *  firstSpawnTreeNode
				   = firstSpawnStatementTreeNode();

  if ( firstSpawnTreeNode != (_TDL_SpawnStatementTreeNode *) NULL )
    return firstSpawnTreeNode -> operator const TCM_Task_Tree_Ref &();
  else
    return _TDL_TreeNode::operator const TCM_Task_Tree_Ref & ();
}


	/* This really should be overriden in subclasses. */
/*virtual*/ /*mutable*/ const char *
_TDL_TreeNodeBranch::getName() const
{
	/* OVERRIDE const AND perform lazy allocation of the name here,     *
	 * as most subclasses will override (and never invoke) this method. */
  if ( branchName == (const char *)NULL )
  {
    char newBranchName [ 100 ];  /* This should be much more than enough... */
    snprintf ( newBranchName, sizeof(newBranchName)-1,
	       "Unnamed _TDL_TreeNodeBranch #%lu", getBranchNumber() );
    ((_TDL_TreeNodeBranch *)this) -> branchName = NewString ( newBranchName );
  }
  return branchName;
}


/*virtual*/ const _TDL_Dlist *
_TDL_TreeNodeBranch::getChildren() const
{
  return & childrenDlist;
}


/*virtual*/ const _TDL_SpawnStatementTreeNode *
_TDL_TreeNodeBranch::firstSpawnStatementTreeNodeConst() const
{
  const _TDL_SpawnStatementTreeNode *  firstSpawnStatementTreeNode;
  const _TDL_Dnode                  *  child;

  for ( child  = getChildren() -> getFirstNode();
	child != (_TDL_Dnode *) NULL;
	child  = child -> getNextNode()
       )
  {
    firstSpawnStatementTreeNode
      = ((const _TDL_TreeNode *) child) -> firstSpawnStatementTreeNodeConst();

    if (    firstSpawnStatementTreeNode
	 != ((const _TDL_SpawnStatementTreeNode *) NULL) )
      return firstSpawnStatementTreeNode;

  } /* FOR ( child in getChildren() ) */

  return _TDL_TreeNode::firstSpawnStatementTreeNodeConst();
}


/*virtual*/ status_t
_TDL_TreeNodeBranch::doDestroy ( BOOLEAN   theCanStillBeReferenced,
				 BOOLEAN   theDestroyIfUnused,
				 BOOLEAN & theFoundSpawnStatementData,
				 int4      theDepth                    )
{
  status_t     returnValue = SUCCESS;
  _TDL_Dnode * child;

	/* Note that we have been destroyed */
  setIsDestroyedSubtree();

  for ( child  = getChildren() -> getFirstNode();
	child != (_TDL_Dnode *) NULL;
	child  = child -> getNextNode()
       )
  {
    if ( ((_TDL_TreeNode *) child) -> doDestroy ( theCanStillBeReferenced,
						  theDestroyIfUnused,
						  theFoundSpawnStatementData,
						  (theDepth + 1)              )
	 != SUCCESS )
    {
      returnValue = FAILURE;
    }
  } /* FOR ( child = getChildren() -> getFirstNode(); child != NULL; ... ) */

  if (   ( theFoundSpawnStatementData == FALSE )
      && ( theDepth                   == 0     ) )
  {
    TDL::getLogStream()
      << "[_TDL_TreeNodeBranch(\"" << getName() << "\"):doDestroy]  Warning:  "
      << "Nothing found to destroy..." << endl;
  }

  return returnValue;
}



	/* Lets allow safe-downcasts... (Basic RTTI) */
/*virtual*/
_TDL_TreeNodeBranch::operator const _TDL_TreeNodeBranch * () const
{
  return this;
}



	/* iostreamBase interface */
/*virtual*/ ostream &
_TDL_TreeNodeBranch::printObject ( ostream    & theOstream,
				   const char * theIndentString /*=""*/ ) const
{
  char  subIndentString [ iostreamBase::MAXIMUM_INDENT ];
  iostreamBase::createSubIndentString ( subIndentString, theIndentString );

  theOstream << theIndentString << "-Begin- _TDL_TreeNodeBranch: "
	     << ((void *)this)  << endl;

  _TDL_TreeNode::printObject ( theOstream, theIndentString );

  theOstream << theIndentString   << " branchNumber: "
	     << getBranchNumber() << endl;

  _TDL_ConstraintsList::printObject ( theOstream, theIndentString );

  theOstream << theIndentString << " children:" << endl;

  getChildren() -> printObject ( theOstream, subIndentString );

  theOstream << theIndentString << "--End-- _TDL_TreeNodeBranch: "
	     << ((void *)this)  << endl;

  return theOstream;
}



	/* Convenience method */
ostream &
_TDL_TreeNodeBranch::printArrayIndexes ( ostream & theOstream ) const
{
  if ( getParent() != (_TDL_TreeNodeBranch *) NULL )
    getParent() -> printArrayIndexes( theOstream );
  if ( getArrayIndex() == _TDL_TreeNode::TOPMOST_ARRAY_INDEX )
    theOstream << ". ";    
  else
    theOstream << getArrayIndex() << " ";
  return theOstream;
}



