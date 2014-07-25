/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

#include "_TDL_TDLStatement.H"
#include "_TDL_SpawnStatementTreeNode.H"
#include "_TDL_WithStatementTreeNode.H"
#include "_TDL_SpawnStatementData.H"
#include "_TDL_HandleManager.H"

/*virtual*/
_TDL_TDLStatement::~_TDL_TDLStatement()
{
	/* In case there's still a pointer to this object after it's deleted.*/
  topmostTreeNode = (_TDL_TreeNode *) NULL;
  cachedTreeNode  = (_TDL_TreeNode *) NULL;
}


_TDL_TDLStatement *
_TDL_TDLStatement::getDefaultWithParent_AsTdlStatement() const
{
  if (   ( getOurHandleManager()  == (_TDL_HandleManager *) NULL )
      || ( getDefaultWithParent() == (const char *) NULL         ) )
    return (_TDL_TDLStatement *) NULL;

  return getOurHandleManager() -> findTDLStatement ( getDefaultWithParent() );
}


 /* When extracting the default-with-parent, what we really want is
  * the _TDL_TreeNode inside the WITH-TDLStatement's iteration tree that
  * corresponds to the same set of iteration indexes as a particular
  * _TDL_SpawnStatementData instance located inside our
  * (this _TDL_TDLStatement object's) iteration tree.
  *
  * We do *NOT* want to create new nodes...  If the WITH-side tree
  * doesn't extend far enough yet, we want to only go down as far as we
  * can go along the iteration indexes determined by the
  * _TDL_SpawnStatementData instance.
  *
  * This also implies that we can use the _TDL_SpawnStatementData's
  * iteration indexes on all the WITH statements going all the way up
  * the default-with-tree, including on statements that will never
  * have as many iteration indexes as it has.
  */
const _TDL_TreeNode *
_TDL_TDLStatement::getDefaultWithParent_AsTreeNode (
		  const _TDL_TDLStatement       * theWithTdlStatement,
		  const _TDL_SpawnStatementData * theSpawnStatementData ) const
{
  const _TDL_SpawnStatementTreeNode * bottommostSpawnTreeNode;
  const _TDL_TreeNode               * spawnFirstParent;
  const _TDL_TreeNode               * spawnSecondParent;
  const _TDL_TreeNode               * withTreeNode;
  const _TDL_TreeNode               * oldWithTreeNode;


	/* Idiocy checks... */
  if ( theWithTdlStatement == (_TDL_TDLStatement *) NULL )
  {
    TDL::getLogStream()
      << "[_TDL_TDLStatement:getDefaultWithParent_AsWithStatementTreeNode()]  "
      << "Warning:  No value provided for theWithTdlStatement" << endl;
    return (_TDL_TreeNode *) NULL;
  }

  if ( getTopmostTreeNode() == ( _TDL_TreeNode *) NULL )
  {
    TDL::getLogStream()
      << "[_TDL_TDLStatement:getDefaultWithParent_AsWithStatementTreeNode()]  "
      << "Warning:  TopmostTreeNode of spawn is NULL.  Very weird." << endl;
    return (_TDL_TreeNode *) NULL;
  }
    

  withTreeNode = theWithTdlStatement -> getTopmostTreeNode();

	/* Make sure we're not flogging a dead horse. */
  if ( withTreeNode == (const _TDL_TreeNode *) NULL )
  {
    return (_TDL_TreeNode *) NULL;
  }



	/* One of the _TDL_SpawnStatementData's _TDL_SpawnStatementTreeNodes
	 * should be in our tree. */
  spawnFirstParent = (const _TDL_TreeNodeBranch *) NULL;

  for ( bottommostSpawnTreeNode = (const _TDL_SpawnStatementTreeNode *)
			          ( theSpawnStatementData
				      -> getSpawnStatementTreeNodeSlist()
				      .  getFirstNode() )
	  ;
	bottommostSpawnTreeNode != (const _TDL_SpawnStatementTreeNode *) NULL
	  ;
	bottommostSpawnTreeNode  = (const _TDL_SpawnStatementTreeNode *)
			           ( bottommostSpawnTreeNode
				       -> _TDL_Snode::getNextNode() )
       )
  {
    for ( spawnFirstParent  = bottommostSpawnTreeNode;
	  spawnFirstParent != (const _TDL_TreeNode *) NULL;
	  spawnFirstParent  = spawnFirstParent -> getParent() )
    {
      if ( spawnFirstParent == getTopmostTreeNode() )
	break;
    }

    if ( spawnFirstParent == getTopmostTreeNode() )
      break;
  }

	/* Make sure we found it. */
  if ( spawnFirstParent != getTopmostTreeNode() )
  {
    TDL::getLogStream()
      << "[_TDL_TDLStatement:getDefaultWithParent_AsWithStatementTreeNode()]  "
      << "Warning:  spawn not found in TDL-tree.  Very strange." << endl;
    return (_TDL_TreeNode *) NULL;
  }

	/* Fencepost singleton case */
  if ( bottommostSpawnTreeNode == (const _TDL_TreeNode *)getTopmostTreeNode() )
  {
	/* Return corresponding with-statement treenode. */
    return theWithTdlStatement -> getTopmostTreeNode();
  }


	/* Current Status:
	 * bottommostSpawnTreeNode == _TDL_SpawnStatementTreeNode in our
	 *                            (this object's) iteration tree.
	 * spawnFirstParent        == getTopmostTreeNode()
	 */

  
	/* This is amazingly inefficient -- an O(N^2) algorithm.
	 * However, we are unlikely to have a large number of nested WITH
	 * statements, and a new/delete operation would probably cost more.
	 */
  spawnSecondParent  = spawnFirstParent;

  do
  {
    spawnFirstParent = spawnSecondParent;

    for( spawnSecondParent                 = bottommostSpawnTreeNode;
	 spawnSecondParent -> getParent() != spawnFirstParent;
	 spawnSecondParent                 = spawnSecondParent -> getParent() )
	; /**** EMPTY FOR LOOP BODY ****/
    

	/* Check for "problems" */
    if ( withTreeNode -> isTreeNodeBranch() != TRUE )
    {
      if ( withTreeNode -> isWithStatementTreeNode() != TRUE )
      {
	TDL::getLogStream()
	  << "[_TDL_TDLStatement:"
	  << "getDefaultWithParent_AsWithStatementTreeNode()]  Warning:  "
	  << "Unexpected:  withTreeNode is neither TreeNodeBranch nor "
	  << "WithStatementTreeNode.  Extremely strange." << endl;
      }
	/* If we don't go far enough yet, return whatever we have so far. */
      return withTreeNode;
    }
    
    oldWithTreeNode = withTreeNode;

    withTreeNode
      = withTreeNode -> getTreeNodeBranchConst()
		     -> getChildWithArrayIndex ( spawnSecondParent
						   -> getArrayIndex() );

    if ( withTreeNode == (const _TDL_TreeNode *) NULL )
    {
	/* If we don't go far enough yet, return whatever we have so far. */
      return oldWithTreeNode;
    }

  } while ( spawnSecondParent != bottommostSpawnTreeNode );


  return withTreeNode;
  
} /* const _TDL_TreeNode *                                    *
   * getDefaultWithParent_AsWithStatementTreeNode (...) const */





_TDL_TreeNode *
_TDL_TDLStatement::getTreeNode ( const _TDL_Dlist & theIterationIndexes,
				 BOOLEAN            theCanCreateNewNodes,
				 BOOLEAN            theLastNodeIsABranch
								/* = FALSE */ )
{
  _TDL_Dnode * currentIndexDnode;

  resetCachedLookup();

	/* Fencepost error:  Deal with degenerate no-index creation case */
  if (   ( theIterationIndexes . getFirstNode() == (_TDL_Dnode    *) NULL )
      && ( getTopmostTreeNode()                 == (_TDL_TreeNode *) NULL )
      && ( theCanCreateNewNodes                 == TRUE                   ) )
  {
    cacheDescendInto ( _TDL_TreeNode::NO_ARRAY_INDEX, FALSE, TRUE );
  }

	/* Iterate through our indexes, incrementally descending the tree */
  for ( currentIndexDnode  = theIterationIndexes . getFirstNode();
	currentIndexDnode != (_TDL_Dnode *) NULL;
	currentIndexDnode  = currentIndexDnode -> getNextNode() )
  {
    cacheDescendInto (
      ((_TDL_IterationIndex *) currentIndexDnode) -> getCurrentIndex(),
      (   (   ( theLastNodeIsABranch               == TRUE                )
	   || ( currentIndexDnode -> getNextNode() != (_TDL_Dnode *) NULL ) )
	? TRUE : FALSE ),
      theCanCreateNewNodes );

    if ( hasCachedTreeNode() == FALSE )
    {
      if ( theCanCreateNewNodes == TRUE )
      {
	TDL::getLogStream()
	  << "[_TDL_TDLStatement:getTreeNode(";
	_TDL_IterationIndex::printIndexList ( TDL::getLogStream(),
					      theIterationIndexes );
	TDL::getLogStream()
	  << ( (theCanCreateNewNodes == TRUE) ? ",NEW-NODES" : ",NO-new-nodes")
	  << "):\"" << getName() << "\"]  "
	  << "Error:  Unable to access (create) TreeNode." << endl;
      } /* if ( theCanCreateNewNodes == TRUE ) */

      return (_TDL_TreeNode *) NULL;
    } /* if ( hasCachedTreeNode() == FALSE ) */
  } /* FOR ( currentIndexDnode IN theIterationIndexes ) */

  return getCachedTreeNodeNonConst();
}


	/* Note: Non-Const methods will create the node if necessary */
	/*           Const methods will merely return NULL!          */
_TDL_Snode *
_TDL_TDLStatement::getStatementData ( const _TDL_Dlist & theIterationIndexes )
{
  getTreeNode ( theIterationIndexes );
  return getStatementDataConst ( theIterationIndexes );
}


	/* Note: Non-Const methods will create the node if necessary */
	/*           Const methods will merely return NULL!          */
_TDL_Snode *
_TDL_TDLStatement::getStatementDataConst (
				       const _TDL_Dlist & theIterationIndexes )
{
  _TDL_TreeNode * treeNode = getTreeNodeConst ( theIterationIndexes );

  if ( treeNode == (_TDL_TreeNode *) NULL )
  {
    TDL::getLogStream()
      << "[_TDL_TDLStatement:getStatementDataConst(";
    _TDL_IterationIndex::printIndexList ( TDL::getLogStream(),
					  theIterationIndexes );
    TDL::getLogStream()
      << "):\"" << getName() << "\"]  "
      << "Error:  Unable to access StatementData." << endl;

    return (_TDL_Snode *) NULL;
  }

  if ( treeNode -> isSpawnStatementTreeNode() == TRUE )
  {
    return & ( treeNode -> getSpawnStatementTreeNode()
			-> getSpawnStatementData() );
  }

  else if ( treeNode -> isWithStatementTreeNode() == TRUE )
  {
    return & ( treeNode -> getWithStatementTreeNode()
			-> getWithStatementData() );
  }

  else if ( treeNode -> isWithStatementData() == TRUE )
  {
    return treeNode -> getWithStatementData();
  }


  TDL::getLogStream()
    << "[_TDL_TDLStatement:getStatementDataConst(";
  _TDL_IterationIndex::printIndexList ( TDL::getLogStream(),
					theIterationIndexes );
  TDL::getLogStream()
    << "):\"" << getName() << "\"]  "
    << "Error:  This Tree-Node has no associated data:" << endl;
  treeNode -> printObject ( TDL::getLogStream(), "     " );

  return (_TDL_Snode *) NULL;
}



	/* Incremental cached-based lookup method. */
_TDL_TreeNode *
_TDL_TDLStatement::getCachedTopmostTreeNodeBranch (
			     BOOLEAN theShouldBeATreeNodeBranch /* = FALSE */ )
{
  _TDL_TreeNode * treeNode;

  if ( getTopmostTreeNode() != (_TDL_TreeNode *) NULL )
  {
    if (   ( theShouldBeATreeNodeBranch                 == TRUE  )
	&& ( getTopmostTreeNode() -> isTreeNodeBranch() == FALSE ) )
    {
      TDL::getLogStream()
	<< "[_TDL_TDLStatement:getCachedTopmostTreeNodeBranch():\""
	<< getName() << "\"]  "
	<< "Warning:  Topmost-TreeNode is not a TreeNodeBranch.  It's a:"
	<< endl;
      getTopmostTreeNode() -> printObject ( TDL::getLogStream(), "     " )
	<< endl;
    }

    return getTopmostTreeNode();
  }
  else
  {
    treeNode = new _TDL_TreeNodeBranch();
    addObjectToDelete  ( (_TDL_TreeNodeBranch *) treeNode );
    treeNode -> setParent ( (_TDL_TreeNodeBranch *) NULL );
    treeNode -> setArrayIndex ( _TDL_TreeNode::TOPMOST_ARRAY_INDEX );
    setTopmostTreeNode ( treeNode );
    return treeNode;
  }
}


	/* Incremental cached-based lookup method. */
void
_TDL_TDLStatement::cacheDescendInto ( int4    theChildIndex,
				      BOOLEAN theIsDescendingIntoBranch,
				      BOOLEAN theCanCreateNewNodes )
{
  _TDL_Snode    * newStatementData = (_TDL_Snode    *) NULL;
  _TDL_TreeNode * newTreeNode      = (_TDL_TreeNode *) NULL;
  _TDL_TreeNode * oldTreeNode      = getCachedTreeNodeNonConst();
  const char    * errorMessage     = (const char    *) NULL;


  if ( getTopmostTreeNode() == (_TDL_TreeNode *) NULL )
  {
    setCachedTreeNode ( (_TDL_TreeNode *) NULL );
  }

	/* Ie: getCachedTreeNode() == NULL */
  else if ( hasCachedTreeNode() == FALSE )
  {
	/* We've hit a bug. */
    TDL::getLogStream()
      << "[_TDL_TDLStatement:cacheDescendInto("
      << theChildIndex << "," 
      << ( (theIsDescendingIntoBranch == TRUE) ? "BRANCH,"   : "NODE," )
      << ( (theCanCreateNewNodes      == TRUE) ? "NEW-NODES" : "NO-new-nodes" )
      << "):\"" << getName() << "\"] Error:  "
      << "cachedTreeNode is NULL." << endl;
    return; /* Abort further action.  Do not try to create this node. */
  }

	/* If doDestroy() has been invoked, there will be a terminating *
	 * (destroyed) leaf-node created.  Don't descend past it.       */
  else if (   (     getCachedTreeNode() -> isTreeNodeBranch()      == FALSE   )
	   && (   ( getCachedTreeNode() -> getIsDestroyedSubtree() == TRUE  )
	       || (   ( getCachedTreeNode()
				     -> isSpawnStatementTreeNode() == TRUE )
		   && ( getCachedTreeNode()
				     -> getSpawnStatementTreeNodeConst()
				     -> isDestroyed()              == TRUE )) )
	   )
  {
    return; /* Abort further action.  Do not try to create this node. */
  }

	/* Singleton case. */
  else if (   ( theChildIndex             == _TDL_TreeNode::NO_ARRAY_INDEX )
	   && ( theIsDescendingIntoBranch == FALSE                         )
	   && ( getCachedTreeNode()       == getTopmostTreeNode()          )
	   && ( getCachedTreeNode()
		  -> isTreeNodeBranch()   == FALSE                         )
	   && ( getCachedTreeNode()
		  -> getArrayIndex()      == _TDL_TreeNode::NO_ARRAY_INDEX ) )
  {	/* The Singleton node already exists...                         *
	 * And there is nothing more to be done for a singleton lookup. */
    return;  /* All done. */
  }

	/* Otherwise we are descending to our target node. */

  else if ( getCachedTreeNode() -> isTreeNodeBranch() == FALSE )
  {
	/* We've hit a premature-endpoint bug. */
    TDL::getLogStream()
      << "[_TDL_TDLStatement:cacheDescendInto("
      << theChildIndex << "," 
      << ( (theIsDescendingIntoBranch == TRUE) ? "BRANCH,"   : "NODE," )
      << ( (theCanCreateNewNodes      == TRUE) ? "NEW-NODES" : "NO-new-nodes" )
      << "):\"" << getName() << "\"] Error:  "
      << "cachedTreeNode is *NOT* a branch!!!." << endl;
    setCachedTreeNode ( (_TDL_TreeNode *) NULL );
    return; /* Abort further action.  Do not try to create this node. */
  }

  else
  {
    setCachedTreeNode ( getCachedTreeNode()
			  -> getTreeNodeBranchConst()
			  -> getChildWithArrayIndex ( theChildIndex ) );
  }



	/* If we can create new tree members, and this member does not exist.*/
  if (   ( theCanCreateNewNodes == TRUE  )
      && ( hasCachedTreeNode()  == FALSE ) )
  {
	/* Do we need to create a topmost-TreeNodeBranch??. */
    if (   ( getTopmostTreeNode() == (_TDL_TreeNode *) NULL        )
	&& ( theChildIndex        != _TDL_TreeNode::NO_ARRAY_INDEX ) )
    {
      oldTreeNode = getCachedTopmostTreeNodeBranch ( TRUE );
    }

	/**************************************/
	/* Now we can create our new TreeNode */
	/**************************************/

    if (   ( theIsDescendingIntoBranch == TRUE                     )

	    /* Note:  Abort descending into complex tree's if we have been *
	     * destroyed.  Instead, create a destroyed child leaf ASAP.    */
	&& (   ( oldTreeNode == (_TDL_TreeNode *) NULL           )
	    || ( oldTreeNode -> getIsDestroyedSubtree() == FALSE ) )
	)
    {
      newTreeNode = new _TDL_TreeNodeBranch();
      addObjectToDelete  ( (_TDL_TreeNodeBranch *) newTreeNode );
    }
    else /* Ie:  Descending into a node. */
    {
      if ( createObjects ( newStatementData, newTreeNode ) == FAILURE )
      {
	errorMessage = "Unable to create child node.";
      }

      else if ( newStatementData == (_TDL_Snode *) NULL )
      {
	errorMessage = "newStatementData is NULL!";
      }

      else if ( newTreeNode == (_TDL_TreeNode *) NULL )
      {
	errorMessage = "newTreeNode is NULL!";
      }

      else if ( addDataObject ( newStatementData ) == FAILURE )
      {
	errorMessage = "Unable to addDataObject ( newStatementData ).";
      }

      if ( errorMessage != (const char *) NULL )
      {
	TDL::getLogStream()
	  << "[_TDL_TDLStatement:cacheDescendInto("
	  << theChildIndex << "):\"" << getName() << "\"]  "
	  << "Error:  " << errorMessage << endl;
	deleteObjects ( newStatementData, newTreeNode );
	return;
      }

	/* If we are creating a destroyed child-leaf, then destroy it! */
      if (   ( oldTreeNode != (_TDL_TreeNode *) NULL          )
	  && ( oldTreeNode -> getIsDestroyedSubtree() == TRUE ) )
      {
	newTreeNode -> destroy();
      }

    } /* IF ( theIsDescendingIntoBranch == TRUE ) ... ELSE ... */


		/* Configure the new TreeNode. */
    newTreeNode -> setArrayIndex ( theChildIndex );
    if ( getTopmostTreeNode() == (_TDL_TreeNode *) NULL )
    {
      setTopmostTreeNode ( newTreeNode );
      newTreeNode -> setParent ( (_TDL_TreeNodeBranch *) NULL );
    }
    else
    {
      oldTreeNode -> getTreeNodeBranch() -> addChild ( newTreeNode );
      newTreeNode -> setParent ( oldTreeNode -> getTreeNodeBranch() );
    }
    setCachedTreeNode ( newTreeNode );

  } /* IF ( theCanCreateNewNodes == TRUE &&  hasCachedTreeNode() == FALSE ) */
} /* void _TDL_TDLStatement::cacheDescendInto ( ... ) */





	/* This is overriden in subclasses that support this operation. */
/*virtual*/ _TDL_TDLStatement *
_TDL_TDLStatement::setTcmTaskTreeNodeName ( STRING theNodeName )
{
  MARKUSED ( theNodeName );

  TDL::getLogStream()
    << "[_TDL_TDLStatement:setTcmTaskTreeName]  Error:  "
    << "Operation is not supported." << endl;

  return this;
}



_TDL_TDLStatement::operator const TCM_Task_Tree_Ref & ()
{
  if ( getTopmostTreeNode() == (_TDL_TreeNode *)NULL )
  {
    TDL::getLogStream()
      << "[_TDL_TDLStatement:operator const TCM_Task_Tree_Ref &()]  Warning:  "
      << "Assuming non-iteration statement." << endl;
    _TDL_Dlist tempDlist(FALSE);
    getTreeNode ( tempDlist );
  }

  if ( getTopmostTreeNode() == (_TDL_TreeNode *)NULL )
  {
    TDL::getLogStream()
      << "[_TDL_TDLStatement:operator const TCM_Task_Tree_Ref &()]  Error:  "
      << "TopmostTreeNode is still NULL." << endl;
    return _TDL_TreeNode::EMPTY_TCM_TASK_TREE_REF;
  }
  else
  {
    return getTopmostTreeNode() -> operator const TCM_Task_Tree_Ref & ();
  }
}


/*virtual*/ status_t
_TDL_TDLStatement::doDestroy ( const _TDL_Dlist & theIterationIndexes,
			       BOOLEAN            theCanStillBeReferenced,
			       BOOLEAN            theDestroyIfUnused,
			       BOOLEAN &          theFoundSpawnStatementData,
			       int4               theDepth                   )
{
    /* Note:  Create a TreeNode (Leaf) corresponding to theIterationIndexes *
     *        which we can then destroy to terminate this TreeNodeBranch.   */
  _TDL_TreeNode * specificTreeNode = getTreeNode ( theIterationIndexes );

	/* If there is nothing to destroy... */
  if ( specificTreeNode == (_TDL_TreeNode *) NULL )
  {
    TDL::getLogStream()
      << "[_TDL_TDLStatement:doDestroy(";
    _TDL_IterationIndex::printIndexList ( TDL::getLogStream(),
					  theIterationIndexes );
    TDL::getLogStream()
      << "):\"" << getName() << "\"]  "
      << "Error:  Unable to access (create) TreeNode to destroy." << endl;
    return FAILURE;
  }
  else	/* Otherwise, doDestroy() that tree-node subtree. */
  {
    return specificTreeNode -> doDestroy ( theCanStillBeReferenced,
					   theDestroyIfUnused,
					   theFoundSpawnStatementData,
					   theDepth );
  }
}

	/* Lets allow safe-downcasts... (Basic RTTI) */
/*virtual*/
_TDL_TDLStatement::operator const _TDL_SpawnStatement * () const
{
  return (const _TDL_SpawnStatement *) NULL;
}

/*virtual*/
_TDL_TDLStatement::operator const _TDL_WithStatement * () const
{
  return (const _TDL_WithStatement *) NULL;
}

	/* And non-const versions...  (These default to just casting away *
         * const'ness after calling the const versions.)                  */
/*virtual*/
_TDL_TDLStatement::operator _TDL_SpawnStatement * ()
{
  return (_TDL_SpawnStatement *) getSpawnStatementConst();
}

/*virtual*/
_TDL_TDLStatement::operator _TDL_WithStatement * ()
{
  return (_TDL_WithStatement *) getWithStatementConst();
}


	/* iostreamBase interface */
/*virtual*/ ostream &
_TDL_TDLStatement::printObject ( ostream    & theOstream,
				 const char * theIndentString /*=""*/ ) const
{
  char  subIndentString [ iostreamBase::MAXIMUM_INDENT ];
  iostreamBase::createSubIndentString ( subIndentString, theIndentString );

  theOstream << theIndentString << "-Begin- _TDL_TDLStatement  "
	     << ((void *)this)  << endl

	     << theIndentString << " Names:" << endl;

  _TDL_NamesList::printObject ( theOstream, subIndentString );

  theOstream << theIndentString << " DataObjectSlist:" << endl;

  getDataObjectSlist() . printObject ( theOstream, subIndentString );

  theOstream << theIndentString
	     << "------- _TDL_TDLStatement  " << ((void *)this) << endl
	     << theIndentString << " Tree:" << endl;

  if ( getTopmostTreeNode() == (_TDL_TreeNode *) NULL )
    theOstream << subIndentString << "NULL" << endl;
  else
    getTopmostTreeNode() -> printObject ( theOstream, subIndentString );

  theOstream << theIndentString << "--End-- _TDL_TDLStatement  "
	     << ((void *)this)  << endl;

  return theOstream;
}

