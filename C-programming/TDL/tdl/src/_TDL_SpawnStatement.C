/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

#include "_TDL_SpawnStatement.H"
#include "_TDL_SpawnStatementData.H"
#include "_TDL_SpawnStatementTreeNode.H"

/*static*/ _TDL_SpawnStatement &
_TDL_SpawnStatement::generateTrivialSpawnStatement (
				     const char              * theName,
				     const TCM_Task_Tree_Ref * theTaskTreeRef
								/* = NULL */  )
{
  _TDL_Dlist             tmpEmptyDlist;
  _TDL_SpawnStatement  * trivialSpawnStatement
	      = new _TDL_SpawnStatement ( 1, theName );

	/* Set our SpawnStatementData to be "DESTROY"'ed */
  trivialSpawnStatement -> getSpawnStatementData ( tmpEmptyDlist )
                        -> destroy ( FALSE );

	/* If necessary, set the underlying TCM_Task_Tree_Ref */
  if ( theTaskTreeRef != (const TCM_Task_Tree_Ref *) NULL )
  {
    trivialSpawnStatement -> getSpawnStatementDataConst ( tmpEmptyDlist )
                          -> setTCMTaskTreeDirectly ( * theTaskTreeRef );
  }

  return * trivialSpawnStatement;
}



/*virtual*/
_TDL_SpawnStatement::~_TDL_SpawnStatement()
{
  setTcmTaskTreeNodeName( STRING(NULL) );
}


	/* _TDL_TDLStatement Interface */
/*virtual*/ status_t
_TDL_SpawnStatement::createObjects ( _TDL_Snode    * & theStatementData,
				     _TDL_TreeNode * & theTreeNode       )
{
  theStatementData = new _TDL_SpawnStatementData ( * this );
  theTreeNode      = ((_TDL_SpawnStatementData *) theStatementData)
			 -> createAndAttachNewSpawnStatementTreeNode();
  return SUCCESS;
}


	/* _TDL_TDLStatement Interface */
/*virtual*/ status_t
_TDL_SpawnStatement::deleteObjects ( _TDL_Snode    *   theStatementData,
				     _TDL_TreeNode *   theTreeNode       )
{
  if ( theStatementData != (_TDL_Snode *) NULL )
  {
    if ( theTreeNode != (_TDL_TreeNode *) NULL )
    {
      if ( theTreeNode -> isSpawnStatementTreeNode() == TRUE )
      {
	((_TDL_SpawnStatementData *) theStatementData)
	    -> removeSpawnStatementTreeNode (
		 theTreeNode -> getSpawnStatementTreeNode() );
	delete theTreeNode;
      }
      else
      {
	TDL::getLogStream()
	  << "[_TDL_SpawnStatement:deleteObjects("
	  << ((void *)theStatementData)
	  << "," << ((void *)theTreeNode) << "):\"" << getName()
	  << "\"]  Error:  theTreeNode is not a SpawnStatementTreeNode.  "
	  << "Unable to delete theTreeNode.  TreeNode is:" << endl;
	theTreeNode -> printObject ( TDL::getLogStream(), "     " ) << endl;
      } /* IF (theTreeNode->isSpawnStatementTreeNode() == TRUE) ... ELSE ... */
    } /* IF ( theTreeNode != (_TDL_TreeNode *) NULL ) */
    delete theStatementData;
    return SUCCESS;
  } /* IF ( theStatementData != (_TDL_Snode *) NULL ) */
  else if ( theTreeNode != (_TDL_TreeNode *) NULL )
  {
	/* Do not delete theTreeNode if theStatementData is NULL.          *
	 * Better to have a core-leak than a reference to deleted pointer. */
    TDL::getLogStream()
      << "[_TDL_SpawnStatement:deleteObjects(" << ((void *)theStatementData)
      << "," << ((void *)theTreeNode) << "):\"" << getName()
      << "\"]  Error:  theStatementData is NULL, theTreeNode is NOT NULL.  "
      << "Unable to delete theTreeNode.  Possible core leak." << endl;
    return FAILURE;
  }
  else /* Ie: both theStatementData & theTreeNode are NULL */
  {
    return SUCCESS;  /* What else are we going to do? */
  }
}


	/* Overriden from _TDL_TDLStatement */
/*virtual*/ _TDL_TDLStatement *
_TDL_SpawnStatement::setTcmTaskTreeNodeName ( STRING theNodeName )
{
  if ( getTcmTaskTreeNodeName() != STRING(NULL) )
    delete [] getTcmTaskTreeNodeName();

  if ( theNodeName == STRING(NULL) )
    tcmTaskTreeNodeName = STRING(NULL);
  else
    tcmTaskTreeNodeName = NewString ( theNodeName );

  return this;
}



	/* Creates the WithStatementData & TreeNode if it does not exist. */
_TDL_SpawnStatementTreeNode *
_TDL_SpawnStatement::getSpawnStatementTreeNode (
				       const _TDL_Dlist & theIterationIndexes )
{
  getTreeNode ( theIterationIndexes );
  return getSpawnStatementTreeNodeConst ( theIterationIndexes );
}



	/* Returns NULL if the specified SpawnStatementData does not exist. */
_TDL_SpawnStatementTreeNode *
_TDL_SpawnStatement::getSpawnStatementTreeNodeConst (
				       const _TDL_Dlist & theIterationIndexes )
{
  _TDL_TreeNode * treeNode = getTreeNodeConst ( theIterationIndexes );

  if ( treeNode == (_TDL_TreeNode *) NULL )
  {
    return (_TDL_SpawnStatementTreeNode *) NULL;
  }

  else if ( treeNode -> isSpawnStatementTreeNode() == TRUE )
  {
    return treeNode -> getSpawnStatementTreeNode();
  }

  else /* Ie: treeNode -> isSpawnStatementTreeNode() == FALSE */
  {
	/* Not a spawn-statement-tree-node bug. */
    TDL::getLogStream()
      << "[_TDL_SpawnStatement:getSpawnStatementTreeNodeConst(\"";
    _TDL_IterationIndex::printIndexList ( TDL::getLogStream(),
					  theIterationIndexes );
    TDL::getLogStream()
      << "\"):\"" << getName()
      << "\"]  Error:  treeNode exists but is *NOT* a SpawnStatementTreeNode!"
      << "  Returning NULL.  treeNode is:" << endl;
    treeNode -> printObject ( TDL::getLogStream(), "     " ) << endl;

    return (_TDL_SpawnStatementTreeNode *) NULL;
  }
}


	/* Creates the SpawnStatementData & TreeNode if it does not exist. */
_TDL_SpawnStatementData *
_TDL_SpawnStatement::getSpawnStatementData (
				       const _TDL_Dlist & theIterationIndexes )
{
  getTreeNode ( theIterationIndexes );
  return getSpawnStatementDataConst ( theIterationIndexes );
}


	/* Returns NULL if the specified SpawnStatementData does not exist. */
_TDL_SpawnStatementData *
_TDL_SpawnStatement::getSpawnStatementDataConst (
				       const _TDL_Dlist & theIterationIndexes )
{
  _TDL_TreeNode * treeNode
		    = getSpawnStatementTreeNodeConst ( theIterationIndexes );

  if ( treeNode == (_TDL_TreeNode *) NULL )
  {
    return (_TDL_SpawnStatementData *) NULL;
  }
  else
  {
    return & ( treeNode -> getSpawnStatementTreeNode()
	                -> getSpawnStatementData() );
  }
}




	/* Lets allow safe-downcasts... (Basic RTTI) */
/*virtual*/
_TDL_SpawnStatement::operator const _TDL_SpawnStatement * () const
{ 
  return this;
}



	/* iostreamBase interface. */
/*virtual*/ ostream &
_TDL_SpawnStatement::printObject ( ostream    & theOstream,
				   const char * theIndentString /*=""*/ ) const
{
  char  subIndentString [ iostreamBase::MAXIMUM_INDENT ];
  iostreamBase::createSubIndentString ( subIndentString, theIndentString );

  theOstream << theIndentString
	     << "-Begin- _TDL_SpawnStatement  " << ((void *)this) << endl

	     << theIndentString << " AllocationFunction:  "
	     << getAllocationFunction().getVoidFunctionPointer()
	     << endl

	     << theIndentString << " Base _TDL_TDLStatement class:" << endl;

  _TDL_TDLStatement::printObject ( theOstream, subIndentString );

  theOstream << theIndentString << "--End-- _TDL_SpawnStatement  "
	     << ((void *)this)  << endl;

  return theOstream;
}

