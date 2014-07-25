/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

#include "_TDL_WithStatement.H"
#include "_TDL_WithStatementData.H"
#include "_TDL_WithStatementTreeNode.H"

/*virtual*/
_TDL_WithStatement::~_TDL_WithStatement()
{}


	/* _TDL_TDLStatement Interface */
/*virtual*/ status_t
_TDL_WithStatement::createObjects ( _TDL_Snode    * & theStatementData,
				    _TDL_TreeNode * & theTreeNode       )
{
  theStatementData = new _TDL_WithStatementData ( * this );
  theTreeNode      = ((_TDL_WithStatementData *) theStatementData)
			 -> createAndAttachNewWithStatementTreeNode();
  return SUCCESS;
}


	/* _TDL_TDLStatement Interface */
/*virtual*/ status_t
_TDL_WithStatement::deleteObjects ( _TDL_Snode    *   theStatementData,
				    _TDL_TreeNode *   theTreeNode       )
{
  if ( theStatementData != (_TDL_Snode *) NULL )
  {
    if ( theTreeNode != (_TDL_TreeNode *) NULL )
    {
      if ( theTreeNode -> isWithStatementTreeNode() == TRUE )
      {
	((_TDL_WithStatementData *) theStatementData)
	    -> removeWithStatementTreeNode ( 
		 theTreeNode -> getWithStatementTreeNode() );
	delete theTreeNode;
      }
      else
      {
	TDL::getLogStream()
	  << "[_TDL_WithStatement:deleteObjects("
	  << ((void *)theStatementData)
	  << "," << ((void *)theTreeNode) << "):\"" << getName()
	  << "\"]  Error:  theTreeNode is not a WithStatementTreeNode.  "
	  << "Unable to delete theTreeNode.  TreeNode is:" << endl;
	theTreeNode -> printObject ( TDL::getLogStream(), "     " ) << endl;
      } /* IF (theTreeNode->isWithStatementTreeNode() == TRUE) ... ELSE ... */
    } /* IF ( theTreeNode != (_TDL_TreeNode *) NULL ) */
    delete theStatementData;
    return SUCCESS;
  } /* IF ( theStatementData != (_TDL_Snode *) NULL ) */
  else if ( theTreeNode != (_TDL_TreeNode *) NULL )
  {
	/* Do not delete theTreeNode if theStatementData is NULL.          *
	 * Better to have a core-leak than a reference to deleted pointer. */
    TDL::getLogStream()
      << "[_TDL_WithStatement:deleteObjects(" << ((void *)theStatementData)
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



	/* Creates the SpawnStatementData & TreeNode if it does not exist. */
_TDL_WithStatementTreeNode *
_TDL_WithStatement::getWithStatementTreeNode (
				       const _TDL_Dlist & theIterationIndexes )
{
  getTreeNode ( theIterationIndexes );
  return getWithStatementTreeNodeConst ( theIterationIndexes );
}



	/* Returns NULL if the specified WithStatementData does not exist. */
_TDL_WithStatementTreeNode *
_TDL_WithStatement::getWithStatementTreeNodeConst (
				       const _TDL_Dlist & theIterationIndexes )
{
  _TDL_TreeNode * treeNode = getTreeNodeConst ( theIterationIndexes );

  if ( treeNode == (_TDL_TreeNode *) NULL )
  {
    return (_TDL_WithStatementTreeNode *) NULL;
  }

  else if ( treeNode -> isWithStatementTreeNode() == TRUE )
  {
    return treeNode -> getWithStatementTreeNode();
  }

  else /* Ie: treeNode -> isWithStatementTreeNode() == FALSE */
  {
	/* Not a with-statement-tree-node bug. */
    TDL::getLogStream()
      << "[_TDL_WithStatement:getWithStatementTreeNodeConst(\"";
    _TDL_IterationIndex::printIndexList ( TDL::getLogStream(),
					  theIterationIndexes );
    TDL::getLogStream()
      << "\"):\"" << getName()
      << "\"]  Error:  treeNode exists but is *NOT* a WithStatementTreeNode!"
      << "  Returning NULL.  treeNode is:" << endl;
    treeNode -> printObject ( TDL::getLogStream(), "     " );

    return (_TDL_WithStatementTreeNode *) NULL;
  }
}


	/* Creates the WithStatementData & TreeNode if it does not exist. */
_TDL_WithStatementData *
_TDL_WithStatement::getWithStatementData (
				       const _TDL_Dlist & theIterationIndexes )
{
  getTreeNode ( theIterationIndexes );
  return getWithStatementDataConst ( theIterationIndexes );
}


	/* Returns NULL if the specified WithStatementData does not exist. */
_TDL_WithStatementData *
_TDL_WithStatement::getWithStatementDataConst (
				       const _TDL_Dlist & theIterationIndexes )
{
  _TDL_TreeNode * treeNode
		    = getWithStatementTreeNodeConst ( theIterationIndexes );

  if ( treeNode == (_TDL_TreeNode *) NULL )
  {
    return (_TDL_WithStatementData *) NULL;
  }
  else
  {
    return & ( treeNode -> getWithStatementTreeNode()
	                -> getWithStatementData() );
  }
}




	/* Lets allow safe-downcasts... (Basic RTTI) */
/*virtual*/
_TDL_WithStatement::operator const _TDL_WithStatement * () const
{
  return this;
}



	/* iostreamBase interface. */
/*virtual*/ ostream &
_TDL_WithStatement::printObject ( ostream    & theOstream,
				  const char * theIndentString /*=""*/ ) const
{
  char  subIndentString [ iostreamBase::MAXIMUM_INDENT ];
  iostreamBase::createSubIndentString ( subIndentString, theIndentString );

  theOstream << theIndentString
	     << "-Begin- _TDL_WithStatement  " << ((void *)this) << endl

	     << theIndentString << " Base _TDL_TDLStatement class:" << endl;

  _TDL_TDLStatement::printObject ( theOstream, subIndentString );

  theOstream << theIndentString << "--End-- _TDL_WithStatement  "
	     << ((void *)this)  << endl;

  return theOstream;
}


