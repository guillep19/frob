/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

#include "_TDL_WithStatementData.H"
#include "_TDL_WithStatementTreeNode.H"


/*virtual*/
_TDL_WithStatementData::~_TDL_WithStatementData()
{ }



_TDL_WithStatementTreeNode *
_TDL_WithStatementData::createAndAttachNewWithStatementTreeNode()
{
  _TDL_WithStatementTreeNode * newWithStatementTreeNode
    = new _TDL_WithStatementTreeNode ( * this );

  if ( getWithStatementTreeNodeSlistNonConst()
         . appendNode ( newWithStatementTreeNode ) == FAILURE )
  {
    TDL::getLogStream()
      << "[_TDL_WithStatementData:createAndAttachNewWithStatementTreeNode]  "
      << "Error:  appendNode FAILED.   Returning NULL!!!" << endl;

    delete newWithStatementTreeNode;
    return (_TDL_WithStatementTreeNode *) NULL;
  }

  return newWithStatementTreeNode;
}



status_t
_TDL_WithStatementData::removeWithStatementTreeNode (
	      _TDL_WithStatementTreeNode * theWithStatementTreeNodeToRemove )
{
  status_t returnValue
	     = (    getWithStatementTreeNodeSlistNonConst()
		      . removeNode ( theWithStatementTreeNodeToRemove )
		 == theWithStatementTreeNodeToRemove ) ? SUCCESS : FAILURE;

  if ( returnValue == FAILURE )
  {
    TDL::getLogStream()
      << "[_TDL_WithStatementData:removeWithStatementTreeNode]  "
      << "Error:  removeNode ( " << ((void*)theWithStatementTreeNodeToRemove)
      << " ) FAILED." << endl;
  }

  return returnValue;
}



	/* Names are really useful for identifying (subclassed) *
	 * _TDL_TreeNode instances during debugging/testing...  */
/*virtual*/ const char *
_TDL_WithStatementData::getName() const
{
  return getWithStatement() . getName();
}

/*virtual*/ BOOLEAN
_TDL_WithStatementData::hasName ( const char * theName ) const
{
  return getWithStatement() . hasName ( theName );
}



	/* Common workhorse method to destroy _TDL_SpawnStatementData's *
         * referred to by this subtree.                                 */
/*virtual*/ status_t
_TDL_WithStatementData::doDestroy ( BOOLEAN   theCanStillBeReferenced,
				    BOOLEAN   theDestroyIfUnused,
				    BOOLEAN & theFoundSpawnStatementData,
				    int4      theDepth                    )
{
  	/* Might as well node that we have been destroyed. */
  for ( _TDL_Snode * node  = getWithStatementTreeNodeSlist() . getFirstNode();
		     node != (_TDL_Snode *) NULL;
		     node  = node -> _TDL_Snode::getNextNode() )
  {
    ((_TDL_WithStatementTreeNode *) node) -> setIsDestroyedSubtree();
  }

  return _TDL_TreeNodeBranch::doDestroy ( theCanStillBeReferenced,
					  theDestroyIfUnused,
					  theFoundSpawnStatementData,
					  theDepth );
}



	/* Lets allow safe-downcasts... (Basic RTTI) */
/*virtual*/
_TDL_WithStatementData::operator const _TDL_WithStatementData * () const
{
  return this;
}



	/* iostreamBase interface. */
/*virtual*/ ostream &
_TDL_WithStatementData::printObject (
				   ostream    & theOstream,
				   const char * theIndentString /*=""*/ ) const
{
  char  subIndentString [ iostreamBase::MAXIMUM_INDENT ];
  iostreamBase::createSubIndentString ( subIndentString, theIndentString );

  theOstream << theIndentString << "-Begin- _TDL_WithStatementDataa  "
	     << ((void *)this)  << endl;

  _TDL_TreeNodeBranch::printObject ( theOstream, subIndentString );

  theOstream << theIndentString
	     << " Associated with: " << ((void *) & (getWithStatement()) )
	     << "  ( \"" << getName() << "\" )" << endl
	     << theIndentString << " WithStatementTreeNodes:" << endl;

	/* Do this manually here to avoid an endless loop. */
  for ( _TDL_Snode * node  = getWithStatementTreeNodeSlist() . getFirstNode();
	             node != (_TDL_Snode *) NULL;
	             node  = node -> getNextNode()
       )
  {
    theOstream << theIndentString << "   _TDL_WithStatementTreeNode:  "
	       << ((void *) node)  << endl;
  }

  theOstream << theIndentString << "--End-- _TDL_WithStatementData  "
	     << ((void *)this)  << endl;

  return theOstream;
}

