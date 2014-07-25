/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

#include "_TDL_SpawnStatementData.H"
#include "_TDL_SpawnStatementTreeNode.H"
#include "_TDL_WithStatement.H"
#include "_TDL_WithStatementData.H"
#include "_TDL_WithStatementTreeNode.H"



/*virtual*/
_TDL_SpawnStatementData::~_TDL_SpawnStatementData()
{
  if ( isNotStarted() == TRUE )
  {
    destroy();
  }

	/* Clear this, just to be safe... */
  setCachedOnAgentConstraint ( (_TDL_OnAgent *) NULL );

	/* Sometimes a object is used after it is deleted... */
	/* It's nice to be able to pick up on this... */
  spawnState = _TDL_SpawnStatementData::DELETED;
}


_TDL_SpawnStatementTreeNode *
_TDL_SpawnStatementData::createAndAttachNewSpawnStatementTreeNode()
{
  _TDL_SpawnStatementTreeNode * newSpawnStatementTreeNode
    = new _TDL_SpawnStatementTreeNode ( * this );

  if ( getSpawnStatementTreeNodeSlistNonConst()
         . appendNode ( newSpawnStatementTreeNode ) == FAILURE )
  {
    TDL::getLogStream()
      << "[_TDL_SpawnStatementData:createAndAttachNewSpawnStatementTreeNode]  "
      << "Error:  appendNode FAILED.   Returning NULL!!!" << endl;

    delete newSpawnStatementTreeNode;
    return (_TDL_SpawnStatementTreeNode *) NULL;
  }

  return newSpawnStatementTreeNode;
}



status_t
_TDL_SpawnStatementData::removeSpawnStatementTreeNode (
	      _TDL_SpawnStatementTreeNode * theSpawnStatementTreeNodeToRemove )
{
  status_t returnValue
	     = (    getSpawnStatementTreeNodeSlistNonConst()
		      . removeNode ( theSpawnStatementTreeNodeToRemove )
		 == theSpawnStatementTreeNodeToRemove ) ? SUCCESS : FAILURE;

  if ( returnValue == FAILURE )
  {
    TDL::getLogStream()
      << "[_TDL_SpawnStatementData:removeSpawnStatementTreeNode]  "
      << "Error:  removeNode ( " << ((void*)theSpawnStatementTreeNodeToRemove)
      << " ) FAILED." << endl;
  }

  return returnValue;
}



const char *
_TDL_SpawnStatementData::getStateString() const
{
  switch ( getState() )
  {
    case _TDL_SpawnStatementData::NOT_ALLOCATED:  return "NOT_ALLOCATED";
    case _TDL_SpawnStatementData::ALLOCATED:      return "ALLOCATED";
    case _TDL_SpawnStatementData::RUNNING:        return "RUNNING";
    case _TDL_SpawnStatementData::DESTROYED:      return "DESTROYED";
    case _TDL_SpawnStatementData::DELETED:        return "***DELETED***";
    default:                                      return "***UNKNOWN***";
  }
}





/*
 * Given the potential for things to mess up here,
 * lets add a little extra error checking...
 */
status_t
_TDL_SpawnStatementData::verifyDistributedCorrectly() const
{
  status_t       returnValue       = SUCCESS;
  _TDL_OnAgent * onAgentConstraint = findOnAgentConstraint();

  if ( getState() != _TDL_SpawnStatementData::NOT_ALLOCATED )
  {
    if ( getNodeHandle() . isNull() )
    {
      TDL::getLogStream()
	<< "[_TDL_SpawnStatementData:verifyDistributedCorrectly]  "
	<< "Internal Error:  "
	<< "State=\"" << getStateString()
	<< "\", but TCM_Task_Tree_Ref is NULL."
	<< endl;
      returnValue = FAILURE;
    }
    else
    {
      if (    _TDL_DO_CHECK_IF_TASK_DISTRIBUTED ( getNodeHandle() )
	   != ( onAgentConstraint != (_TDL_OnAgent *) NULL )     )
      {
	TDL::getLogStream()
	  << endl << endl << endl
	  << "[_TDL_SpawnStatementData:isDistributed]  "
	  << "URGENT INTERNAL ERROR:  "
	  << "SpawnState = \"" << getStateString()
	  << "\".   findOnAgentConstraintConst() = \""
	  << ( ( onAgentConstraint == (_TDL_OnAgent *) NULL )
	      ? "null/NON-distributed" : "NOT-null/DISTRIBUTED")
	  << "\".   TCM_IsDistributedNode(getNodeHandle) = \""
	  << ( (_TDL_DO_CHECK_IF_TASK_DISTRIBUTED (getNodeHandle()) == TRUE)
	      ? "TRUE" : "FALSE")
	  << "\".  These last two should be matching up.  They are not.  "
	  << "PLEASE REPORT THIS BUG!!!"
	  << endl << endl << endl;
	returnValue = FAILURE;
      }
    } /* IF ( getNodeHandle() . isNull() ) ...  ELSE ... */

    if ( onAgentConstraint != getCachedOnAgentConstraint() )
    {
      TDL::getLogStream()
	<< endl << endl << endl
	<< "[_TDL_SpawnStatementData:isDistributed]  "
	<< "URGENT INTERNAL ERROR:  "
	<< "spawnState = \"" << getStateString()
	<< "\".   findOnAgentConstraintConst() = \""
	<< ( ( onAgentConstraint ==  (_TDL_OnAgent *) NULL )
	    ? "NULL"
	    : onAgentConstraint -> getAgentName() )

	<< "\".   getCachedOnAgentConstraint() = \""
	<< ( ( getCachedOnAgentConstraint() ==  (_TDL_OnAgent *) NULL )
	    ? "NULL"
	    : getCachedOnAgentConstraint() -> getAgentName() )

	<< "\".  These last two should be the same.  They are not.  "
	<< "PLEASE REPORT THIS BUG!!!"
	<< endl << endl << endl;
      returnValue = FAILURE;
    }
  } /* if ( getState() != _TDL_SpawnStatementData::NOT_ALLOCATED ) */

  return returnValue;

} /* status_t _TDL_SpawnStatementData::verifyDistributedCorrectly() const */





/*
 * theSpawnTreeNodeBranch was, originally, a parent of a spawn statement,
 * in either the dynamic iteration tree (TreeNodeBranch) or the dynamic
 * WITH tree (WithStatementData).
 */
static _TDL_OnAgent *
findOnAgentConstraint_InternalLoop (
			   const _TDL_TreeNodeBranch * theSpawnTreeNodeBranch )
{
  _TDL_Constraint                   * constraint;
  const _TDL_WithStatementTreeNode  * targetWithTreeNode;
  const _TDL_TreeNode               * withTreeNode;
  const _TDL_TreeNodeBranch         * withTreeNodeBranch;
  _TDL_OnAgent                      * onAgentConstraint
					= (_TDL_OnAgent *)NULL;

  while ( theSpawnTreeNodeBranch != (const _TDL_TreeNodeBranch *)NULL )
  {
	  /* Check these constraints. */
    for ( constraint  = (_TDL_Constraint *)
		        ( theSpawnTreeNodeBranch -> getConstraintSlist()
						 .  getFirstNode() );
	  constraint != (_TDL_Constraint *) NULL;
	  constraint  = (_TDL_Constraint *) ( constraint -> getNextNode() )
	 )
    {
      if ( constraint -> isOnAgentConstraint() == TRUE )
      {
	if ( onAgentConstraint == (_TDL_OnAgent *)NULL )
	  onAgentConstraint = (_TDL_OnAgent *) constraint;

	else if (    constraint        -> getConstraintIndex()
		  >= onAgentConstraint -> getConstraintIndex() )
	  onAgentConstraint = (_TDL_OnAgent *) constraint;
      }
    } /* FOR ( constraint ... ) */


	  /* If we have a With-Statement
	   * Check any iteration-tree constraints on this with-statement,
	   */
    if ( theSpawnTreeNodeBranch -> isWithStatementData() )
    {
	/* Note:  getWithStatementTreeNodeSlist() contains only one
	 * TreeNode -- which corresponds to the TDL Iteration Index Tree.
	 * E.g.: All WithStatementTreeNode's should be located inside
	 * the TDL Iteration Tree(s).  WithStatementData's are what is
	 * located inside TDL CODE Tree's.
	 */

	/* Walk up the iteration tree, checking Iteration constraints */
      for ( targetWithTreeNode  = (const _TDL_WithStatementTreeNode *)
				  ( theSpawnTreeNodeBranch
				      -> getWithStatementDataConst()
				      -> getWithStatementTreeNodeSlist()
				      .  getFirstNode() )
	      ;
	    targetWithTreeNode != (const _TDL_WithStatementTreeNode *) NULL
	      ;
	    targetWithTreeNode  = (const _TDL_WithStatementTreeNode *)
				  ( targetWithTreeNode
				      -> _TDL_Snode::getNextNode() )
	   )
      {
	withTreeNode       = targetWithTreeNode;
	withTreeNodeBranch = targetWithTreeNode -> getParent();

	while ( withTreeNodeBranch != (const _TDL_TreeNodeBranch *) NULL )
	{
		/* Check any constraints. */
	  for ( constraint  = (_TDL_Constraint *)
			      ( withTreeNodeBranch -> getConstraintSlist()
						   .  getFirstNode() );
		constraint != (_TDL_Constraint *) NULL;
		constraint  = (_TDL_Constraint *)
			      ( constraint -> getNextNode() )
	       )
	  {
	    if ( constraint -> isOnAgentConstraint() == TRUE )
	    {
	      if ( onAgentConstraint == (_TDL_OnAgent *)NULL )
		onAgentConstraint = (_TDL_OnAgent *) constraint;

	      else if (    constraint        -> getConstraintIndex()
			>= onAgentConstraint -> getConstraintIndex() )
		onAgentConstraint = (_TDL_OnAgent *) constraint;
	    }
	  } /* FOR ( constraint ... ) */

		/* Move up one level. */
	  withTreeNode       = withTreeNodeBranch;
	  withTreeNodeBranch = withTreeNodeBranch -> getParent();

	} /* while ( withTreeNodeBranch != NULL ) */
      }/*FOR(iteration-tree-nodes associated with this WithStatementData)*/
    } /* if ( theSpawnTreeNodeBranch -> isWithStatementData() ) */

	  /* Move up one level. */
    theSpawnTreeNodeBranch = theSpawnTreeNodeBranch -> getParent();

  } /* while ( theSpawnTreeNodeBranch != (_TDL_TreeNodeBranch *)NULL ) */

  return onAgentConstraint;
}


/*
 * Returns NULL if an ON-AGENT constraint is not found.
 *
 * Note: Almost verbatim equivalent of _TDL_HandleManager.C function:
 *  applyInsertionConstraintsFromAllParentTreeNodes(...)
 *
 * With the **NOTABLE** exception that we also search the defaultWithParent
 * hierarchy in addition to everything else!
 */
_TDL_OnAgent *
_TDL_SpawnStatementData::findOnAgentConstraint() const
{
  const _TDL_SpawnStatementTreeNode * targetSpawnTreeNode;
  const _TDL_TDLStatement           * defaultWithParent_AsTdlStatement;
  const _TDL_TreeNode               * defaultWithParent_AsTreeNode;
  _TDL_Constraint                   * constraint;
  _TDL_OnAgent                      * possibleOnAgentConstraint;
  _TDL_OnAgent                      * onAgentConstraint
					= (_TDL_OnAgent *)NULL;

      /* Check constraints cached on current _TDL_SpawnStatementData */
  for ( constraint  = (_TDL_Constraint *)
		      ( getConstraintSlist() .  getFirstNode() );
	constraint != (_TDL_Constraint *) NULL;
	constraint  = (_TDL_Constraint *) ( constraint -> getNextNode() )
       )
  {
    if ( constraint -> isOnAgentConstraint() == TRUE )
    {
      if ( onAgentConstraint == (_TDL_OnAgent *)NULL )
	onAgentConstraint = (_TDL_OnAgent *) constraint;

      else if (    constraint        -> getConstraintIndex()
		>= onAgentConstraint -> getConstraintIndex() )
	onAgentConstraint = (_TDL_OnAgent *) constraint;
    }
  } /* FOR ( constraint ... ) */



      /* Walk up the tree, applying With-Do/Iteration constraints */
  for ( targetSpawnTreeNode = (const _TDL_SpawnStatementTreeNode *)
			      ( getSpawnStatementTreeNodeSlist()
				  . getFirstNode() )
	  ;
	targetSpawnTreeNode != (const _TDL_SpawnStatementTreeNode *) NULL
	  ;
	targetSpawnTreeNode  = (const _TDL_SpawnStatementTreeNode *)
				     ( targetSpawnTreeNode
				          -> _TDL_Snode::getNextNode() )
       )
  {
    possibleOnAgentConstraint = findOnAgentConstraint_InternalLoop (
					  targetSpawnTreeNode -> getParent() );

    if ( possibleOnAgentConstraint != (_TDL_OnAgent *)NULL )
    {
      if ( onAgentConstraint == (_TDL_OnAgent *)NULL )
	onAgentConstraint = possibleOnAgentConstraint;

      else if (    possibleOnAgentConstraint -> getConstraintIndex()
		>= onAgentConstraint         -> getConstraintIndex() )
	onAgentConstraint = possibleOnAgentConstraint;
    }

  } /* FOR ( All treeNodes associated with the current SpawnStatementData ) */




      /* Walk up the Default-With-Tree, applying With/Iteration constraints */
  defaultWithParent_AsTdlStatement
    = getSpawnStatement() . getDefaultWithParent_AsTdlStatement();

  while ( defaultWithParent_AsTdlStatement != (_TDL_TDLStatement *) NULL )
  {
    defaultWithParent_AsTreeNode
      = getSpawnStatement()
          . getDefaultWithParent_AsTreeNode ( defaultWithParent_AsTdlStatement,
					      this );

    possibleOnAgentConstraint = (_TDL_OnAgent *) NULL;
    

    if ( defaultWithParent_AsTreeNode != (_TDL_TreeNode *)NULL )
    {

      if ( defaultWithParent_AsTreeNode -> isWithStatementTreeNode() == TRUE )
      {
        possibleOnAgentConstraint = findOnAgentConstraint_InternalLoop (
	  & ( defaultWithParent_AsTreeNode -> getWithStatementTreeNodeConst()
					   -> getWithStatementData() )   );
      }

      else if ( defaultWithParent_AsTreeNode -> isTreeNodeBranch() == TRUE )
      {
        possibleOnAgentConstraint = findOnAgentConstraint_InternalLoop (      
	  defaultWithParent_AsTreeNode -> getTreeNodeBranchConst()       );
      }

      else
      {
	TDL::getLogStream()
	  << "[_TDL_SpawnStatementData::findOnAgentConstraint(\"" << getName()
	  << "\")]  Warning:  getDefaultWithParent_AsTreeNode() returned a "
	  << ( (    defaultWithParent_AsTreeNode -> isSpawnStatementTreeNode()
	         == TRUE )
	      ? "SpawnStatementTreeNode"
	      : ( ( defaultWithParent_AsTreeNode -> isWithStatementData()
		    == TRUE )
	         ? "WithStatementData" : "??TreeNode" ) )
	  << "object -- this was unexpected, and indicates a more serious "
	  << "problem.  PLEASE REPORT THIS ERROR!" << endl;
      }

    } /* if ( defaultWithParent_AsTreeNode != (_TDL_TreeNode *)NULL ) ... */


    if ( possibleOnAgentConstraint != (_TDL_OnAgent *)NULL )
    {
      if ( onAgentConstraint == (_TDL_OnAgent *)NULL )
	onAgentConstraint = possibleOnAgentConstraint;

      else if (    possibleOnAgentConstraint -> getConstraintIndex()
		>= onAgentConstraint         -> getConstraintIndex() )
	onAgentConstraint = possibleOnAgentConstraint;
    }


    defaultWithParent_AsTdlStatement
      = defaultWithParent_AsTdlStatement
          -> getDefaultWithParent_AsTdlStatement();
     
  } /* WHILE( defaultWithParent_AsTdlStatement != (_TDL_TDLStatement *)NULL )*/



  return onAgentConstraint;

} /* _TDL_OnAgent * _TDL_SpawnStatementData::findOnAgentConstraint() const */




status_t
_TDL_SpawnStatementData::startRunning ( )
{
  switch ( getState() )
  {
    case _TDL_SpawnStatementData::NOT_ALLOCATED:
      if ( allocate() == FAILURE )
      {
	TDL::getLogStream()
	  << "[_TDL_SpawnStatementData::startRunning(\"" << getName()
	  << "\")]  ERROR:  allocate() FAILED!!!  Aborting..."
	  << endl;
	return FAILURE;
      }

      /* NO BREAK OR RETURN!!!  GO DIRECTLY TO "ALLOCATED" case */

    case _TDL_SpawnStatementData::ALLOCATED:
      if ( getNodeHandle() . isNull() )
      {
	TDL::getLogStream()
	  << "[_TDL_SpawnStatementData::startRunning(\"" << getName()
	  << "\")]  ERROR: In ALLOCATED-state, but node is NULL!!  Aborting..."
	  << endl;
	return FAILURE;
      }

	/* Record that we are now "Running" this node. */
      spawnState = _TDL_SpawnStatementData::RUNNING;

	/* Yes, I am paranoid.  Lets double check everything one last time. */
      verifyDistributedCorrectly();

      return SUCCESS;


    case _TDL_SpawnStatementData::RUNNING:
	/* It's already Running.  What are we going to do?  Fail? */
      return SUCCESS;


    case _TDL_SpawnStatementData::DESTROYED:
      TDL::getLogStream()
	<< "[_TDL_SpawnStatementData::startRunning(\"" << getName()
	<< "\")]  ERROR:  Node has already been destroyed!" << endl;
      return FAILURE;

    case _TDL_SpawnStatementData::DELETED:
      TDL::getLogStream()
	<< "[_TDL_SpawnStatementData::startRunning(\"" << getName()
	<< "\")]  ERROR:  Object has already been DELETED!" << endl;
      return FAILURE;

    default:
      TDL::getLogStream()
	<< "[_TDL_SpawnStatementData::startRunning(\"" << getName()
	<< "\")]  ERROR:  Unknown state: " << int4(getState()) << endl;
      return FAILURE;
  }
}



status_t
_TDL_SpawnStatementData::allocate()
{
  _TDL_OnAgent *                  onAgentConstraint;
  _TDL_SpawnStatementData::FLAGS  state = getState();

  if (   ( state == _TDL_SpawnStatementData::ALLOCATED )
      && (     getSpawnStatement() . getAllocationFunction() . getType()
	    == _TDL_AllocationFunction::DELAYED_ALLOCATION ) )
    state = _TDL_SpawnStatementData::NOT_ALLOCATED;

  switch ( state )
  {
    case _TDL_SpawnStatementData::NOT_ALLOCATED:

	/* Do we have an on-agent constraint? */
      onAgentConstraint = findOnAgentConstraint();

	/* Lets double check everything, and allocate the beast... */
      switch ( getSpawnStatement() . getAllocationFunction() . getType() )
      {
	case _TDL_AllocationFunction::LOCAL_NONDISTRIBUTED_ONLY:
	  if ( onAgentConstraint != (_TDL_OnAgent *) NULL )
	  {
	    TDL::getLogStream()
	      << "[_TDL_SpawnStatementData::allocate(\"" << getName()
	      << "\")]  WARNING:  On-Agent constraint (ON \""
	      << onAgentConstraint -> getAgentName()
	      << "\") found for local-invocation-only Task.  "
	      << "This *should* *NOT* have been possible.  "
	      << "PLEASE REPORT THIS ERROR!!!  (Continuing as local Task.)"
	      << endl;
	  }
	  setTCMTaskTreeDirectly (
	    getSpawnStatement()
	      . getAllocationFunction()
	      . invokeFunction ( getSpawnStatement()
				   . getTcmTaskTreeNodeName() ) );
	  setCachedOnAgentConstraint ( (_TDL_OnAgent *) NULL );
	  break;


	case _TDL_AllocationFunction::EITHER_LOCAL_OR_DISTRIBUTED:
		/* We're pretty much cool with anything here... */
	  setTCMTaskTreeDirectly (
	    getSpawnStatement()
	      . getAllocationFunction()
	      . invokeFunction ( getSpawnStatement()
				   . getTcmTaskTreeNodeName(),

				 ( (onAgentConstraint == (_TDL_OnAgent *) NULL)
				  ? STRING(NULL)
				  : onAgentConstraint -> getAgentName()
				 )
			       )
	    );
	  setCachedOnAgentConstraint ( onAgentConstraint );
	  break; 


	case _TDL_AllocationFunction::DISTRIBUTED_ONLY:
	  if ( onAgentConstraint == (_TDL_OnAgent *) NULL )
	  {
	    TDL::getLogStream()
	      << "[_TDL_SpawnStatementData::allocate(\"" << getName()
	      << "\")]  ERROR:  No On-Agent constraint was found for this "
	      << "distributed-invocation-only Task.  "
	      << "This *should* *NOT* have been possible.  "
	      << "PLEASE REPORT THIS ERROR!!!  (Aborting...)"
	      << endl;
	    return FAILURE;
	  }
	  else
	  {
	    setTCMTaskTreeDirectly (
	      getSpawnStatement()
	        . getAllocationFunction()
	        . invokeFunction ( getSpawnStatement()
				     . getTcmTaskTreeNodeName(),
				   onAgentConstraint -> getAgentName() ) );
	    setCachedOnAgentConstraint ( onAgentConstraint );
	  }
	  break;


	  /* Nothing to store here.
	   * Returns failure, as we don't allocate.
	   * Note: These strings -- name/agentName -- must/will be persistent
	   *       for the lifespan of the _TDL_AllocationFunction object.
	   */
	case _TDL_AllocationFunction::DELAYED_ALLOCATION:
	  setCachedOnAgentConstraint ( onAgentConstraint );
	  spawnState = _TDL_SpawnStatementData::ALLOCATED;
	  return FAILURE;


	default:
	  TDL::getLogStream()
	    << "[_TDL_SpawnStatementData::allocate(\"" << getName()
	    << "\")]  ERROR:  getAllocationFunction() is NULL!!!  Aborting..."
	    << endl;
	  return FAILURE;

      } /* switch ( getSpawnStatement().getAllocationFunction().getType() ) */


	/* It's not paranoia if the compiler/user is really out to get you...*/
      if ( getNodeHandle() . isNull() )
      {
	TDL::getLogStream()
	  << "[_TDL_SpawnStatementData::allocate(\"" << getName() << "\")]  "
	  << "Warning:  getAllocationFunction().invokeFunction() "
	  << "has returned NULL!  Aborting..."
	  << endl;
	return FAILURE;
      }
      spawnState = _TDL_SpawnStatementData::ALLOCATED;
      return SUCCESS;


    case _TDL_SpawnStatementData::ALLOCATED:
    case _TDL_SpawnStatementData::RUNNING:
	/* It's already allocated.  What are we going to do?  Fail? */
      return SUCCESS;


    case _TDL_SpawnStatementData::DESTROYED:
      TDL::getLogStream()
	<< "[_TDL_SpawnStatementData::allocate(\"" << getName()
	<< "\")]  Warning:  Node has already been destroyed!" << endl;
      return FAILURE;

    case _TDL_SpawnStatementData::DELETED:
      TDL::getLogStream()
	<< "[_TDL_SpawnStatementData::allocate(\"" << getName()
	<< "\")]  ERROR:  Object has already been DELETED!" << endl;
      return FAILURE;

    default:
      TDL::getLogStream()
	<< "[_TDL_SpawnStatementData::allocate(\"" << getName()
	<< "\")]  ERROR:  Unknown state: " << int4(getState()) << endl;
      return FAILURE;
  }
}



status_t
_TDL_SpawnStatementData::destroy ( BOOLEAN theCanStillBeReferenced /*=FALSE*/ )
{
	/* Might as well note that we have been destroyed. */
  for ( _TDL_Snode * node  = getSpawnStatementTreeNodeSlist() . getFirstNode();
		     node != (_TDL_Snode *) NULL;
		     node  = node -> _TDL_Snode::getNextNode() )
  {
    ((_TDL_SpawnStatementTreeNode *) node) -> setIsDestroyedSubtree();
  }


  switch ( getState() )
  {
    case _TDL_SpawnStatementData::NOT_ALLOCATED:
	/* If we are not using this node anymore
         *    Or (if we need to keep the allocated node handle around
	 *        AND we can't allocate it... )
	 */
      if (   (     theCanStillBeReferenced == FALSE     )
	  || (   ( theCanStillBeReferenced == TRUE    )
	      && ( allocate()              == FAILURE ) ) )
      {
		/* If the allocate() returned FAILURE */
	if ( theCanStillBeReferenced == TRUE )
	{
	  TDL::getLogStream()
	    << "[_TDL_SpawnStatementData::destroy(\"" << getName() << "\")]  "
	    << "WARNING:  Unable to allocate() node before destroy()'ing "
	    << "it.  Using destroyed node of NULL!" << endl;
	}
	spawnState = _TDL_SpawnStatementData::DESTROYED;
	getNodeHandleNonConst() . operator= ( (Task_Tree_Node *) NULL );
	return SUCCESS;
      }

      /* NO BREAK STATEMENT.  Continue on with "ALLOCATED" case. */

    case _TDL_SpawnStatementData::ALLOCATED:
      if (   ( getNodeHandle() . isNull()                               )
	  || ( TCM_DeallocateNode ( getNodeHandleNonConst() ) == TCM_Ok ) )
      {
	spawnState = _TDL_SpawnStatementData::DESTROYED;
	if ( theCanStillBeReferenced == FALSE )
	  getNodeHandleNonConst() . clear();
	return SUCCESS;
      }
      else
      {
	TDL::getLogStream()
	  << "[_TDL_SpawnStatementData::destroy(\"" << getName()
	  << "\")]  FAILURE:  TCM_DeallocateNode did *NOT* return TCM_Ok.  "
	  << "\"destroy()\" operation aborted!" << endl;
	return FAILURE;
      }


    case _TDL_SpawnStatementData::RUNNING:
      TDL::getLogStream()
	<< "[_TDL_SpawnStatementData::destroy(\"" << getName()
	<< "\")]  ERROR:  Object is currently running.  "
	<< "Destruction must occur *BEFORE* it starts running!"
	<< "\"destroy()\" operation aborted!" << endl;
      return FAILURE;


    case _TDL_SpawnStatementData::DESTROYED:
	/* It's already Destroyed.  What are we going to do?  Fail? */
      return SUCCESS;


    case _TDL_SpawnStatementData::DELETED:
      TDL::getLogStream()
	<< "[_TDL_SpawnStatementData::destroy(\"" << getName()
	<< "\")]  ERROR:  Object has already been DELETED!" << endl;
      return FAILURE;

    default:
      TDL::getLogStream()
	<< "[_TDL_SpawnStatementData::destroy(\"" << getName()
	<< "\")]  ERROR:  Unknown state: " << int4(getState()) << endl;
      return FAILURE;
  }
}



status_t
_TDL_SpawnStatementData::destroyIfUnused ( BOOLEAN theCanStillBeReferenced  )
{								  /* = FALSE */
	/* Might as well note that we have been destroyed. */
  for ( _TDL_Snode * node  = getSpawnStatementTreeNodeSlist() . getFirstNode();
		     node != (_TDL_Snode *) NULL;
		     node  = node -> _TDL_Snode::getNextNode() )
  {
    ((_TDL_SpawnStatementTreeNode *) node) -> setIsDestroyedSubtree();
  }

  if ( isStarted() == TRUE )
    return SUCCESS;
  else
    return destroy ( theCanStillBeReferenced );
}


status_t
_TDL_SpawnStatementData::doDestroy ( BOOLEAN   theCanStillBeReferenced,
				     BOOLEAN   theDestroyIfUnused      )
{
  if ( theDestroyIfUnused == TRUE )
    return destroyIfUnused ( theCanStillBeReferenced );
  else
    return destroy         ( theCanStillBeReferenced );
}



status_t
_TDL_SpawnStatementData::startRunningNullSpawnStatementData()
{
  if ( getState() == _TDL_SpawnStatementData::RUNNING )
    return SUCCESS;

  if (   (   ( getState()   == _TDL_SpawnStatementData::NOT_ALLOCATED     )
	  || ( getState()   == _TDL_SpawnStatementData::ALLOCATED         ) )

      && ( getSpawnStatement() . getAllocationFunction() . isNull() == TRUE ) )
  {
    getNodeHandleNonConst() = NULL;
    spawnState = _TDL_SpawnStatementData::RUNNING;
    return SUCCESS;
  }
  else
  {
    return FAILURE;
  }
}



	/* Allow automatic conversion to TCM_Task_Tree_Ref.
	 * (Will also transparently allocate() unallocated instances,
	 *  performing lazy allocation.)
	 */
_TDL_SpawnStatementData::operator const TCM_Task_Tree_Ref & ()
{
  if ( getState() == _TDL_SpawnStatementData::NOT_ALLOCATED )
  {
	/* Failure is expected for delayed-allocation allocate() */
    if (   ( allocate()                                    == FAILURE )
	&& ( getSpawnStatement() . getAllocationFunction()
	                         . isDelayedAllocation()   == FALSE   ) )

    {
      TDL::getLogStream()
	<< "[_TDL_SpawnStatementData::operator const TCM_Task_Tree_Ref & (\""
	<< getName() << "\")]  ERROR:  allocate() FAILED!!!" << endl;
    }
  }
  return getNodeHandle();
}


	/* iostreamBase interface. */
/*virtual*/ ostream &
_TDL_SpawnStatementData::printObject (
				   ostream    & theOstream,
				   const char * theIndentString /*=""*/ ) const
{
  char  subIndentString [ iostreamBase::MAXIMUM_INDENT ];
  iostreamBase::createSubIndentString ( subIndentString, theIndentString );

  theOstream << theIndentString << "-Begin- _TDL_SpawnStatementData  "
	     << ((void *)this)  << endl

	     << theIndentString
	     << " Associated with: " << ((void *) & (getSpawnStatement()) )
	     << "  ( \"" << getName() << "\" )" << endl

	     << theIndentString << " State..........: \""
	     << getStateString() << "\"  (" << int4(getState()) << ")" << endl

	     << theIndentString << " handleRef......: "
	     << ((void *)  ( getNodeHandle() . operator*() )) << endl

	     << theIndentString << " SpawnStatementTreeNodes:" << endl;

	/* Do this manually here to avoid an endless loop. */
  for ( _TDL_Snode * node  = getSpawnStatementTreeNodeSlist() . getFirstNode();
	             node != (_TDL_Snode *) NULL;
	             node  = node -> getNextNode()
       )
  {
    theOstream << theIndentString << "   _TDL_SpawnStatementTreeNode:  "
	       << ((void *) node)  << endl;
  }

  theOstream << theIndentString << "--End-- _TDL_SpawnStatementData  "
	     << ((void *)this)  << endl;

  return theOstream;
}

