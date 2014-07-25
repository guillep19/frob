/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

#include "_TDL_HandleManager.H"

	/* Kludge for TDL_REF in generated _TDL_CreateAction_...() functions.*/
const TCM_Task_Tree_Ref &
_TDL_TrivialHandleManager::operator[] ( const char * theName ) const
{
  for ( int4 i=0;  i<_TDL_HandleManager::NUMBER_OF_TASK_NAMES;  i++ )
  {
    if ( StringEqual ( theName, _TDL_HandleManager::ENCLOSING_TASK_NAMES [i] )
	 == TRUE )
    {
      return tcmTaskTreeRef;
    }
  }
  TDL::getLogStream()
    << "[_TDL_TrivialHandleManager(\"" << taskName
    << "\"):operator[]]  Error:  No match found for \"" << theName
    << "\"." << endl;
  return nullTcmTaskTreeRef;
}


/*static*/
const char * _TDL_HandleManager::ENCLOSING_TASK_NAMES [
				     _TDL_HandleManager::NUMBER_OF_TASK_NAMES ]
		= { "_TDL_HandleManager::ENCLOSING_TASK",
		    "THIS",
		    "PARENT" /* Depreciated, but still valid. */ };

/*static*/
const char *  _TDL_HandleManager::NO_TASK_NAME = "NO-CURRENT-TASK";

/*static*/
const char *  _TDL_HandleManager::ROOT_NAME = "_TDL_HandleManager_root";

/*static*/  /* NON-CONST so we can return it later on... */
_TDL_TDLStatement &  _TDL_HandleManager::ERROR_TDL_STATEMENT
		       = _TDL_SpawnStatement::generateTrivialSpawnStatement (
					  "_TDL_HandleManager_ERROR_ELEMENT" );

/*static*/ _TDL_Constraint & _TDL_HandleManager::NO_CONSTRAINT
	      = * new _TDL_Parallel(); /* Closest thing to a NULL constraint */



/*virtual*/
_TDL_HandleManager::~_TDL_HandleManager()
{
	/* Clean up internal data structures just in case this object */
	/* is still used after it has been destroyed.                 */
  currentTreeNodeBranch = (_TDL_TreeNodeBranch *) NULL;
  clearInvokingSpawn();
}



	/* Note:  addSpawnStatement ( _TDL_SpawnStatement * ) ASSUMES *
	 * THAT theSpawnStatement has been dynamically allocated off  *
	 * the heap, and it ASSUMES THAT this _TDL_HandleManager      *
	 * *SHOULD* DELETE theSpawnStatement when it is destroyed.    */
_TDL_SpawnStatement *
_TDL_HandleManager::addSpawnStatement( _TDL_SpawnStatement * theSpawnStatement,
				       BOOLEAN               theAddToEndOfList)
{								  /* = TRUE */
  status_t  addToSpawnStatementSlistResult;

  if ( theSpawnStatement != (_TDL_SpawnStatement *) NULL )
  {
    if ( theAddToEndOfList == TRUE )
      addToSpawnStatementSlistResult
	= getSpawnStatementSlistNonConst() . appendNode  ( theSpawnStatement );
    else
      addToSpawnStatementSlistResult
	= getSpawnStatementSlistNonConst() . prependNode ( theSpawnStatement );

    if ( addToSpawnStatementSlistResult == SUCCESS )
    {
      theSpawnStatement -> setOurHandleManager ( this );
      return theSpawnStatement;
    }
    else
    {
      printHeader ( TDL::getLogStream(), "addSpawnStatement" )
	<< "Error:  getSpawnStatementSlistNonConst() . "
	<< ( ( theAddToEndOfList == TRUE ) ? "appendNode" : "prependNode" )
	<< " ( theSpawnStatement ) FAILED!!!   Aborting add."
	<< endl;
      return (_TDL_SpawnStatement *) NULL;
    }
  }
  else
  {
    printHeader ( TDL::getLogStream(), "addSpawnStatement" )
      << "Error:  theSpawnStatement is a NULL pointer!!!   Aborting add."
      << endl;
    return (_TDL_SpawnStatement *) NULL;
  }
}



	/* Convenience method */
_TDL_WithStatement * 
_TDL_HandleManager::addWithStatement ( u_int4 theInitialCapacity )
{			      /* = _TDL_ArrayList::DEFAULT_INITIAL_CAPACITY */
  return addWithStatement ( new _TDL_WithStatement ( theInitialCapacity ) );
}


	/* Note:  addWithStatement ( _TDL_WithStatement * ) ASSUMES *
	 * THAT theWithStatement has been dynamically allocated off *
	 * the heap, and it ASSUMES THAT this _TDL_HandleManager    *
	 * *SHOULD* DELETE theWithStatement when it is destroyed.   */
_TDL_WithStatement *
_TDL_HandleManager::addWithStatement ( _TDL_WithStatement * theWithStatement )
{
  if ( theWithStatement != (_TDL_WithStatement *) NULL )
  {
    if (    getWithStatementSlistNonConst() . appendNode ( theWithStatement )
	 == SUCCESS )
    {
      theWithStatement -> setOurHandleManager ( this );
      return theWithStatement;
    }
    else
    {
      printHeader ( TDL::getLogStream(), "addWithStatement" )
	<< "Error:  getWithStatementSlistNonConst() "
	<< ". appendNode ( theWithStatement ) FAILED!!!   Aborting add."
	<< endl;
      return (_TDL_WithStatement *) NULL;
    }
  }
  else
  {
    printHeader ( TDL::getLogStream(), "addWithStatement" )
      << "Error:  theWithStatement is a NULL pointer!!!   Aborting add."
      << endl;
    return (_TDL_WithStatement *)NULL;
  }
}


	/* Note:  _TDL_TDLStatement will auto-promote to a TCM_Task_Tree_Ref
	 * if it contains a _TDL_SpawnStatementTreeNode object in its
	 * tree-(array)-hierarchy.  In the event that it does not
	 * contain a _TDL_SpawnStatementTreeNode, an empty
	 * TCM_Task_Tree_Ref (_TDL_TreeNode::EMPTY_TCM_TASK_TREE_REF)
	 * will be auto-promoted.
	 * See also:  _TDL_TreeNode::containsSpawnStatementTreeNode()
	 *
	 * In the event that nothing is found named "theName",
	 * _TDL_HandleManager::ERROR_TDL_STATEMENT is returned.
	 */
_TDL_TDLStatement &
_TDL_HandleManager::operator[] ( const char * theName ) const
{
  _TDL_TDLStatement * tdlStatement = findTDLStatement ( theName );

  if ( tdlStatement != (_TDL_TDLStatement *) NULL )
  {
    tdlStatement -> resetCachedLookup();
    return * tdlStatement;
  }
  else
  {
    printHeader ( TDL::getLogStream(), "operator[]" )
      << "Error:  No match found for \"" << theName << "\"." << endl;

    return _TDL_HandleManager::ERROR_TDL_STATEMENT;
  }
}


_TDL_SpawnStatement *
_TDL_HandleManager::findSpawnStatement (const char * theName) const
{
	/* Idiocy check */
  if ( theName == (const char *)NULL )
    return (_TDL_SpawnStatement *) NULL;

	/* Is this a reference to our parent? */
  if ( getEnclosingTask() . hasName ( theName ) )
    return & getEnclosingTask();

	/* Find Spawn-Statement named theName */
  for ( _TDL_Snode * node  = getSpawnStatementSlist() . getFirstNode();
	             node != (_TDL_Snode *) NULL;
	             node  = node -> getNextNode()
       )
  {
    if ( ((_TDL_SpawnStatement *) node) -> hasName ( theName ) )
    {
      return (_TDL_SpawnStatement *) node;
    }
  }

  return (_TDL_SpawnStatement *) NULL;
}


_TDL_WithStatement * 
_TDL_HandleManager::findWithStatement ( const char * theName ) const
{
	/* Idiocy check */
  if ( theName == (const char *)NULL )
    return (_TDL_WithStatement *) NULL;

	/* Find the branch named theName */
  for ( _TDL_Snode * node  = getWithStatementSlist() . getFirstNode();
	             node != (_TDL_Snode *) NULL;
	             node  = node -> getNextNode()
       )
  {
    if ( ((_TDL_WithStatement *) node) -> hasName ( theName ) )
    {
      return (_TDL_WithStatement *) node;
    }
  }

  return (_TDL_WithStatement *) NULL;
}



_TDL_TDLStatement *
_TDL_HandleManager::findTDLStatement ( const char * theName ) const
{
  _TDL_SpawnStatement * spawnStatement = findSpawnStatement ( theName );

  if ( spawnStatement != (_TDL_SpawnStatement *) NULL )
    return spawnStatement;

  else
    return findWithStatement ( theName );
}



	/* Convenience methods for invoking destroy */
status_t
_TDL_HandleManager::destroy ( const char * theName,
			      BOOLEAN      theCanStillBeReferenced /*=FALSE*/ )
{
  _TDL_TDLStatement * tdlStatement = findTDLStatement ( theName );

  if ( tdlStatement != (_TDL_TDLStatement *) NULL )
  {
    return tdlStatement -> destroy ( getCurrentIterationIndexes(),
				     theCanStillBeReferenced       );
  }
  else
  {
    printHeader ( TDL::getLogStream(), "destroy" )
      << "Error:  No match found for \"" << theName
      << "\".  Returning FAILURE." << endl;

    return FAILURE;
  }
}


	/* Convenience methods for invoking destroy */
status_t
_TDL_HandleManager::destroyIfUnused ( const char * theName,
				      BOOLEAN      theCanStillBeReferenced
								   /*=FALSE*/ )
{
  _TDL_TDLStatement * tdlStatement = findTDLStatement ( theName );

  if ( tdlStatement != (_TDL_TDLStatement *) NULL )
  {
    return tdlStatement -> destroyIfUnused ( getCurrentIterationIndexes(),
					     theCanStillBeReferenced       );
  }
  else
  {
    printHeader ( TDL::getLogStream(), "destroyIfUnused" )
      << "Error:  No match found for \"" << theName
      << "\".  Returning FAILURE." << endl;

    return FAILURE;
  }
}







	/* Descends into an iteration loop */
_TDL_IterationIndex *
_TDL_HandleManager::pushIteration()
{
  _TDL_IterationIndex *  iterationIndex
    = (_TDL_IterationIndex *) (getUnusedIterationIndexesNonConst() . pop());

  if ( iterationIndex == ((_TDL_IterationIndex *) NULL) )
    iterationIndex = new _TDL_IterationIndex();
  else
    iterationIndex -> reset();

  getCurrentIterationIndexesNonConst() . push ( iterationIndex );

  return iterationIndex;
}

	/* Increments the current iteration loop */
status_t
_TDL_HandleManager::incrementIteration()
{
  _TDL_IterationIndex *  iterationIndex
    = (_TDL_IterationIndex *) (getCurrentIterationIndexes() . topOfStack());

  if ( iterationIndex == ((_TDL_IterationIndex *) NULL) )
  {
    printHeader ( TDL::getLogStream(), "incrementIteration", "" )
      << "Error:  No index available to increment." << endl;
    return FAILURE;
  }
  else
  {
    iterationIndex -> incrementIndex();
    return SUCCESS;
  }
}

	/* Ascends up out of an iteration loop. */
status_t
_TDL_HandleManager::popIteration()
{
  _TDL_IterationIndex *  iterationIndex
    = (_TDL_IterationIndex *) (getCurrentIterationIndexesNonConst() . pop());

  if ( iterationIndex == ((_TDL_IterationIndex *) NULL) )
  {
    printHeader ( TDL::getLogStream(), "popIteration", "" )
      << "Error:  No index available to pop." << endl;
    return FAILURE;
  }
  else
  {
    iterationIndex -> reset();
    getUnusedIterationIndexesNonConst() . push ( iterationIndex );
    return SUCCESS;
  }
}



	/* Descends into a WITH Statement */
status_t
_TDL_HandleManager::pushWithStatement ( const char * theWithStatementName )
{
  _TDL_WithStatement     * withStatement
			     = findWithStatement ( theWithStatementName );
  _TDL_WithStatementData * withStatementData;

	/* Idiocy check */
  if ( theWithStatementName == (const char *) NULL )
  {
    printHeader ( TDL::getLogStream(), "pushWithStatement", "" )
      << "Error:  theWithStatementName is NULL.   "
      << "Aborting pushWithStatement..." << endl;
    return FAILURE;
  }

	/* If there is a real problem, warn & try to recover... */
  if ( currentTreeNodeBranch == (_TDL_TreeNodeBranch *) NULL )
  {
    printHeader ( TDL::getLogStream(), "pushWithStatement", "" )
      << "Warning:  Current-TreeNodeBranch is NULL.  "
      << "Reseting back to ROOT-TreeNodeBranch..." << endl;
    currentTreeNodeBranch = getRootTreeNodeBranchNonConst();
  }

	/* Find the branch named theBranchName */
  if ( withStatement == (_TDL_WithStatement *) NULL )
  {
    printHeader ( TDL::getLogStream(), "pushWithStatement", "" )
      << "Error:  No With-Statement with name \"" << theWithStatementName
      << "\"." << endl;
    return FAILURE;
  }


	/* If we have already used this node... */
  if (   (    withStatement
	        -> getWithStatementDataConst ( getCurrentIterationIndexes () )
	   != (_TDL_WithStatementData *) NULL
	  )
      && (    withStatement
	        -> getWithStatementDataConst ( getCurrentIterationIndexes () )
	        -> getParent()
	   != (_TDL_TreeNodeBranch *) NULL
	  )
      )
  {
    printHeader ( TDL::getLogStream(), "pushWithStatement", "" )
      << "Error:  With-Statement has already been used.  "
      << "Aborting pushWithStatement..." << endl;
    return FAILURE;
  }


	/* Create our new withStatementData */
  withStatementData
    = withStatement -> getWithStatementData ( getCurrentIterationIndexes() );

	/* Double-check that. */
  if ( withStatementData == (_TDL_WithStatementData *) NULL )
  {
    printHeader ( TDL::getLogStream(), "pushWithStatement", "" )
      << "Error:  FAILED to create new With-Statement-Data.  "
      << "Aborting pushWithStatement..." << endl;
    return FAILURE;
  }

	/* Set up the pointers */
  withStatementData     -> setParent ( currentTreeNodeBranch );
  currentTreeNodeBranch -> addChild  ( withStatementData     );

	/* Reset the currentTreeNodeBranch to descend into withStatementData */
  currentTreeNodeBranch = withStatementData;

  return SUCCESS;
}



	/* Ascends up out of a WITH Statement */
status_t
_TDL_HandleManager::popWithStatement ( const char * theWithStatementName )
{
	/* Idiocy check */
  if ( theWithStatementName == (const char *) NULL )
  {
    printHeader ( TDL::getLogStream(), "popWithStatement", "" )
      << "Error:  theWithStatementName is NULL.   Aborting popWithStatement..."
      << endl;
    return FAILURE;
  }

	/* More Idiocy */
  if (    currentTreeNodeBranch
       == getRootTreeNodeBranch().getTreeNodeBranchConst() )
  {
    printHeader ( TDL::getLogStream(), "popWithStatement", "" )
      << "Error:  Currently at ROOT TreeNodeBranch.  popWithStatement aborted."
      << endl;
    return FAILURE;
  }

	/* If there is a real problem, warn & try to recover... */
  if ( currentTreeNodeBranch == (_TDL_TreeNodeBranch *) NULL )
  {
    printHeader ( TDL::getLogStream(), "popWithStatement", "" )
      << "Error:  Current-TreeNodeBranch is NULL.  "
      << "Reseting back to ROOT-TreeNodeBranch..." << endl;
    currentTreeNodeBranch = getRootTreeNodeBranchNonConst();
    return FAILURE;
  }

	/* If we are at this branch, pop */
  if ( currentTreeNodeBranch -> hasName ( theWithStatementName ) )
  {
	/* And pop us up one branch-level */
    currentTreeNodeBranch = currentTreeNodeBranch -> getParent();
    return SUCCESS;
  }


  printHeader ( TDL::getLogStream(), "popWithStatement", "" )
    << "Error:  Current TreeNodeBranch is not named \""
    << theWithStatementName << "\".  It is named \""
    << getCurrentTreeNodeBranch() -> getName() << "\"." << endl;
  return FAILURE;
}



    /* Used to completely (& uniformly) cancel invoking of the Current-Spawn */
void	/* (Aborts Invoking of the Current-Spawn) */
_TDL_HandleManager::clearInvokingSpawn()
{
	/* If the current Task is not yet running, try to destroy it
	 * so that any task that depends on this task can still be run...
	 */
  if (   ( getIsStartingTask()                == TRUE  )
      && ( getCurrentTask() -> isNotStarted() == FALSE ) )
  {
    getCurrentTask() -> destroy();
  }

	/* Try to remove it from the tree... */
  if (   ( getIsStartingTask()             == TRUE                         )
      && ( getCurrentTask() -> getParent() != (_TDL_TreeNodeBranch *) NULL ) )
  {
    getCurrentTask() -> getParent() -> removeChild ( getCurrentTask() );
  }

	/* And break the Task's link (pointer) to the tree. */
  if ( getIsStartingTask() == TRUE )
  {
    getCurrentTask() -> setParent ( (_TDL_TreeNodeBranch *) NULL );
  }

	/* Do *NOT* attempt to deallocate the currentTask, or alter its  *
	 * internal array-tree structure.  This way, we have a chance in *
	 * hell of debugging the code if something does go wrong...      */
 
  finishInvokingSpawn();
}


	/* Completes Invoking of the Current-Spawn */
void
_TDL_HandleManager::finishInvokingSpawn()
{
  setCurrentTaskName ( _TDL_HandleManager::NO_TASK_NAME );
  currentTask  = (_TDL_SpawnStatementTreeNode *) NULL;
  insertedTask = FALSE;
  clearTcmTaskTreeParent();
}


	/* Used to start invoking a SPAWN statement.  Sets the Current-Spawn.
	 * (Sets the state to enable setAction, applyConstraint, & insertSpawn.
	 *  If any of them fail, further activity in those methods is disabled
	 *  until another startInvoking() call is made...)
	 */
status_t
_TDL_HandleManager::startInvokingSpawn (
			     const char              * theSpawnName,
			     const _TDL_Constraint   & theOptionalConstraint,
				     /* = _TDL_HandleManager::NO_CONSTRAINT*/
			     const TCM_Task_Tree_Ref & theTcmTaskTreeParent
				     /* = NULL */                            )
{
  _TDL_SpawnStatementData     * spawnStatementData;
  _TDL_SpawnStatementTreeNode * spawnStatementTreeNode;

	/* Set _TDL_HandleManager default state... */
  clearInvokingSpawn();

	/* Lets only do this in one place... */
  spawnStatementData = getSpawnStatementData (
				theSpawnName,
				"startInvokingSpawn/getSpawnStatementData",
				(   ( theSpawnName != (const char *) NULL )
				  ? theSpawnName : "" ),
				TRUE, /* Abort if already started */
				theOptionalConstraint,
				TRUE /* Skip allocate() */ );

	/* IF there was a problem, abort... */
  if ( spawnStatementData == (_TDL_SpawnStatementData *) NULL )
    return FAILURE;


  	/* Create our new spawnStatementTreeNode */
  spawnStatementTreeNode
    = spawnStatementData -> createAndAttachNewSpawnStatementTreeNode();

	/* Double-check that. */
  if ( spawnStatementTreeNode == (_TDL_SpawnStatementTreeNode *) NULL )
  {
    printHeader ( TDL::getLogStream(), "startInvokingSpawn", theSpawnName )
      << "Error:  FAILED to create new Spawn-Statement-Tree-Node.  "
      << "Aborting startInvokingSpawn()..." << endl;
    return FAILURE;
  }


	/* Set up the new _TDL_HandleManager state... */
  setCurrentTaskName ( theSpawnName );
  currentTask  = spawnStatementTreeNode;
  insertedTask = FALSE;
  setTcmTaskTreeParent ( theTcmTaskTreeParent );

	/* And add this into our TDL-CODE tree-structure */
  getCurrentTreeNodeBranchNonConst() -> addChild ( spawnStatementTreeNode );
  spawnStatementTreeNode -> setParent ( getCurrentTreeNodeBranchNonConst() );

	/* Perform the allocate() here, as necessary...                   *
	 * Here, as part of the tree, allocate() can search the           *
	 * tree hierarchy for any with-statement on-agent constraints.    *
	 * Note: Failure of allocate() on delayed-allocation is expected. */
  if (   ( spawnStatementData -> allocate()                    == FAILURE )
      && ( spawnStatementData -> getSpawnStatement()
	     . getAllocationFunction() . isDelayedAllocation() == FALSE   ) )
  {
    printHeader ( TDL::getLogStream(), "startInvokingSpawn", theSpawnName )
      << "Error:  Unable to allocate() SpawnStatementData.  "
	<< "Aborting startInvokingSpawn()..." << endl;
    return FAILURE;
  }

  return SUCCESS;
}



	/* If the result is not TCM_Ok, it disables further activity on
	 * the Current-Spawn and prints a warning message.
	 */
status_t
_TDL_HandleManager::setAction ( const _TDL_ActionOrVoid & theActionOrVoid )
{
  TCM_Return_Type  result;

	/* Idiocy check */
  if ( getIsStartingTask() != TRUE )
  {
    printHeader ( TDL::getLogStream(), "setAction", getCurrentTaskName() )
      << "Error:  No Current-Spawn.  startInvokingSpawn() needs to be invoked."
      << "  Aborting rest of Task Insertion." << endl;
    clearInvokingSpawn();
    return FAILURE;
  }

	/* Idiocy check */
  if ( getHasInsertedTask() == TRUE )
  {
    printHeader ( TDL::getLogStream(), "setAction", getCurrentTaskName() )
      << "Error:  The Current-Task has already been insertSpawn()'ed.  "
      << "Aborting rest of Task Insertion." << endl;
    clearInvokingSpawn();
    return FAILURE;
  }

	/* Delayed Allocation failure, or perhaps a more general failure... */
  if ( getCurrentTaskTcmTaskTreeRef() . isNull() )
  {
    printHeader ( TDL::getLogStream(), "setAction", getCurrentTaskName() )
      << "Error:  The Current-Task has a NULL TCM_Task_Tree_Ref.  "
      << "Aborting rest of Task Insertion." << endl;
    clearInvokingSpawn();
    return FAILURE;
  }


	/* Is it an Action?  (Local-Task) */
  if ( theActionOrVoid . getIsActionNotVoid() == TRUE )
  {
    if ( _TDL_DO_CHECK_IF_TASK_DISTRIBUTED ( * getCurrentTask() ) == FALSE )
    {
	/* Set the action... */
      result = _TDL_DO_SET_ACTUAL_ACTION ( * getCurrentTask(),
					     theActionOrVoid   );
        /* Did setAction fail? */
      if ( result != TCM_Ok )
      {
	printHeader ( TDL::getLogStream(), "setAction", getCurrentTaskName() )
	  << "Error:  TCM_SetActualAction() failed with a value of "
	  << int4(result) << ".   Aborting rest of Task Insertion." << endl;
	clearInvokingSpawn();
	return FAILURE;
      }
    }
    else /* E.g.: getCurrentTask() is DISTRIBUTED */
    {
      printHeader ( TDL::getLogStream(), "setAction", getCurrentTaskName() )
	<< "Internal Error:  Attempting to set normal (Action *) onto "
	<< "DISTRIBUTED TCM_Task_Tree_Ref node.   "
	<< "Aborting rest of Task Insertion." << endl;
      clearInvokingSpawn();
      return FAILURE;
    }
  }

       /* Nope.  It's a Void.  (Distributed-Task) */
  else /* IF ( theActionOrVoid . getIsActionNotVoid() == TRUE ) ... ELSE ... */
  {
    if ( _TDL_DO_CHECK_IF_TASK_DISTRIBUTED ( * getCurrentTask() ) == FALSE )
    {
      printHeader ( TDL::getLogStream(), "setAction", getCurrentTaskName() )
	<< "Internal Error:  Attempting to set DISTRIBUTED (void *) "
	<< "action-data onto a normal (non-distributed) TCM_Task_Tree_Ref "
	<< "node.   Aborting rest of Task Insertion." << endl;
      clearInvokingSpawn();
      return FAILURE;
    }

    else if ( theActionOrVoid . hasDistributedActionFunction() == FALSE )
    {
      printHeader ( TDL::getLogStream(), "setAction", getCurrentTaskName() )
	<< "Internal Error:  SetDistributedActionFunction is NULL!  "
	<< "Aborting rest of Task Insertion." << endl;
      clearInvokingSpawn();
      return FAILURE;
    }

    else /* E.g.: getCurrentTask() is DISTRIBUTED */
    {
	/* Set the distributed action data... */
      result = ( * theActionOrVoid . getDistributedActionFunction() )
	       ( * getCurrentTask(),
		   theActionOrVoid . getVoidPointer(),
		   theActionOrVoid . getOverloadedTaskNameIndex() );

	    /* Did set-distributed-action fail? */
      if ( result != TCM_Ok )
      {
	printHeader ( TDL::getLogStream(), "setAction", getCurrentTaskName() )
	  << "Error:  SetDistributedActionFunction() failed with a value of "
	  << int4(result) << ".   Aborting rest of Task Insertion." << endl;
	clearInvokingSpawn();
	return FAILURE;
      }
    }

	/* Cleanup to remove the obvious core leak. */
    if (   ( theActionOrVoid . getShouldDeleteVoidPointer() == TRUE         )
	&& ( theActionOrVoid . getVoidPointer()             != (void *)NULL ) )
    {
      free ( theActionOrVoid . getVoidPointer() );
    }

  } /* IF ( theActionOrVoid . getIsActionNotVoid() == TRUE ) ... ELSE ... */


  return SUCCESS;
}


	/* Applies theConstraint to the Current-Spawn.  If problem(s) arise,
	 * it disables futher activity and prints a warning message.
	 */
status_t
_TDL_HandleManager::applyConstraint ( const _TDL_Constraint & theConstraint )
{
	/* Idiocy check */
  if ( getIsStartingTask() != TRUE )
  {
    printHeader( TDL::getLogStream(), "applyConstraint", getCurrentTaskName() )
      << "Error:  No Current-Spawn.  startInvokingSpawn() needs to be invoked."
      << "  Aborting rest of Task Insertion." << endl;
    clearInvokingSpawn();
    return FAILURE;
  }

	/* Safety check:  The on-agent constraint is "special".  It won't   *
	 * work if the node is not distributable, or is already allocated.  *
	 * (And here, in this method, it's always already allocated!)       */
  if ( theConstraint . isOnAgentConstraint() == TRUE )
  {
    if ( getCurrentTask() -> isDistributable() == FALSE )
    {
      printHeader ( TDL::getLogStream(), "applyConstraint" )
	<< "Warning:  Internal Error:  Attempting to apply ON-AGENT "
	<< "constraint to NON-Distributable Task!!!  Looks like trouble!"
	<< endl;
    }

    if ( getCurrentTask() -> isAllocated() == TRUE )
    {
      printHeader ( TDL::getLogStream(), "applyConstraintTo" )
	<< "Warning:  Internal Error:  Attempting to apply ON-AGENT "
	<< "constraint to Task that is ALREADY ALLOCATED!!!  "
	<< "This is not going to work!" << endl;
    }
  } /* if ( theConstraint . isOnAgentConstraint() == TRUE ) */



	/* Is this a pre-insertion or post-insertion constraint-application? */
  if ( getHasInsertedTask() == FALSE )
  {
    if ( theConstraint
	   . applyConstraintsBeforeInsertion ( * getCurrentTask(),
					       (_TDL_TreeNode *) NULL,
					       TRUE )
	 != TCM_Ok )
    {
      printHeader(TDL::getLogStream(), "applyConstraint", getCurrentTaskName())
	<< "Error:  Failure occured while applying [before-insertion] "
	<< "constraint.  Aborting rest of Task Insertion..." << endl;
      clearInvokingSpawn();
      return FAILURE;
    }
  }
  else
  {
    if ( theConstraint
	   . applyConstraintsAfterInsertion ( * getCurrentTask(),
					      (_TDL_TreeNode *) NULL,
					      TRUE )
	 != TCM_Ok )
    {
      printHeader(TDL::getLogStream(), "applyConstraint", getCurrentTaskName())
	<< "Error:  Failure occured while applying [after-insertion] "
	<< "constraint.  Aborting rest of Task Insertion..." << endl;
      clearInvokingSpawn();
      return FAILURE;
    }
  }

  return SUCCESS;
}




 /* Internal routine to walk up the trees,
  * applying any and all With-Do/Iteration constraints.
  *
  * Note:  Almost verbatim equivalent of
  *        _TDL_SpawnStatementData::findOnAgentConstraint().
  */
static void
applyInsertionConstraintsFromAllParentTreeNodes (
			   _TDL_SpawnStatementTreeNode * theCurrentTask,
			   BOOLEAN                       theIsBeforeInsertion )
{
  const _TDL_SpawnStatementTreeNode * targetSpawnTreeNode;
  const _TDL_TreeNode               * spawnTreeNode;
  const _TDL_TreeNodeBranch         * spawnTreeNodeBranch;
  _TDL_Constraint                   * constraint;
  const _TDL_WithStatementTreeNode  * targetWithTreeNode;
  const _TDL_TreeNode               * withTreeNode;
  const _TDL_TreeNodeBranch         * withTreeNodeBranch;


      /* Record that we should now enable constraint overriding */
  theCurrentTask -> getSpawnStatementData()
		 .  setShouldOverrideConstraints ( TRUE );


      /* Apply constraints cached on current _TDL_SpawnStatementData */
  for ( constraint  = (_TDL_Constraint *)
		      ( theCurrentTask -> getSpawnStatementData()
				       .  getConstraintSlist()
				       .  getFirstNode() );
	constraint != (_TDL_Constraint *) NULL;
	constraint  = (_TDL_Constraint *) ( constraint -> getNextNode() )
       )
  {
    if ( theIsBeforeInsertion == TRUE )
      constraint -> applyConstraintsBeforeInsertion ( * theCurrentTask,
						      theCurrentTask,
						      FALSE );
    else
      constraint -> applyConstraintsAfterInsertion  ( * theCurrentTask,
						      theCurrentTask,
						      FALSE );
  }


      /* Walk up the tree, applying With-Do/Iteration constraints */
  for ( targetSpawnTreeNode  = (const _TDL_SpawnStatementTreeNode *)
				     ( theCurrentTask
					  -> getSpawnStatementData()
				          .  getSpawnStatementTreeNodeSlist()
				          .  getFirstNode() )
	  ;
	targetSpawnTreeNode != (const _TDL_SpawnStatementTreeNode *) NULL
	  ;
	targetSpawnTreeNode  = (const _TDL_SpawnStatementTreeNode *)
				     ( targetSpawnTreeNode
				          -> _TDL_Snode::getNextNode() )
       )
  {
    spawnTreeNode       = targetSpawnTreeNode;
    spawnTreeNodeBranch = targetSpawnTreeNode -> getParent();
    
    while ( spawnTreeNodeBranch != (const _TDL_TreeNodeBranch *)NULL )
    {
	/* Apply these constraints. */
      for ( constraint  = (_TDL_Constraint *)
		             ( spawnTreeNodeBranch -> getConstraintSlist()
						   .  getFirstNode() );
	    constraint != (_TDL_Constraint *) NULL;
	    constraint  = (_TDL_Constraint *) ( constraint -> getNextNode() )
	   )
      {
	if ( theIsBeforeInsertion == TRUE )
	  constraint -> applyConstraintsBeforeInsertion ( * theCurrentTask,
						            spawnTreeNode,
						            FALSE );
	else
	  constraint -> applyConstraintsAfterInsertion  ( * theCurrentTask,
						            spawnTreeNode,
						            FALSE );
      }

	/* If we have a With-Statement
	 * Apply any iteration-tree constraints on this with-statement,
	 */
      if ( spawnTreeNodeBranch -> isWithStatementData() )
      {
	  /* Note:  getWithStatementTreeNodeSlist() contains only one
	   * TreeNode -- which corresponds to the TDL Iteration Index Tree.
	   * E.g.: All WithStatementTreeNode's should be located inside
	   * the TDL Iteration Tree(s).  WithStatementData's are what is
	   * located inside TDL CODE Tree's.
	   */

	  /* Walk up the iteration tree, applying Iteration constraints */
	for ( targetWithTreeNode  = (const _TDL_WithStatementTreeNode *)
				        ( spawnTreeNodeBranch
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
		/* Apply any constraints. */
	    for ( constraint  = (_TDL_Constraint *)
			           ( withTreeNodeBranch -> getConstraintSlist()
						        .  getFirstNode() );
		  constraint != (_TDL_Constraint *) NULL;
		  constraint  = (_TDL_Constraint *)
				   ( constraint -> getNextNode() )
		 )
	    {
	      if ( theIsBeforeInsertion == TRUE )
		constraint
		  -> applyConstraintsBeforeInsertion ( * theCurrentTask,
						         withTreeNode,
						         FALSE );
	      else
		constraint
		  -> applyConstraintsAfterInsertion  ( * theCurrentTask,
						         withTreeNode,
						         FALSE );
	    }

		/* Move up one level. */
	    withTreeNode       = withTreeNodeBranch;
	    withTreeNodeBranch = withTreeNodeBranch -> getParent();

	  } /* while ( withTreeNodeBranch != NULL ) */
	} /* FOR(iteration-tree-nodes associated with this WithStatementData)*/
      } /* if ( spawnTreeNodeBranch -> isWithStatementData() ) */

	/* Move up one level. */
      spawnTreeNode       = spawnTreeNodeBranch;
      spawnTreeNodeBranch = spawnTreeNodeBranch -> getParent();

    } /* while ( spawnTreeNodeBranch != (_TDL_TreeNodeBranch *)NULL ) */
  } /* FOR ( All treeNodes associated with the current SpawnStatementData ) */


      /* Record that we should now disable constraint overriding */
  theCurrentTask -> getSpawnStatementData()
		 .  setShouldOverrideConstraints ( FALSE );
}




	/* Inserts (starts) this SPAWN'ed Task.
	 * (After first applying any pending WITH-Statement constraints.)
	 * Disables further activity upon the Current-Spawn.
	 */
status_t
_TDL_HandleManager::insertSpawn ( BOOLEAN theIsLastChild /* = TRUE */ )
{
  TCM_Return_Type  insertNodeReturnValue;


	/* Idiocy check */
  if ( getIsStartingTask() != TRUE )
  {
    printHeader ( TDL::getLogStream(), "insertSpawn", getCurrentTaskName() )
      << "Error:  No Current-Spawn.  startInvokingSpawn() needs to be invoked."
      << "  Aborting rest of Task Insertion." << endl;
    clearInvokingSpawn();
    return FAILURE;
  }

	/* Idiocy check */
  if ( getHasInsertedTask() == TRUE )
  {
    printHeader ( TDL::getLogStream(), "insertSpawn", getCurrentTaskName() )
      << "Error:  The Current-Task has already been insertSpawn()'ed.  "
      << "Aborting rest of Task Insertion." << endl;
    clearInvokingSpawn();
    return FAILURE;
  }



    /* Walk up the trees, applying pre-insert With-Do/Iteration constraints */
  applyInsertionConstraintsFromAllParentTreeNodes ( getCurrentTask(), TRUE );


	/* Insert the task... */

#ifdef    _TDL_DISABLE_NODE_INSERTION_FOR_TESTING
  insertNodeReturnValue = TCM_Ok;
  _TDL_MARKUSED ( theIsLastChild );
#else  /* _TDL_DISABLE_NODE_INSERTION_FOR_TESTING */
  insertNodeReturnValue
    = TCM_InsertNode ( (   ( hasTcmTaskTreeParent() == TRUE )
			 ? getTcmTaskTreeParent()
			 : getEnclosingTaskReference()
		        ),
		       * getCurrentTask(),
		       theIsLastChild );
#endif /* _TDL_DISABLE_NODE_INSERTION_FOR_TESTING */


	/* Record that we just inserted the task in this Instance-Variable */
  insertedTask = TRUE;


	/* Test TCM_InsertNode's return value for problems. */
  if ( insertNodeReturnValue != TCM_Ok )
  {
    printHeader ( TDL::getLogStream(), "insertSpawn", getCurrentTaskName() )
      << "Error:  TCM_InsertNode failed with a value of "
      << int4(insertNodeReturnValue) << ".  Aborting rest of Task Insertion."
      << endl;
    clearInvokingSpawn();
    return FAILURE;
  }



    /* Walk up the trees, applying post-insert With-Do/Iteration constraints */
  applyInsertionConstraintsFromAllParentTreeNodes ( getCurrentTask(), FALSE );



	/* Record that we have started running. */
  if ( getCurrentTask() -> getSpawnStatementData() . startRunning() == FAILURE)
  {
    printHeader ( TDL::getLogStream(), "insertSpawn", getCurrentTaskName() )
      << "Warning:  Unable to record that Spawn'ed Task's state is now "
      << "RUNNING...   Possible state/reference corruption in libtdl code."
      << endl;
    getCurrentTask() -> printObject ( TDL::getLogStream(), "     " ) << endl;
  }

  return SUCCESS;
} /* status_t _TDL_HandleManager::insertSpawn (BOOLEAN theIsLastChild =TRUE) */



	/* Adds theConstraint to the current WITH-Statement-Data instance.
	 * (Which corresponds to a specific iteration index.)
	 * Assumes that theConstraint has been dynamically allocated off
	 * the heap, and that theConstraint should be deleted when this
	 * _TDL_HandleManager (or rather _TDL_TreeWithStatementData)
	 * object is deleted!
	 */
_TDL_HandleManager &
_TDL_HandleManager::doAddWithStatementConstraint (
					      _TDL_Constraint * theConstraint )
{
	/* If there is a problem... */
  if ( getCurrentTreeNodeBranch() == (const _TDL_TreeNodeBranch *) NULL )
  {
    printHeader ( TDL::getLogStream(),
		  "doAddWithStatementConstraint", getCurrentTaskName() )
      << "Error:  Current-TreeNodeBranch is NULL.  Unable to add constraint:"
      << endl;
    theConstraint -> printObject ( TDL::getLogStream(), "     " ) << endl;
  }

  else if ( getCurrentTreeNodeBranch() -> isWithStatementData() == TRUE )
  {
    getCurrentTreeNodeBranchNonConst() -> getWithStatementData()
				       -> addConstraint ( theConstraint );
  }

  else if ( getCurrentTreeNodeBranch() -> isWithStatementTreeNode() == TRUE )
  {
    getCurrentTreeNodeBranchNonConst() -> getWithStatementTreeNode()
				       -> getWithStatementData()
				       .  addConstraint ( theConstraint );
  }

  else
  {
    printHeader ( TDL::getLogStream(),
		  "doAddWithStatementConstraint", getCurrentTaskName() )
      << "Error:  Current-TreeNodeBranch is *NOT* a WITH-Statement.  "
      << "Unable to add constraint:" << endl;
    theConstraint -> printObject ( TDL::getLogStream(), "     " ) << endl;
  }

  return *this;
}




	/* Performs a constraint immediately, on the specified TreeNode(s).
	 * The TreeNode may be an iteration-set or a WithStatement-set,
	 * in which case this method will also clone theConstraint and attach
	 * it to the corresponding TreeNodeBranch (base-class) of that object.
	 */
status_t
_TDL_HandleManager::applyConstraintTo ( const char *            theName,
					const _TDL_Constraint & theConstraint )
{
  _TDL_TDLStatement * tdlStatement = findTDLStatement ( theName );
  _TDL_TreeNode     * treeNode;
  _TDL_Dlist          emptyIterationIndexes;

  if ( tdlStatement == (_TDL_TDLStatement *) NULL )
  {
    printHeader ( TDL::getLogStream(), "applyConstraintTO" )
      << "Error:  Unable to find any Spawn or With Statement named \""
      << theName << "\".  Constraint was *NOT* applied." << endl;
    return FAILURE;
  }

  treeNode = tdlStatement -> getTreeNode ( emptyIterationIndexes );

  if ( treeNode == (_TDL_TreeNode *) NULL )
  {
    printHeader ( TDL::getLogStream(), "applyConstraintTO" )
      << "Error:  Unable to access (create?) Data object for TDLStatement:"
      << endl;
    tdlStatement -> printObject ( TDL::getLogStream(), "     " ) << endl;
    return FAILURE;
  }

  return applyConstraintTo ( treeNode, theConstraint );
}

	/* Performs a constraint immediately, on the specified TreeNode(s).
	 * The TreeNode may be an iteration-set or a WithStatement-set,
	 * in which case this method will also clone theConstraint and attach
	 * it to the corresponding TreeNodeBranch (base-class) of that object.
	 */
status_t
_TDL_HandleManager::applyConstraintTo (
	     _TDL_TreeNode         * theTreeNode,
	     const _TDL_Constraint & theConstraint,
	     BOOLEAN                 theIsInternalRecursiveCall /* = FALSE */ )
{
  status_t          returnValue      = SUCCESS;
  _TDL_Constraint * clonedConstraint = (_TDL_Constraint *) NULL;
  _TDL_TreeNode   * previousTreeNode;
  _TDL_Dnode      * child;
  TCM_Return_Type   constraintReturnValue;


	/* Idiocy check */
  if ( theTreeNode == (_TDL_TreeNode *) NULL )
  {
    printHeader ( TDL::getLogStream(), "applyConstraintTo" )
      << "Error:  theTreeNode is NULL" << endl;
    return FAILURE;
  }

	/* Do we have a problem? */
  if (   ( theTreeNode -> isTreeNodeBranch()         == FALSE )
      && ( theTreeNode -> isSpawnStatementTreeNode() == FALSE )
      && ( theTreeNode -> isWithStatementTreeNode()  == FALSE )
      && ( theTreeNode -> isWithStatementData()      == FALSE ) )
  {
    printHeader ( TDL::getLogStream(), "applyConstraintTo" )
      << "INTERNAL ERROR:  Unknown TreeNode encountered:" << endl;
    theTreeNode -> printObject ( TDL::getLogStream(), "     " ) << endl;
    return FAILURE;
  }



	/* Safety check:  The on-agent constraint is "special". *
	 * It won't work if the node is a spawn-statement       *
	 * and is not distributable or is already allocated.    */
  if (   ( theTreeNode -> isSpawnStatementTreeNode() == TRUE )
      && ( theConstraint . isOnAgentConstraint()     == TRUE ) )
  {
    if (    theTreeNode -> getSpawnStatementTreeNode() -> isDistributable()
	 == FALSE )
    {
      printHeader ( TDL::getLogStream(), "applyConstraintTo" )
	<< "Warning:  Internal Error:  Attempting to apply ON-AGENT "
	<< "constraint to NON-Distributable Task!!!  Looks like trouble!"
	<< endl;
    }

    if ( theTreeNode -> getSpawnStatementTreeNode() -> isAllocated() == TRUE )
    {
      printHeader ( TDL::getLogStream(), "applyConstraintTo" )
	<< "Warning:  Internal Error:  Attempting to apply ON-AGENT "
	<< "constraint to Task that is ALREADY ALLOCATED!!!  "
	<< "This is not going to work!" << endl;
    }
  } /* if (   ( theTreeNode -> isSpawnStatementTreeNode() == TRUE )   *
     *     && ( theConstraint . isOnAgentConstraint()     == TRUE ) ) */




	/* Deal with SPAWN statements
	 *
	 * Note:  Non-Distributable tasks are dealt normally here.
	 * Distributable tasks require that we cache the constraints until
	 * after the last on-agent constraint -- A criteria that is
	 * accomplished by waiting until after the SPAWN statement.
	 * Therefore, if a distributable task isStarted(), we can (and must)
	 * invoke the constraint here.
	 */
  if (   (     ( theTreeNode -> isSpawnStatementTreeNode()  ) == TRUE    )
      && (   ( ( theTreeNode -> getSpawnStatementTreeNode()
	                     -> isDistributable()           ) == FALSE )
	  || ( ( theTreeNode -> getSpawnStatementTreeNode()
	                     -> isAllocated()               ) == TRUE  ) ) )
  {
    constraintReturnValue
      = theConstraint
          . applyConstraints ( * (theTreeNode -> getSpawnStatementTreeNode()),
			       getCurrentTreeNodeBranchNonConst(),
			       TRUE );

    if ( constraintReturnValue != TCM_Ok )
    {
      printHeader ( TDL::getLogStream(), "applyConstraintTo" )
	<< "Error:  applyConstraints() returned an error value of '"
	<< int4(constraintReturnValue) << "'." << endl;
      return FAILURE;
    }
    else
    {
      return SUCCESS;
    }
  }


  if ( theIsInternalRecursiveCall == FALSE )
  {
	/* Otherwise clone this constraint */
    clonedConstraint = theConstraint . clone();

	/* IF this cloned contraint has a reference value of previous,
	 * do some fancy footwork to reset that...
	 */
    if ( clonedConstraint -> getIsPreviousReference() == TRUE )
    {
      if ( _TDL_Constraint::findPrevious ( theTreeNode,
					   getCurrentTreeNodeBranchNonConst(),
					   TRUE,
					   previousTreeNode,
					   clonedConstraint )
	   != SUCCESS )
      {
	printHeader ( TDL::getLogStream(), "applyConstraintTo" )
	  << "Error:  Unable to find value for PREVIOUS.  "
	  << "Aborting applyConstraintTo()." << endl;

	delete clonedConstraint;
	return FAILURE;
      }

      clonedConstraint -> setReferenceTreeNode ( previousTreeNode );

	/* We can now recurse and use the above mechanism for applying
	 * this constraint, since we are no longer a PREVIOUS constraint
	 */
    }
  }


	/* Recurse through our children, applying constraints to them. */
  if ( theTreeNode -> getChildren() != ((const _TDL_Dlist *) NULL) )
  {
    for ( child  = theTreeNode -> getChildren() -> getFirstNode();
	  child != (_TDL_Dnode *) NULL;
	  child  = child -> getNextNode()
	 )
    {
      if (    applyConstraintTo ( (_TDL_TreeNode *) child,
				  (   (theIsInternalRecursiveCall == FALSE)
				    ? ( * clonedConstraint ) : theConstraint ),
				  TRUE )
	   != SUCCESS )
	returnValue = FAILURE;
    }
  } /* IF ( theTreeNode -> getChildren() != NULL ) */


	/* Deal with WITH Statments and iteration sets: Keep applying this
	 * constraint to any new SPAWNs that are subsequently added to this
	 * set of SPAWN statements.
	 */
  if ( theIsInternalRecursiveCall == TRUE )
  {
    ;/* NULL (EMPTY) STATEMENT. DO NOTHING IF THIS IS A RECURSIVE INVOCATION!*/
  }

  else if ( theTreeNode -> isSpawnStatementTreeNode() == TRUE )
  {
    theTreeNode -> getSpawnStatementTreeNode()
	        -> getSpawnStatementData()
		.  addConstraint ( clonedConstraint );
  }

  else if ( theTreeNode -> isTreeNodeBranch() == TRUE )
  {
    if ( theTreeNode -> getTreeNodeBranch()
		     -> addConstraint ( clonedConstraint ) == FAILURE )
    {
      printHeader ( TDL::getLogStream(), "applyConstraintTo" )
	<< "Warning:  Append of Constraint To TreeNodeBranch failed."
	<< endl;
      return FAILURE;
    }
  }

  else if ( theTreeNode -> isWithStatementTreeNode() == TRUE )
  {
    if ( theTreeNode -> getWithStatementTreeNode()
		     -> getWithStatementData()
		     .  addConstraint ( clonedConstraint ) == FAILURE )
    {
      printHeader ( TDL::getLogStream(), "applyConstraintTo" )
	<< "Warning:  Append of Constraint To WithStatementTreeNode failed."
	<< endl;
      return FAILURE;
    }
  }

  else if ( theTreeNode -> isWithStatementData() == TRUE )
  {
    if ( theTreeNode -> getWithStatementData()
		     -> addConstraint ( clonedConstraint ) == FAILURE )
    {
      printHeader ( TDL::getLogStream(), "applyConstraintTo" )
	<< "Warning:  Append of Constraint To WithStatementData failed."
	<< endl;
      return FAILURE;
    }
  }

  else
  {
    printHeader ( TDL::getLogStream(), "applyConstraintTo" )
      << "Error:  Impossible Programmer Error encountered.  Node is *NOT* "
      << "a TreeNodeBranch, WithStatementTreeNode, or WithStatementData."
      << endl;
    return FAILURE;
  }

  return returnValue;
}



	/* Allow users to bind and use non-TDL TCM tasks */
status_t
_TDL_HandleManager::bindTCMTask( const TCM_Task_Tree_Ref &  theTCMTask,
				 const char *               theTCMTaskName)
{
  _TDL_Dlist tempDlist(FALSE);
  _TDL_SpawnStatementData   * tcmSpawnStatementData
		= addSpawnStatement ( new _TDL_SpawnStatement ( 1 ),
				      FALSE /* Add to beginning of list,  */
					    /* overriding previous BINDs. */ )
		    -> addName ( theTCMTaskName )
		    -> getSpawnStatement()
		    -> getSpawnStatementData ( tempDlist );

  if (    tcmSpawnStatementData -> startRunningNullSpawnStatementData()
       == FAILURE )
  {
    printHeader ( TDL::getLogStream(), "bindTCMTask" )
      << "Error:  startRunningNullSpawnStatementData() FAILED." << endl;
    return FAILURE;
  }
  else
  {
    tcmSpawnStatementData -> setTCMTaskTreeDirectly ( theTCMTask );
    return SUCCESS;
  }
}




int4
_TDL_HandleManager::getCurrentIterationIndex ( u_int4 theIndexIndex ) const
{
  u_int4 indexCount = 0;

  for ( _TDL_Dnode * dnode  = getCurrentIterationIndexes() . getFirstNode();
	             dnode != (_TDL_Dnode *) NULL;
	             dnode  = dnode -> getNextNode()
       )
  {
    if ( theIndexIndex <= indexCount )
      return ((_TDL_IterationIndex *) dnode) -> getCurrentIndex();

    indexCount ++;
  }

  printHeader ( TDL::getLogStream(), "getCurrentIterationIndex" )
    << "Error:  Tried to obtain index # " << theIndexIndex
    << " when there was (were) only " << indexCount
    << " index(es) available.  Returning ERROR_ARRAY_INDEX (-3)." << endl;
  return _TDL_TreeNode::ERROR_ARRAY_INDEX;
}



	/* This convenience method is used throughout _TDL_HandleManager to
	 * gain access to the enclosing [PARENT] task.  Specifically, it is
	 * used when Spawning (Creating) new children tasks as the source of
	 * the enclosing or parent task for those children.
	 *
	 * By default it just returns the NodeHandle of getEnclosingTask().
	 * However, it is virtual so that it can be overridden in subclasses,
	 * if alternate behavior is desired.
	 */
/*virtual*/ const TCM_Task_Tree_Ref &
_TDL_HandleManager::getEnclosingTaskReference()
{
  _TDL_Dlist tempDlist(FALSE);
  return getEnclosingTask() .  getSpawnStatementDataConst ( tempDlist )
			    -> getNodeHandle();
}



	/* Note:  If necessary, this can allocate an instance    *
	 * of "theName" for the current iteration indexes.       *
	 * It will return NULL if this node has already be used  *
	 * and theAbortIfAlreadyStarted is set to TRUE.          */
_TDL_SpawnStatementData *
_TDL_HandleManager::getSpawnStatementData (
		 const char *            theSpawnName,
		 const char *            theLocation,              /* = NULL */
		 const char *            theCurrentTaskName,       /* = NULL */
		 BOOLEAN                 theAbortIfAlreadyStarted, /* = TRUE */
		 const _TDL_Constraint & theOptionalConstraint,
				  /* = _TDL_HandleManager::NO_CONSTRAINT*/
		 BOOLEAN                 theSkipAllocate )        /* = FALSE */
{
  _TDL_SpawnStatement         * spawnStatement
				  = findSpawnStatement ( theSpawnName );
  _TDL_SpawnStatementData     * spawnStatementData;

	/* Establish default printHeader constants */
  if ( theLocation == (const char *) NULL )
    theLocation = "getSpawnStatementData";
  if ( theCurrentTaskName == (const char *) NULL )
  {
    if ( StringEqual ( getCurrentTaskName(), _TDL_HandleManager::NO_TASK_NAME )
	 == TRUE )
      theCurrentTaskName = "";
    else
      theCurrentTaskName = getCurrentTaskName();
  }

	/* Idiocy checks */
  if ( theSpawnName == (const char *) NULL )
  {
    printHeader ( TDL::getLogStream(), theLocation, theCurrentTaskName )
      << "Error:  theSpawnName is NULL.  Aborting " << theLocation << "()..."
      << endl;
    return (_TDL_SpawnStatementData *) NULL;
  }

  if ( spawnStatement == (_TDL_SpawnStatement *) NULL )
  {
    printHeader ( TDL::getLogStream(), theLocation, theCurrentTaskName )
      << "Error:  No Known SpawnStatement has name \"" << theSpawnName
      << "\".  Aborting " << theLocation << "()..." << endl;
    return (_TDL_SpawnStatementData *) NULL;
  }


	/* If we have already used this node... */
  if (   ( theAbortIfAlreadyStarted == TRUE
	  )
      && (    spawnStatement
	        -> getSpawnStatementDataConst ( getCurrentIterationIndexes () )
	   != (_TDL_SpawnStatementData *) NULL
	  )
      && (    spawnStatement
	        -> getSpawnStatementDataConst ( getCurrentIterationIndexes () )
	        -> isNotStarted()
	   == FALSE
	  )
      )
  {
    printHeader ( TDL::getLogStream(), theLocation, theCurrentTaskName )
      << "Error:  SpawnStatement has already been used.  "
      << "Aborting " << theLocation << "()..." << endl;
    return (_TDL_SpawnStatementData *) NULL;
  }

	/* Get/Create our new spawnStatementData */
  spawnStatementData
    = spawnStatement -> getSpawnStatementData ( getCurrentIterationIndexes() );


	/* Double-check that. */
  if ( spawnStatementData == (_TDL_SpawnStatementData *) NULL )
  {
    printHeader ( TDL::getLogStream(), theLocation, theCurrentTaskName )
      << "Error:  FAILED to create new Spawn-Statement-Data.  "
      << "Aborting " << theLocation << "()..." << endl;
    return (_TDL_SpawnStatementData *) NULL;
  }


  
	/* For convenience, we can optionally apply a constraint here.
	 * (Thereby insuring we are using the correct iteration indexes...)
	 */
  if ( (& theOptionalConstraint) != (& _TDL_HandleManager::NO_CONSTRAINT) )
  {
    _TDL_SpawnStatementTreeNode * spawnStatementTreeNode
      = (_TDL_SpawnStatementTreeNode *) ( spawnStatementData
				            -> getSpawnStatementTreeNodeSlist()
				            .  getFirstNode() );

    if ( spawnStatementTreeNode == (_TDL_SpawnStatementTreeNode *) NULL )
    {
      printHeader ( TDL::getLogStream(), theLocation, theCurrentTaskName )
	<< "Internal Error:  FAILED to access Spawn-Statement-Tree-Node.  "
	<< "Unable to apply optional-constraint:  "
	<< theOptionalConstraint;
    }

	/* By utilizing the common applyConstraintTo() interface, we simplify
	 * debugging issues.  Also, it will resolves any "PREVIOUS" issues for
	 * us, etc..
	 */
    else if ( applyConstraintTo ( spawnStatementTreeNode,
				  theOptionalConstraint   ) != SUCCESS )
    {
      printHeader ( TDL::getLogStream(), theLocation, theCurrentTaskName )
	<< "Internal Error:  FAILED to properly apply optional-constraint: "
	<< theOptionalConstraint;
    }
  } /* if ( theOptionalConstraint != (_TDL_Constraint *) NULL ) */



	/* Umm, it did/does allocate, right? */
  if ( theSkipAllocate != TRUE )
  {
    if (   (    spawnStatementData -> getState()
	     == _TDL_SpawnStatementData::NOT_ALLOCATED )
        && (    spawnStatementData -> allocate()
	     == FAILURE )
        )
    {
      printHeader ( TDL::getLogStream(), theLocation, theSpawnName )
	<< "Error:  Unable to allocate() SpawnStatementData.  "
	<< "Aborting " << theLocation << "()..." << endl;
      return (_TDL_SpawnStatementData *) NULL;
    }
  }



#ifdef DEBUG_SKIP_ALLOCATE
	/* If the node is already allocate()'ed, and the allocate() occurred
	 * AFTER the last ON-AGENT constraint for Distributed Tasks, then
	 * there is no problem...
	 */
  if (   ( theSkipAllocate == TRUE               )
      && (     spawnStatementData -> getState()
	   == _TDL_SpawnStatementData::ALLOCATED ) )
  {
    printHeader ( TDL::getLogStream(), theLocation, theCurrentTaskName )
      << "Warning:  Node was supposed to skip allocate(), but allocate() "
      << "had already been run on this _TDL_SpawnStatementData object.  "
      << "This may cause problems with certain constraints such as on-agent!"
      << endl;
  }
#endif /* DEBUG_SKIP_ALLOCATE */


	/* Check for REAL trouble */
  if (   ( theAbortIfAlreadyStarted == TRUE                      )

      && (   (   ( theSkipAllocate != TRUE                   )
	      && (    spawnStatementData -> getState()
		   != _TDL_SpawnStatementData::ALLOCATED     ) )

	  || (   ( theSkipAllocate == TRUE                   )
	      && (    spawnStatementData -> getState()
		   != _TDL_SpawnStatementData::NOT_ALLOCATED )
	      && (    spawnStatementData -> getState()
		   != _TDL_SpawnStatementData::ALLOCATED     ) ) ) )
  {
    printHeader ( TDL::getLogStream(), theLocation, theCurrentTaskName )
      << "Error:  Bad State (" << int4(spawnStatementData -> getState())
      << ") for SpawnStatementData.  Spawn can *NOT* be started.  "
      << "Aborting " << theLocation << "()..." << endl;
    return (_TDL_SpawnStatementData *) NULL;
  }


  return spawnStatementData;
}



	/* Explicit support for ON-TERMINATE constraint. */
_TDL_SpawnStatementTreeNode *
_TDL_HandleManager::doSetOnTerminateTaskAction ( const char  * theSpawnName,
						 _TDL_Action * theAction    )
{
  _TDL_SpawnStatementData     * spawnStatementData;
  _TDL_SpawnStatementTreeNode * spawnStatementTreeNode;
  TCM_Return_Type               result;
  const char                  * spawnNameForPrintHeader;

  if ( theSpawnName != (const char *) NULL )
    spawnNameForPrintHeader = theSpawnName;
  else
    spawnNameForPrintHeader = "";

	/* Lets only do this in one place... */
  spawnStatementData = getSpawnStatementData (
			   theSpawnName,
			   "doSetOnTerminateTaskAction/getSpawnStatementData",
			   spawnNameForPrintHeader,
			   TRUE );

	/* IF there was a problem, abort... */
  if ( spawnStatementData == (_TDL_SpawnStatementData *) NULL )
    return (_TDL_SpawnStatementTreeNode *) NULL;

   	/* Create a new spawnStatementTreeNode */
  spawnStatementTreeNode
    = spawnStatementData -> createAndAttachNewSpawnStatementTreeNode();

	/* Double-check that. */
  if ( spawnStatementTreeNode == (_TDL_SpawnStatementTreeNode *) NULL )
  {
    printHeader ( TDL::getLogStream(), "doSetOnTerminateTaskAction", 
		  spawnNameForPrintHeader )
      << "Error:  FAILED to create new Spawn-Statement-Tree-Node.  "
      << "Aborting startInvokingSpawn()..." << endl;
    return (_TDL_SpawnStatementTreeNode *) NULL;
  }


  result = TCM_SetActualAction ( * spawnStatementData, theAction );

	/* Did setAction work? */
  if ( result != TCM_Ok )
  {
    printHeader ( TDL::getLogStream(), "doSetOnTerminateTaskAction",
		  spawnNameForPrintHeader )
      << "Error:  TCM_SetActualAction() failed with a value of "
      << int4(result) << ".   Aborting with NULL value." << endl;
    return (_TDL_SpawnStatementTreeNode *) NULL;
  }

  return spawnStatementTreeNode;
}



	/* iostreamBase interface. */
/*virtual*/ ostream &
_TDL_HandleManager::printObject ( ostream    & theOstream,
				  const char * theIndentString /*=""*/ ) const
{
  char  subIndentString [ iostreamBase::MAXIMUM_INDENT ];
  iostreamBase::createSubIndentString ( subIndentString, theIndentString );

  theOstream << theIndentString << "-Begin- _TDL_HandleManager  "
	     << ((void *)this)  << "  ( " << getTaskName() << " )" << endl

	     << theIndentString << " rootTreeNodeBranch....: "
	     << ((const void *) (& getRootTreeNodeBranch()))
	     << "  ( " << getRootTreeNodeBranch() . getName() << " )" << endl

	     << theIndentString << " currentTreeNodeBranch.: "
	     << ((const void *) getCurrentTreeNodeBranch())
	     << "  ( " << getCurrentTreeNodeBranch() -> getName() << " )"
	     << endl

	     << theIndentString << " isStartingTask........: "
	     << ( (getIsStartingTask()  == TRUE) ? "TRUE" : "FALSE" )  << endl

	     << theIndentString << " hasInsertedTask.......: "
	     << ( (getHasInsertedTask() == TRUE) ? "TRUE" : "FALSE" )  << endl

	     << theIndentString << " currentTaskName.......: "
	     << getCurrentTaskName() << endl

	     << theIndentString << " currentTask...........: "
	     << ((void *)getCurrentTask())
	     << "  ( "
	     << ( ( getCurrentTask() == (_TDL_SpawnStatementTreeNode *) NULL )
		 ? "NULL" : getCurrentTask() -> getName() )
	     << " )" << endl


	     << endl << theIndentString << " Array Indexes.........: ";
  _TDL_IterationIndex::printIndexList ( theOstream,
					getCurrentIterationIndexes() ) << endl;
  getCurrentIterationIndexes() . printObject ( theOstream, subIndentString );


  theOstream << endl
	     << theIndentString << "--------------------" << endl
	     << theIndentString << " Enclosing (Parent) Task: " << endl;
  getEnclosingTask() . printObject ( theOstream, subIndentString );
  

  theOstream << endl
	     << theIndentString << "--------------------" << endl
	     << theIndentString << " TDL-Code-Tree:" << endl;
  getRootTreeNodeBranch() . printObject ( theOstream, subIndentString );


  theOstream << endl
	     << theIndentString << "--------------------" << endl
	     << theIndentString << " TDL-HandleManager-Spawns:" << endl;
  getSpawnStatementSlist() . printObject ( theOstream, subIndentString );

  theOstream << endl
	     << theIndentString << "--------------------" << endl
	     << theIndentString << " TDL-HandleManager-Withs:" << endl;

  getWithStatementSlist() . printObject ( theOstream, subIndentString );


  theOstream << endl
	     << theIndentString << "----------------------------------" << endl
	     << theIndentString << "--End-- _TDL_HandleManager  "
	     << ((void *)this)  << "  ( " << getTaskName() << " )" << endl;

  return theOstream;
}


