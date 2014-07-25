/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

#ifndef _TDL_HandleManager_H
#define _TDL_HandleManager_H

#define _TDL_INTERNAL_
#include "tdl.H"
#include "_TDL_Snode.H"
#include "_TDL_Slist.H"
#include "_TDL_Dnode.H"
#include "_TDL_Dlist.H"
#include "_TDL_TreeNode.H"
#include "_TDL_TreeNodeBranch.H"
#include "_TDL_IterationIndex.H"
#include "_TDL_TDLStatement.H"
#include "_TDL_SpawnStatement.H"
#include "_TDL_SpawnStatementData.H"
#include "_TDL_SpawnStatementTreeNode.H"
#include "_TDL_Exceptions.H"
#include "_TDL_Constraints.H"
#include "_TDL_WithStatement.H"
#include "_TDL_WithStatementData.H"
#include "_TDL_WithStatementTreeNode.H"


/*
 * The _TDL_TrivialHandleManager exists to keep the TDL_REF macro functioning
 * inside the generated _TDL_CreateAction_...() function, but only for the
 * trivial singleton case of TDL_REF(THIS).
 */
class _TDL_TrivialHandleManager
{
protected:
  const char *       taskName;
  TCM_Task_Tree_Ref  tcmTaskTreeRef, nullTcmTaskTreeRef;

public:
  _TDL_TrivialHandleManager ( const char *      theTaskName,
			      TCM_Task_Tree_Ref theTcmTaskTreeRef )
    : taskName           ( theTaskName       ),
      tcmTaskTreeRef     ( theTcmTaskTreeRef ),
      nullTcmTaskTreeRef (                   )
  {}

  ~_TDL_TrivialHandleManager() { tcmTaskTreeRef = (Task_Tree_Node *) NULL; }

  const TCM_Task_Tree_Ref & operator[] ( const char * theName ) const;
};




/*
 * The _TDL_HandleManager is the master class that manages a whole set
 * of data (and data-objects) representing a single TDL task's set of
 * subtasks (TDL SPAWNS & WITH statements).
 *
 * It should be noted that _TDL_HandleManager deals with the
 * TDL-CODE-TREE -- Ie: What constraints are being applied to what
 * subtasks.  This should NOT be confused with the TCM-Task-Tree,
 * which is a horse of a completely different color.
 *
 * Also:  The _TDL_HandleManager can (and should be) deleted/destroyed
 * at the end of a Task's Handling.  (Since it will have completed its
 * job of mananging the TDL-CODE-TREE at that point.  The TCM-Task-Tree,
 * which is handled through the TCM libraries, will not be destroyed,
 * or even affected, when the _TDL_HandleManager instance is destroyed.)
 */

class _TDL_HandleManager
#ifndef DISABLE_ISTREAMBASE
		 : public virtual iostreamBase
#endif /* ! DISABLE_ISTREAMBASE */
{
public:
  enum CONSTANTS
  {
    NUMBER_OF_TASK_NAMES = 3,
    MAX_TASK_NAME_LENGTH = 127
  };
  static const char *              ENCLOSING_TASK_NAMES [NUMBER_OF_TASK_NAMES];
  static const char *              NO_TASK_NAME;
  static const char *              ROOT_NAME;
  static       _TDL_TDLStatement & ERROR_TDL_STATEMENT;
  static       _TDL_Constraint &   NO_CONSTRAINT;

protected:
  _TDL_Slist            spawnStatementSlist;
  _TDL_Slist            withStatementSlist;

  _TDL_TreeNodeBranch   rootTreeNodeBranch;
  _TDL_TreeNodeBranch * currentTreeNodeBranch;
  
  const char * const    taskName;        /* Name of Task we are handling */

  char                  currentTaskName  /* Name of Task we are creating */
			      [ _TDL_HandleManager::MAX_TASK_NAME_LENGTH + 1 ];
  _TDL_SpawnStatementTreeNode * currentTask; /*     Task we are creating */

  BOOLEAN               insertedTask;    /* TRUE after insert() */

  TCM_Task_Tree_Ref     tcmTaskTreeParent; /* Optional PARENT value for task.*/
  TCM_Task_Tree_Ref     nullTcmTaskTreeRef;/* Permanently NULL Ref. */

  _TDL_SpawnStatement   enclosingTask;

  _TDL_Dlist            currentIterationIndexes;
  _TDL_Dlist            unusedIterationIndexes;


public:
	/* theTaskName is cached, and used in error messages... */
  _TDL_HandleManager ( const char *               theTaskName,
		       const TCM_Task_Tree_Ref &  theEnclosingTaskReference )
    : spawnStatementSlist     ( TRUE /* Deletes contained elements */        ),
      withStatementSlist      ( TRUE /* Deletes contained elements */        ),
      rootTreeNodeBranch      ( _TDL_HandleManager::ROOT_NAME                ),
      currentTreeNodeBranch   ( (_TDL_TreeNodeBranch *) NULL                 ),
      taskName                ( theTaskName                                  ),
      currentTask             ( (_TDL_SpawnStatementTreeNode *) NULL         ),
      insertedTask            ( FALSE                                        ),
      nullTcmTaskTreeRef      (                                              ),
      enclosingTask           ( _TDL_HandleManager::NUMBER_OF_TASK_NAMES,
				_TDL_HandleManager::ENCLOSING_TASK_NAMES [0] ),
      currentIterationIndexes ( TRUE /* Deletes contained elements */        ),
      unusedIterationIndexes  ( TRUE /* Deletes contained elements */        )
  {
    setCurrentTaskName ( _TDL_HandleManager::NO_TASK_NAME );

	/* This *MUST* be NULL! */
    rootTreeNodeBranch . setParent ( (_TDL_TreeNodeBranch *) NULL );

    currentTreeNodeBranch = & rootTreeNodeBranch;

    for ( int4 i=1;  i < _TDL_HandleManager::NUMBER_OF_TASK_NAMES;  i++ )
      enclosingTask .  addName ( _TDL_HandleManager::ENCLOSING_TASK_NAMES [i]);
    enclosingTask .  getSpawnStatementData ( currentIterationIndexes )
                  -> startRunningNullSpawnStatementData();

    setEnclosingTaskReference ( theEnclosingTaskReference );
  }

  virtual ~_TDL_HandleManager();


	/* Note:  This is a down & dirty kludge that is here solely to permit
	 * _TDL_HandlerManager to be used in ways it was never designed to be
	 * used.  For purposes of the implementing TDL, this method is *ONLY* 
	 * called from _TDL_HandleManager::_TDL_HandleManager.
	 *
	 * Note:  The phrase "Enclosing Task" refers to the same task as the
	 * TDL keyword PARENT.  So "theEnclosingTaskReference" forms the parent
	 * of whatever children this _TDL_HandlerManager object creates.
	 */
  void setEnclosingTaskReference (
			 const TCM_Task_Tree_Ref &  theEnclosingTaskReference )
  {
    _TDL_Dlist tempDlist(FALSE);
    enclosingTask .  getSpawnStatementData  ( tempDlist                 )
                  -> setTCMTaskTreeDirectly ( theEnclosingTaskReference );
  }
  

	/* Note:  addSpawnStatement ( _TDL_SpawnStatement * ) ASSUMES *
	 * THAT theSpawnStatement has been dynamically allocated off  *
	 * the heap, and it ASSUMES THAT this _TDL_HandleManager      *
	 * *SHOULD* DELETE theSpawnStatement when it is destroyed.    */
  _TDL_SpawnStatement * addSpawnStatement (
			      _TDL_SpawnStatement * theSpawnStatement,
			      BOOLEAN               theAddToEndOfList = TRUE );


	/* Originally just Convenience methods.
	 * However, they now provide a compile-time interface to
	 * prevent linkage if the right combination of local/distributed
	 * tasks are not present.  In short: If the user attempts to employ
	 * the ON-AGENT constraint on a non-distributed local-only task,
	 * or vice versa, the addSpawnStatement_*() method will not match
	 * the signature of the allocation function, deliberately creating
	 * a compile-time linkage error.
	 */
  _TDL_SpawnStatement * addSpawnStatement_Local (
	_TDL_AllocationFunctionType_Local          theAllocationFunction,
	u_int4                                     theInitialCapacity
				  = _TDL_ArrayList::DEFAULT_INITIAL_CAPACITY,
	BOOLEAN                                    theAddToEndOfList = TRUE  )
  { return addSpawnStatement ( new _TDL_SpawnStatement(theAllocationFunction,
						       theInitialCapacity    ),
			       theAddToEndOfList );
  }

  _TDL_SpawnStatement * addSpawnStatement_Local (
	_TDL_AllocationFunctionType_Either         theAllocationFunction,
	u_int4                                     theInitialCapacity
				  = _TDL_ArrayList::DEFAULT_INITIAL_CAPACITY,
	BOOLEAN                                    theAddToEndOfList = TRUE  )
  { return addSpawnStatement ( new _TDL_SpawnStatement(theAllocationFunction,
						       theInitialCapacity    ),
			       theAddToEndOfList );
  }



  _TDL_SpawnStatement * addSpawnStatement_Either (
	_TDL_AllocationFunctionType_Either         theAllocationFunction,
	u_int4                                     theInitialCapacity
				  = _TDL_ArrayList::DEFAULT_INITIAL_CAPACITY,
	BOOLEAN                                    theAddToEndOfList = TRUE  )
  { return addSpawnStatement ( new _TDL_SpawnStatement(theAllocationFunction,
						       theInitialCapacity    ),
			       theAddToEndOfList );
  }



  _TDL_SpawnStatement * addSpawnStatement_Distributed (
	_TDL_AllocationFunctionType_Either         theAllocationFunction,
	u_int4                                     theInitialCapacity
				  = _TDL_ArrayList::DEFAULT_INITIAL_CAPACITY,
	BOOLEAN                                    theAddToEndOfList = TRUE  )
  { return addSpawnStatement ( new _TDL_SpawnStatement(theAllocationFunction,
						       theInitialCapacity    ),
			       theAddToEndOfList );
  }

  _TDL_SpawnStatement * addSpawnStatement_Distributed (
	_TDL_AllocationFunctionType_Distributed    theAllocationFunction,
	u_int4                                     theInitialCapacity
				  = _TDL_ArrayList::DEFAULT_INITIAL_CAPACITY,
	BOOLEAN                                    theAddToEndOfList = TRUE  )
  { return addSpawnStatement ( new _TDL_SpawnStatement(theAllocationFunction,
						       theInitialCapacity    ),
			       theAddToEndOfList );
  }



  _TDL_SpawnStatement * addSpawnStatement_DelayedAllocation (
	u_int4                                     theInitialCapacity
				  = _TDL_ArrayList::DEFAULT_INITIAL_CAPACITY,
	BOOLEAN                                    theAddToEndOfList = TRUE  )
  { return addSpawnStatement ( new _TDL_SpawnStatement (
				 _TDL_AllocationFunction::DELAYED_ALLOCATION,
				 theInitialCapacity ),
			       theAddToEndOfList );
  }


	/* Convenience method */
  _TDL_WithStatement *  addWithStatement (
			    u_int4        theInitialCapacity
			          = _TDL_ArrayList::DEFAULT_INITIAL_CAPACITY );

	/* Note:  addWithStatement ( _TDL_WithStatement * ) ASSUMES *
	 * THAT theWithStatement has been dynamically allocated off *
	 * the heap, and it ASSUMES THAT this _TDL_HandleManager    *
	 * *SHOULD* DELETE theWithStatement when it is destroyed.   */
  _TDL_WithStatement *  addWithStatement (
				       _TDL_WithStatement * theWithStatement );


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
  _TDL_TDLStatement & operator[] ( const char * theName ) const;

	/* Finds the _TDL_SpawnStatement or _TDL_WithStatement named theName.
	 * Returns NULL if no match is found...
	 */
  _TDL_SpawnStatement * findSpawnStatement ( const char * theName ) const;
  _TDL_WithStatement  * findWithStatement  ( const char * theName ) const;
  _TDL_TDLStatement   * findTDLStatement   ( const char * theName ) const;



	/* Convenience methods for invoking destroy */
  status_t destroy         ( const char * theName,
			     BOOLEAN      theCanStillBeReferenced = FALSE );
  status_t destroyIfUnused ( const char * theName,
			     BOOLEAN      theCanStillBeReferenced = FALSE );



	/* Descends into an iteration loop */
  _TDL_IterationIndex * pushIteration();

	/* Increments the current iteration loop */
  status_t incrementIteration();

	/* Ascends up out of an iteration loop. */
  status_t popIteration();



	/* Descends into a WITH Statement */
  status_t pushWithStatement ( const char * theWithStatementName );

	/* Ascends up out of a WITH Statement */
  status_t popWithStatement  ( const char * theWithStatementName );



    /* Used to completely (& uniformly) cancel invoking of the Current-Spawn */
  void     clearInvokingSpawn();  /* Aborts    Invoking of the Current-Spawn */
  void     finishInvokingSpawn(); /* Completes Invoking of the Current-Spawn */


	/* Used to start invoking a SPAWN statement.  Sets the Current-Spawn.
	 * (Sets the state to enable setAction, applyConstraint, & insertSpawn.
	 *  If any of them fail, further activity in those methods is disabled
	 *  until another startInvoking() call is made...)
	 */
  status_t startInvokingSpawn (
			    const char              * theSpawnName,
			    const _TDL_Constraint   & theOptionalConstraint,
			    const TCM_Task_Tree_Ref & theTcmTaskTreeParent   );

  status_t startInvokingSpawn (
			    const char            * theSpawnName,
			    const _TDL_Constraint & theOptionalConstraint
					 = _TDL_HandleManager::NO_CONSTRAINT )
	     { return startInvokingSpawn ( theSpawnName,
					   theOptionalConstraint,
					   NULL );
	     }

  status_t startInvokingSpawn (
			    const char              * theSpawnName,
			    const TCM_Task_Tree_Ref & theTcmTaskTreeParent )
	     { return startInvokingSpawn ( theSpawnName,
					   _TDL_HandleManager::NO_CONSTRAINT,
					   theTcmTaskTreeParent );
	     }


	/* If the result is not TCM_Ok, it disables further activity on
	 * the Current-Spawn and prints a warning message.
	 */
  status_t setAction ( const _TDL_ActionOrVoid & theActionOrVoid );

	/* Applies theConstraint to the Current-Spawn.  If problem(s) arise,
	 * it disables futher activity and prints a warning message.
	 */
  status_t applyConstraint ( const _TDL_Constraint & theConstraint );

	/* Inserts (starts) this SPAWN'ed Task.
	 * (After first applying any pending WITH-Statement constraints.)
	 * Disables further activity upon the Current-Spawn.
	 */
  status_t insertSpawn ( BOOLEAN theIsLastChild = TRUE );


	/* Convenience methods -- they let us chain operations together... */
  _TDL_HandleManager & doSetAction( const _TDL_ActionOrVoid & theActionOrVoid )
					 { if ( getIsStartingTask() )
					     setAction ( theActionOrVoid );
					   return *this; }
  _TDL_HandleManager & doApplyConstraint (
				        const _TDL_Constraint & theConstraint )
					 { if ( getIsStartingTask() )
					     applyConstraint ( theConstraint );
					   return *this; }

  _TDL_HandleManager & doInsertSpawn ( BOOLEAN theIsLastChild = TRUE )
					 { if ( getIsStartingTask() )
					     insertSpawn ( theIsLastChild );
					   return *this; }


	/* Adds theConstraint to the current WITH-Statement-Data instance.
	 * (Which corresponds to a specific iteration index.)
	 * Assumes that theConstraint has been dynamically allocated off
	 * the heap, and that theConstraint should be deleted when this
	 * _TDL_HandleManager (or rather _TDL_TreeWithStatementData)
	 * object is deleted!
	 */
  _TDL_HandleManager & doAddWithStatementConstraint (
					     _TDL_Constraint * theConstraint );



	/* Performs a constraint immediately, on the specified TreeNode(s).
	 * The TreeNode may be an iteration-set or a WithStatement-set,
	 * in which case this method will also clone theConstraint and attach
	 * it to the corresponding TreeNodeBranch (base-class) of that object.
	 */
  status_t applyConstraintTo ( const char *            theName,
			       const _TDL_Constraint & theConstraint  );

  status_t applyConstraintTo (
		  _TDL_TreeNode         * theTreeNode,
		  const _TDL_Constraint & theConstraint,
		  BOOLEAN                 theIsInternalRecursiveCall = FALSE );



	/* Allow users to bind and use non-TDL TCM tasks */
  status_t bindTCMTask ( const TCM_Task_Tree_Ref &  theTCMTask,
			 const char *               theTCMTaskName );



	/* Data access methods */
  const _TDL_Slist & getSpawnStatementSlist()const{return spawnStatementSlist;}
  const _TDL_Slist & getWithStatementSlist() const{return withStatementSlist; }
  const char *       getTaskName()           const{return taskName;           }

  const _TDL_Dlist & getCurrentIterationIndexes() const
					    { return currentIterationIndexes; }
  int4               getCurrentIterationIndex ( u_int4 theIndexIndex ) const;


	/* Accessing the Enclosing-Task-Reference */
  _TDL_SpawnStatement & getEnclosingTask() const /*mutable*/
		     { return ((_TDL_HandleManager *) this) -> enclosingTask; }

	/* Accessing the Enclosing-Task-Reference */
  _TDL_SpawnStatementTreeNode * getEnclosingTaskTreeNode() const /*mutable*/
		{  _TDL_Dlist tempDlist(FALSE);
		  return getEnclosingTask()
			   . getSpawnStatementTreeNode ( tempDlist ); }

	/* This convenience method is used throughout _TDL_HandleManager to
	 * gain access to the enclosing [PARENT] task.  Specifically, it is
	 * used when Spawning (Creating) new children tasks as the source of
	 * the enclosing or parent task for those children.
	 *
	 * By default it just returns the NodeHandle of getEnclosingTask().
	 * However, it is virtual so that it can be overridden in subclasses,
	 * if alternate behavior is desired.
	 */
  virtual const TCM_Task_Tree_Ref & getEnclosingTaskReference();



	/* Tree-Structure access methods */
  const _TDL_TreeNodeBranch & getRootTreeNodeBranch()    const
					      { return rootTreeNodeBranch;    }
  const _TDL_TreeNodeBranch * getCurrentTreeNodeBranch() const
					      { return currentTreeNodeBranch; }


	/* State for current task-spawning in progress... */
  _TDL_SpawnStatementTreeNode * getCurrentTask() const { return currentTask; }
  const char * getCurrentTaskName() const { return currentTaskName; }
  BOOLEAN      getHasInsertedTask() const { return insertedTask;    }
  BOOLEAN      getIsStartingTask()  const
	    { return getCurrentTask() != (_TDL_SpawnStatementTreeNode *)NULL; }

  TCM_Task_Tree_Ref & getCurrentTaskTcmTaskTreeRef() 
    { return getIsStartingTask()
       ? getCurrentTask() -> getSpawnStatementData()
	                  .  getNodeHandleNonConstWithAllocate()
       : nullTcmTaskTreeRef . clear();
    }



	/* Note:  If necessary, this can allocate an instance    *
	 * of "theName" for the current iteration indexes.       *
	 * It will return NULL if this node has already be used  *
	 * and theAbortIfAlreadyStarted is set to TRUE.          */
  _TDL_SpawnStatementData * getSpawnStatementData (
	const char *            theSpawnName,
	const char *            theLocation              = (const char *) NULL,
	const char *            theCurrentTaskName       = (const char *) NULL,
	BOOLEAN                 theAbortIfAlreadyStarted = TRUE,
	const _TDL_Constraint & theOptionalConstraint
				  = _TDL_HandleManager::NO_CONSTRAINT,
	BOOLEAN                 theSkipAllocate          = FALSE );

	/* If getSpawnStatementData() returns NULL, it could be a problem... */
  TCM_Task_Tree_Ref & getTaskTreeRef (
			         const char * theSpawnName,
			         BOOLEAN      theAbortIfAlreadyStarted = TRUE )
	  {
	    _TDL_SpawnStatementData * spawnStatementData
	      = getSpawnStatementData (
		  theSpawnName, "getTaskTreeRef/getSpawnStatementData",
		  (const char *) NULL, theAbortIfAlreadyStarted );
	    if ( spawnStatementData != (_TDL_SpawnStatementData *) NULL )
	      return spawnStatementData -> getNodeHandleNonConstWithAllocate();
	    else
	      return nullTcmTaskTreeRef . clear();
	  }

	/* Explicit support for ON-TERMINATE constraint. */
  _TDL_SpawnStatementTreeNode * doSetOnTerminateTaskAction (
						  const char  * theSpawnName,
						  _TDL_Action * theAction    );


	/* iostreamBase interface. */
  virtual ostream & printObject ( ostream    & theOstream,
				  const char * theIndentString = "") const;


protected:
  _TDL_Slist & getSpawnStatementSlistNonConst() { return spawnStatementSlist; }
  _TDL_Slist & getWithStatementSlistNonConst()  { return withStatementSlist;  }

  _TDL_TreeNodeBranch & getRootTreeNodeBranchNonConst()
					      { return rootTreeNodeBranch;    }
  _TDL_TreeNodeBranch * getCurrentTreeNodeBranchNonConst()
					      { return currentTreeNodeBranch; }

  _TDL_Dlist & getCurrentIterationIndexesNonConst()
					    { return currentIterationIndexes; }
  _TDL_Dlist & getUnusedIterationIndexesNonConst()
					    { return  unusedIterationIndexes; }

  void setCurrentTaskName ( const char * theCurrentTaskName )
		{
		  strncpy ( currentTaskName, theCurrentTaskName,
			    _TDL_HandleManager::MAX_TASK_NAME_LENGTH );
		  currentTaskName [ _TDL_HandleManager::MAX_TASK_NAME_LENGTH ]
		    = NULL_CHAR;
		}


  const TCM_Task_Tree_Ref & getTcmTaskTreeParent() const
	    { return tcmTaskTreeParent; }
  BOOLEAN hasTcmTaskTreeParent()
	    { return TO_BOOLEAN ( getTcmTaskTreeParent() . isNotNull() ); }
  void    setTcmTaskTreeParent( const TCM_Task_Tree_Ref & theTcmTaskTreeParent)
	    { tcmTaskTreeParent = theTcmTaskTreeParent; }
  void  clearTcmTaskTreeParent()
	    { tcmTaskTreeParent . clear(); }


  ostream & printHeader (
		   ostream & theOstream,
		   const char * theMethodName,
		   const char * theCurrentTaskName = (const char *)NULL ) const
		{
		  theOstream
		    << "[_TDL_HandleManager(\"" << getTaskName()
		    << "\"";
		  if ( theCurrentTaskName != (const char *) NULL )
		  {
		    theOstream << ":\"" << theCurrentTaskName << "\":";
		    _TDL_IterationIndex::printIndexList ( TDL::getLogStream(),
						getCurrentIterationIndexes() );
		  }
		  TDL::getLogStream()
		    << "):" << theMethodName << "]  ";
		  return theOstream;
		}



	/* These operations are disabled (prohibited) & unimplemented */
private:
  _TDL_HandleManager (             const _TDL_HandleManager & theObjectToCopy);
  _TDL_HandleManager & operator = (const _TDL_HandleManager & theObjectToCopy);
};


#endif /* _TDL_HandleManager_H */

