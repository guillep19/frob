/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

#ifndef _TDL_SpawnStatementData_H
#define _TDL_SpawnStatementData_H

#define _TDL_INTERNAL_
#include "tdl.H"
#include "_TDL_Snode.H"
#include "_TDL_SpawnStatement.H"
#include "_TDL_Constraints.H"
#include "_TDL_ConstraintsList.H"

/*
 * _TDL_SpawnStatementData represents the actual data associated with
 * a specific TDL SPAWN invocation. In the event that the TDL SPAWN
 * occurs inside an iterative loop, an instance of this class will
 * represent one (and only one) single occurrence of that iteration.
 * (More _TDL_SpawnStatementData objects will be created for the other
 * iterations.)
 *
 * (See _TDL_SpawnStatement.H for the Association Relationship.)
 */

class _TDL_SpawnStatementTreeNode;

class _TDL_SpawnStatementData : public _TDL_Snode,
				public _TDL_ConstraintsList
{
public:
  enum FLAGS
  {
    NOT_ALLOCATED = 1,
    ALLOCATED,
    RUNNING,
    DESTROYED,
    DELETED
  };


protected:
  _TDL_SpawnStatementData::FLAGS  spawnState;
  TCM_Task_Tree_Ref               tcmTaskTreeRefHandle;

  _TDL_SpawnStatement &           spawnStatement;
  _TDL_Slist                      spawnStatementTreeNodeSlist;

  u_int8			  overrideConstraintInformation [
			     _TDL_Constraint::NUMBER_OF_OVERRIDE_CONSTRAINTS ];
  BOOLEAN                         shouldOverrideConstraints;

  _TDL_OnAgent *                  onAgentConstraint;


public:
  _TDL_SpawnStatementData ( _TDL_SpawnStatement & theSpawnStatement )
    :  _TDL_Snode                  (                                        ),
       _TDL_ConstraintsList        (                                        ),
       spawnState                  ( _TDL_SpawnStatementData::NOT_ALLOCATED ),
       tcmTaskTreeRefHandle        (                                        ),
       spawnStatement              ( theSpawnStatement                      ),
       spawnStatementTreeNodeSlist ( TRUE /* Delete contained objects */    ),
       shouldOverrideConstraints   ( FALSE                                  ),
       onAgentConstraint           ( (_TDL_OnAgent *) NULL                  )
  { _TDL_Constraint::initializeOverrideConstraintInformation (
					     overrideConstraintInformation ); }

  virtual ~_TDL_SpawnStatementData();


	/* Our applying on-agent constraint is cached here during allocation */
  const _TDL_OnAgent * getCachedOnAgentConstraint() const
						  { return onAgentConstraint; }

	/* Support for Delayed Allocation */
  const char * getTaskName()       const
     { return getSpawnStatement() . getTcmTaskTreeNodeName(); }

  const char * getTaskAgentName()  const
     { return (getCachedOnAgentConstraint() == (_TDL_OnAgent *) NULL)
	    ? (const char *) NULL
	    : getCachedOnAgentConstraint() -> getAgentName();
     }


	/* These are used in _TDL_Constraints and _TDL_HandleManager    *
	 * to block previously overridden constraints during insertion. *
	 * They are stored (located) here as a matter of convenience.   */
  u_int8 * getOverrideConstraintInformation()
				      { return overrideConstraintInformation; }
  BOOLEAN getShouldOverrideConstraints() const
					  { return shouldOverrideConstraints; }
  void    setShouldOverrideConstraints( BOOLEAN theShouldOverrideConstraints )
		  { shouldOverrideConstraints = theShouldOverrideConstraints; }



  const _TDL_Slist & getSpawnStatementTreeNodeSlist() const
					{ return spawnStatementTreeNodeSlist; }

  _TDL_SpawnStatementTreeNode * createAndAttachNewSpawnStatementTreeNode();
  status_t                      removeSpawnStatementTreeNode (
	     _TDL_SpawnStatementTreeNode * theSpawnStatementTreeNodeToRemove );


  _TDL_SpawnStatement &   getSpawnStatement() const { return spawnStatement;  }
  _TDL_SpawnStatementData::FLAGS   getState() const { return spawnState;      }
  const char *               getStateString() const;
  const TCM_Task_Tree_Ref &   getNodeHandle() const
					      { return tcmTaskTreeRefHandle; }

      /* If possible, avoid avoid using these methods!                     *
       * They allow one to directly alter the underlying TCM_Task_Tree_Ref */
  TCM_Task_Tree_Ref & getNodeHandleNonConst()  { return tcmTaskTreeRefHandle; }
  void setTCMTaskTreeDirectly ( const TCM_Task_Tree_Ref & theTaskTreeRef )
				  { getNodeHandleNonConst() = theTaskTreeRef; }
      /* Unfortunately, delayed allocation forces us to use this method... */
  TCM_Task_Tree_Ref & getNodeHandleNonConstWithAllocate()
    { operator const TCM_Task_Tree_Ref & (); return getNodeHandleNonConst(); }
  


  BOOLEAN  isAllocated() const
		{ return getState() != _TDL_SpawnStatementData::NOT_ALLOCATED;}

  BOOLEAN  isNotStarted() const
		{ return getState() == _TDL_SpawnStatementData::NOT_ALLOCATED
		    ||   getState() == _TDL_SpawnStatementData::ALLOCATED;    }

  BOOLEAN  isStarted() const
		{ return getState() == _TDL_SpawnStatementData::RUNNING
		    ||   getState() == _TDL_SpawnStatementData::DESTROYED;    }

  BOOLEAN  isDestroyed() const
		{ return getState() == _TDL_SpawnStatementData::DESTROYED;    }

  BOOLEAN  isDistributable() const
		{ return getSpawnStatement() . getAllocationFunction()
					     . isDistributable();             }

	/* Given the potential for things to mess up here, *
	 * lets add a little extra error checking...       */
  status_t  verifyDistributedCorrectly() const;

	/* Returns NULL if an ON-AGENT constraint is not found. */
  _TDL_OnAgent * findOnAgentConstraint() const;



	/* Control methods for this object */
  status_t startRunning ( );
  status_t allocate     ( );
  status_t destroy         ( BOOLEAN   theCanStillBeReferenced = FALSE );
  status_t destroyIfUnused ( BOOLEAN   theCanStillBeReferenced = FALSE );
  status_t doDestroy       ( BOOLEAN   theCanStillBeReferenced,
			     BOOLEAN   theDestroyIfUnused      );

	/* Means by which we can set a "Running" state when our       *
	 * associated _TDL_SpawnStatement's allocateFunction is NULL. */
  status_t startRunningNullSpawnStatementData ( );


  const char * getName() const  { return getSpawnStatement() . getName(); }


	/* Allow automatic conversion to TCM_Task_Tree_Ref.
	 * (Will also transparently allocate() unallocated instances,
	 *  performing lazy allocation.)
	 */
  operator const TCM_Task_Tree_Ref & ();


	/* iostreamBase interface */
  virtual ostream & printObject ( ostream    & theOstream,
				  const char * theIndentString = "" ) const;


  int operator == (const _TDL_SpawnStatementData & theSpawnStatementData) const
				  { return (& theSpawnStatementData) == this; }
  int operator != (const _TDL_SpawnStatementData & theSpawnStatementData) const
				  { return (& theSpawnStatementData) != this; }


protected:
  _TDL_Slist & getSpawnStatementTreeNodeSlistNonConst()
					{ return spawnStatementTreeNodeSlist; }

  void setCachedOnAgentConstraint ( _TDL_OnAgent * theOnAgentConstraint )
				{ onAgentConstraint = theOnAgentConstraint; }



	/* These operations are disabled (prohibited) & unimplemented */
private:
  _TDL_SpawnStatementData (  const _TDL_SpawnStatementData & theObjectToCopy );
  _TDL_SpawnStatementData & operator = (
			     const _TDL_SpawnStatementData & theObjectToCopy );
};

#endif /* _TDL_SpawnStatementData_H */

