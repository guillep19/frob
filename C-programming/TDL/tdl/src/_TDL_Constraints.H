/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

#ifndef _TDL_Constraints_H
#define _TDL_Constraints_H

#define _TDL_INTERNAL_
#include "tdl.H"
#include "_TDL_Snode.H"
#include "_TDL_TreeNode.H"
#include "_TDL_TreeNodeBranch.H"
#include "_TDL_Exceptions.H"

/*
 * _TDL_Constraint is an abstract base class for all the Constraint
 * subclasses.  It defines the interface methods that permit constraints
 * to be invoked, as well as the abstract methods that subclasses will
 * define to implement their corresponding constraints.
 */
class _TDL_Constraint : public _TDL_Snode
{
public:
  enum CONSTANTS {
	/* Override-constraint types. */
    MAXIMUM_ACTIVATIONS_TYPE       = 0,
    MAXIMUM_TRIGGERS_TYPE          = 1,
    PERIOD_TYPE                    = 2,
    INITIAL_WAIT_TYPE              = 3,
    ON_AGENT                       = 4,
	/* Relevant Constants */
    NUMBER_OF_OVERRIDE_CONSTRAINTS = 5,
    NO_INDEX                       = 0
  };

public:
  static _TDL_TreeNode & PREVIOUS;
  static _TDL_TreeNode & SELF;

private:
  static u_int8 masterConstraintCount; /*Total number of Constraints created.*/
  


	/* Find's the PREVIOUS tree-node for theNodeToConstrain.
	 *
	 * thePreviousSearchStart is used to indicate where the search for 
	 * "PREVIOUS" should be started.
	 *
	 *   For With-DO constraints, thePreviousSearchStart implies that we
	 *   should search thePreviousSearchStart's parent for a sibling of 
	 *   thePreviousSearchStart that occured before thePreviousSearchStart.
	 *
	 *   For Spawn constraints, when thePreviousSearchStart is NOT NULL,
	 *   thePreviousSearchStart implies that we should search for a child
	 *   of thePreviousSearchStart starting at thePreviousSearchStart's
	 *   last offspring.
	 *
	 *   This distinction for the value of thePreviousSearchStart is
	 *   extremely significant.
	 *
	 * theIsSpawnConstraint is used to indicate whether this is a SPAWN
	 * constraint or a WITH-DO constraint.  It determines how the value
	 * of thePreviousSearchStart is utilized.  (Also:  SPAWN constraints
	 * can resolve PREVIOUS into ancestor WITH-DO statements, whereas
	 * WITH-DO's constraints can not.)
	 */
public:
  static status_t findPrevious ( _TDL_TreeNode   *   theNodeToConstrain,
			   const _TDL_TreeNode   *   thePreviousSearchStart,
				 BOOLEAN             theIsSpawnConstraint,
				 _TDL_TreeNode   * & theReturnValue,
			   const _TDL_Constraint *   theRelevantConstraint );


  static void initializeOverrideConstraintInformation (
				   u_int8 * theOverrideConstraintInformation );


	/* Instance data. */
protected:
  u_int8   constraintIndex; /* Determines order of creation for constraints. */


public:
  _TDL_Constraint ( )
    : _TDL_Snode      (                                           ),
      constraintIndex ( ++ _TDL_Constraint::masterConstraintCount )
  {}

  _TDL_Constraint ( const _TDL_Constraint & theConstraint )
    : iostreamBase(),
      _TDL_Snode      (                                      ),
      constraintIndex ( theConstraint . getConstraintIndex() )
  {}

  virtual ~_TDL_Constraint();

  virtual _TDL_Constraint * clone() const TCM_PURE_VIRTUAL_METHOD ;


  u_int8 getConstraintIndex() const { return constraintIndex; }


	/* Method to determine whether this constraint has been overriden. */
  BOOLEAN shouldOverrideConstraint (
	       _TDL_SpawnStatementTreeNode & theNodeToConstrain,
	       u_int4                        theOverrideConstraintType ) const;



	/* Abstract method.  Implemented in subclasses to actually
	 * perform the TCM commands that invoke a particular constraint
	 *
	 * theReferenceTreeNode is only utilized in subclasses of
	 * _TDL_ConstraintWithReference.  theReferenceTreeNode refers to
	 * a specific spawned-task, and is a completely different entity
	 * than thePreviousSearchStart (which is used to limit the
	 * search for the value of "PREVIOUS").
	 */
  virtual TCM_Return_Type performConstraint (
	        _TDL_SpawnStatementTreeNode & theNodeToConstrain,
	        _TDL_SpawnStatementTreeNode * theReferenceTreeNode
	      ) const TCM_PURE_VIRTUAL_METHOD ;


	/* Workhorse method for applying constraints.  Actually just invokes
	 * performConstraint ( theNodeToConstrain, NULL ).  It is overriden
	 * in some subclasses to invoke performConstraint with a non-NULL
	 * value for theReferenceTreeNode.
	 *
	 * See the Static method findPrevious() for a complete description
	 * of thePreviousSearchStart and theIsSpawnConstraint.
	 */
  virtual TCM_Return_Type applyConstraints (
		    _TDL_SpawnStatementTreeNode & theNodeToConstrain,
	      const _TDL_TreeNode               * thePreviousSearchStart
						    = (_TDL_TreeNode *) NULL,
		    BOOLEAN                       theIsSpawnConstraint
						    = TRUE             ) const;


	/* Primary _TDL_Constraint constraint-invoking interface...
	 * These are the methods that are invoked on a constraint object
	 * to apply a particular constraint.
	 */
		/* General case -- just invokes applyConstraints() */
  virtual TCM_Return_Type applyConstraintsBeforeInsertion (
		    _TDL_SpawnStatementTreeNode & theNodeToConstrain,
	      const _TDL_TreeNode               * thePreviousSearchStart
						    = (_TDL_TreeNode *) NULL,
		    BOOLEAN                       theIsSpawnConstraint
						    = TRUE             ) const;

		/* General case -- just does nothing */
  virtual TCM_Return_Type applyConstraintsAfterInsertion  (
		    _TDL_SpawnStatementTreeNode & theNodeToConstrain,
	      const _TDL_TreeNode               * thePreviousSearchStart
						    = (_TDL_TreeNode *) NULL,
		    BOOLEAN                       theIsSpawnConstraint
						    = TRUE             ) const;


		/* Convenience method:                                       *
		 * Note: theNodeToConstrain must be a SpawnStatementTreeNode */
  TCM_Return_Type applyConstraints (
		    _TDL_TreeNode               * theNodeToConstrain,
	      const _TDL_TreeNode               * thePreviousSearchStart
						    = (_TDL_TreeNode *) NULL,
		    BOOLEAN                       theIsSpawnConstraint
						    = TRUE             ) const;

		/* Convenience method:                                       *
		 * Note: theNodeToConstrain must be a SpawnStatementTreeNode */
  TCM_Return_Type applyConstraintsBeforeInsertion (
		    _TDL_TreeNode               * theNodeToConstrain,
	      const _TDL_TreeNode               * thePreviousSearchStart
						    = (_TDL_TreeNode *) NULL,
		    BOOLEAN                       theIsSpawnConstraint
						    = TRUE             ) const;

		/* Convenience method:                                       *
		 * Note: theNodeToConstrain must be a SpawnStatementTreeNode */
  TCM_Return_Type applyConstraintsAfterInsertion  (
		    _TDL_TreeNode               * theNodeToConstrain,
	      const _TDL_TreeNode               * thePreviousSearchStart
						    = (_TDL_TreeNode *) NULL,
		    BOOLEAN                       theIsSpawnConstraint
						    = TRUE             ) const;


	/* These are no-ops.  They form an interface for use in classes
	 * derived from _TDL_ConstraintWithReference
	 */
  virtual _TDL_TreeNode * getReferenceTreeNode() const;
  virtual void setReferenceTreeNode( _TDL_TreeNode * theReferenceTreeNode );

  BOOLEAN getIsPreviousReference() const
	      { return getReferenceTreeNode() == & _TDL_Constraint::PREVIOUS; }

  BOOLEAN getIsSelfReference() const
	      { return getReferenceTreeNode() == & _TDL_Constraint::SELF; }


	/* Another no-op.  Interface to indicate subclasses that can use *
	 * a reference of NULL.                                          */
  virtual BOOLEAN getCanUseNullReference() const;

	/* On-Agent constraints are a very special case -- they cannot be *
	 * merely applied.  They instead must be queried.                 */
  virtual BOOLEAN isOnAgentConstraint() const; /* { return FALSE; } */

	/* Constraint-name method.  Occasionally useful for common warnings. */
  virtual const char * getConstraintName() const; //{return "_TDL_Constraint";}

	/* iostreamBase interface */
  virtual ostream & printObject ( ostream    & theOstream,
				  const char * theIndentString = "" ) const;
};



/*
 * _TDL_ConstraintWithReference is an abstract base class for all the
 * Constraint subclasses that constrain based on a reference node.
 *
 * _TDL_ConstraintWithReference deals with the details of applying
 * the constraint based on every element of the reference node set.
 *
 * (The actual constraint is invoked through the abstract method
 *  performConstraint() which is inherited from _TDL_Constraint,
 *  and defined in subclasses of _TDL_ConstraintWithReference.)
 */
class _TDL_ConstraintWithReference : public _TDL_Constraint
{
protected:
  _TDL_TreeNode * referenceTreeNode; /* Tag-Task */

public:
  _TDL_ConstraintWithReference ( _TDL_TreeNode & theReferenceTreeNode )
    : _TDL_Constraint   (                        ),
      referenceTreeNode ( & theReferenceTreeNode )
  {}

  _TDL_ConstraintWithReference ( _TDL_TreeNode * theReferenceTreeNode )
    : _TDL_Constraint   (                        ),
      referenceTreeNode (   theReferenceTreeNode )
  {}

  _TDL_ConstraintWithReference (
			   const _TDL_ConstraintWithReference & theConstraint )
    : iostreamBase(),
      _TDL_Constraint   ( theConstraint                     ),
      referenceTreeNode ( theConstraint . referenceTreeNode )
  {}

  virtual ~_TDL_ConstraintWithReference();

  virtual _TDL_Constraint * clone() const TCM_PURE_VIRTUAL_METHOD ;

	/* Workhorse method for perform'ing Constraints on a subtree.
	 *  theReferenceTreeElement is the actual element or subtree
	 *  against which this constraint will be applied.
	 */
  TCM_Return_Type recursivelyPerformConstraints (
		    _TDL_SpawnStatementTreeNode & theNodeToConstrain,
		    _TDL_TreeNode               * theReferenceTreeNode ) const;


	/* Workhorse method for applying constraints */
	/* Override this to deal with "PREVIOUS" case... */
  virtual TCM_Return_Type applyConstraints (
		    _TDL_SpawnStatementTreeNode & theNodeToConstrain,
	      const _TDL_TreeNode               * thePreviousSearchStart
						    = (_TDL_TreeNode *) NULL,
		    BOOLEAN                       theIsSpawnConstraint
						    = TRUE             ) const;


  virtual _TDL_TreeNode * getReferenceTreeNode() const;
  virtual void setReferenceTreeNode( _TDL_TreeNode * theReferenceTreeNode );

  virtual const char * getConstraintName() const;

	/* iostreamBase interface */
  virtual ostream & printObject ( ostream    & theOstream,
				  const char * theIndentString = "" ) const;
};




/*
 * _TDL_ConstraintWithEvent is an abstract base class for all the
 * Constraint subclasses that constrain based on an event.
 *
 * _TDL_ConstraintWithEvent deals with storing the details of the event.
 *
 * (The actual constraint is invoked through the abstract method
 *  performConstraint() which is inherited from _TDL_Constraint,
 *  and defined in subclasses of _TDL_ConstraintWithEvent.)
 */
class _TDL_ConstraintWithEvent : public _TDL_ConstraintWithReference
{
protected:
  const TCM_Interval_Enum  referenceInterval;
  const TCM_Point_Enum     referenceState;

public:
  _TDL_ConstraintWithEvent (       _TDL_TreeNode     & theReferenceTreeNode,
			     const TCM_Interval_Enum & theReferenceInterval,
			     const TCM_Point_Enum    & theReferenceState )
    : _TDL_ConstraintWithReference ( theReferenceTreeNode ),
      referenceInterval            ( theReferenceInterval ),
      referenceState               ( theReferenceState    )
  {}

  _TDL_ConstraintWithEvent (       _TDL_TreeNode     * theReferenceTreeNode,
			     const TCM_Interval_Enum & theReferenceInterval,
			     const TCM_Point_Enum    & theReferenceState )
    : _TDL_ConstraintWithReference ( theReferenceTreeNode ),
      referenceInterval            ( theReferenceInterval ),
      referenceState               ( theReferenceState    )
  {}

  _TDL_ConstraintWithEvent ( const _TDL_ConstraintWithEvent & theConstraint )
    : iostreamBase(),
      _TDL_ConstraintWithReference ( theConstraint                     ),
      referenceInterval            ( theConstraint . referenceInterval ),
      referenceState               ( theConstraint . referenceState    )
  {}

  virtual ~_TDL_ConstraintWithEvent();

  virtual _TDL_Constraint * clone() const TCM_PURE_VIRTUAL_METHOD ;

  const TCM_Interval_Enum & getReferenceInterval() const
						  { return referenceInterval; }
  const TCM_Point_Enum    & getReferenceState()    const
						  { return referenceState;    }

  virtual const char * getConstraintName() const;

	/* iostreamBase interface */
  virtual ostream & printObject ( ostream    & theOstream,
				  const char * theIndentString = "" ) const;
};



/*
 * _TDL_ConstraintOption is an abstract base class for all the
 * Constraint subclasses that constrain an interval (Handling, Expansion,
 * or Execution) of the constrained node.
 *
 * _TDL_ConstraintOption deals with storing the details which interval
 * (Handling, Expansion, or Execution) of the constrained node is being
 * constrained.
 */
class _TDL_ConstraintOption
{
protected:
  const TCM_Interval_Enum   nodeToConstrainInterval;

public:
  _TDL_ConstraintOption ( const TCM_Interval_Enum & theNodeToConstrainInterval)
    : nodeToConstrainInterval ( theNodeToConstrainInterval )
  {}

  _TDL_ConstraintOption ( const _TDL_ConstraintOption & theConstraint )
    : nodeToConstrainInterval ( theConstraint . nodeToConstrainInterval )
  {}

  virtual ~_TDL_ConstraintOption();

  virtual _TDL_Constraint * clone() const TCM_PURE_VIRTUAL_METHOD ;

  const TCM_Interval_Enum & getNodeToConstrainInterval() const
					    { return nodeToConstrainInterval; }

  virtual const char * getConstraintName() const;

	/* iostreamBase interface */
  virtual ostream & printObject ( ostream    & theOstream,
				  const char * theIndentString = "" ) const;
};



/*
 * _TDL_ConstraintTime is an abstract base class for all the
 * Constraint subclasses that constrain based on time.
 *
 * _TDL_ConstraintTime deals with storing the details of that time.
 *
 * Note:  _TDL_ConstraintTime does *NOT* convert between hour:min:sec.fraction
 *        and time-in-MSecs formats.  It's just a lightweight storage class.
 */

class _TDL_ConstraintTime
{
protected:
  const u_int4  hours;
  const u_int4  minutes;
  const u_int4  seconds;
  const double  fractionsOfASecond;
  const MSecs   timeInMSecs;
  const BOOLEAN hasTimeInMSecs;

public:
  _TDL_ConstraintTime ( u_int4  theHours,
			u_int4  theMinutes,
			u_int4  theSeconds,
			double  theFractionsOfASecond )
    : hours              ( theHours              ),
      minutes            ( theMinutes            ),
      seconds            ( theSeconds            ),
      fractionsOfASecond ( theFractionsOfASecond ),
      timeInMSecs        ( _TDL_NO_TIME          ),
      hasTimeInMSecs     ( FALSE                 )
  {}

  _TDL_ConstraintTime ( MSecs   theTimeInMSecs )
    : hours              ( _TDL_NO_TIME_HOURS                 ),
      minutes            ( _TDL_NO_TIME_MINUTES               ),
      seconds            ( _TDL_NO_TIME_SECONDS               ),
      fractionsOfASecond ( _TDL_NO_TIME_FRACTIONS_OF_A_SECOND ),
      timeInMSecs        ( theTimeInMSecs                     ),
      hasTimeInMSecs     ( TRUE                               )
  {}

  _TDL_ConstraintTime ( const _TDL_ConstraintTime & theConstraint )
    : hours              ( theConstraint . hours              ),
      minutes            ( theConstraint . minutes            ),
      seconds            ( theConstraint . seconds            ),
      fractionsOfASecond ( theConstraint . fractionsOfASecond ),
      timeInMSecs        ( theConstraint . timeInMSecs        ),
      hasTimeInMSecs     ( theConstraint . hasTimeInMSecs     )
  {}


  virtual ~_TDL_ConstraintTime();

  virtual _TDL_Constraint * clone() const TCM_PURE_VIRTUAL_METHOD ;

  u_int4  getHours()              const { return hours;              }
  u_int4  getMinutes()            const { return minutes;            }
  u_int4  getSeconds()            const { return seconds;            }
  double  getFractionsOfASecond() const { return fractionsOfASecond; }
  MSecs   getMSecs()              const { return timeInMSecs;        }
  BOOLEAN getHasTimeInMSecs()     const { return hasTimeInMSecs;     }

  virtual const char * getConstraintName() const;

	/* iostreamBase interface */
  virtual ostream & printObject ( ostream    & theOstream,
				  const char * theIndentString = "" ) const;
};


/*****************************************************************************/
/*****************************************************************************/


class _TDL_ExpandFirst : public _TDL_Constraint
{
public:
  _TDL_ExpandFirst() {}

  _TDL_ExpandFirst ( const _TDL_ExpandFirst & theConstraint )
    : iostreamBase(),
      _TDL_Constraint ( theConstraint )
  {}

  virtual ~_TDL_ExpandFirst();

  virtual _TDL_Constraint * clone() const;

  virtual TCM_Return_Type performConstraint (
		  _TDL_SpawnStatementTreeNode & theNodeToConstrain,
		  _TDL_SpawnStatementTreeNode * theReferenceTreeNode ) const;

  virtual const char * getConstraintName() const;

	/* iostreamBase interface */
  virtual ostream & printObject ( ostream    & theOstream,
				  const char * theIndentString = "" ) const;
};


class _TDL_DelayExpansion : public _TDL_Constraint
{
public:
  _TDL_DelayExpansion() {}

  _TDL_DelayExpansion ( const _TDL_DelayExpansion & theConstraint )
    : iostreamBase(),
      _TDL_Constraint ( theConstraint )
  {}

  virtual ~_TDL_DelayExpansion();

  virtual _TDL_Constraint * clone() const;

  virtual TCM_Return_Type performConstraint (
		  _TDL_SpawnStatementTreeNode & theNodeToConstrain,
		  _TDL_SpawnStatementTreeNode * theReferenceTreeNode ) const;

  virtual const char * getConstraintName() const;

	/* iostreamBase interface */
  virtual ostream & printObject ( ostream    & theOstream,
				  const char * theIndentString = "" ) const;
};


class _TDL_SequentialHandling : public _TDL_ConstraintWithReference
{
public:
  _TDL_SequentialHandling ( _TDL_TreeNode & theReferenceTreeNode )
    : _TDL_ConstraintWithReference ( theReferenceTreeNode )
  {}

  _TDL_SequentialHandling ( _TDL_TreeNode * theReferenceTreeNode )
    : _TDL_ConstraintWithReference ( theReferenceTreeNode )
  {}

  _TDL_SequentialHandling ( const _TDL_SequentialHandling & theConstraint )
    : iostreamBase(),
      _TDL_ConstraintWithReference ( theConstraint )
  {}

  virtual ~_TDL_SequentialHandling();

  virtual _TDL_Constraint * clone() const;

  virtual TCM_Return_Type performConstraint (
		  _TDL_SpawnStatementTreeNode & theNodeToConstrain,
		  _TDL_SpawnStatementTreeNode * theReferenceTreeNode ) const;

  virtual const char * getConstraintName() const;

	/* iostreamBase interface */
  virtual ostream & printObject ( ostream    & theOstream,
				  const char * theIndentString = "" ) const;
};


class _TDL_SequentialExpansion : public _TDL_ConstraintWithReference
{
public:
  _TDL_SequentialExpansion ( _TDL_TreeNode & theReferenceTreeNode )
    : _TDL_ConstraintWithReference ( theReferenceTreeNode )
  {}

  _TDL_SequentialExpansion ( _TDL_TreeNode * theReferenceTreeNode )
    : _TDL_ConstraintWithReference ( theReferenceTreeNode )
  {}

  _TDL_SequentialExpansion ( const _TDL_SequentialExpansion & theConstraint )
    : iostreamBase(),
      _TDL_ConstraintWithReference ( theConstraint )
  {}

  virtual ~_TDL_SequentialExpansion();

  virtual _TDL_Constraint * clone() const;

  virtual TCM_Return_Type performConstraint (
		  _TDL_SpawnStatementTreeNode & theNodeToConstrain,
		  _TDL_SpawnStatementTreeNode * theReferenceTreeNode ) const;

  virtual const char * getConstraintName() const;

	/* iostreamBase interface */
  virtual ostream & printObject ( ostream    & theOstream,
				  const char * theIndentString = "" ) const;
};


class _TDL_SequentialExecution : public _TDL_ConstraintWithReference
{
public:
  _TDL_SequentialExecution ( _TDL_TreeNode & theReferenceTreeNode )
    : _TDL_ConstraintWithReference ( theReferenceTreeNode )
  {}

  _TDL_SequentialExecution ( _TDL_TreeNode * theReferenceTreeNode )
    : _TDL_ConstraintWithReference ( theReferenceTreeNode )
  {}

  _TDL_SequentialExecution ( const _TDL_SequentialExecution & theConstraint )
    : iostreamBase(),
      _TDL_ConstraintWithReference ( theConstraint )
  {}

  virtual ~_TDL_SequentialExecution();

  virtual _TDL_Constraint * clone() const;

  virtual TCM_Return_Type performConstraint (
		  _TDL_SpawnStatementTreeNode & theNodeToConstrain,
		  _TDL_SpawnStatementTreeNode * theReferenceTreeNode ) const;

  virtual const char * getConstraintName() const;

	/* iostreamBase interface */
  virtual ostream & printObject ( ostream    & theOstream,
				  const char * theIndentString = "" ) const;
};


class _TDL_Serial : public _TDL_ConstraintWithReference
{
public:
  _TDL_Serial ( _TDL_TreeNode & theReferenceTreeNode )
    : _TDL_ConstraintWithReference ( theReferenceTreeNode )
  {}

  _TDL_Serial ( _TDL_TreeNode * theReferenceTreeNode )
    : _TDL_ConstraintWithReference ( theReferenceTreeNode )
  {}

  _TDL_Serial ( const _TDL_Serial & theConstraint )
    : iostreamBase(),
      _TDL_ConstraintWithReference ( theConstraint )
  {}

  virtual ~_TDL_Serial();

  virtual _TDL_Constraint * clone() const;

  virtual TCM_Return_Type performConstraint (
		  _TDL_SpawnStatementTreeNode & theNodeToConstrain,
		  _TDL_SpawnStatementTreeNode * theReferenceTreeNode ) const;

  virtual const char * getConstraintName() const;

	/* iostreamBase interface */
  virtual ostream & printObject ( ostream    & theOstream,
				  const char * theIndentString = "" ) const;
};


/* Placeholder class -- Parallel Doesn't really do anything... */
class _TDL_Parallel : public _TDL_Constraint
{
public:
  _TDL_Parallel() {}

  _TDL_Parallel ( const _TDL_Parallel & theConstraint )
    : iostreamBase(),
      _TDL_Constraint ( theConstraint )
  {}

  virtual ~_TDL_Parallel();

  virtual _TDL_Constraint * clone() const;

  virtual TCM_Return_Type performConstraint (
		  _TDL_SpawnStatementTreeNode & theNodeToConstrain,
		  _TDL_SpawnStatementTreeNode * theReferenceTreeNode ) const;

  virtual const char * getConstraintName() const;

	/* iostreamBase interface */
  virtual ostream & printObject ( ostream    & theOstream,
				  const char * theIndentString = "" ) const;
};


class _TDL_Wait : public _TDL_Constraint
{
public:
  _TDL_Wait() {}

  _TDL_Wait ( const _TDL_Wait & theConstraint )
    : iostreamBase(),
      _TDL_Constraint ( theConstraint )
  {}

  virtual ~_TDL_Wait();

  virtual _TDL_Constraint * clone() const;

  virtual TCM_Return_Type performConstraint (
		  _TDL_SpawnStatementTreeNode & theNodeToConstrain,
		  _TDL_SpawnStatementTreeNode * theReferenceTreeNode ) const;

  virtual TCM_Return_Type applyConstraintsBeforeInsertion (
		    _TDL_SpawnStatementTreeNode & theNodeToConstrain,
	      const _TDL_TreeNode               * thePreviousSearchStart
						    = (_TDL_TreeNode *) NULL,
		    BOOLEAN                       theIsSpawnConstraint
						    = TRUE             ) const;

  virtual TCM_Return_Type applyConstraintsAfterInsertion  (
		    _TDL_SpawnStatementTreeNode & theNodeToConstrain,
	      const _TDL_TreeNode               * thePreviousSearchStart
						    = (_TDL_TreeNode *) NULL,
		    BOOLEAN                       theIsSpawnConstraint
						    = TRUE             ) const;

  virtual const char * getConstraintName() const;

	/* iostreamBase interface */
  virtual ostream & printObject ( ostream    & theOstream,
				  const char * theIndentString = "" ) const;
};



class _TDL_DisableUntilEvent : public _TDL_ConstraintWithEvent,
			       public _TDL_ConstraintOption
{
public:
  _TDL_DisableUntilEvent(       _TDL_TreeNode     & theReferenceTreeNode,
			  const TCM_Interval_Enum & theReferenceInterval,
			  const TCM_Point_Enum    & theReferenceState,
			  const TCM_Interval_Enum & theNodeToConstrainInterval)
    : _TDL_ConstraintWithEvent ( theReferenceTreeNode,
				 theReferenceInterval,
				 theReferenceState          ),
      _TDL_ConstraintOption    ( theNodeToConstrainInterval )
  {}

  _TDL_DisableUntilEvent(       _TDL_TreeNode     * theReferenceTreeNode,
			  const TCM_Interval_Enum & theReferenceInterval,
			  const TCM_Point_Enum    & theReferenceState,
			  const TCM_Interval_Enum & theNodeToConstrainInterval)
    : _TDL_ConstraintWithEvent ( theReferenceTreeNode,
				 theReferenceInterval,
				 theReferenceState          ),
      _TDL_ConstraintOption    ( theNodeToConstrainInterval )
  {}

  _TDL_DisableUntilEvent ( const _TDL_DisableUntilEvent & theConstraint )
    : iostreamBase(),
      _TDL_ConstraintWithEvent ( theConstraint ),
      _TDL_ConstraintOption    ( theConstraint )
  {}

  virtual ~_TDL_DisableUntilEvent();

  virtual _TDL_Constraint * clone() const;

  virtual TCM_Return_Type performConstraint (
		  _TDL_SpawnStatementTreeNode & theNodeToConstrain,
		  _TDL_SpawnStatementTreeNode * theReferenceTreeNode ) const;

  virtual const char * getConstraintName() const;

	/* iostreamBase interface */
  virtual ostream & printObject ( ostream    & theOstream,
				  const char * theIndentString = "" ) const;
};



class _TDL_DisableUntilTime : public _TDL_Constraint,
			      public _TDL_ConstraintOption,
			      public _TDL_ConstraintTime
{
public:
  _TDL_DisableUntilTime ( const TCM_Interval_Enum & theNodeToConstrainInterval,
			  u_int4                    theHours,
			  u_int4                    theMinutes,
			  u_int4                    theSeconds,
			  double                    theFractionsOfASecond )
    : _TDL_Constraint       (                            ),
      _TDL_ConstraintOption ( theNodeToConstrainInterval ),
      _TDL_ConstraintTime   ( theHours,
			      theMinutes,
			      theSeconds,
			      theFractionsOfASecond      )
  {}

  _TDL_DisableUntilTime ( const TCM_Interval_Enum & theNodeToConstrainInterval,
			  MSecs                     theAbsoluteTime )
    : _TDL_Constraint       (                            ),
      _TDL_ConstraintOption ( theNodeToConstrainInterval ),
      _TDL_ConstraintTime   ( theAbsoluteTime            )
  {}

  _TDL_DisableUntilTime ( const _TDL_DisableUntilTime & theConstraint )
    : iostreamBase(),
      _TDL_Constraint       ( theConstraint ),
      _TDL_ConstraintOption ( theConstraint ),
      _TDL_ConstraintTime   ( theConstraint )
  {}

  virtual ~_TDL_DisableUntilTime();

  virtual _TDL_Constraint * clone() const;

  virtual TCM_Return_Type performConstraint (
		  _TDL_SpawnStatementTreeNode & theNodeToConstrain,
		  _TDL_SpawnStatementTreeNode * theReferenceTreeNode ) const;

  virtual const char * getConstraintName() const;

	/* iostreamBase interface */
  virtual ostream & printObject ( ostream    & theOstream,
				  const char * theIndentString = "" ) const;
};



class _TDL_DisableForTime : public _TDL_ConstraintWithEvent,
			    public _TDL_ConstraintOption,
			    public _TDL_ConstraintTime
{
public:
  _TDL_DisableForTime (       _TDL_TreeNode     & theReferenceTreeNode,
			const TCM_Interval_Enum & theReferenceInterval,
			const TCM_Point_Enum    & theReferenceState,
			const TCM_Interval_Enum & theNodeToConstrainInterval,
			u_int4                    theHours,
			u_int4                    theMinutes,
			u_int4                    theSeconds,
			double                    theFractionsOfASecond )
    : _TDL_ConstraintWithEvent ( theReferenceTreeNode,
				 theReferenceInterval,
				 theReferenceState          ),
      _TDL_ConstraintOption    ( theNodeToConstrainInterval ),
      _TDL_ConstraintTime      ( theHours,
				 theMinutes,
				 theSeconds,
				 theFractionsOfASecond      )
  {}

  _TDL_DisableForTime (       _TDL_TreeNode     * theReferenceTreeNode,
			const TCM_Interval_Enum & theReferenceInterval,
			const TCM_Point_Enum    & theReferenceState,
			const TCM_Interval_Enum & theNodeToConstrainInterval,
			u_int4                    theHours,
			u_int4                    theMinutes,
			u_int4                    theSeconds,
			double                    theFractionsOfASecond )
    : _TDL_ConstraintWithEvent ( theReferenceTreeNode,
				 theReferenceInterval,
				 theReferenceState          ),
      _TDL_ConstraintOption    ( theNodeToConstrainInterval ),
      _TDL_ConstraintTime      ( theHours,
				 theMinutes,
				 theSeconds,
				 theFractionsOfASecond      )
  {}

  _TDL_DisableForTime (       _TDL_TreeNode     & theReferenceTreeNode,
			const TCM_Interval_Enum & theReferenceInterval,
			const TCM_Point_Enum    & theReferenceState,
			const TCM_Interval_Enum & theNodeToConstrainInterval,
			MSecs                     theRelativeTime )
    : _TDL_ConstraintWithEvent ( theReferenceTreeNode,
				 theReferenceInterval,
				 theReferenceState          ),
      _TDL_ConstraintOption    ( theNodeToConstrainInterval ),
      _TDL_ConstraintTime      ( theRelativeTime            )
  {}

  _TDL_DisableForTime (       _TDL_TreeNode     * theReferenceTreeNode,
			const TCM_Interval_Enum & theReferenceInterval,
			const TCM_Point_Enum    & theReferenceState,
			const TCM_Interval_Enum & theNodeToConstrainInterval,
			MSecs                     theRelativeTime )
    : _TDL_ConstraintWithEvent ( theReferenceTreeNode,
				 theReferenceInterval,
				 theReferenceState          ),
      _TDL_ConstraintOption    ( theNodeToConstrainInterval ),
      _TDL_ConstraintTime      ( theRelativeTime            )
  {}

  _TDL_DisableForTime ( const TCM_Interval_Enum & theNodeToConstrainInterval,
			u_int4                    theHours,
			u_int4                    theMinutes,
			u_int4                    theSeconds,
			double                    theFractionsOfASecond )
    : _TDL_ConstraintWithEvent ( (_TDL_TreeNode *) NULL,
				 Unknown_Interval,
				 Unknown_Point              ),
      _TDL_ConstraintOption    ( theNodeToConstrainInterval ),
      _TDL_ConstraintTime      ( theHours,
				 theMinutes,
				 theSeconds,
				 theFractionsOfASecond      )
  {}

  _TDL_DisableForTime ( const TCM_Interval_Enum & theNodeToConstrainInterval,
			MSecs                     theRelativeTime )
    : _TDL_ConstraintWithEvent ( (_TDL_TreeNode *) NULL,
				 Unknown_Interval,
				 Unknown_Point              ),
      _TDL_ConstraintOption    ( theNodeToConstrainInterval ),
      _TDL_ConstraintTime      ( theRelativeTime            )
  {}

  _TDL_DisableForTime ( const _TDL_DisableForTime & theConstraint )
    : iostreamBase(),
      _TDL_ConstraintWithEvent ( theConstraint ),
      _TDL_ConstraintOption    ( theConstraint ),
      _TDL_ConstraintTime      ( theConstraint )
  {}

  virtual ~_TDL_DisableForTime();

  virtual _TDL_Constraint * clone() const;

  virtual TCM_Return_Type performConstraint (
		  _TDL_SpawnStatementTreeNode & theNodeToConstrain,
		  _TDL_SpawnStatementTreeNode * theReferenceTreeNode ) const;

  virtual BOOLEAN getCanUseNullReference() const;

  virtual const char * getConstraintName() const;

	/* iostreamBase interface */
  virtual ostream & printObject ( ostream    & theOstream,
				  const char * theIndentString = "" ) const;
};



class _TDL_TerminateAtEvent : public _TDL_ConstraintWithEvent
{
public:
  _TDL_TerminateAtEvent(       _TDL_TreeNode     & theReferenceTreeNode,
			 const TCM_Interval_Enum & theReferenceInterval,
			 const TCM_Point_Enum    & theReferenceState    )
    : _TDL_ConstraintWithEvent ( theReferenceTreeNode,
				 theReferenceInterval,
				 theReferenceState    )
  {}

  _TDL_TerminateAtEvent(       _TDL_TreeNode     * theReferenceTreeNode,
			 const TCM_Interval_Enum & theReferenceInterval,
			 const TCM_Point_Enum    & theReferenceState    )
    : _TDL_ConstraintWithEvent ( theReferenceTreeNode,
				 theReferenceInterval,
				 theReferenceState    )
  {}

  _TDL_TerminateAtEvent ( const _TDL_TerminateAtEvent & theConstraint )
    : iostreamBase(),
      _TDL_ConstraintWithEvent ( theConstraint )
  {}

  virtual ~_TDL_TerminateAtEvent();

  virtual _TDL_Constraint * clone() const;

  virtual TCM_Return_Type performConstraint (
		  _TDL_SpawnStatementTreeNode & theNodeToConstrain,
		  _TDL_SpawnStatementTreeNode * theReferenceTreeNode ) const;

	/* Treat as terminate in zero time for NULL references */
  virtual BOOLEAN getCanUseNullReference() const;

  virtual const char * getConstraintName() const;

	/* iostreamBase interface */
  virtual ostream & printObject ( ostream    & theOstream,
				  const char * theIndentString = "" ) const;
};



class _TDL_TerminateAtTime : public _TDL_Constraint,
			     public _TDL_ConstraintTime
{
public:
  _TDL_TerminateAtTime ( u_int4 theHours,
			 u_int4 theMinutes,
			 u_int4 theSeconds,
			 double theFractionsOfASecond )
    : _TDL_Constraint     (                       ),
      _TDL_ConstraintTime ( theHours,
			    theMinutes,
			    theSeconds,
			    theFractionsOfASecond )
  {}

  _TDL_TerminateAtTime ( MSecs theAbsoluteTime )
    : _TDL_Constraint     (                    ),
      _TDL_ConstraintTime (    theAbsoluteTime )
  {}

  _TDL_TerminateAtTime ( const _TDL_TerminateAtTime & theConstraint )
    : iostreamBase(),
      _TDL_Constraint     ( theConstraint ),
      _TDL_ConstraintTime ( theConstraint )
  {}

  virtual ~_TDL_TerminateAtTime();

  virtual _TDL_Constraint * clone() const;

  virtual TCM_Return_Type performConstraint (
		  _TDL_SpawnStatementTreeNode & theNodeToConstrain,
		  _TDL_SpawnStatementTreeNode * theReferenceTreeNode ) const;

  virtual const char * getConstraintName() const;

	/* iostreamBase interface */
  virtual ostream & printObject ( ostream    & theOstream,
				  const char * theIndentString = "" ) const;
};



class _TDL_TerminateInTime : public _TDL_ConstraintWithEvent,
			     public _TDL_ConstraintTime
{
public:
  _TDL_TerminateInTime (       _TDL_TreeNode     & theReferenceTreeNode,
			 const TCM_Interval_Enum & theReferenceInterval,
			 const TCM_Point_Enum    & theReferenceState,
			 u_int4                    theHours,
			 u_int4                    theMinutes,
			 u_int4                    theSeconds,
			 double                    theFractionsOfASecond )
    : _TDL_ConstraintWithEvent ( theReferenceTreeNode,
				 theReferenceInterval,
				 theReferenceState     ),
      _TDL_ConstraintTime      ( theHours,
				 theMinutes,
				 theSeconds,
				 theFractionsOfASecond )
  {}

  _TDL_TerminateInTime (       _TDL_TreeNode     * theReferenceTreeNode,
			 const TCM_Interval_Enum & theReferenceInterval,
			 const TCM_Point_Enum    & theReferenceState,
			 u_int4                    theHours,
			 u_int4                    theMinutes,
			 u_int4                    theSeconds,
			 double                    theFractionsOfASecond )
    : _TDL_ConstraintWithEvent ( theReferenceTreeNode,
				 theReferenceInterval,
				 theReferenceState     ),
      _TDL_ConstraintTime      ( theHours,
				 theMinutes,
				 theSeconds,
				 theFractionsOfASecond )
  {}

  _TDL_TerminateInTime (       _TDL_TreeNode     & theReferenceTreeNode,
			 const TCM_Interval_Enum & theReferenceInterval,
			 const TCM_Point_Enum    & theReferenceState,
			 MSecs                     theRelativeTime      )
    : _TDL_ConstraintWithEvent ( theReferenceTreeNode,
				 theReferenceInterval,
				 theReferenceState     ),
      _TDL_ConstraintTime      ( theRelativeTime       )
  {}

  _TDL_TerminateInTime (       _TDL_TreeNode     * theReferenceTreeNode,
			 const TCM_Interval_Enum & theReferenceInterval,
			 const TCM_Point_Enum    & theReferenceState,
			 MSecs                     theRelativeTime      )
    : _TDL_ConstraintWithEvent ( theReferenceTreeNode,
				 theReferenceInterval,
				 theReferenceState     ),
      _TDL_ConstraintTime      ( theRelativeTime       )
  {}

  _TDL_TerminateInTime ( u_int4                    theHours,
			 u_int4                    theMinutes,
			 u_int4                    theSeconds,
			 double                    theFractionsOfASecond )
    : _TDL_ConstraintWithEvent ( (_TDL_TreeNode *) NULL,
				 Unknown_Interval,
				 Unknown_Point           ),
      _TDL_ConstraintTime      ( theHours,
				 theMinutes,
				 theSeconds,
				 theFractionsOfASecond   )
  {}

  _TDL_TerminateInTime ( MSecs                     theRelativeTime )
    : _TDL_ConstraintWithEvent ( (_TDL_TreeNode *) NULL,
				 Unknown_Interval,
				 Unknown_Point           ),
      _TDL_ConstraintTime      ( theRelativeTime         )
  {}

  _TDL_TerminateInTime ( const _TDL_TerminateInTime & theConstraint )
    : iostreamBase(),
      _TDL_ConstraintWithEvent ( theConstraint ),
      _TDL_ConstraintTime      ( theConstraint )
  {}

  virtual ~_TDL_TerminateInTime();

  virtual _TDL_Constraint * clone() const;

  virtual TCM_Return_Type performConstraint (
		  _TDL_SpawnStatementTreeNode & theNodeToConstrain,
		  _TDL_SpawnStatementTreeNode * theReferenceTreeNode ) const;

  virtual BOOLEAN getCanUseNullReference() const;

  virtual const char * getConstraintName() const;

	/* iostreamBase interface */
  virtual ostream & printObject ( ostream    & theOstream,
				  const char * theIndentString = "" ) const;
};



class _TDL_TerminateImmediate : public _TDL_Constraint
{
public:
  _TDL_TerminateImmediate ( )
    : _TDL_Constraint ( )
  {}

  _TDL_TerminateImmediate ( const _TDL_TerminateImmediate & theConstraint )
    : iostreamBase(),
      _TDL_Constraint ( theConstraint )
  {}

  virtual ~_TDL_TerminateImmediate();

  virtual _TDL_Constraint * clone() const;

  virtual TCM_Return_Type performConstraint (
		  _TDL_SpawnStatementTreeNode & theNodeToConstrain,
		  _TDL_SpawnStatementTreeNode * theReferenceTreeNode ) const;

  virtual TCM_Return_Type applyConstraintsBeforeInsertion (
		    _TDL_SpawnStatementTreeNode & theNodeToConstrain,
	      const _TDL_TreeNode               * thePreviousSearchStart
						    = (_TDL_TreeNode *) NULL,
		    BOOLEAN                       theIsSpawnConstraint
						    = TRUE             ) const;

  virtual TCM_Return_Type applyConstraintsAfterInsertion  (
		    _TDL_SpawnStatementTreeNode & theNodeToConstrain,
	      const _TDL_TreeNode               * thePreviousSearchStart
						    = (_TDL_TreeNode *) NULL,
		    BOOLEAN                       theIsSpawnConstraint
						    = TRUE             ) const;

  virtual const char * getConstraintName() const;

	/* iostreamBase interface */
  virtual ostream & printObject ( ostream    & theOstream,
				  const char * theIndentString = "" ) const;
};




   /* Caveat:  Activate only works on Monitors! */
class _TDL_ActivateAtEvent : public _TDL_ConstraintWithEvent
{
public:
  _TDL_ActivateAtEvent (       _TDL_TreeNode     & theReferenceTreeNode,
			 const TCM_Interval_Enum & theReferenceInterval,
			 const TCM_Point_Enum    & theReferenceState   )
    : _TDL_ConstraintWithEvent ( theReferenceTreeNode,
				 theReferenceInterval,
				 theReferenceState    )
  {}

  _TDL_ActivateAtEvent (       _TDL_TreeNode     * theReferenceTreeNode,
			 const TCM_Interval_Enum & theReferenceInterval,
			 const TCM_Point_Enum    & theReferenceState   )
    : _TDL_ConstraintWithEvent ( theReferenceTreeNode,
				 theReferenceInterval,
				 theReferenceState    )
  {}

  _TDL_ActivateAtEvent ( const _TDL_ActivateAtEvent & theConstraint )
    : iostreamBase(),
      _TDL_ConstraintWithEvent ( theConstraint )
  {}

  virtual ~_TDL_ActivateAtEvent();

  virtual _TDL_Constraint * clone() const;

     /* Caveat:  Activate only works on Monitors! */
  virtual TCM_Return_Type performConstraint (
		  _TDL_SpawnStatementTreeNode & theNodeToConstrain,
		  _TDL_SpawnStatementTreeNode * theReferenceTreeNode ) const;

	/* Treat as activate in zero time after handling started *
	 * for NULL references.                                  */
  virtual BOOLEAN getCanUseNullReference() const;

  virtual const char * getConstraintName() const;

	/* iostreamBase interface */
  virtual ostream & printObject ( ostream    & theOstream,
				  const char * theIndentString = "" ) const;
};



   /* Caveat:  Activate only works on Monitors! */
class _TDL_ActivateAtTime : public _TDL_Constraint,
			    public _TDL_ConstraintTime
{
public:
  _TDL_ActivateAtTime ( u_int4 theHours,
			u_int4 theMinutes,
			u_int4 theSeconds,
			double theFractionsOfASecond )
    : _TDL_Constraint     (                       ),
      _TDL_ConstraintTime ( theHours,
			    theMinutes,
			    theSeconds,
			    theFractionsOfASecond )
  {}

  _TDL_ActivateAtTime ( MSecs theAbsoluteTime )
    : _TDL_Constraint     (                   ),
      _TDL_ConstraintTime (   theAbsoluteTime )
  {}

  _TDL_ActivateAtTime ( const _TDL_ActivateAtTime & theConstraint )
    : iostreamBase(),
      _TDL_Constraint     ( theConstraint ),
      _TDL_ConstraintTime ( theConstraint )
  {}

  virtual ~_TDL_ActivateAtTime();

  virtual _TDL_Constraint * clone() const;

     /* Caveat:  Activate only works on Monitors! */
  virtual TCM_Return_Type performConstraint (
		  _TDL_SpawnStatementTreeNode & theNodeToConstrain,
		  _TDL_SpawnStatementTreeNode * theReferenceTreeNode ) const;

  virtual const char * getConstraintName() const;

	/* iostreamBase interface */
  virtual ostream & printObject ( ostream    & theOstream,
				  const char * theIndentString = "" ) const;
};



   /* Caveat:  Activate only works on Monitors! */
class _TDL_ActivateInTime : public _TDL_ConstraintWithEvent,
			    public _TDL_ConstraintTime
{
public:
  _TDL_ActivateInTime (       _TDL_TreeNode     & theReferenceTreeNode,
		        const TCM_Interval_Enum & theReferenceInterval,
			const TCM_Point_Enum    & theReferenceState,
			u_int4                    theHours,
			u_int4                    theMinutes,
			u_int4                    theSeconds,
			double                    theFractionsOfASecond )
    : _TDL_ConstraintWithEvent ( theReferenceTreeNode,
				 theReferenceInterval,
				 theReferenceState     ),
      _TDL_ConstraintTime      ( theHours,
				 theMinutes,
				 theSeconds,
				 theFractionsOfASecond )
  {}

  _TDL_ActivateInTime (       _TDL_TreeNode     * theReferenceTreeNode,
		        const TCM_Interval_Enum & theReferenceInterval,
			const TCM_Point_Enum    & theReferenceState,
			u_int4                    theHours,
			u_int4                    theMinutes,
			u_int4                    theSeconds,
			double                    theFractionsOfASecond )
    : _TDL_ConstraintWithEvent ( theReferenceTreeNode,
				 theReferenceInterval,
				 theReferenceState     ),
      _TDL_ConstraintTime      ( theHours,
				 theMinutes,
				 theSeconds,
				 theFractionsOfASecond )
  {}

  _TDL_ActivateInTime (       _TDL_TreeNode     & theReferenceTreeNode,
		        const TCM_Interval_Enum & theReferenceInterval,
			const TCM_Point_Enum    & theReferenceState,
			MSecs                     theRelativeTime )
    : _TDL_ConstraintWithEvent ( theReferenceTreeNode,
				 theReferenceInterval,
				 theReferenceState     ),
      _TDL_ConstraintTime      ( theRelativeTime       )
  {}

  _TDL_ActivateInTime (       _TDL_TreeNode     * theReferenceTreeNode,
		        const TCM_Interval_Enum & theReferenceInterval,
			const TCM_Point_Enum    & theReferenceState,
			MSecs                     theRelativeTime )
    : _TDL_ConstraintWithEvent ( theReferenceTreeNode,
				 theReferenceInterval,
				 theReferenceState     ),
      _TDL_ConstraintTime      ( theRelativeTime       )
  {}

  _TDL_ActivateInTime ( u_int4                    theHours,
			u_int4                    theMinutes,
			u_int4                    theSeconds,
			double                    theFractionsOfASecond )
    : _TDL_ConstraintWithEvent ( (_TDL_TreeNode *) NULL,
				 Unknown_Interval,
				 Unknown_Point           ),
      _TDL_ConstraintTime      ( theHours,
				 theMinutes,
				 theSeconds,
				 theFractionsOfASecond   )
  {}

  _TDL_ActivateInTime ( MSecs                     theRelativeTime )
    : _TDL_ConstraintWithEvent ( (_TDL_TreeNode *) NULL,
				 Unknown_Interval,
				 Unknown_Point           ),
      _TDL_ConstraintTime      ( theRelativeTime         )
  {}

  _TDL_ActivateInTime ( const _TDL_ActivateInTime & theConstraint )
    : iostreamBase(),
      _TDL_ConstraintWithEvent ( theConstraint ),
      _TDL_ConstraintTime      ( theConstraint )
  {}

  virtual ~_TDL_ActivateInTime();

  virtual _TDL_Constraint * clone() const;

     /* Caveat:  Activate only works on Monitors! */
  virtual TCM_Return_Type performConstraint (
		  _TDL_SpawnStatementTreeNode & theNodeToConstrain,
		  _TDL_SpawnStatementTreeNode * theReferenceTreeNode ) const;

  virtual BOOLEAN getCanUseNullReference() const;

  virtual const char * getConstraintName() const;

	/* iostreamBase interface */
  virtual ostream & printObject ( ostream    & theOstream,
				  const char * theIndentString = "" ) const;
};



   /* Caveat:  Activate only works on Monitors! */
class _TDL_ActivateImmediate : public _TDL_Constraint
{
public:
  _TDL_ActivateImmediate ( )
    : _TDL_Constraint ( )
  {}

  _TDL_ActivateImmediate ( const _TDL_ActivateImmediate & theConstraint )
    : iostreamBase(),
      _TDL_Constraint ( theConstraint )
  {}

  virtual ~_TDL_ActivateImmediate();

  virtual _TDL_Constraint * clone() const;

     /* Caveat:  Activate only works on Monitors! */
  virtual TCM_Return_Type performConstraint (
		  _TDL_SpawnStatementTreeNode & theNodeToConstrain,
		  _TDL_SpawnStatementTreeNode * theReferenceTreeNode ) const;

  virtual TCM_Return_Type applyConstraintsBeforeInsertion (
		    _TDL_SpawnStatementTreeNode & theNodeToConstrain,
	      const _TDL_TreeNode               * thePreviousSearchStart
						    = (_TDL_TreeNode *) NULL,
		    BOOLEAN                       theIsSpawnConstraint
						    = TRUE             ) const;

  virtual TCM_Return_Type applyConstraintsAfterInsertion  (
		    _TDL_SpawnStatementTreeNode & theNodeToConstrain,
	      const _TDL_TreeNode               * thePreviousSearchStart
						    = (_TDL_TreeNode *) NULL,
		    BOOLEAN                       theIsSpawnConstraint
						    = TRUE             ) const;

  virtual const char * getConstraintName() const;

	/* iostreamBase interface */
  virtual ostream & printObject ( ostream    & theOstream,
				  const char * theIndentString = "" ) const;
};





class _TDL_SetMonitorMaximumActivations : public _TDL_Constraint
{
protected:
  const u_int4 maximumActivations;

public:
  _TDL_SetMonitorMaximumActivations( u_int4 theMaximumActivations )
    : _TDL_Constraint    (                       ),
      maximumActivations ( theMaximumActivations )
  {}

  _TDL_SetMonitorMaximumActivations (
		      const _TDL_SetMonitorMaximumActivations & theConstraint )
    : iostreamBase(),
      _TDL_Constraint    ( theConstraint                         ),
      maximumActivations ( theConstraint.getMaximumActivations() )
  {}

  virtual ~_TDL_SetMonitorMaximumActivations();

  virtual _TDL_Constraint * clone() const;

  virtual TCM_Return_Type performConstraint (
		  _TDL_SpawnStatementTreeNode & theNodeToConstrain,
		  _TDL_SpawnStatementTreeNode * theReferenceTreeNode ) const;

  virtual const char * getConstraintName() const;

	/* iostreamBase interface */
  virtual ostream & printObject ( ostream    & theOstream,
				  const char * theIndentString = "" ) const;

  u_int4 getMaximumActivations() const { return maximumActivations; }
};




class _TDL_SetMonitorMaximumTriggers : public _TDL_Constraint
{
protected:
  const u_int4 maximumTriggers;

public:
  _TDL_SetMonitorMaximumTriggers( u_int4 theMaximumTriggers )
    : _TDL_Constraint (                    ),
      maximumTriggers ( theMaximumTriggers )
  {}

  _TDL_SetMonitorMaximumTriggers (
			 const _TDL_SetMonitorMaximumTriggers & theConstraint )
    : iostreamBase(),
      _TDL_Constraint ( theConstraint                      ),
      maximumTriggers ( theConstraint.getMaximumTriggers() )
  {}

  virtual ~_TDL_SetMonitorMaximumTriggers();

  virtual _TDL_Constraint * clone() const;

  virtual TCM_Return_Type performConstraint (
		  _TDL_SpawnStatementTreeNode & theNodeToConstrain,
		  _TDL_SpawnStatementTreeNode * theReferenceTreeNode ) const;

  virtual const char * getConstraintName() const;

	/* iostreamBase interface */
  virtual ostream & printObject ( ostream    & theOstream,
				  const char * theIndentString = "" ) const;

  u_int4 getMaximumTriggers() const { return maximumTriggers; }
};




class _TDL_SetMonitorPeriod : public _TDL_Constraint,
			      public _TDL_ConstraintTime
{
public:
  _TDL_SetMonitorPeriod ( u_int4 theHours,
			  u_int4 theMinutes,
			  u_int4 theSeconds,
			  double theFractionsOfASecond )
    : _TDL_Constraint     (                       ),
      _TDL_ConstraintTime ( theHours,
			    theMinutes,
			    theSeconds,
			    theFractionsOfASecond )
  {}

  _TDL_SetMonitorPeriod ( MSecs theAbsoluteTime )
    : _TDL_Constraint     (                   ),
      _TDL_ConstraintTime (   theAbsoluteTime )
  {}

  _TDL_SetMonitorPeriod ( const _TDL_SetMonitorPeriod & theConstraint )
    : iostreamBase(),
      _TDL_Constraint     ( theConstraint ),
      _TDL_ConstraintTime ( theConstraint )
  {}

  virtual ~_TDL_SetMonitorPeriod();

  virtual _TDL_Constraint * clone() const;

  virtual TCM_Return_Type performConstraint (
		  _TDL_SpawnStatementTreeNode & theNodeToConstrain,
		  _TDL_SpawnStatementTreeNode * theReferenceTreeNode ) const;

  virtual const char * getConstraintName() const;

	/* iostreamBase interface */
  virtual ostream & printObject ( ostream    & theOstream,
				  const char * theIndentString = "" ) const;
};




class _TDL_SetMonitorInitialWait : public _TDL_Constraint
{
protected:
  const BOOLEAN initialWait;

public:
  _TDL_SetMonitorInitialWait ( BOOLEAN theInitialWait )
    : _TDL_Constraint (                ),
      initialWait     ( theInitialWait )
  {}

  _TDL_SetMonitorInitialWait (const _TDL_SetMonitorInitialWait & theConstraint)
    : iostreamBase(),
      _TDL_Constraint ( theConstraint                  ),
      initialWait     ( theConstraint.getInitialWait() )
  {}

  virtual ~_TDL_SetMonitorInitialWait();

  virtual _TDL_Constraint * clone() const;

  virtual TCM_Return_Type performConstraint (
		  _TDL_SpawnStatementTreeNode & theNodeToConstrain,
		  _TDL_SpawnStatementTreeNode * theReferenceTreeNode ) const;

  virtual const char * getConstraintName() const;

	/* iostreamBase interface */
  virtual ostream & printObject ( ostream    & theOstream,
				  const char * theIndentString = "" ) const;

  BOOLEAN getInitialWait() const { return initialWait; }
};





class _TDL_AddExceptionHandler : public _TDL_Constraint
{
protected:
	/* Note:  actionRef and exceptionHandler are the same object.
	 * "exceptionHandler" is the object that we need, and "actionRef"
	 * is used to keep the exceptionHandler from being prematurely deleted.
	 */
  _TDL_ActionRef      actionRef;
  _TDL_BaseHandler  * exceptionHandler;
  int4                index;

public:
  _TDL_AddExceptionHandler ( _TDL_BaseHandler * theExceptionHandler,
			     int4               theIndex            )
    : _TDL_Constraint    (                       ),
      actionRef          ( theExceptionHandler   ),
      exceptionHandler   ( theExceptionHandler   ),
      index              ( theIndex              )
  {}

  _TDL_AddExceptionHandler ( const _TDL_AddExceptionHandler & theConstraint )
    : iostreamBase(),
      _TDL_Constraint    ( theConstraint                    ),
      actionRef          ( theConstraint.exceptionHandler   ),
      exceptionHandler   ( theConstraint.exceptionHandler   ),
      index              ( theConstraint.index              )
  {}

  virtual ~_TDL_AddExceptionHandler();

  virtual _TDL_Constraint * clone() const;

  virtual TCM_Return_Type performConstraint (
		  _TDL_SpawnStatementTreeNode & theNodeToConstrain,
		  _TDL_SpawnStatementTreeNode * theReferenceTreeNode ) const;

  virtual const char * getConstraintName() const;

	/* iostreamBase interface */
  virtual ostream & printObject ( ostream    & theOstream,
				  const char * theIndentString = "" ) const;

  _TDL_BaseHandler * getExceptionHandler() const  { return exceptionHandler; }
  int4               getIndex()            const  { return index;            }
};



	/* Note:  Runs the "Reference" TreeNode if and when *
         *        theNodeToConstrain is terminated.         */
class _TDL_OnTermination : public _TDL_ConstraintWithReference
{
public:
  _TDL_OnTermination ( _TDL_TreeNode & theTreeNodeToRunIfTerminationOccurs )
    : _TDL_ConstraintWithReference ( theTreeNodeToRunIfTerminationOccurs )
  {}

  _TDL_OnTermination ( _TDL_TreeNode * theTreeNodeToRunIfTerminationOccurs )
    : _TDL_ConstraintWithReference ( theTreeNodeToRunIfTerminationOccurs )
  {}

  _TDL_OnTermination ( const _TDL_OnTermination & theConstraint )
    : iostreamBase(),
      _TDL_ConstraintWithReference ( theConstraint )
  {}

  virtual ~_TDL_OnTermination();

  virtual _TDL_Constraint * clone() const;

	/* theReferenceTreeNode === theTreeNodeToRunIfTerminationOccurs */
  virtual TCM_Return_Type performConstraint (
		  _TDL_SpawnStatementTreeNode & theNodeToConstrain,
		  _TDL_SpawnStatementTreeNode * theReferenceTreeNode ) const;

  virtual const char * getConstraintName() const;

	/* Deal with the NULL reference case internally as an error! */
  virtual BOOLEAN getCanUseNullReference() const;

	/* iostreamBase interface */
  virtual ostream & printObject ( ostream    & theOstream,
				  const char * theIndentString = "" ) const;
};



class _TDL_OnAgent : public _TDL_Constraint
{
protected:
  STRING agentName;

public:
  _TDL_OnAgent ( STRING theAgentName )
    : _TDL_Constraint (              ),
      agentName       ( theAgentName )
  {}

  _TDL_OnAgent ( const _TDL_OnAgent & theConstraint )
    : iostreamBase(),
      _TDL_Constraint ( theConstraint                ),
      agentName       ( theConstraint.getAgentName() )
  {}

  virtual ~_TDL_OnAgent();

  virtual _TDL_Constraint * clone() const;

  virtual TCM_Return_Type performConstraint (
		  _TDL_SpawnStatementTreeNode & theNodeToConstrain,
		  _TDL_SpawnStatementTreeNode * theReferenceTreeNode ) const;

  virtual BOOLEAN isOnAgentConstraint() const; /* { return TRUE; } */

  virtual const char * getConstraintName() const;

	/* iostreamBase interface */
  virtual ostream & printObject ( ostream    & theOstream,
				  const char * theIndentString = "" ) const;

  STRING getAgentName() const { return agentName; }
};


#endif /* _TDL_Constraints_H */
