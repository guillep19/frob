/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

#include "_TDL_Constraints.H"
#include "_TDL_SpawnStatementTreeNode.H"
#include "_TDL_SpawnStatementData.H"



/*** class _TDL_Constraint ***/

// Needed to handle compiler sublety regarding passing references around
static _TDL_Dlist tempDlist(FALSE);

/*static*/  /* NON-CONST so we can use it later on... */
_TDL_TreeNode & _TDL_Constraint::PREVIOUS
                  = * ( _TDL_SpawnStatement::generateTrivialSpawnStatement (
                                                  "_TDL_Constraint::PREVIOUS" )
                          .  getSpawnStatementData ( tempDlist )
                          -> createAndAttachNewSpawnStatementTreeNode() );

/*static*/  /* NON-CONST so we can use it later on... */
_TDL_TreeNode & _TDL_Constraint::SELF
                  = * ( _TDL_SpawnStatement::generateTrivialSpawnStatement (
                                                      "_TDL_Constraint::SELF" )
                          .  getSpawnStatementData ( tempDlist )
                          -> createAndAttachNewSpawnStatementTreeNode() );


	 /* The total number of Constraints that have been created so far. */
/*static*/ /*private*/
u_int8 _TDL_Constraint::masterConstraintCount = _TDL_Constraint::NO_INDEX;



/*static*/ status_t
 _TDL_Constraint::findPrevious (   _TDL_TreeNode   *   theNodeToConstrain,
			     const _TDL_TreeNode   *   thePreviousSearchStart,
			           BOOLEAN             theIsSpawnConstraint,
			           _TDL_TreeNode   * & theReturnValue,
			     const _TDL_Constraint *   theRelevantConstraint )
{
  const _TDL_TreeNodeBranch   * parentTreeNodeBranch;
        _TDL_TreeNode         * previousTreeNode;
  const _TDL_TreeNode         * oldParentTreeNodeBranch;


	/* Default return value */
  theReturnValue = (_TDL_TreeNode *) NULL;


	/* Handle With-Do / PREVIOUS Case... */  
  if ( theIsSpawnConstraint == FALSE )
  {
	/*   For With-DO constraints, thePreviousSearchStart implies that we
	 *   should search thePreviousSearchStart's parent for a sibling of 
	 *   thePreviousSearchStart that occured before thePreviousSearchStart.
	 */
    if ( thePreviousSearchStart == (_TDL_TreeNode *) NULL )
    {
      TDL::getLogStream()
	<< "[_TDL_Constraint::findPrevious]  Error:  "
	<< "thePreviousSearchStart is NULL.  "
	<< "Unable to find PREVIOUS for WITH-DO constraint." << endl;
      return FAILURE;
    }

    parentTreeNodeBranch = thePreviousSearchStart -> getParent();
    previousTreeNode
      = parentTreeNodeBranch -> getPreviousChild ( thePreviousSearchStart );

	/* If No previous nodes in this with-do block */
    if (   ( previousTreeNode == (_TDL_TreeNode *) NULL            )
	&& ( TDL::getIsReporting ( TDL::TDL_DEBUG | TDL::VERBOSE ) ) )
    {
      TDL::getLogStream()
	<< "[_TDL_Constraint::findPrevious]  TDL_DEBUG:  "
	<< "[With-Do]   No previous nodes for available for \""
	<< theNodeToConstrain -> getName() << "\" Constraint:"
	<< endl;
      theRelevantConstraint -> printObject ( TDL::getLogStream(), "   " );
      TDL::getLogStream() << endl;
    }

    theReturnValue = previousTreeNode;
    return SUCCESS;
  } /* IF ( theIsSpawnConstraint == FALSE ) */


	/* Handle Spawn / PREVIOUS Case... */
  else /* Ie:  theIsSpawnConstraint == TRUE */
  {
    if ( thePreviousSearchStart == (_TDL_TreeNode *) NULL )
    {
      parentTreeNodeBranch = theNodeToConstrain -> getParent();

      if ( parentTreeNodeBranch == (const _TDL_TreeNodeBranch *) NULL )
      {
	TDL::getLogStream()
	  << "[_TDL_Constraint::findPrevious]  Error:  "
	  << "theNodeToConstrain -> getParent() is NULL.  "
	  << "Unable to find PREVIOUS for SPAWN constraint." << endl;
	return FAILURE;
      }

      previousTreeNode
	= parentTreeNodeBranch -> getPreviousChild ( theNodeToConstrain );
    } /*  IF  (  thePreviousSearchStart == (_TDL_TreeNode *) NULL ) */
    else /* Ie:  thePreviousSearchStart != (_TDL_TreeNode *) NULL */
    {
	/*   For Spawn constraints, when thePreviousSearchStart is NOT NULL,
	 *   thePreviousSearchStart implies that we should search for a child
	 *   of thePreviousSearchStart starting at thePreviousSearchStart's
	 *   last offspring.
	 */
      if ( thePreviousSearchStart -> isTreeNodeBranch() == FALSE )
      {
	TDL::getLogStream()
	  << "[_TDL_Constraint::findPrevious]  Warning:  "
	  << "thePreviousSearchStart is not a Tree-Node-Branch Object.  "
	  << "Using thePreviousSearchStart's parent." << endl;

	parentTreeNodeBranch = thePreviousSearchStart -> getParent();

	if ( parentTreeNodeBranch == (const _TDL_TreeNodeBranch *) NULL )
	{
	  TDL::getLogStream()
	    << "[_TDL_Constraint::findPrevious]  Error:  "
	    << "thePreviousSearchStart -> getParent() is NULL.  "
	    << "Unable to find PREVIOUS for SPAWN constraint." << endl;
	  return FAILURE;
	}

	previousTreeNode
	  = parentTreeNodeBranch -> getPreviousChild( thePreviousSearchStart );
      } /*  IF (  thePreviousSearchStart -> isTreeNodeBranch() == FALSE ) */
      else /* Ie: thePreviousSearchStart -> isTreeNodeBranch() == TRUE    */
      {
	parentTreeNodeBranch
	  = thePreviousSearchStart -> getTreeNodeBranchConst();

	if ( parentTreeNodeBranch == (const _TDL_TreeNodeBranch *) NULL )
	{
	  TDL::getLogStream()
	    << "[_TDL_Constraint::findPrevious]  Error:  "
	    << "thePreviousSearchStart -> getTreeNodeBranch() is NULL.  "
	    << "Unable to find PREVIOUS for SPAWN constraint." << endl;
	  return FAILURE;
	}

	previousTreeNode
	  = parentTreeNodeBranch -> getPreviousChild( (_TDL_TreeNode *) NULL );
      }
    } /* IF (thePreviousSearchStart == (_TDL_TreeNode *)NULL) ... ELSE ...*/


    while (   ( parentTreeNodeBranch != (const _TDL_TreeNodeBranch *) NULL )
	   && ( previousTreeNode     == (      _TDL_TreeNode       *) NULL ) )
    {
	/* Searching at or before the parentTreeNodeBranch */
      oldParentTreeNodeBranch = parentTreeNodeBranch;

	/* At the grandparent's level... */
      parentTreeNodeBranch = parentTreeNodeBranch -> getParent();

      if ( parentTreeNodeBranch != (_TDL_TreeNodeBranch *) NULL )
      {
	   /* Actually do the previous-search */
	previousTreeNode
	  = parentTreeNodeBranch -> getPreviousChild (oldParentTreeNodeBranch);
      }
    } /* WHILE ( ... ) */

	/* IF No previous nodes available */
    if (   ( previousTreeNode == ( (_TDL_TreeNode *) NULL )        )
	&& ( TDL::getIsReporting ( TDL::TDL_DEBUG | TDL::VERBOSE ) ) )
    {
      TDL::getLogStream()
	<< "[_TDL_Constraint::findPrevious]  TDL_DEBUG:  "
	<< "[Spawn]   No previous nodes for available for \""
	<< theNodeToConstrain -> getName() << "\" Constraint:"
	<< endl;
      theRelevantConstraint -> printObject ( TDL::getLogStream(), "   " );
      TDL::getLogStream() << endl;
    }

    theReturnValue = previousTreeNode;
    return SUCCESS;
  }  /* IF ( theIsSpawnConstraint == FALSE ) ... ELSE ... */


	/* We should never get here... */
  TDL::getLogStream()
    << "[_TDL_Constraint::findPrevious]  "
    << "Error:  Unable to find PREVIOUS for constraint.  "
    << "Programmer Error encountered."
    << endl;

  return FAILURE;

} /* static status_t findPrevious ( ... ) */




/*static*/ void
_TDL_Constraint::initializeOverrideConstraintInformation (
				    u_int8 * theOverrideConstraintInformation )
{
  for ( int4 i=0;  i < _TDL_Constraint::NUMBER_OF_OVERRIDE_CONSTRAINTS;  i++ )
  {
    theOverrideConstraintInformation [ i ] = _TDL_Constraint::NO_INDEX;
  }
}



/*virtual*/
_TDL_Constraint::~_TDL_Constraint()
{ }



/*
 * Used in _TDL_HandleManager to enable/disable reporting overrides
 * while walking up the internal-to-libtdl tree structure to apply constraints.
 */
BOOLEAN
_TDL_Constraint::shouldOverrideConstraint (
		_TDL_SpawnStatementTreeNode & theNodeToConstrain,
		u_int4                        theOverrideConstraintType ) const
{
  if (   ( theNodeToConstrain
	     . getSpawnStatementData()
	     . getShouldOverrideConstraints() == TRUE )
      && ( theNodeToConstrain
	     . getSpawnStatementData()
	     . getOverrideConstraintInformation() [ theOverrideConstraintType ]
	   > getConstraintIndex() ) )
  {
    return TRUE;
  }
  else
  {
	/* Record that we have run, to override future invocations. */
    theNodeToConstrain
        . getSpawnStatementData()
        . getOverrideConstraintInformation() [ theOverrideConstraintType ]
      = getConstraintIndex();

    return FALSE;
  }
}



/*virtual*/ TCM_Return_Type
_TDL_Constraint::applyConstraints ( 
	_TDL_SpawnStatementTreeNode & theNodeToConstrain,
  const _TDL_TreeNode               * thePreviousSearchStart, /*=NULL*/
	BOOLEAN                       theIsSpawnConstraint    /*=TRUE*/ ) const
{
  _TDL_MARKUSED ( thePreviousSearchStart );
  _TDL_MARKUSED ( theIsSpawnConstraint   );
  return performConstraint ( theNodeToConstrain,
			     (_TDL_SpawnStatementTreeNode *) NULL );
}


/*virtual*/ TCM_Return_Type
_TDL_Constraint::applyConstraintsBeforeInsertion (
	_TDL_SpawnStatementTreeNode & theNodeToConstrain,
  const _TDL_TreeNode               * thePreviousSearchStart, /*=NULL*/
	BOOLEAN                       theIsSpawnConstraint    /*=TRUE*/ ) const
{
  return applyConstraints ( theNodeToConstrain,
			    thePreviousSearchStart,
			    theIsSpawnConstraint   );
}

/*virtual*/ TCM_Return_Type
_TDL_Constraint::applyConstraintsAfterInsertion (
	_TDL_SpawnStatementTreeNode & theNodeToConstrain,
  const _TDL_TreeNode               * thePreviousSearchStart, /*=NULL*/
	BOOLEAN                       theIsSpawnConstraint    /*=TRUE*/ ) const
{
  _TDL_MARKUSED ( theNodeToConstrain     );
  _TDL_MARKUSED ( thePreviousSearchStart );
  _TDL_MARKUSED ( theIsSpawnConstraint   );
  return TCM_Error;
}



	/* Note:  static convenience function. */
static status_t
checkIsSpawnStatementTreeNode ( const char    * theLocationString,
				_TDL_TreeNode * theNodeToConstrain )
{
  if ( theNodeToConstrain == (_TDL_TreeNode *) NULL )
  {
    TDL::getLogStream()
      << "[" << theLocationString << "]  Error:  "
      << "theNodeToConstrain is NULL." << endl;
    return FAILURE;
  }

  if ( theNodeToConstrain -> isSpawnStatementTreeNode() == FALSE )
  {
    TDL::getLogStream()
      << "[" << theLocationString << "]  Error:  "
      << "theNodeToConstrain is NOT a SpawnStatementTreeNode.  It is a:"
      << endl;
    theNodeToConstrain -> printObject ( TDL::getLogStream(), "     " ) << endl;
    return FAILURE;
  }

  return SUCCESS;
}

	/* Convenience method:                                       *
	 * Note: theNodeToConstrain must be a SpawnStatementTreeNode */
TCM_Return_Type
_TDL_Constraint::applyConstraints ( 
	_TDL_TreeNode               * theNodeToConstrain,
  const _TDL_TreeNode               * thePreviousSearchStart, /*=NULL*/
	BOOLEAN                       theIsSpawnConstraint    /*=TRUE*/ ) const
{
  if ( checkIsSpawnStatementTreeNode ( "_TDL_Constraint:applyConstraints",
				       theNodeToConstrain )
       == SUCCESS )
    return applyConstraints (
	     * ( theNodeToConstrain -> getSpawnStatementTreeNode() ),
	     thePreviousSearchStart,
	     theIsSpawnConstraint );
  else
    return TCM_Error;
}


	/* Convenience method:                                       *
	 * Note: theNodeToConstrain must be a SpawnStatementTreeNode */
TCM_Return_Type
_TDL_Constraint::applyConstraintsBeforeInsertion (
	_TDL_TreeNode               * theNodeToConstrain,
  const _TDL_TreeNode               * thePreviousSearchStart, /*=NULL*/
	BOOLEAN                       theIsSpawnConstraint    /*=TRUE*/ ) const
{
  if ( checkIsSpawnStatementTreeNode (
			     "_TDL_Constraint:applyConstraintsBeforeInsertion",
			     theNodeToConstrain )
       == SUCCESS )
    return applyConstraintsBeforeInsertion (
	     * ( theNodeToConstrain -> getSpawnStatementTreeNode() ),
	     thePreviousSearchStart,
	     theIsSpawnConstraint );
  else
    return TCM_Error;
}

	/* Convenience method:                                       *
	 * Note: theNodeToConstrain must be a SpawnStatementTreeNode */
TCM_Return_Type
_TDL_Constraint::applyConstraintsAfterInsertion (
	_TDL_TreeNode               * theNodeToConstrain,
  const _TDL_TreeNode               * thePreviousSearchStart, /*=NULL*/
	BOOLEAN                       theIsSpawnConstraint    /*=TRUE*/ ) const
{
  if ( checkIsSpawnStatementTreeNode ( 
			     "_TDL_Constraint:applyConstraintsAfterInsertion",
			     theNodeToConstrain )
       == SUCCESS )
    return applyConstraintsAfterInsertion (
	     * ( theNodeToConstrain -> getSpawnStatementTreeNode() ),
	     thePreviousSearchStart,
	     theIsSpawnConstraint );
  else
    return TCM_Error;
}



/*virtual*/ _TDL_TreeNode *
_TDL_Constraint::getReferenceTreeNode() const
{
  return (_TDL_TreeNode *)NULL;
}


/*virtual*/ void
_TDL_Constraint::setReferenceTreeNode ( _TDL_TreeNode * theReferenceTreeNode )
{
  _TDL_MARKUSED ( theReferenceTreeNode );
}


/*virtual*/ BOOLEAN 
_TDL_Constraint::getCanUseNullReference() const
{
  return FALSE;
}


 /* On-Agent constraints are a very special case -- they cannot be *
  * merely applied.  They instead must be queried.                 */
/*virtual*/ BOOLEAN 
_TDL_Constraint::isOnAgentConstraint() const
{
  return FALSE;
}


/*virtual*/ const char *
_TDL_Constraint::getConstraintName() const
{
  return "_TDL_Constraint";
}

	/* iostreamBase interface */
/*virtual*/ ostream &
_TDL_Constraint::printObject ( ostream    & theOstream,
			       const char * theIndentString /*=""*/ ) const
{
  _TDL_MARKUSED ( theIndentString );
  return theOstream;
}



/*** class _TDL_ConstraintWithReference ***/


/*virtual*/
_TDL_ConstraintWithReference::~_TDL_ConstraintWithReference()
{
  referenceTreeNode = (_TDL_TreeNode *) NULL;
}


TCM_Return_Type
_TDL_ConstraintWithReference::recursivelyPerformConstraints (
		     _TDL_SpawnStatementTreeNode & theNodeToConstrain,
		     _TDL_TreeNode               * theReferenceTreeNode ) const
{
  TCM_Return_Type   currentValue, returnValue = TCM_Ok;
  _TDL_Dnode      * child;

	/* Is there a reference node? */
  if ( theReferenceTreeNode == (_TDL_TreeNode *) NULL )
  {
    TDL::getLogStream()
      << "[_TDL_ConstraintWithReference::recursivelyPerformConstraints]  "
      << "Error:  theReferenceTreeNode is NULL." << endl;

    return TCM_Error;
  }

	/* Singleton Spawn-Statement-Tree-Node case. */
  if ( theReferenceTreeNode -> isSpawnStatementTreeNode() == TRUE )
  {
    return performConstraint ( theNodeToConstrain,
			       theReferenceTreeNode
			         -> getSpawnStatementTreeNode() );
  }

	/* Handle empty branches. */
  if ( theReferenceTreeNode -> containsSpawnStatementTreeNode() == FALSE )
  {
    TDL::getLogStream()
      << "[_TDL_ConstraintWithReference::recursivelyPerformConstraints]  "
      << "Warning:  EMPTY REFERENCE contains no _TDL_SpawnStatementTreeNode "
      << "objects." << endl;

        /* Use _TDL_Constraint::PREVIOUS has a DESTROYED NODE REFERENCE */
        /* Not every constraint treats having no reference as a NO-OP.  */
    return performConstraint ( theNodeToConstrain,
                               _TDL_Constraint::PREVIOUS );
  }


	/* Handle Branches */
  if (   ( theReferenceTreeNode -> isTreeNodeBranch()        == TRUE )
      || ( theReferenceTreeNode -> isWithStatementTreeNode() == TRUE ) )
  {
    for ( child  = theReferenceTreeNode -> getChildren() -> getFirstNode();
	  child != (_TDL_Dnode *) NULL;
	  child  = child -> getNextNode()
	 )
    {
	/* Don't recurse into an empty [enclosed] treeNodeBranch
	 * when we actually have [enclosed] SpawnStatementTreeNode objects.
	 */
      if (((_TDL_TreeNode *)child) -> containsSpawnStatementTreeNode() == TRUE)
      {
	currentValue
	  = recursivelyPerformConstraints ( theNodeToConstrain,
					    (_TDL_TreeNode *) child );
	if ( currentValue != TCM_Ok )
	  returnValue = currentValue;
      }
    }

    return returnValue;
  }


  TDL::getLogStream()
    << "[_TDL_ConstraintWithReference::recursivelyPerformConstraints]  "
    << "Error:  theReferenceTreeNode is neither Data nor Branch:  "
    << "(Programmer Error encountered.)" << endl;
  theReferenceTreeNode -> printObject ( TDL::getLogStream(), "     " );
  TDL::getLogStream() << endl;
  return TCM_Error;
} /* TCM_Return_Type _TDL_...::recursivelyPerformConstraints ( ... ) */



/*virtual*/ TCM_Return_Type
_TDL_ConstraintWithReference::applyConstraints (
	_TDL_SpawnStatementTreeNode & theNodeToConstrain,
  const _TDL_TreeNode               * thePreviousSearchStart, /*=NULL*/
	BOOLEAN                       theIsSpawnConstraint    /*=TRUE*/ ) const
{
  _TDL_TreeNode * previousTreeNode;


	/* Trivial case... */
  if ( getReferenceTreeNode() == (_TDL_TreeNode *) NULL )
  {
	/* Originally an error.  However, some subclasses were intended
	 * to set the value returned by getReferenceTreeNode() to NULL
	 * as a means of indicating a non-reference subset of that particular
	 * constraint.  (Specifically _TDL_DisableForTime,
	 * _TDL_TerminateInTime, _TDL_ActivateInTime.)
	 *
	 * Given the new non-persistent TCM code, we must now deal better
	 * with NULL tasks.
	 *
	 * Therefore, for _TDL_SequentialHandling, _TDL_SequentialExpansion,
	 * _TDL_SequentialExecution, _TDL_Serial, and _TDL_DisableUntilEvent,
	 * we will now print a warning and return TCM_Ok as a NO-OP.
	 *
	 * For _TDL_TerminateAtEvent we will treat it as a 0 millisecond
	 * time-delayed termination.
	 *
	 * For _TDL_ActivateAtEvent, we will treat it as a 0 millisecond
	 * time-delayed activation AFTER the handling starts.
	 *
	 * _TDL_OnTermination, will deal with NULL internally as an error.
	 */
    if ( getCanUseNullReference() == TRUE )
      return performConstraint ( theNodeToConstrain,
				 (_TDL_SpawnStatementTreeNode *) NULL );
    else
    {
      TDL::getLogStream()
	<< "[_TDL_ConstraintWithReference:applyConstraints]  "
	<< "Warning:  Constraint \"" << getConstraintName() << "\""
	<< "has a null value.  Treating as a NO-OP." << endl;
      return TCM_Ok; /* Constraint is a NO-OP. */
    }
  }


	/* If this is a "SELF" reference. */
	/* Ie: IF  getReferenceTreeNode() == & _TDL_Constraint::SELF */
  if ( getIsSelfReference() == TRUE )
  {
	/* Do the trivial case... */
    return performConstraint (   theNodeToConstrain,
			       & theNodeToConstrain );
  }

	/* If this is ***NOT*** a "PREVIOUS" reference.... */
	/* Ie: IF  getReferenceTreeNode() != & _TDL_Constraint::PREVIOUS */
  if ( getIsPreviousReference() == FALSE )
  {
	/* Do the straightforward reference case... */
    return recursivelyPerformConstraints ( theNodeToConstrain,
					   getReferenceTreeNode() );
  }


	/* Otherwise, do the "PREVIOUS" case... */
  if ( _TDL_Constraint::findPrevious ( & theNodeToConstrain,
				         thePreviousSearchStart,
				         theIsSpawnConstraint,
				         previousTreeNode,
				         this                    ) != SUCCESS )
  {
    TDL::getLogStream()
      << "[_TDL_ConstraintWithReference:applyConstraints]  "
      << "Error:  Unable to find value for PREVIOUS." << endl;
  }

  if ( previousTreeNode == (_TDL_TreeNode *) NULL )
  {
	/* If previousTreeNode is NULL, we have run off the top of a WITH
	 * statement or similar structure.
         *
	 * This is not affected by the new non-persistence TCM code.
	 *
	 * Generally, as above with a NULL task reference, Constraints
	 * that can still function without a reference task should do so.
	 * (This includes _TDL_DisableForTime, _TDL_TerminateInTime,
	 * and _TDL_ActivateInTime, which exploit a NULL task as a means
	 * of indicating a non-reference variant of the constraint.
	 * As well as _TDL_TerminateAtEvent and _TDL_ActivateAtEvent,
	 * which will treat a null reference as a zero-time-delay constraint
	 * invocation.  And _TDL_OnTermination which will fail internally.)
	 *
	 * The remaining constraints are NO-OPs, and should be ignored
	 * for user convenience.
	 */
    if ( getCanUseNullReference() == TRUE )
      return performConstraint ( theNodeToConstrain,
				 (_TDL_SpawnStatementTreeNode *) NULL );
    else
      return TCM_Ok;  /* Just silently kill these Constraints */
  }
  else
  {
    return recursivelyPerformConstraints ( theNodeToConstrain,
					   previousTreeNode   );
  }
}



/*virtual*/ _TDL_TreeNode *
_TDL_ConstraintWithReference::getReferenceTreeNode() const
{
  return referenceTreeNode;
}


/*virtual*/ void
_TDL_ConstraintWithReference::setReferenceTreeNode (
					 _TDL_TreeNode * theReferenceTreeNode )
{
  referenceTreeNode = theReferenceTreeNode;
}


/*virtual*/ const char *
_TDL_ConstraintWithReference::getConstraintName() const
{
  return "_TDL_ConstraintWithReference";
}

	/* iostreamBase interface */
/*virtual*/ ostream &
_TDL_ConstraintWithReference::printObject ( 
				   ostream    & theOstream,
				   const char * theIndentString /*=""*/ ) const
{
  theOstream << theIndentString << " ActualReferenceNode = "
	     << ((void *) getReferenceTreeNode() )
	     << "   (\""
	     << (  (getReferenceTreeNode() == (_TDL_TreeNode *)NULL)
		  ? "NULL"
		  : (  ( getIsPreviousReference() )
		      ? "PREVIOUS"
		      : (  ( getIsSelfReference() )
			  ? "CHILD (SELF)"
			  : getReferenceTreeNode() -> getName() ) ) )
	     << "\")" << endl;
  return _TDL_Constraint::printObject ( theOstream, theIndentString );
}



/*** class _TDL_ConstraintWithEvent ***/

/*virtual*/
_TDL_ConstraintWithEvent::~_TDL_ConstraintWithEvent()
{
	/* Override const & reset values... */
  * ( (TCM_Interval_Enum *) (& referenceInterval) ) = Unknown_Interval;
  * ( (TCM_Point_Enum    *) (& referenceState   ) ) = Unknown_Point;
}

/*virtual*/ const char *
_TDL_ConstraintWithEvent::getConstraintName() const
{
  return "_TDL_ConstraintWithEvent";
}


	/* iostreamBase interface */
/*virtual*/ ostream &
_TDL_ConstraintWithEvent::printObject (
				   ostream    & theOstream,
				   const char * theIndentString /*=""*/ ) const
{
  theOstream << theIndentString << " referenceInterval = ";
  switch ( getReferenceInterval() )
  {
    case Unknown_Interval:    theOstream << "Unknown";    break;
    case Handling_Interval:   theOstream << "Handling";   break;
    case Planning_Interval:   theOstream << "Expansion";  break;
    case Achieving_Interval:  theOstream << "Execution";  break;
    default:
      theOstream << "INVALID ( " << int4(getReferenceInterval()) << " )";
      break;
  }
  theOstream << " Interval" << endl;

  theOstream << theIndentString << " referenceState = ";
  switch ( getReferenceState() )
  {
    case Unknown_Point:  theOstream << "Unknown";    break;
    case Start_Point:    theOstream << "Enabled";    break;
    case End_Point:      theOstream << "Completed";  break;
    default:
      theOstream << "INVALID ( " << int4(getReferenceState()) << " )";
      break;
  }
  theOstream << " State" << endl;

  return _TDL_ConstraintWithReference::printObject ( theOstream,
						     theIndentString );
}



/*** class _TDL_ConstraintOption ***/

/*virtual*/
_TDL_ConstraintOption::~_TDL_ConstraintOption()
{
	/* Override const & reset value... */
  * ( (TCM_Interval_Enum *) (& nodeToConstrainInterval) ) = Unknown_Interval;
}

/*virtual*/ const char *
_TDL_ConstraintOption::getConstraintName() const
{
  return "_TDL_ConstraintOption";
}


/*virtual*/ ostream &
_TDL_ConstraintOption::printObject( ostream    & theOstream,
				    const char * theIndentString /*=""*/) const
{
  theOstream << theIndentString << " NodeToConstrainInterval = ";
  switch ( getNodeToConstrainInterval() )
  {
    case Unknown_Interval:    theOstream << "Unknown";    break;
    case Handling_Interval:   theOstream << "Handling";   break;
    case Planning_Interval:   theOstream << "Expansion";  break;
    case Achieving_Interval:  theOstream << "Execution";  break;
    default:
      theOstream << "INVALID ( " << int4(getNodeToConstrainInterval()) << " )";
      break;
  }
  theOstream << " Interval" << endl;

  return theOstream;
}



/*** class _TDL_ConstraintTime ***/

/*virtual*/
_TDL_ConstraintTime::~_TDL_ConstraintTime()
{
	/* Override const & reset values... */
  * ( (u_int4 *) (& hours   ) )           = _TDL_NO_TIME_HOURS;
  * ( (u_int4 *) (& minutes ) )           = _TDL_NO_TIME_MINUTES;
  * ( (u_int4 *) (& seconds ) )           = _TDL_NO_TIME_SECONDS;
  * ( (double *) (& fractionsOfASecond ) )= _TDL_NO_TIME_FRACTIONS_OF_A_SECOND;
  * ( (MSecs  *) (& timeInMSecs ) )       = _TDL_NO_TIME;
}


/*virtual*/ const char *
_TDL_ConstraintTime::getConstraintName() const
{
  return "_TDL_ConstraintTime";
}


/*virtual*/ ostream &
_TDL_ConstraintTime::printObject ( ostream    & theOstream,
				   const char * theIndentString /*=""*/ ) const
{
  theOstream << theIndentString << " Time= ";

  if ( getHasTimeInMSecs() == TRUE )
  {
    theOstream << getMSecs()  << "  (MSecs)";
  }
  else
  {
    theOstream << getHours()              << " : "
	       << getMinutes()            << " : "
	       << getSeconds()            << " . "
	       << getFractionsOfASecond();
  }
  return theOstream;
}



/*****************************************************************************/
/*****************************************************************************/

/*** _TDL_ExpandFirst ***/

/*virtual*/
_TDL_ExpandFirst::~_TDL_ExpandFirst()
{}

/*virtual*/ _TDL_Constraint *
_TDL_ExpandFirst::clone() const
{
  return new _TDL_ExpandFirst ( *this );
}

/*virtual*/ TCM_Return_Type
_TDL_ExpandFirst::performConstraint (
		     _TDL_SpawnStatementTreeNode & theNodeToConstrain,
		     _TDL_SpawnStatementTreeNode * theReferenceTreeNode ) const
{
  if ( theNodeToConstrain . isDestroyed() == TRUE )
  {
    TDL::getLogStream()
      << "[_TDL_ExpandFirst:performConstraint]  Error:  "
      << "Constraining Destroyed Node." << endl;
    return TCM_Error;
  }

	/* Constraining running node is unlikely to work... */
  if ( theNodeToConstrain . getState() == _TDL_SpawnStatementData::RUNNING )
  {
    TDL::getLogStream()
      << "[_TDL_ExpandFirst:performConstraint]  Warning:  Constraining "
      << "node that has already started running is unlikely to work." << endl;
  }


	/* Disable Compiler warnings about unused variables */
  _TDL_MARKUSED ( theReferenceTreeNode );

	/* Lets be verbose */
  if ( TDL::getIsReporting ( TDL::VERBOSE_BRIEF ) )
  {
    TDL::getLogStream()
      << "_TDL_ExpandFirst ( Constrain=" << theNodeToConstrain . getName();
    theNodeToConstrain . printArrayIndexes ( TDL::getLogStream() );
    TDL::getLogStream()
      << " )" << endl;
  }
  if ( TDL::getIsReporting ( TDL::VERBOSE_FULL ) )
  {
    printObject ( TDL::getLogStream(), "" );
    TDL::getLogStream() << endl;
  }

#ifndef _TDL_DISABLE_CONSTRAINTS_FOR_TESTING
  return _TDL_DO_TCM_EXPAND_FIRST ( theNodeToConstrain );
#else
  _TDL_MARKUSED ( theNodeToConstrain );
  return TCM_Ok;
#endif
}

/*virtual*/ const char *
_TDL_ExpandFirst::getConstraintName() const
{
  return "_TDL_ExpandFirst";
}


	/* iostreamBase interface */
/*virtual*/ ostream &
_TDL_ExpandFirst::printObject ( ostream    & theOstream,
				const char * theIndentString /*=""*/ ) const
{
  theOstream << theIndentString << "Constraint:  _TDL_ExpandFirst  "
	     << ((void *)this)  << endl;
  return _TDL_Constraint::printObject ( theOstream, theIndentString );
}



/*** class _TDL_DelayExpansion ***/

/*virtual*/
_TDL_DelayExpansion::~_TDL_DelayExpansion()
{}

/*virtual*/ _TDL_Constraint *
_TDL_DelayExpansion::clone() const
{
  return new _TDL_DelayExpansion ( *this );
}


/*virtual*/ TCM_Return_Type
_TDL_DelayExpansion::performConstraint (
		     _TDL_SpawnStatementTreeNode & theNodeToConstrain,
		     _TDL_SpawnStatementTreeNode * theReferenceTreeNode ) const
{
  if ( theNodeToConstrain . isDestroyed() == TRUE )
  {
    TDL::getLogStream()
      << "[_TDL_DelayExpansion:performConstraint]  Error:  "
      << "Constraining Destroyed Node." << endl;
    return TCM_Error;
  }

	/* Constraining running node is unlikely to work... */
  if ( theNodeToConstrain . getState() == _TDL_SpawnStatementData::RUNNING )
  {
    TDL::getLogStream()
      << "[_TDL_DelayExpansion:performConstraint]  Warning:  Constraining "
      << "node that has already started running is unlikely to work." << endl;
  }

	/* Disable Compiler warnings about unused variables */
  _TDL_MARKUSED ( theReferenceTreeNode );

	/* Lets be verbose */
  if ( TDL::getIsReporting ( TDL::VERBOSE_BRIEF ) )
  {
    TDL::getLogStream()
      << "_TDL_DelayExpansion ( Constrain=" << theNodeToConstrain.getName();
    theNodeToConstrain . printArrayIndexes ( TDL::getLogStream() );
    TDL::getLogStream()
      << " )" << endl;
  }
  if ( TDL::getIsReporting ( TDL::VERBOSE_FULL ) )
  {
    printObject ( TDL::getLogStream(), "" );
    TDL::getLogStream() << endl;
  }

#ifndef _TDL_DISABLE_CONSTRAINTS_FOR_TESTING
  return _TDL_DO_TCM_DELAY_EXPANSION ( theNodeToConstrain );
#else
  _TDL_MARKUSED ( theNodeToConstrain );
  return TCM_Ok;
#endif
}

/*virtual*/ const char *
_TDL_DelayExpansion::getConstraintName() const
{
  return "_TDL_DelayExpansion";
}


	/* iostreamBase interface */
/*virtual*/ ostream &
_TDL_DelayExpansion::printObject ( ostream    & theOstream,
				   const char * theIndentString /*=""*/ ) const
{
  theOstream << theIndentString << "Constraint:  _TDL_DelayExpansion  ("
	     << ((void *)this)  << ")" << endl;
  return _TDL_Constraint::printObject ( theOstream, theIndentString );
}



/*** class _TDL_SequentialHandling ***/

/*virtual*/
_TDL_SequentialHandling::~_TDL_SequentialHandling()
{}

/*virtual*/ _TDL_Constraint *
_TDL_SequentialHandling::clone() const
{
  return new _TDL_SequentialHandling ( *this );
}


/*virtual*/ TCM_Return_Type
_TDL_SequentialHandling::performConstraint (
		     _TDL_SpawnStatementTreeNode & theNodeToConstrain,
		     _TDL_SpawnStatementTreeNode * theReferenceTreeNode ) const
{
  if ( theNodeToConstrain . isDestroyed() == TRUE )
  {
    TDL::getLogStream()
      << "[_TDL_SequentialHandling:performConstraint]  Error:  "
      << "Constraining Destroyed Node." << endl;
    return TCM_Error;
  }

  if ( theReferenceTreeNode == (_TDL_SpawnStatementTreeNode *) NULL )
  {
    TDL::getLogStream()
      << "[_TDL_SequentialHandling:performConstraint]  Error:  "
      << "NULL value for theReferenceTreeNode." << endl;
    return TCM_Error;
  }

	/* Constraining running node is unlikely to work... */
  if ( theNodeToConstrain . getState() == _TDL_SpawnStatementData::RUNNING )
  {
    TDL::getLogStream()
      << "[_TDL_SequentialHandling:performConstraint]  Warning:  Constraining "
      << "node that has already started running is unlikely to work." << endl;
  }

	/* Lets be verbose */
  if ( TDL::getIsReporting ( TDL::VERBOSE_BRIEF ) )
  {
    TDL::getLogStream()
      << "_TDL_SequentialHandling ( Constrain="
      << theNodeToConstrain . getName();
    theNodeToConstrain . printArrayIndexes ( TDL::getLogStream() );
    TDL::getLogStream()
      << " , Ref=" << theReferenceTreeNode -> getName();
    theReferenceTreeNode -> printArrayIndexes ( TDL::getLogStream() );
    TDL::getLogStream()
      << " [Ref_flags: " << theReferenceTreeNode -> getStateString() << "]"
      << " )" << endl;
  }
  if ( TDL::getIsReporting ( TDL::VERBOSE_FULL ) )
  {
    printObject ( TDL::getLogStream(), "" );
    TDL::getLogStream() << endl;
  }

	/* If the reference node is destroyed,  *
	 * there's nothing to constrain against */
  if ( theReferenceTreeNode -> isDestroyed() == TRUE )
  {
    if ( TDL::getIsReporting ( TDL::VERBOSE_BRIEF ) )
    {
      TDL::getLogStream()
	<< "[_TDL_SequentialHandling:performConstraint]  Warning:  "
	<< "theReferenceTreeNode is destroyed.  Skipping Constraint..." << endl;
    }
    return TCM_Ok;
  }


#ifndef _TDL_DISABLE_CONSTRAINTS_FOR_TESTING
  return _TDL_DO_TCM_SEQUENTIAL_HANDLING ( * theReferenceTreeNode,
					     theNodeToConstrain );
#else
  _TDL_MARKUSED ( theNodeToConstrain   );
  _TDL_MARKUSED ( theReferenceTreeNode );
  return TCM_Ok;
#endif
}

/*virtual*/ const char *
_TDL_SequentialHandling::getConstraintName() const
{
  return "_TDL_SequentialHandling";
}


/*virtual*/ ostream &
_TDL_SequentialHandling::printObject (
				   ostream    & theOstream,
				   const char * theIndentString /*=""*/ ) const
{
  theOstream << theIndentString << "Constraint:  _TDL_SequentialHandling  ("
	     << ((void *)this)  << ")" << endl;
  return _TDL_ConstraintWithReference::printObject ( theOstream,
						     theIndentString );
}



/*** class _TDL_SequentialExpansion ***/

/*virtual*/
_TDL_SequentialExpansion::~_TDL_SequentialExpansion()
{}

/*virtual*/ _TDL_Constraint *
_TDL_SequentialExpansion::clone() const
{
  return new _TDL_SequentialExpansion ( *this );
}


/*virtual*/ TCM_Return_Type
_TDL_SequentialExpansion::performConstraint (
		     _TDL_SpawnStatementTreeNode & theNodeToConstrain,
		     _TDL_SpawnStatementTreeNode * theReferenceTreeNode ) const
{
  if ( theNodeToConstrain . isDestroyed() == TRUE )
  {
    TDL::getLogStream()
      << "[_TDL_SequentialExpansion:performConstraint]  Error:  "
      << "Constraining Destroyed Node." << endl;
    return TCM_Error;
  }

  if ( theReferenceTreeNode == (_TDL_SpawnStatementTreeNode *) NULL )
  {
    TDL::getLogStream()
      << "[_TDL_SequentialExpansion:performConstraint]  Error:  "
      << "NULL value for theReferenceTreeNode."
      << endl;
    return TCM_Error;
  }

	/* Constraining running node is unlikely to work... */
  if ( theNodeToConstrain . getState() == _TDL_SpawnStatementData::RUNNING )
  {
    TDL::getLogStream()
      << "[_TDL_SequentialExpansion:performConstraint]  Warning:  Constraining"
      << " node that has already started running is unlikely to work." << endl;
  }

	/* Lets be verbose */
  if ( TDL::getIsReporting ( TDL::VERBOSE_BRIEF ) )
  {
    TDL::getLogStream()
      << "_TDL_SequentialExpansion ( Constrain="
      << theNodeToConstrain . getName();
    theNodeToConstrain . printArrayIndexes ( TDL::getLogStream() );
    TDL::getLogStream()
      << " , Ref=" << theReferenceTreeNode -> getName();
    theReferenceTreeNode -> printArrayIndexes ( TDL::getLogStream() );
    TDL::getLogStream()
      << " [Ref_flags: " << theReferenceTreeNode -> getStateString() << "]"
      << " )" << endl;
  }
  if ( TDL::getIsReporting ( TDL::VERBOSE_FULL ) )
  {
    printObject ( TDL::getLogStream(), "" );
    TDL::getLogStream() << endl;
  }

	/* If the reference node is destroyed,  *
	 * there's nothing to constrain against */
  if ( theReferenceTreeNode -> isDestroyed() == TRUE )
  {
    if ( TDL::getIsReporting ( TDL::VERBOSE_BRIEF ) )
    {
      TDL::getLogStream()
	<< "[_TDL_SequentialExpansion:performConstraint]  Warning:  "
	<< "theReferenceTreeNode is destroyed.  Skipping Constraint..."
	<< endl;
    }
    return TCM_Ok;
  }


#ifndef _TDL_DISABLE_CONSTRAINTS_FOR_TESTING
  return _TDL_DO_TCM_SEQUENTIAL_EXPANSION ( * theReferenceTreeNode,
					      theNodeToConstrain );
#else
  _TDL_MARKUSED ( theNodeToConstrain   );
  _TDL_MARKUSED ( theReferenceTreeNode );
  return TCM_Ok;
#endif
}

/*virtual*/ const char *
_TDL_SequentialExpansion::getConstraintName() const
{
  return "_TDL_SequentialExpansion";
}


/*virtual*/ ostream &
_TDL_SequentialExpansion::printObject (
				   ostream    & theOstream,
				   const char * theIndentString /*=""*/ ) const
{
  theOstream << theIndentString << "Constraint:  _TDL_SequentialExpansion  ("
	     << ((void *)this)  << ")" << endl;
  return _TDL_ConstraintWithReference::printObject ( theOstream,
						     theIndentString );
}



/*** class _TDL_SequentialExecution ***/

/*virtual*/
_TDL_SequentialExecution::~_TDL_SequentialExecution()
{}

/*virtual*/ _TDL_Constraint *
_TDL_SequentialExecution::clone() const
{
  return new _TDL_SequentialExecution ( *this );
}


/*virtual*/ TCM_Return_Type
_TDL_SequentialExecution::performConstraint (
		     _TDL_SpawnStatementTreeNode & theNodeToConstrain,
		     _TDL_SpawnStatementTreeNode * theReferenceTreeNode ) const
{
  if ( theNodeToConstrain . isDestroyed() == TRUE )
  {
    TDL::getLogStream()
      << "[_TDL_SequentialExecution:performConstraint]  Error:  "
      << "Constraining Destroyed Node." << endl;
    return TCM_Error;
  }

  if ( theReferenceTreeNode == (_TDL_SpawnStatementTreeNode *) NULL )
  {
    TDL::getLogStream()
      << "[_TDL_SequentialExecution:performConstraint]  Error:  "
      << "NULL value for theReferenceTreeNode."
      << endl;
    return TCM_Error;
  }

	/* Constraining running node is unlikely to work... */
  if ( theNodeToConstrain . getState() == _TDL_SpawnStatementData::RUNNING )
  {
    TDL::getLogStream()
      << "[_TDL_SequentialExecution:performConstraint]  Warning:  Constraining"
      << " node that has already started running is unlikely to work." << endl;
  }

	/* Lets be verbose */
  if ( TDL::getIsReporting ( TDL::VERBOSE_BRIEF ) )
  {
    TDL::getLogStream()
      << "_TDL_SequentialExecution ( Constrain="
      << theNodeToConstrain . getName();
    theNodeToConstrain . printArrayIndexes ( TDL::getLogStream() );
    TDL::getLogStream()
      << " , Ref=" << theReferenceTreeNode -> getName();
    theReferenceTreeNode -> printArrayIndexes ( TDL::getLogStream() );
    TDL::getLogStream()
      << " [Ref_flags: " << theReferenceTreeNode -> getStateString() << "]"
      << " )" << endl;
  }
  if ( TDL::getIsReporting ( TDL::VERBOSE_FULL ) )
  {
    printObject ( TDL::getLogStream(), "" );
    TDL::getLogStream() << endl;
  }

	/* If the reference node is destroyed,  *
	 * there's nothing to constrain against */
  if ( theReferenceTreeNode -> isDestroyed() == TRUE )
  {
    if ( TDL::getIsReporting ( TDL::VERBOSE_BRIEF ) )
    {
      TDL::getLogStream()
	<< "[_TDL_SequentialExecution:performConstraint]  Warning:  "
	<< "theReferenceTreeNode is destroyed.  Skipping Constraint..."
	<< endl;
    }
    return TCM_Ok;
  }


#ifndef _TDL_DISABLE_CONSTRAINTS_FOR_TESTING
  return _TDL_DO_TCM_SEQUENTIAL_EXECUTION ( * theReferenceTreeNode,
					      theNodeToConstrain );
#else
  _TDL_MARKUSED ( theNodeToConstrain   );
  _TDL_MARKUSED ( theReferenceTreeNode );
  return TCM_Ok;
#endif
}

/*virtual*/ const char *
_TDL_SequentialExecution::getConstraintName() const
{
  return "_TDL_SequentialExecution";
}

/*virtual*/ ostream &
_TDL_SequentialExecution::printObject (
				   ostream    & theOstream,
				   const char * theIndentString /*=""*/ ) const
{
  theOstream << theIndentString << "Constraint:  _TDL_SequentialExecution  ("
	     << ((void *)this)  << ")" << endl;
  return _TDL_ConstraintWithReference::printObject ( theOstream,
						     theIndentString );
}



/*** class _TDL_Serial ***/

/*virtual*/
_TDL_Serial::~_TDL_Serial()
{}

/*virtual*/ _TDL_Constraint *
_TDL_Serial::clone() const
{
  return new _TDL_Serial ( *this );
}


/*virtual*/ TCM_Return_Type
_TDL_Serial::performConstraint ( 
		     _TDL_SpawnStatementTreeNode & theNodeToConstrain,
		     _TDL_SpawnStatementTreeNode * theReferenceTreeNode ) const
{
  if ( theNodeToConstrain . isDestroyed() == TRUE )
  {
    TDL::getLogStream()
      << "[_TDL_Serial:performConstraint]  Error:  "
      << "Constraining Destroyed Node." << endl;
    return TCM_Error;
  }

  if ( theReferenceTreeNode == (_TDL_SpawnStatementTreeNode *) NULL )
  {
    TDL::getLogStream()
      << "[_TDL_Serial:performConstraint]  Error:  "
      << "NULL value for theReferenceTreeNode."
      << endl;
    return TCM_Error;
  }

	/* Constraining running node is unlikely to work... */
  if ( theNodeToConstrain . getState() == _TDL_SpawnStatementData::RUNNING )
  {
    TDL::getLogStream()
      << "[_TDL_Serial:performConstraint]  Warning:  Constraining "
      << "node that has already started running is unlikely to work." << endl;
  }

	/* Lets be verbose */
  if ( TDL::getIsReporting ( TDL::VERBOSE_BRIEF ) )
  {
    TDL::getLogStream()
      << "_TDL_Serial ( Constrain=" << theNodeToConstrain . getName();
    theNodeToConstrain . printArrayIndexes ( TDL::getLogStream() );
    TDL::getLogStream()
      << " , Ref=" << theReferenceTreeNode -> getName();
    theReferenceTreeNode -> printArrayIndexes ( TDL::getLogStream() );
    TDL::getLogStream()
      << " [Ref_flags: " << theReferenceTreeNode -> getStateString() << "]"
      << " )" << endl;
  }
  if ( TDL::getIsReporting ( TDL::VERBOSE_FULL ) )
  {
    printObject ( TDL::getLogStream(), "" );
    TDL::getLogStream() << endl;
  }

	/* If the reference node is destroyed,  *
	 * there's nothing to constrain against */
  if ( theReferenceTreeNode -> isDestroyed() == TRUE )
  {
    if ( TDL::getIsReporting ( TDL::VERBOSE_BRIEF ) )
    {
      TDL::getLogStream()
	<< "[_TDL_Serial:performConstraint]  Warning:  "
	<< "theReferenceTreeNode is destroyed.  Skipping Constraint..."
	<< endl;
    }
    return TCM_Ok;
  }


#ifndef _TDL_DISABLE_CONSTRAINTS_FOR_TESTING
  return _TDL_DO_TCM_SERIAL ( * theReferenceTreeNode,
			        theNodeToConstrain );
#else
  _TDL_MARKUSED ( theNodeToConstrain   );
  _TDL_MARKUSED ( theReferenceTreeNode );
  return TCM_Ok;
#endif
}

/*virtual*/ const char *
_TDL_Serial::getConstraintName() const
{
  return "_TDL_Serial";
}

/*virtual*/ ostream &
_TDL_Serial::printObject ( ostream    & theOstream,
			   const char * theIndentString /*=""*/ ) const
{
  theOstream << theIndentString << "Constraint:  _TDL_Serial  ("
	     << ((void *)this)  << ")" << endl;
  return _TDL_ConstraintWithReference::printObject ( theOstream,
						     theIndentString );
}



/*** class _TDL_Parallel ***/

 /* Placeholder class */

/*virtual*/
_TDL_Parallel::~_TDL_Parallel()
{}

/*virtual*/ _TDL_Constraint *
_TDL_Parallel::clone() const
{
  return new _TDL_Parallel ( *this );
}


/*virtual*/ TCM_Return_Type
_TDL_Parallel::performConstraint(
		     _TDL_SpawnStatementTreeNode & theNodeToConstrain,
		     _TDL_SpawnStatementTreeNode * theReferenceTreeNode ) const
{
  if ( theNodeToConstrain . isDestroyed() == TRUE )
  {
    TDL::getLogStream()
      << "[_TDL_Parallel:performConstraint]  Warning:  "
      << "Constraining Destroyed Node." << endl;
  }

	/* Lets be verbose */
  if ( TDL::getIsReporting ( TDL::VERBOSE_BRIEF ) )
  {
    TDL::getLogStream()
      << "_TDL_Parallel ( Constrain=" << theNodeToConstrain . getName();
    theNodeToConstrain . printArrayIndexes ( TDL::getLogStream() );
    TDL::getLogStream()
      << " )" << endl;
  }
  if ( TDL::getIsReporting ( TDL::VERBOSE_FULL ) )
  {
    printObject ( TDL::getLogStream(), "" );
    TDL::getLogStream() << endl;
  }

	/* Disable Compiler warnings about unused variables */
  _TDL_MARKUSED ( theNodeToConstrain   );
  _TDL_MARKUSED ( theReferenceTreeNode );
  return TCM_Ok;
}

/*virtual*/ const char *
_TDL_Parallel::getConstraintName() const
{
  return "_TDL_Parallel";
}

/*virtual*/ ostream &
_TDL_Parallel::printObject ( ostream    & theOstream,
			     const char * theIndentString /*=""*/ ) const
{
  theOstream << theIndentString << "Constraint:  _TDL_Parallel  "
	     << ((void *)this)  << endl;
  return _TDL_Constraint::printObject ( theOstream, theIndentString );
}



/*** class _TDL_Wait ***/

/*virtual*/
_TDL_Wait::~_TDL_Wait()
{}

/*virtual*/ _TDL_Constraint *
_TDL_Wait::clone() const
{
  return new _TDL_Wait ( *this );
}


/*virtual*/ TCM_Return_Type
_TDL_Wait::performConstraint (
		     _TDL_SpawnStatementTreeNode & theNodeToConstrain,
		     _TDL_SpawnStatementTreeNode * theReferenceTreeNode ) const
{
  if ( theNodeToConstrain . isDestroyed() == TRUE )
  {
    TDL::getLogStream()
      << "[_TDL_Wait:performConstraint]  Error:  "
      << "Constraining Destroyed Node." << endl;
    return TCM_Error;
  }

	/* Wait on a non-running node will wait forever... */
  if ( theNodeToConstrain . getState() != _TDL_SpawnStatementData::RUNNING )
  {
    TDL::getLogStream()
      << "[_TDL_Wait:performConstraint]  Error:  "
      << "Unable to WAIT on Node that is not running." << endl;
    printObject ( TDL::getLogStream(), "" );
    TDL::getLogStream() << endl;
    return TCM_Error;
  }

	/* Disable Compiler warnings about unused variables */
  _TDL_MARKUSED ( theReferenceTreeNode );

	/* Lets be verbose */
  if ( TDL::getIsReporting ( TDL::VERBOSE_BRIEF ) )
  {
    TDL::getLogStream()
      << "_TDL_Wait ( Constrain=" << theNodeToConstrain . getName();
    theNodeToConstrain . printArrayIndexes ( TDL::getLogStream() );
    TDL::getLogStream()
      << " )" << endl;
  }
  if ( TDL::getIsReporting ( TDL::VERBOSE_FULL ) )
  {
    printObject ( TDL::getLogStream(), "" );
    TDL::getLogStream() << endl;
  }

#ifndef _TDL_DISABLE_CONSTRAINTS_FOR_TESTING
  return _TDL_DO_TCM_WAIT ( theNodeToConstrain );
#else
  _TDL_MARKUSED ( theNodeToConstrain );
  return TCM_Ok;
#endif
}


/*virtual*/ TCM_Return_Type
_TDL_Wait::applyConstraintsBeforeInsertion (
	_TDL_SpawnStatementTreeNode & theNodeToConstrain,
  const _TDL_TreeNode               * thePreviousSearchStart, /*=NULL*/
	BOOLEAN                       theIsSpawnConstraint    /*=TRUE*/ ) const
{
	/* Disable Compiler warnings about unused variables */
  _TDL_MARKUSED ( theNodeToConstrain     );
  _TDL_MARKUSED ( thePreviousSearchStart );
  _TDL_MARKUSED ( theIsSpawnConstraint   );
  return TCM_Error;
}

/*virtual*/ TCM_Return_Type
_TDL_Wait::applyConstraintsAfterInsertion (
	_TDL_SpawnStatementTreeNode & theNodeToConstrain,
  const _TDL_TreeNode               * thePreviousSearchStart, /*=NULL*/
	BOOLEAN                       theIsSpawnConstraint    /*=TRUE*/ ) const
{
  return applyConstraints ( theNodeToConstrain,
			    thePreviousSearchStart,
			    theIsSpawnConstraint   );
}


/*virtual*/ const char *
_TDL_Wait::getConstraintName() const
{
  return "_TDL_Wait";
}


/*virtual*/ ostream &
_TDL_Wait::printObject ( ostream    & theOstream,
			 const char * theIndentString /*=""*/ ) const
{
  theOstream << theIndentString << "Constraint:  _TDL_Wait  ("
	     << ((void *)this) << ")" << endl;
  return _TDL_Constraint::printObject ( theOstream, theIndentString );
}




/*** class _TDL_DisableUntilEvent ***/

/*virtual*/
_TDL_DisableUntilEvent::~_TDL_DisableUntilEvent()
{}

/*virtual*/ _TDL_Constraint *
_TDL_DisableUntilEvent::clone() const
{
  return new _TDL_DisableUntilEvent ( *this );
}


/*virtual*/ TCM_Return_Type
_TDL_DisableUntilEvent::performConstraint (
		     _TDL_SpawnStatementTreeNode & theNodeToConstrain,
		     _TDL_SpawnStatementTreeNode * theReferenceTreeNode ) const
{
  if ( theNodeToConstrain . isDestroyed() == TRUE )
  {
    TDL::getLogStream()
      << "[_TDL_DisableUntilEvent:performConstraint]  Error:  "
      << "Constraining Destroyed Node." << endl;
    return TCM_Error;
  }

  if ( theReferenceTreeNode == (_TDL_SpawnStatementTreeNode *) NULL )
  {
    TDL::getLogStream()
      << "[_TDL_DisableUntilEvent:performConstraint]  Error:  "
      << "NULL value for theReferenceTreeNode."
      << endl;
    return TCM_Error;
  }

	/* Constraining running node is unlikely to work... */
  if ( theNodeToConstrain . getState() == _TDL_SpawnStatementData::RUNNING )
  {
    TDL::getLogStream()
      << "[_TDL_DisableUntilEvent:performConstraint]  Warning:  Constraining "
      << "node that has already started running is unlikely to work." << endl;
  }

	/* Lets be verbose */
  if ( TDL::getIsReporting ( TDL::VERBOSE_BRIEF ) )
  {
    TDL::getLogStream()
      << "_TDL_DisableUntilEvent ( Constrain="
      << theNodeToConstrain . getName();
    theNodeToConstrain . printArrayIndexes ( TDL::getLogStream() );
    TDL::getLogStream() << " , ConstrainInterval=";
    _TDL_PRINT_INTERVAL ( TDL::getLogStream(), getNodeToConstrainInterval() );
    TDL::getLogStream()
      << " , Ref=" << theReferenceTreeNode -> getName();
    theReferenceTreeNode -> printArrayIndexes ( TDL::getLogStream() );
    TDL::getLogStream()
      << " [Ref_flags: " << theReferenceTreeNode -> getStateString() << "]"
      << " , RefInterval=";
    _TDL_PRINT_INTERVAL ( TDL::getLogStream(), getReferenceInterval()       );
    TDL::getLogStream() << " , RefState=";
    _TDL_PRINT_STATE    ( TDL::getLogStream(), getReferenceState()          );
    TDL::getLogStream() << " )" << endl;
  }
  if ( TDL::getIsReporting ( TDL::VERBOSE_FULL ) )
  {
    printObject ( TDL::getLogStream(), "" );
    TDL::getLogStream() << endl;
  }

	/* If the reference node is destroyed,  *
	 * there's nothing to constrain against */
  if ( theReferenceTreeNode -> isDestroyed() == TRUE )
  {
    if ( TDL::getIsReporting ( TDL::VERBOSE_BRIEF ) )
    {
      TDL::getLogStream()
	<< "[_TDL_DisableUntilEvent:performConstraint]  Warning:  "
	<< "theReferenceTreeNode is destroyed.  Skipping Constraint..."
	<< endl;
    }
    return TCM_Ok;
  }


#ifndef _TDL_DISABLE_CONSTRAINTS_FOR_TESTING
  return _TDL_DO_TCM_DISABLE_UNTIL_EVENT ( * theReferenceTreeNode,
					     getReferenceInterval(),
					     getReferenceState(),
					     theNodeToConstrain,
					     getNodeToConstrainInterval() );
#else
  _TDL_MARKUSED ( theNodeToConstrain   );
  _TDL_MARKUSED ( theReferenceTreeNode );
  return TCM_Ok;
#endif
}

/*virtual*/ const char *
_TDL_DisableUntilEvent::getConstraintName() const
{
  return "_TDL_DisableUntilEvent";
}

/*virtual*/ ostream &
_TDL_DisableUntilEvent::printObject( ostream    & theOstream,
				     const char * theIndentString /*=""*/)const
{
  theOstream << theIndentString << "Constraint:  _TDL_DisableUntilEvent  ("
	     << ((void *)this) << ")" << endl;
  _TDL_ConstraintOption::printObject ( theOstream, theIndentString );
  return _TDL_ConstraintWithEvent::printObject ( theOstream, theIndentString );
}



/*** class _TDL_DisableUntilTime ***/

/*virtual*/
_TDL_DisableUntilTime::~_TDL_DisableUntilTime()
{}

/*virtual*/ _TDL_Constraint *
_TDL_DisableUntilTime::clone() const
{
  return new _TDL_DisableUntilTime ( *this );
}


/*virtual*/ TCM_Return_Type
_TDL_DisableUntilTime::performConstraint (
		     _TDL_SpawnStatementTreeNode & theNodeToConstrain,
		     _TDL_SpawnStatementTreeNode * theReferenceTreeNode ) const
{
  if ( theNodeToConstrain . isDestroyed() == TRUE )
  {
    TDL::getLogStream()
      << "[_TDL_DisableUntilTime:performConstraint]  Error:  "
      << "Constraining Destroyed Node." << endl;
    return TCM_Error;
  }

	/* Constraining running node is unlikely to work... */
  if ( theNodeToConstrain . getState() == _TDL_SpawnStatementData::RUNNING )
  {
    TDL::getLogStream()
      << "[_TDL_DisableUntilTime:performConstraint]  Warning:  Constraining "
      << "node that has already started running is unlikely to work." << endl;
  }


	/* Disable Compiler warnings about unused variables */
  _TDL_MARKUSED ( theReferenceTreeNode );

	/* Lets be verbose */
  if ( TDL::getIsReporting ( TDL::VERBOSE_BRIEF ) )
  {
    TDL::getLogStream()
      << "_TDL_DisableUntilTime ( Constrain="
      << theNodeToConstrain . getName();
    theNodeToConstrain . printArrayIndexes ( TDL::getLogStream() );
    TDL::getLogStream()
      << " , ConstrainInterval=";
    _TDL_PRINT_INTERVAL ( TDL::getLogStream(), getNodeToConstrainInterval() );
    TDL::getLogStream() << " , ";
    _TDL_ConstraintTime::printObject ( TDL::getLogStream(), "" );
    TDL::getLogStream() << " )" << endl;
  }
  if ( TDL::getIsReporting ( TDL::VERBOSE_FULL ) )
  {
    printObject ( TDL::getLogStream(), "" );
    TDL::getLogStream() << endl;
  }

#ifndef _TDL_DISABLE_CONSTRAINTS_FOR_TESTING
  if ( _TDL_ConstraintTime::getHasTimeInMSecs() == TRUE )
  {
    return _TDL_DO_TCM_DISABLE_UNTIL ( theNodeToConstrain,
				       getNodeToConstrainInterval(),
				       getMSecs() );
  }
  else
  {
    return _TDL_DO_TCM_DISABLE_UNTIL ( theNodeToConstrain,
				       getNodeToConstrainInterval(),
				       getHours(),
				       getMinutes(),
				       getSeconds(),
				       getFractionsOfASecond() );
  }
#else
  _TDL_MARKUSED ( theNodeToConstrain );
  return TCM_Ok;
#endif
}

/*virtual*/ const char *
_TDL_DisableUntilTime::getConstraintName() const
{
  return "_TDL_DisableUntilTime";
}

/*virtual*/ ostream &
_TDL_DisableUntilTime::printObject( ostream    & theOstream,
				    const char * theIndentString /*=""*/) const
{
  theOstream << theIndentString << "Constraint:  _TDL_DisableUntilTime  ("
	     << ((void *)this) << ")" << endl;
  _TDL_ConstraintOption::printObject ( theOstream, theIndentString );
  _TDL_ConstraintTime::printObject   ( theOstream, theIndentString ) << endl;
  return _TDL_Constraint::printObject( theOstream, theIndentString );
}



/*** class _TDL_DisableForTime ***/

/*virtual*/
_TDL_DisableForTime::~_TDL_DisableForTime()
{}

/*virtual*/ _TDL_Constraint *
_TDL_DisableForTime::clone() const
{
  return new _TDL_DisableForTime ( *this );
}


/*virtual*/ TCM_Return_Type
_TDL_DisableForTime::performConstraint (
		     _TDL_SpawnStatementTreeNode & theNodeToConstrain,
		     _TDL_SpawnStatementTreeNode * theReferenceTreeNode ) const
{
  if ( theNodeToConstrain . isDestroyed() == TRUE )
  {
    TDL::getLogStream()
      << "[_TDL_DisableForTime:performConstraint]  Error:  "
      << "Constraining Destroyed Node." << endl;
    return TCM_Error;
  }

	/* Constraining running node is unlikely to work... */
  if ( theNodeToConstrain . getState() == _TDL_SpawnStatementData::RUNNING )
  {
    TDL::getLogStream()
      << "[_TDL_DisableForTime:performConstraint]  Warning:  Constraining "
      << "node that has already started running is unlikely to work." << endl;
  }

	/* Lets be verbose */
  if ( TDL::getIsReporting ( TDL::VERBOSE_BRIEF ) )
  {
    TDL::getLogStream()
      << "_TDL_DisableForTime ( Constrain=" << theNodeToConstrain . getName();
    theNodeToConstrain . printArrayIndexes ( TDL::getLogStream() );
    TDL::getLogStream()
      << " , ConstrainInterval=";
    _TDL_PRINT_INTERVAL ( TDL::getLogStream(), getNodeToConstrainInterval() );
    if ( theReferenceTreeNode != (_TDL_SpawnStatementTreeNode *) NULL )
    {
      TDL::getLogStream() << " , ";
      if ( theReferenceTreeNode -> isDestroyed() == TRUE )
	TDL::getLogStream() << " [****REF DESTROYED*****] ";
      TDL::getLogStream()
	<< " Ref=" << theReferenceTreeNode -> getName();
      theReferenceTreeNode -> printArrayIndexes ( TDL::getLogStream() );
      TDL::getLogStream()
	<< " [Ref_flags: " << theReferenceTreeNode -> getStateString() << "] "
	<< " , RefInterval=";
      _TDL_PRINT_INTERVAL ( TDL::getLogStream(), getReferenceInterval() );
      TDL::getLogStream() << " , RefState=";
      _TDL_PRINT_STATE    ( TDL::getLogStream(), getReferenceState()    );
    }
    TDL::getLogStream() << " , ";
    _TDL_ConstraintTime::printObject ( TDL::getLogStream(), "" );
    TDL::getLogStream() << " )" << endl;
  }
  if ( TDL::getIsReporting ( TDL::VERBOSE_FULL ) )
  {
    printObject ( TDL::getLogStream(), "" );
    TDL::getLogStream() << endl;
  }


  if (   (  TDL::getIsReporting ( TDL::VERBOSE_BRIEF )                  )
      && ( theReferenceTreeNode != (_TDL_SpawnStatementTreeNode *) NULL )
      && ( theReferenceTreeNode -> isDestroyed() == TRUE                ) )
  {
    TDL::getLogStream()
      << "[_TDL_DisableForTime:performConstraint]  Warning:  "
      << "\"AFTER <event>\"'s reference-node is destroyed.  "
      << "AFTER clause ignored." << endl;
  }



#ifndef _TDL_DISABLE_CONSTRAINTS_FOR_TESTING
  if (   ( theReferenceTreeNode != (_TDL_SpawnStatementTreeNode *) NULL )
      && ( theReferenceTreeNode -> isDestroyed() == FALSE               ) )
  {
    if ( _TDL_ConstraintTime::getHasTimeInMSecs() == TRUE )
    {
      return _TDL_DO_TCM_DISABLE_FOR ( * theReferenceTreeNode,
				         getReferenceInterval(),
				         getReferenceState(),
				         theNodeToConstrain,
				         getNodeToConstrainInterval(),
				         getMSecs() );
    }
    else
    {
      return _TDL_DO_TCM_DISABLE_FOR ( * theReferenceTreeNode,
				         getReferenceInterval(),
				         getReferenceState(),
				         theNodeToConstrain,
				         getNodeToConstrainInterval(),
				         getHours(),
				         getMinutes(),
				         getSeconds(),
				         getFractionsOfASecond() );
    }
  }
  else
  {
    if ( _TDL_ConstraintTime::getHasTimeInMSecs() == TRUE )
    {
      return _TDL_DO_TCM_DISABLE_FOR (   theNodeToConstrain,
					 getNodeToConstrainInterval(),
					 getMSecs() );
    }
    else
    {
      return _TDL_DO_TCM_DISABLE_FOR (   theNodeToConstrain,
					 getNodeToConstrainInterval(),
					 getHours(),
					 getMinutes(),
					 getSeconds(),
					 getFractionsOfASecond() );
    }
  }
#else
  _TDL_MARKUSED ( theNodeToConstrain   );
  _TDL_MARKUSED ( theReferenceTreeNode );
  return TCM_Ok;
#endif
}



/*virtual*/ BOOLEAN 
_TDL_DisableForTime::getCanUseNullReference() const
{
  return TRUE;
}


/*virtual*/ const char *
_TDL_DisableForTime::getConstraintName() const
{
  return "_TDL_DisableForTime";
}


/*virtual*/ ostream &
_TDL_DisableForTime::printObject ( ostream    & theOstream,
				   const char * theIndentString /*=""*/ ) const
{
  theOstream << theIndentString << "Constraint:  _TDL_DisableForTime  ("
	     << ((void *)this) << ")" << endl;
  _TDL_ConstraintOption::printObject ( theOstream, theIndentString );
  _TDL_ConstraintTime::printObject   ( theOstream, theIndentString ) << endl;
  return _TDL_ConstraintWithEvent::printObject ( theOstream, theIndentString );
}



/*** class _TDL_TerminateAtEvent ***/

/*virtual*/
_TDL_TerminateAtEvent::~_TDL_TerminateAtEvent()
{}

/*virtual*/ _TDL_Constraint *
_TDL_TerminateAtEvent::clone() const
{
  return new _TDL_TerminateAtEvent ( *this );
}


/*virtual*/ TCM_Return_Type
_TDL_TerminateAtEvent::performConstraint (
		     _TDL_SpawnStatementTreeNode & theNodeToConstrain,
		     _TDL_SpawnStatementTreeNode * theReferenceTreeNode ) const
{
  if ( theNodeToConstrain . isDestroyed() == TRUE )
  {
    TDL::getLogStream()
      << "[_TDL_TerminateAtEvent:performConstraint]  Error:  "
      << "Constraining Destroyed Node." << endl;
    return TCM_Error;
  }

  if ( theReferenceTreeNode == (_TDL_SpawnStatementTreeNode *) NULL )
  {
    TDL::getLogStream()
      << "[_TDL_TerminateAtEvent:performConstraint]  Warning:  "
      << "NULL value for the Event (theReferenceTreeNode).  "
      << "Terminating in zero time." << endl;
  }

	/* Lets be verbose */
  if ( TDL::getIsReporting ( TDL::VERBOSE_BRIEF ) )
  {
    TDL::getLogStream()
      << "_TDL_TerminateAtEvent ( Constrain="
      << theNodeToConstrain . getName();
    theNodeToConstrain . printArrayIndexes ( TDL::getLogStream() );

    if ( theReferenceTreeNode == (_TDL_SpawnStatementTreeNode *) NULL )
    {
      TDL::getLogStream()
	<< " , Ref=NULL";
    }
    else
    {
      TDL::getLogStream()
	<< " , Ref=" << theReferenceTreeNode -> getName();
      theReferenceTreeNode -> printArrayIndexes ( TDL::getLogStream() );
      TDL::getLogStream()
	<< " [Ref_flags: " << theReferenceTreeNode -> getStateString() << "]";
    }

    TDL::getLogStream()
      << " , RefInterval=";
    _TDL_PRINT_INTERVAL ( TDL::getLogStream(), getReferenceInterval()       );
    TDL::getLogStream() << " , RefState=";
    _TDL_PRINT_STATE    ( TDL::getLogStream(), getReferenceState()          );
    TDL::getLogStream() << " )" << endl;
  }
  if ( TDL::getIsReporting ( TDL::VERBOSE_FULL ) )
  {
    printObject ( TDL::getLogStream(), "" );
    TDL::getLogStream() << endl;
  }


	/* Treat as terminate in 0 time for NULL references */
  if ( theReferenceTreeNode == (_TDL_SpawnStatementTreeNode *) NULL )
  {
#ifndef _TDL_DISABLE_CONSTRAINTS_FOR_TESTING
    return _TDL_DO_TCM_TERMINATE_IN ( theNodeToConstrain, 0, 0, 0, 0.0 );
#else
    return TCM_Ok;
#endif
  }

	/* If the reference node is destroyed,  *
	 * there's nothing to terminate on.     */
  if ( theReferenceTreeNode -> isDestroyed() == TRUE )
  {
    if ( TDL::getIsReporting ( TDL::VERBOSE_BRIEF ) )
    {
      TDL::getLogStream()
	<< "[_TDL_TerminateAtEvent:performConstraint]  Error:  "
	<< "The Reference Task is \"DESTROYED\".  Most likely, Task \""
	<< theReferenceTreeNode -> getName()
	<< "\" exists inside a conditional statement (IF or SWITCH) whose "
	<< "predicate evaluated to FALSE.  This TerminateAtEvent constraint "
	<< "will therefore terminate the Current Task (\""
	<< theNodeToConstrain . getName() << "\")  IN ZERO TIME." << endl;
    }
#ifndef _TDL_DISABLE_CONSTRAINTS_FOR_TESTING
    return _TDL_DO_TCM_TERMINATE_IN ( theNodeToConstrain, 0, 0, 0, 0.0 );
#endif
  }

#ifndef _TDL_DISABLE_CONSTRAINTS_FOR_TESTING
  return _TDL_DO_TCM_TERMINATE_AT_EVENT ( * theReferenceTreeNode,
					    getReferenceInterval(),
					    getReferenceState(),
					    theNodeToConstrain     );
#else
  _TDL_MARKUSED ( theNodeToConstrain   );
  _TDL_MARKUSED ( theReferenceTreeNode );
  return TCM_Ok;
#endif
}

	/* Treat as terminate in 0 time for NULL references */
/*virtual*/ BOOLEAN
_TDL_TerminateAtEvent::getCanUseNullReference() const
{
  return TRUE;
}


/*virtual*/ const char *
_TDL_TerminateAtEvent::getConstraintName() const
{
  return "_TDL_TerminateAtEvent";
}

/*virtual*/ ostream &
_TDL_TerminateAtEvent::printObject( ostream    & theOstream,
				    const char * theIndentString /*=""*/) const
{
  theOstream << theIndentString << "Constraint:  _TDL_TerminateAtEvent  ("
	     << ((void *)this) << ")" << endl;
  return _TDL_ConstraintWithEvent::printObject ( theOstream, theIndentString );
}



/*** class _TDL_TerminateAtTime ***/

/*virtual*/
_TDL_TerminateAtTime::~_TDL_TerminateAtTime()
{}

/*virtual*/ _TDL_Constraint *
_TDL_TerminateAtTime::clone() const
{
  return new _TDL_TerminateAtTime ( *this );
}


/*virtual*/ TCM_Return_Type
_TDL_TerminateAtTime::performConstraint (
		     _TDL_SpawnStatementTreeNode & theNodeToConstrain,
		     _TDL_SpawnStatementTreeNode * theReferenceTreeNode ) const
{
  if ( theNodeToConstrain . isDestroyed() == TRUE )
  {
    TDL::getLogStream()
      << "[_TDL_TerminateAtTime:performConstraint]  Error:  "
      << "Constraining Destroyed Node." << endl;
    return TCM_Error;
  }

	/* Disable Compiler warnings about unused variables */
  _TDL_MARKUSED ( theReferenceTreeNode );

	/* Lets be verbose */
  if ( TDL::getIsReporting ( TDL::VERBOSE_BRIEF ) )
  {
    TDL::getLogStream()
      << "_TDL_TerminateAtTime ( Constrain=" << theNodeToConstrain . getName();
    theNodeToConstrain . printArrayIndexes ( TDL::getLogStream() );  
  TDL::getLogStream()
      << " , ";
    _TDL_ConstraintTime::printObject ( TDL::getLogStream(), "" );
    TDL::getLogStream() << " )" << endl;
  }
  if ( TDL::getIsReporting ( TDL::VERBOSE_FULL ) )
  {
    printObject ( TDL::getLogStream(), "" );
    TDL::getLogStream() << endl;
  }

#ifndef _TDL_DISABLE_CONSTRAINTS_FOR_TESTING
  if ( _TDL_ConstraintTime::getHasTimeInMSecs() == TRUE )
  {
    return _TDL_DO_TCM_TERMINATE_AT ( theNodeToConstrain,
				      getMSecs() );
  }
  else
  {
    return _TDL_DO_TCM_TERMINATE_AT ( theNodeToConstrain,
				      getHours(),
				      getMinutes(),
				      getSeconds(),
				      getFractionsOfASecond() );
  }
#else
  _TDL_MARKUSED ( theNodeToConstrain );
  return TCM_Ok;
#endif
}


/*virtual*/ const char *
_TDL_TerminateAtTime::getConstraintName() const
{
  return "_TDL_TerminateAtTime";
}


/*virtual*/ ostream &
_TDL_TerminateAtTime::printObject ( ostream    & theOstream,
				    const char * theIndentString /*=""*/) const
{
  theOstream << theIndentString << "Constraint:  _TDL_TerminateAtTime  ("
	     << ((void *)this) << ")" << endl;
  _TDL_ConstraintTime::printObject    ( theOstream, theIndentString ) << endl;
  return _TDL_Constraint::printObject ( theOstream, theIndentString );
}



/*** class _TDL_TerminateInTime ***/

/*virtual*/
_TDL_TerminateInTime::~_TDL_TerminateInTime()
{}

/*virtual*/ _TDL_Constraint *
_TDL_TerminateInTime::clone() const
{
  return new _TDL_TerminateInTime ( *this );
}


/*virtual*/ TCM_Return_Type
_TDL_TerminateInTime::performConstraint (
		     _TDL_SpawnStatementTreeNode & theNodeToConstrain,
		     _TDL_SpawnStatementTreeNode * theReferenceTreeNode ) const
{
  if ( theNodeToConstrain . isDestroyed() == TRUE )
  {
    TDL::getLogStream()
      << "[_TDL_TerminateInTime:performConstraint]  Error:  "
      << "Constraining Destroyed Node." << endl;
    return TCM_Error;
  }

	/* Lets be verbose */
  if ( TDL::getIsReporting ( TDL::VERBOSE_BRIEF ) )
  {
    TDL::getLogStream()
      << "_TDL_TerminateInTime ( Constrain=" << theNodeToConstrain . getName();
    theNodeToConstrain . printArrayIndexes ( TDL::getLogStream() );
    if ( theReferenceTreeNode != (_TDL_SpawnStatementTreeNode *) NULL )
    {
      TDL::getLogStream() << " , ";
      if ( theReferenceTreeNode -> isDestroyed() == TRUE )
	TDL::getLogStream() << " [****REF DESTROYED*****] ";
      TDL::getLogStream()
	<< " Ref=" << theReferenceTreeNode -> getName();
      theReferenceTreeNode -> printArrayIndexes ( TDL::getLogStream() );
      TDL::getLogStream()
	<< " [Ref_flags: " << theReferenceTreeNode -> getStateString() << "] "
	<< " , RefInterval=";
      _TDL_PRINT_INTERVAL ( TDL::getLogStream(), getReferenceInterval() );
      TDL::getLogStream() << " , RefState=";
      _TDL_PRINT_STATE    ( TDL::getLogStream(), getReferenceState()    );
    }
    TDL::getLogStream() << " , ";
    _TDL_ConstraintTime::printObject ( TDL::getLogStream(), "" );
    TDL::getLogStream() << " )" << endl;
  }
  if ( TDL::getIsReporting ( TDL::VERBOSE_FULL ) )
  {
    printObject ( TDL::getLogStream(), "" );
    TDL::getLogStream() << endl;
  }

  if (   (  TDL::getIsReporting ( TDL::VERBOSE_BRIEF )                  )
      && ( theReferenceTreeNode != (_TDL_SpawnStatementTreeNode *) NULL )
      && ( theReferenceTreeNode -> isDestroyed() == TRUE                ) )
  {
    TDL::getLogStream()
      << "[_TDL_TerminateInTime:performConstraint]  Warning:  "
      << "\"AFTER <event>\"'s reference-node is destroyed.  "
      << "AFTER clause ignored." << endl;
  }


#ifndef _TDL_DISABLE_CONSTRAINTS_FOR_TESTING
  if (   ( theReferenceTreeNode != (_TDL_SpawnStatementTreeNode *) NULL )
      && ( theReferenceTreeNode -> isDestroyed() == FALSE               ) )
  {
    if ( _TDL_ConstraintTime::getHasTimeInMSecs() == TRUE )
    {
      return _TDL_DO_TCM_TERMINATE_IN ( * theReferenceTreeNode,
				          getReferenceInterval(),
				          getReferenceState(),
				          theNodeToConstrain,
					  getMSecs() );
    }
    else
    {
      return _TDL_DO_TCM_TERMINATE_IN ( * theReferenceTreeNode,
					  getReferenceInterval(),
					  getReferenceState(),
					  theNodeToConstrain,
					  getHours(),
					  getMinutes(),
					  getSeconds(),
					  getFractionsOfASecond() );
    }
  }
  else
  {
    if ( _TDL_ConstraintTime::getHasTimeInMSecs() == TRUE )
    {
      return _TDL_DO_TCM_TERMINATE_IN (   theNodeToConstrain,
					  getMSecs() );
    }
    else
    {
      return _TDL_DO_TCM_TERMINATE_IN (   theNodeToConstrain,
					  getHours(),
					  getMinutes(),
					  getSeconds(),
					  getFractionsOfASecond() );
    }
  }
#else
  _TDL_MARKUSED ( theNodeToConstrain   );
  _TDL_MARKUSED ( theReferenceTreeNode );
  return TCM_Ok;
#endif
}



/*virtual*/ BOOLEAN 
_TDL_TerminateInTime::getCanUseNullReference() const
{
  return TRUE;
}


/*virtual*/ const char *
_TDL_TerminateInTime::getConstraintName() const
{
  return "_TDL_TerminateInTime";
}


/*virtual*/ ostream &
_TDL_TerminateInTime::printObject ( ostream    & theOstream,
				    const char * theIndentString /*=""*/) const
{
  theOstream << theIndentString << "Constraint:  _TDL_TerminateInTime  ("
	     << ((void *)this)  << ")" << endl;
  _TDL_ConstraintTime::printObject ( theOstream, theIndentString ) << endl;
  return _TDL_ConstraintWithEvent::printObject ( theOstream, theIndentString );
}




/*** class _TDL_TerminateImmediate ***/

/*virtual*/
_TDL_TerminateImmediate::~_TDL_TerminateImmediate()
{}

/*virtual*/ _TDL_Constraint *
_TDL_TerminateImmediate::clone() const
{
  return new _TDL_TerminateImmediate ( *this );
}


/*virtual*/ TCM_Return_Type
_TDL_TerminateImmediate::performConstraint (
		     _TDL_SpawnStatementTreeNode & theNodeToConstrain,
		     _TDL_SpawnStatementTreeNode * theReferenceTreeNode ) const
{
  if ( theNodeToConstrain . isDestroyed() == TRUE )
  {
    TDL::getLogStream()
      << "[_TDL_TerminateImmediate:performConstraint]  Error:  "
      << "Constraining Destroyed Node." << endl;
    return TCM_Error;
  }

	/* Disable Compiler warnings about unused variables */
  _TDL_MARKUSED ( theReferenceTreeNode );

	/* Lets be verbose */
  if ( TDL::getIsReporting ( TDL::VERBOSE_BRIEF ) )
  {
    TDL::getLogStream()
      << "_TDL_TerminateImmediate ( Constrain="
      << theNodeToConstrain . getName();
    theNodeToConstrain . printArrayIndexes ( TDL::getLogStream() );
    TDL::getLogStream() << " )" << endl;
  }
  if ( TDL::getIsReporting ( TDL::VERBOSE_FULL ) )
  {
    printObject ( TDL::getLogStream(), "" );
    TDL::getLogStream() << endl;
  }

#ifndef _TDL_DISABLE_CONSTRAINTS_FOR_TESTING
  return _TDL_DO_TCM_TERMINATE_IMMEDIATE ( theNodeToConstrain );
#else
  _TDL_MARKUSED ( theNodeToConstrain );
  return TCM_Ok;
#endif
}


/*virtual*/ TCM_Return_Type
_TDL_TerminateImmediate::applyConstraintsBeforeInsertion (
	_TDL_SpawnStatementTreeNode & theNodeToConstrain,
  const _TDL_TreeNode               * thePreviousSearchStart, /*=NULL*/
	BOOLEAN                       theIsSpawnConstraint    /*=TRUE*/ ) const
{
	/* Disable Compiler warnings about unused variables */
  _TDL_MARKUSED ( theNodeToConstrain     );
  _TDL_MARKUSED ( thePreviousSearchStart );
  _TDL_MARKUSED ( theIsSpawnConstraint   );
  return TCM_Error;
}

/*virtual*/ TCM_Return_Type
_TDL_TerminateImmediate::applyConstraintsAfterInsertion (
	_TDL_SpawnStatementTreeNode & theNodeToConstrain,
  const _TDL_TreeNode               * thePreviousSearchStart, /*=NULL*/
	BOOLEAN                       theIsSpawnConstraint    /*=TRUE*/ ) const
{
  return applyConstraints ( theNodeToConstrain,
			    thePreviousSearchStart,
			    theIsSpawnConstraint   );
}


/*virtual*/ const char *
_TDL_TerminateImmediate::getConstraintName() const
{
  return "_TDL_TerminateImmediate";
}


/*virtual*/ ostream &
_TDL_TerminateImmediate::printObject (
				    ostream    & theOstream,
				    const char * theIndentString /*=""*/) const
{
  theOstream << theIndentString << "Constraint:  _TDL_TerminateImmediate  ("
	     << ((void *)this) << ")" << endl;
  return _TDL_Constraint::printObject ( theOstream, theIndentString );
}




/*** class _TDL_ActivateAtEvent ***/

/*virtual*/
_TDL_ActivateAtEvent::~_TDL_ActivateAtEvent()
{}

/*virtual*/ _TDL_Constraint *
_TDL_ActivateAtEvent::clone() const
{
  return new _TDL_ActivateAtEvent ( *this );
}


   /* Caveat:  Activate only works on Monitors! */
/*virtual*/ TCM_Return_Type
_TDL_ActivateAtEvent::performConstraint (
		     _TDL_SpawnStatementTreeNode & theNodeToConstrain,
		     _TDL_SpawnStatementTreeNode * theReferenceTreeNode ) const
{
  if ( theNodeToConstrain . isDestroyed() == TRUE )
  {
    TDL::getLogStream()
      << "[_TDL_ActivateAtEvent:performConstraint]  Error:  "
      << "Constraining Destroyed Node." << endl;
    return TCM_Error;
  }

  if ( theReferenceTreeNode == (_TDL_SpawnStatementTreeNode *) NULL )
  {
    TDL::getLogStream()
      << "[_TDL_ActivateAtEvent:performConstraint]  Warning:  "
      << "NULL value for the Event (theReferenceTreeNode).  "
      << "Activating in zero time after handling starts."
      << endl;
  }

	/* Lets be verbose */
  if ( TDL::getIsReporting ( TDL::VERBOSE_BRIEF ) )
  {
    TDL::getLogStream()
      << "_TDL_ActivateAtEvent ( Constrain=" << theNodeToConstrain . getName();
    theNodeToConstrain . printArrayIndexes ( TDL::getLogStream() );

    if ( theReferenceTreeNode == (_TDL_SpawnStatementTreeNode *) NULL )
    {
      TDL::getLogStream()
	<< " , Ref=NULL";
    }
    else
    {
      TDL::getLogStream()
	<< " , Ref=" << theReferenceTreeNode -> getName();
      theReferenceTreeNode -> printArrayIndexes ( TDL::getLogStream() );
      TDL::getLogStream()
	<< " [Ref_flags: " << theReferenceTreeNode -> getStateString() << "]";
    }

    TDL::getLogStream()
      << " , RefInterval=";
    _TDL_PRINT_INTERVAL ( TDL::getLogStream(), getReferenceInterval()       );
    TDL::getLogStream() << " , RefState=";
    _TDL_PRINT_STATE    ( TDL::getLogStream(), getReferenceState()          );
    TDL::getLogStream() << " )" << endl;
  }
  if ( TDL::getIsReporting ( TDL::VERBOSE_FULL ) )
  {
    printObject ( TDL::getLogStream(), "" );
    TDL::getLogStream() << endl;
  }


	/* Treat as activate in zero time after handling started *
	 * for NULL references.                                  */
  if ( theReferenceTreeNode == (_TDL_SpawnStatementTreeNode *) NULL )
  {
#ifndef _TDL_DISABLE_CONSTRAINTS_FOR_TESTING
    return _TDL_DO_TCM_ACTIVATE_AT_EVENT ( theNodeToConstrain,
					   Handling_Interval,
					   Start_Point,
					   theNodeToConstrain );
#else
    return TCM_Ok;
#endif
  }

	/* If the reference node is destroyed,  *
	 * there's nothing to activate on.     */
  if ( theReferenceTreeNode -> isDestroyed() == TRUE )
  {
    if ( TDL::getIsReporting ( TDL::VERBOSE_BRIEF ) )
    {
      TDL::getLogStream()
	<< "[_TDL_ActivateAtEvent:performConstraint]  Error:  "
	<< "The Reference Task is \"DESTROYED\".  Most likely, Task \""
	<< theReferenceTreeNode -> getName()
	<< "\" exists inside a conditional statement (IF or SWITCH) whose "
	<< "predicate evaluated to FALSE.  This ActivateAtEvent constraint "
	<< "will therefore activate the Current Task (\""
	<< theNodeToConstrain . getName()
	<< "\")  AS SOON AS ITS HANDLING STARTS." << endl;
    }
#ifndef _TDL_DISABLE_CONSTRAINTS_FOR_TESTING
    return _TDL_DO_TCM_ACTIVATE_AT_EVENT ( theNodeToConstrain,
					   Handling_Interval,
					   Start_Point,
					   theNodeToConstrain );
#endif
  }

#ifndef _TDL_DISABLE_CONSTRAINTS_FOR_TESTING
  return _TDL_DO_TCM_ACTIVATE_AT_EVENT ( * theReferenceTreeNode,
					   getReferenceInterval(),
					   getReferenceState(),
					   theNodeToConstrain     );
#else
  _TDL_MARKUSED ( theNodeToConstrain   );
  _TDL_MARKUSED ( theReferenceTreeNode );
  return TCM_Ok;
#endif
}

	/* Treat as activate in zero time after handling started *
	 * for NULL references.                                  */
/*virtual*/ BOOLEAN
_TDL_ActivateAtEvent::getCanUseNullReference() const
{
  return TRUE;
}


/*virtual*/ const char *
_TDL_ActivateAtEvent::getConstraintName() const
{
  return "_TDL_ActivateAtEvent";
}


/*virtual*/ ostream &
_TDL_ActivateAtEvent::printObject ( ostream    & theOstream,
				    const char * theIndentString /*=""*/) const
{
  theOstream << theIndentString << "Constraint:  _TDL_ActivateAtEvent  ("
	     << ((void *)this)  << ")" << endl;
  return _TDL_ConstraintWithEvent::printObject ( theOstream, theIndentString );
}



/*** class _TDL_ActivateAtTime ***/

/*virtual*/
_TDL_ActivateAtTime::~_TDL_ActivateAtTime()
{}

/*virtual*/ _TDL_Constraint *
_TDL_ActivateAtTime::clone() const
{
  return new _TDL_ActivateAtTime ( *this );
}


   /* Caveat:  Activate only works on Monitors! */
/*virtual*/ TCM_Return_Type
_TDL_ActivateAtTime::performConstraint (
		     _TDL_SpawnStatementTreeNode & theNodeToConstrain,
		     _TDL_SpawnStatementTreeNode * theReferenceTreeNode ) const
{
  if ( theNodeToConstrain . isDestroyed() == TRUE )
  {
    TDL::getLogStream()
      << "[_TDL_ActivateAtTime:performConstraint]  Error:  "
      << "Constraining Destroyed Node." << endl;
    return TCM_Error;
  }

	/* Disable Compiler warnings about unused variables */
  _TDL_MARKUSED ( theReferenceTreeNode );

	/* Lets be verbose */
  if ( TDL::getIsReporting ( TDL::VERBOSE_BRIEF ) )
  {
    TDL::getLogStream()
      << "_TDL_ActivateAtTime ( Constrain=" << theNodeToConstrain . getName();
    theNodeToConstrain . printArrayIndexes ( TDL::getLogStream() );
    TDL::getLogStream() << " , ";
    _TDL_ConstraintTime::printObject ( TDL::getLogStream(), "" );
    TDL::getLogStream() << " )" << endl;
  }
  if ( TDL::getIsReporting ( TDL::VERBOSE_FULL ) )
  {
    printObject ( TDL::getLogStream(), "" );
    TDL::getLogStream() << endl;
  }

#ifndef _TDL_DISABLE_CONSTRAINTS_FOR_TESTING
  if ( _TDL_ConstraintTime::getHasTimeInMSecs() == TRUE )
  {
    return _TDL_DO_TCM_ACTIVATE_AT ( theNodeToConstrain,
				     getMSecs() );
  }
  else
  {
    return _TDL_DO_TCM_ACTIVATE_AT ( theNodeToConstrain,
				     getHours(),
				     getMinutes(),
				     getSeconds(),
				     getFractionsOfASecond() );
  }
#else
  _TDL_MARKUSED ( theNodeToConstrain );
  return TCM_Ok;
#endif
}


/*virtual*/ const char *
_TDL_ActivateAtTime::getConstraintName() const
{
  return "_TDL_ActivateAtTime";
}


/*virtual*/ ostream &
_TDL_ActivateAtTime::printObject ( ostream    & theOstream,
				   const char * theIndentString /*=""*/ ) const
{
  theOstream << theIndentString << "Constraint:  _TDL_ActivateAtTime  ("
	     << ((void *)this)  << ")" << endl;
  _TDL_ConstraintTime::printObject    ( theOstream, theIndentString ) << endl;
  return _TDL_Constraint::printObject ( theOstream, theIndentString );
}



/*** class _TDL_ActivateInTime ***/

/*virtual*/
_TDL_ActivateInTime::~_TDL_ActivateInTime()
{}

/*virtual*/ _TDL_Constraint *
_TDL_ActivateInTime::clone() const
{
  return new _TDL_ActivateInTime ( *this );
}


   /* Caveat:  Activate only works on Monitors! */
/*virtual*/ TCM_Return_Type
_TDL_ActivateInTime::performConstraint (
		     _TDL_SpawnStatementTreeNode & theNodeToConstrain,
		     _TDL_SpawnStatementTreeNode * theReferenceTreeNode ) const
{
  if ( theNodeToConstrain . isDestroyed() == TRUE )
  {
    TDL::getLogStream()
      << "[_TDL_ActivateInTime:performConstraint]  Error:  "
      << "Constraining Destroyed Node." << endl;
    return TCM_Error;
  }

	/* Lets be verbose */
  if ( TDL::getIsReporting ( TDL::VERBOSE_BRIEF ) )
  {
    TDL::getLogStream()
      << "_TDL_ActivateInTime ( Constrain=" << theNodeToConstrain . getName();
    theNodeToConstrain . printArrayIndexes ( TDL::getLogStream() );
    if ( theReferenceTreeNode != (_TDL_SpawnStatementTreeNode *) NULL )
    {
      TDL::getLogStream() << " , ";
      if ( theReferenceTreeNode -> isDestroyed() == TRUE )
	TDL::getLogStream() << " [****REF DESTROYED*****] ";
      TDL::getLogStream()
	<< " Ref=" << theReferenceTreeNode -> getName();
      theReferenceTreeNode -> printArrayIndexes ( TDL::getLogStream() );
      TDL::getLogStream()
	<< " [Ref_flags: " << theReferenceTreeNode -> getStateString() << "] "
	<< " , RefInterval=";
      _TDL_PRINT_INTERVAL ( TDL::getLogStream(), getReferenceInterval() );
      TDL::getLogStream() << " , RefState=";
      _TDL_PRINT_STATE    ( TDL::getLogStream(), getReferenceState()    );
    }
    TDL::getLogStream() << " , ";
    _TDL_ConstraintTime::printObject ( TDL::getLogStream(), "" );
    TDL::getLogStream() << " )" << endl;
  }
  if ( TDL::getIsReporting ( TDL::VERBOSE_FULL ) )
  {
    printObject ( TDL::getLogStream(), "" );
    TDL::getLogStream() << endl;
  }

  if (   (  TDL::getIsReporting ( TDL::VERBOSE_BRIEF )                  )
      && ( theReferenceTreeNode != (_TDL_SpawnStatementTreeNode *) NULL )
      && ( theReferenceTreeNode->isDestroyed() == TRUE                  ) )
  {
    TDL::getLogStream()
      << "[_TDL_ActivateInTime:performConstraint]  Warning:  "
      << "\"AFTER <event>\"'s reference-node is destroyed.  "
      << "AFTER clause ignored." << endl;
  }


#ifndef _TDL_DISABLE_CONSTRAINTS_FOR_TESTING
  if (   ( theReferenceTreeNode != (_TDL_SpawnStatementTreeNode *) NULL )
      && ( theReferenceTreeNode -> isDestroyed() == FALSE               ) )
  {
    if ( _TDL_ConstraintTime::getHasTimeInMSecs() == TRUE )
    {
      return _TDL_DO_TCM_ACTIVATE_IN ( * theReferenceTreeNode,
					 getReferenceInterval(),
					 getReferenceState(),
					 theNodeToConstrain,
				         getMSecs() );
    }
    else
    {
      return _TDL_DO_TCM_ACTIVATE_IN ( * theReferenceTreeNode,
					 getReferenceInterval(),
					 getReferenceState(),
					 theNodeToConstrain,
					 getHours(),
					 getMinutes(),
					 getSeconds(),
					 getFractionsOfASecond() );
    }
  }
  else
  {
    if ( _TDL_ConstraintTime::getHasTimeInMSecs() == TRUE )
    {
      return _TDL_DO_TCM_ACTIVATE_IN (   theNodeToConstrain,
					 getMSecs() );
    }
    else
    {
      return _TDL_DO_TCM_ACTIVATE_IN (   theNodeToConstrain,
					 getHours(),
					 getMinutes(),
					 getSeconds(),
					 getFractionsOfASecond() );
    }
  }
#else
  _TDL_MARKUSED ( theNodeToConstrain   );
  _TDL_MARKUSED ( theReferenceTreeNode );
  return TCM_Ok;
#endif
}



/*virtual*/ BOOLEAN 
_TDL_ActivateInTime::getCanUseNullReference() const
{
  return TRUE;
}


/*virtual*/ const char *
_TDL_ActivateInTime::getConstraintName() const
{
  return "_TDL_ActivateInTime";
}


/*virtual*/ ostream &
_TDL_ActivateInTime::printObject ( ostream    & theOstream,
				   const char * theIndentString /*=""*/ ) const
{
  theOstream << theIndentString << "Constraint:  _TDL_ActivateInTime  ("
	     << ((void *)this)  << ")" << endl;
  _TDL_ConstraintTime::printObject ( theOstream, theIndentString ) << endl;
  return _TDL_ConstraintWithEvent::printObject ( theOstream, theIndentString );
}




/*** class _TDL_ActivateImmediate ***/

/*virtual*/
_TDL_ActivateImmediate::~_TDL_ActivateImmediate()
{}

/*virtual*/ _TDL_Constraint *
_TDL_ActivateImmediate::clone() const
{
  return new _TDL_ActivateImmediate ( *this );
}


   /* Caveat:  Activate only works on Monitors! */
/*virtual*/ TCM_Return_Type
_TDL_ActivateImmediate::performConstraint (
		     _TDL_SpawnStatementTreeNode & theNodeToConstrain,
		     _TDL_SpawnStatementTreeNode * theReferenceTreeNode ) const
{
  if ( theNodeToConstrain . isDestroyed() == TRUE )
  {
    TDL::getLogStream()
      << "[_TDL_ActivateImmediate:performConstraint]  Error:  "
      << "Constraining Destroyed Node." << endl;
    return TCM_Error;
  }

	/* Disable Compiler warnings about unused variables */
  _TDL_MARKUSED ( theReferenceTreeNode );

	/* Lets be verbose */
  if ( TDL::getIsReporting ( TDL::VERBOSE_BRIEF ) )
  {
    TDL::getLogStream()
      << "_TDL_ActivateImmediate ( Constrain="
      << theNodeToConstrain . getName();
    theNodeToConstrain . printArrayIndexes ( TDL::getLogStream() );
    TDL::getLogStream() << " )" << endl;
  }
  if ( TDL::getIsReporting ( TDL::VERBOSE_FULL ) )
  {
    printObject ( TDL::getLogStream(), "" );
    TDL::getLogStream() << endl;
  }

#ifndef _TDL_DISABLE_CONSTRAINTS_FOR_TESTING
  return _TDL_DO_TCM_ACTIVATE_IMMEDIATE ( theNodeToConstrain );
#else
  _TDL_MARKUSED ( theNodeToConstrain );
  return TCM_Ok;
#endif
}


/*virtual*/ TCM_Return_Type
_TDL_ActivateImmediate::applyConstraintsBeforeInsertion (
	_TDL_SpawnStatementTreeNode & theNodeToConstrain,
  const _TDL_TreeNode               * thePreviousSearchStart, /*=NULL*/
	BOOLEAN                       theIsSpawnConstraint    /*=TRUE*/ ) const
{
	/* Disable Compiler warnings about unused variables */
  _TDL_MARKUSED ( theNodeToConstrain     );
  _TDL_MARKUSED ( thePreviousSearchStart );
  _TDL_MARKUSED ( theIsSpawnConstraint   );
  return TCM_Error;
}

/*virtual*/ TCM_Return_Type
_TDL_ActivateImmediate::applyConstraintsAfterInsertion (
	_TDL_SpawnStatementTreeNode & theNodeToConstrain,
  const _TDL_TreeNode               * thePreviousSearchStart, /*=NULL*/
	BOOLEAN                       theIsSpawnConstraint    /*=TRUE*/ ) const
{
  return applyConstraints ( theNodeToConstrain,
			    thePreviousSearchStart,
			    theIsSpawnConstraint   );
}


/*virtual*/ const char *
_TDL_ActivateImmediate::getConstraintName() const
{
  return "_TDL_ActivateImmediate";
}


/*virtual*/ ostream &
_TDL_ActivateImmediate::printObject (
				   ostream    & theOstream,
				   const char * theIndentString /*=""*/ ) const
{
  theOstream << theIndentString << "Constraint:  _TDL_ActivateImmediate  ("
	     << ((void *)this)  << ")" << endl;
  return _TDL_Constraint::printObject ( theOstream, theIndentString );
}




/*** class _TDL_SetMonitorMaximumActivations ***/

/*virtual*/
_TDL_SetMonitorMaximumActivations::~_TDL_SetMonitorMaximumActivations()
{}

/*virtual*/ _TDL_Constraint *
_TDL_SetMonitorMaximumActivations::clone() const
{
  return new _TDL_SetMonitorMaximumActivations ( * this );
}


/*virtual*/ TCM_Return_Type
_TDL_SetMonitorMaximumActivations::performConstraint (
		     _TDL_SpawnStatementTreeNode & theNodeToConstrain,
		     _TDL_SpawnStatementTreeNode * theReferenceTreeNode ) const
{
  BOOLEAN  constraintOverriden;

  if ( theNodeToConstrain . isDestroyed() == TRUE )
  {
    TDL::getLogStream()
      << "[_TDL_SetMonitorMaximumActivations:performConstraint]  Error:  "
      << "Constraining Destroyed Node." << endl;
    return TCM_Error;
  }

	/* Disable Compiler warnings about unused variables */
  _TDL_MARKUSED ( theReferenceTreeNode );


	/* Deal with being overriden. */
  constraintOverriden
    = shouldOverrideConstraint ( theNodeToConstrain,
				 _TDL_Constraint::MAXIMUM_ACTIVATIONS_TYPE );

	/* Lets be verbose */
  if (   (     TDL::getIsReporting ( TDL::VERBOSE_BRIEF ) )
      || (     TDL::getIsReporting ( TDL::VERBOSE       )
	   &&  ( constraintOverriden == TRUE            ) ) )
  {
    if ( constraintOverriden == TRUE )
      TDL::getLogStream() << "Constraint overriden (and canceled):    ";      

    TDL::getLogStream()
      << "_TDL_SetMonitorMaximumActivations ( Constrain="
      << theNodeToConstrain . getName();
    theNodeToConstrain . printArrayIndexes ( TDL::getLogStream() );
    TDL::getLogStream()
      << ", maximum_activates = " << getMaximumActivations()
      << " )" << endl;
  }

	/* Abort if we are being overriden. */
  if ( constraintOverriden == TRUE )
    return TCM_Ok;

  if ( TDL::getIsReporting ( TDL::VERBOSE_FULL ) )
  {
    printObject ( TDL::getLogStream(), "" );
    TDL::getLogStream() << endl;
  }


#ifndef _TDL_DISABLE_CONSTRAINTS_FOR_TESTING
  return _TDL_DO_SET_MONITOR_MAXIMUM_ACTIVATIONS ( theNodeToConstrain,
						   getMaximumActivations() );
#else
  _TDL_MARKUSED ( theNodeToConstrain );
  return TCM_Ok;
#endif
}

/*virtual*/ const char *
_TDL_SetMonitorMaximumActivations::getConstraintName() const
{
  return "_TDL_SetMonitorMaximumActivations";
}

/*virtual*/ ostream &
_TDL_SetMonitorMaximumActivations::printObject(
				    ostream    & theOstream,
				    const char * theIndentString /*=""*/) const
{
  theOstream << theIndentString
	     << "Constraint:  _TDL_SetMonitorMaximumActivations  ("
	     << ((void *)this) << ")" << endl

	     << theIndentString
	     << " Maximum Activates = " << getMaximumActivations() << endl;
  return _TDL_Constraint::printObject( theOstream, theIndentString );
}




/*** class _TDL_SetMonitorMaximumTriggers ***/

/*virtual*/
_TDL_SetMonitorMaximumTriggers::~_TDL_SetMonitorMaximumTriggers()
{}

/*virtual*/ _TDL_Constraint *
_TDL_SetMonitorMaximumTriggers::clone() const
{
  return new _TDL_SetMonitorMaximumTriggers ( * this );
}


/*virtual*/ TCM_Return_Type
_TDL_SetMonitorMaximumTriggers::performConstraint (
		     _TDL_SpawnStatementTreeNode & theNodeToConstrain,
		     _TDL_SpawnStatementTreeNode * theReferenceTreeNode ) const
{
  BOOLEAN  constraintOverriden;

  if ( theNodeToConstrain . isDestroyed() == TRUE )
  {
    TDL::getLogStream()
      << "[_TDL_SetMonitorMaximumTriggers:performConstraint]  Error:  "
      << "Constraining Destroyed Node." << endl;
    return TCM_Error;
  }

	/* Disable Compiler warnings about unused variables */
  _TDL_MARKUSED ( theReferenceTreeNode );


	/* Deal with being overriden. */
  constraintOverriden
    = shouldOverrideConstraint ( theNodeToConstrain,
				 _TDL_Constraint::MAXIMUM_TRIGGERS_TYPE );

	/* Lets be verbose */
  if (   (     TDL::getIsReporting ( TDL::VERBOSE_BRIEF ) )
      || (     TDL::getIsReporting ( TDL::VERBOSE       )
	   &&  ( constraintOverriden == TRUE            ) ) )
  {
    if ( constraintOverriden == TRUE )
      TDL::getLogStream() << "Constraint overriden (and canceled):    ";      

    TDL::getLogStream()
      << "_TDL_SetMonitorMaximumTriggers ( Constrain="
      << theNodeToConstrain . getName();
    theNodeToConstrain . printArrayIndexes ( TDL::getLogStream() );
    TDL::getLogStream() << ", maximum_triggers = " << getMaximumTriggers()
			<< " )" << endl;
  }

	/* Abort if we are being overriden. */
  if ( constraintOverriden == TRUE )
    return TCM_Ok;

  if ( TDL::getIsReporting ( TDL::VERBOSE_FULL ) )
  {
    printObject ( TDL::getLogStream(), "" );
    TDL::getLogStream() << endl;
  }

#ifndef _TDL_DISABLE_CONSTRAINTS_FOR_TESTING
  return _TDL_DO_SET_MONITOR_MAXIMUM_TRIGGERS ( theNodeToConstrain,
						getMaximumTriggers() );
#else
  _TDL_MARKUSED ( theNodeToConstrain );
  return TCM_Ok;
#endif
}

/*virtual*/ const char *
_TDL_SetMonitorMaximumTriggers::getConstraintName() const
{
  return "_TDL_SetMonitorMaximumTriggers";
}

/*virtual*/ ostream &
_TDL_SetMonitorMaximumTriggers::printObject(
				    ostream    & theOstream,
				    const char * theIndentString /*=""*/) const
{
  theOstream << theIndentString
	     << "Constraint:  _TDL_SetMonitorMaximumTriggers  ("
	     << ((void *)this) << ")" << endl

	     << theIndentString
	     << " Maximum Triggers = " << getMaximumTriggers() << endl;
  return _TDL_Constraint::printObject( theOstream, theIndentString );
}




/*** class _TDL_SetMonitorPeriod ***/

/*virtual*/
_TDL_SetMonitorPeriod::~_TDL_SetMonitorPeriod()
{}

/*virtual*/ _TDL_Constraint *
_TDL_SetMonitorPeriod::clone() const
{
  return new _TDL_SetMonitorPeriod ( * this );
}


/*virtual*/ TCM_Return_Type
_TDL_SetMonitorPeriod::performConstraint (
		     _TDL_SpawnStatementTreeNode & theNodeToConstrain,
		     _TDL_SpawnStatementTreeNode * theReferenceTreeNode ) const
{
  BOOLEAN  constraintOverriden;

  if ( theNodeToConstrain . isDestroyed() == TRUE )
  {
    TDL::getLogStream()
      << "[_TDL_SetMonitorPeriod:performConstraint]  Error:  "
      << "Constraining Destroyed Node." << endl;
    return TCM_Error;
  }

	/* Disable Compiler warnings about unused variables */
  _TDL_MARKUSED ( theReferenceTreeNode );


	/* Deal with being overriden. */
  constraintOverriden
    = shouldOverrideConstraint ( theNodeToConstrain,
				 _TDL_Constraint::PERIOD_TYPE );

	/* Lets be verbose */
  if (   (     TDL::getIsReporting ( TDL::VERBOSE_BRIEF ) )
      || (     TDL::getIsReporting ( TDL::VERBOSE       )
	   &&  ( constraintOverriden == TRUE            ) ) )
  {
    if ( constraintOverriden == TRUE )
      TDL::getLogStream() << "Constraint overriden (and canceled):    ";      

    TDL::getLogStream()
      << "_TDL_SetMonitorPeriod ( Constrain="
      << theNodeToConstrain . getName();
    theNodeToConstrain . printArrayIndexes ( TDL::getLogStream() );
    TDL::getLogStream() << ", ";
    _TDL_ConstraintTime::printObject ( TDL::getLogStream(), "" );
    TDL::getLogStream() << " )" << endl;
  }

	/* Abort if we are being overriden. */
  if ( constraintOverriden == TRUE )
    return TCM_Ok;

  if ( TDL::getIsReporting ( TDL::VERBOSE_FULL ) )
  {
    printObject ( TDL::getLogStream(), "" );
    TDL::getLogStream() << endl;
  }

#ifndef _TDL_DISABLE_CONSTRAINTS_FOR_TESTING
  if ( _TDL_ConstraintTime::getHasTimeInMSecs() == TRUE )
  {
    return _TDL_DO_SET_MONITOR_PERIOD ( theNodeToConstrain, getMSecs() );
  }
  else
  {
    return _TDL_DO_SET_MONITOR_PERIOD ( theNodeToConstrain,
					getHours(),
					getMinutes(),
					getSeconds(),
					getFractionsOfASecond() );
  }
#else
  _TDL_MARKUSED ( theNodeToConstrain );
  return TCM_Ok;
#endif
}

/*virtual*/ const char *
_TDL_SetMonitorPeriod::getConstraintName() const
{
  return "_TDL_SetMonitorPeriod";
}


/*virtual*/ ostream &
_TDL_SetMonitorPeriod::printObject( ostream    & theOstream,
				    const char * theIndentString /*=""*/) const
{
  theOstream << theIndentString << "Constraint:  _TDL_SetMonitorPeriod  ("
	     << ((void *)this) << ")" << endl;
  _TDL_ConstraintTime::printObject   ( theOstream, theIndentString ) << endl;
  return _TDL_Constraint::printObject( theOstream, theIndentString );
}




/*** class _TDL_SetMonitorInitialWait ***/

/*virtual*/
_TDL_SetMonitorInitialWait::~_TDL_SetMonitorInitialWait()
{}

/*virtual*/ _TDL_Constraint *
_TDL_SetMonitorInitialWait::clone() const
{
  return new _TDL_SetMonitorInitialWait ( * this );
}


/*virtual*/ TCM_Return_Type
_TDL_SetMonitorInitialWait::performConstraint (
		     _TDL_SpawnStatementTreeNode & theNodeToConstrain,
		     _TDL_SpawnStatementTreeNode * theReferenceTreeNode ) const
{
  BOOLEAN  constraintOverriden;

  if ( theNodeToConstrain . isDestroyed() == TRUE )
  {
    TDL::getLogStream()
      << "[_TDL_SetMonitorInitialWait:performConstraint]  Error:  "
      << "Constraining Destroyed Node." << endl;
    return TCM_Error;
  }

	/* Disable Compiler warnings about unused variables */
  _TDL_MARKUSED ( theReferenceTreeNode );


	/* Deal with being overriden. */
  constraintOverriden
    = shouldOverrideConstraint ( theNodeToConstrain,
				 _TDL_Constraint::INITIAL_WAIT_TYPE );

	/* Lets be verbose */
  if (   (     TDL::getIsReporting ( TDL::VERBOSE_BRIEF ) )
      || (     TDL::getIsReporting ( TDL::VERBOSE       )
	   &&  ( constraintOverriden == TRUE            ) ) )
  {
    if ( constraintOverriden == TRUE )
      TDL::getLogStream() << "Constraint overriden (and canceled):    ";      

    TDL::getLogStream()
      << "_TDL_SetMonitorInitialWait ( Constrain="
      << theNodeToConstrain . getName();
    theNodeToConstrain . printArrayIndexes ( TDL::getLogStream() );
    TDL::getLogStream() << ", monitor_initial_wait = "
			<< ( getInitialWait() == TRUE ? "TRUE" : "FALSE" )
			<< " )" << endl;
  }

	/* Abort if we are being overriden. */
  if ( constraintOverriden == TRUE )
    return TCM_Ok;

  if ( TDL::getIsReporting ( TDL::VERBOSE_FULL ) )
  {
    printObject ( TDL::getLogStream(), "" );
    TDL::getLogStream() << endl;
  }

#ifndef _TDL_DISABLE_CONSTRAINTS_FOR_TESTING
  return _TDL_DO_SET_MONITOR_INITIAL_WAIT ( theNodeToConstrain, 
					    getInitialWait()   );
#else
  _TDL_MARKUSED ( theNodeToConstrain );
  return TCM_Ok;
#endif
}

/*virtual*/ const char *
_TDL_SetMonitorInitialWait::getConstraintName() const
{
  return "_TDL_SetMonitorInitialWait";
}


/*virtual*/ ostream &
_TDL_SetMonitorInitialWait::printObject(
				    ostream    & theOstream,
				    const char * theIndentString /*=""*/) const
{
  theOstream << theIndentString
	     << "Constraint:  _TDL_SetMonitorInitialWait  ("
	     << ((void *)this) << ")" << endl

	     << theIndentString
	     << " Initial Wait = "
	     << ( getInitialWait() == TRUE ? "TRUE" : "FALSE" ) << endl;
  return _TDL_Constraint::printObject( theOstream, theIndentString );
}




/*** class _TDL_AddExceptionHandler ***/

/*virtual*/
_TDL_AddExceptionHandler::~_TDL_AddExceptionHandler()
{
  actionRef          = (_TDL_Action      *) NULL;
  exceptionHandler   = (_TDL_BaseHandler *) NULL;
  index              = -1;
}

/*virtual*/ _TDL_Constraint *
_TDL_AddExceptionHandler::clone() const
{
  return new _TDL_AddExceptionHandler ( *this );
}


/*virtual*/ TCM_Return_Type
_TDL_AddExceptionHandler::performConstraint (
		     _TDL_SpawnStatementTreeNode & theNodeToConstrain,
		     _TDL_SpawnStatementTreeNode * theReferenceTreeNode ) const
{
  if ( theNodeToConstrain . isDestroyed() == TRUE )
  {
    TDL::getLogStream()
      << "[_TDL_AddExceptionHandler:performConstraint]  Error:  "
      << "Adding Exception to Destroyed Node." << endl;
    return TCM_Error;
  }

	/* Constraining running node is questionable... */
  if ( theNodeToConstrain . getState() == _TDL_SpawnStatementData::RUNNING )
  {
    TDL::getLogStream()
      << "[_TDL_AddExceptionHandler:performConstraint]  Warning:  Adding "
      << "Exception to node that has already started running is questionable."
      << endl;
  }


	/* Disable Compiler warnings about unused variables */
  _TDL_MARKUSED ( theReferenceTreeNode );

	/* Lets be verbose */
  if ( TDL::getIsReporting ( TDL::VERBOSE_BRIEF ) )
  {
    TDL::getLogStream()
      << "_TDL_AddExceptionHandler ( Node=" << theNodeToConstrain . getName();
    theNodeToConstrain . printArrayIndexes ( TDL::getLogStream() );
    TDL::getLogStream()
      << ", handler=" << ((void *) getExceptionHandler())
      << ", index=" << getIndex() << " )" << endl;
  }
  if ( TDL::getIsReporting ( TDL::VERBOSE_FULL ) )
  {
    printObject ( TDL::getLogStream(), "" );
    TDL::getLogStream() << endl;
  }


#ifndef _TDL_DISABLE_CONSTRAINTS_FOR_TESTING
  return _TDL_DO_ADD_EXCEPTION_HANDLER ( theNodeToConstrain,
					 getExceptionHandler(),
					 getIndex() );
#else
  _TDL_MARKUSED ( theNodeToConstrain );
  return TCM_Ok;
#endif
}

/*virtual*/ const char *
_TDL_AddExceptionHandler::getConstraintName() const
{
  return "_TDL_AddExceptionHandler";
}

/*virtual*/ ostream &
_TDL_AddExceptionHandler::printObject (
				   ostream    & theOstream,
				   const char * theIndentString /*=""*/ ) const
{
  theOstream << theIndentString << "Constraint:  _TDL_AddExceptionHandler  ("
	     << ((void *)this) << ")" << endl
	     << " handler = " << ((void *) getExceptionHandler())
	     << " index = " << getIndex() << endl;
  return _TDL_Constraint::printObject ( theOstream, theIndentString );
}



/*** class _TDL_OnTermination ***/

/*virtual*/
_TDL_OnTermination::~_TDL_OnTermination()
{}

/*virtual*/ _TDL_Constraint *
_TDL_OnTermination::clone() const
{
  return new _TDL_OnTermination ( *this );
}


	/* theReferenceTreeNode === theTreeNodeToRunIfTerminationOccurs */
/*virtual*/ TCM_Return_Type
_TDL_OnTermination::performConstraint ( 
		     _TDL_SpawnStatementTreeNode & theNodeToConstrain,
		     _TDL_SpawnStatementTreeNode * theReferenceTreeNode ) const
{
  if ( theNodeToConstrain . isDestroyed() == TRUE )
  {
    TDL::getLogStream()
      << "[_TDL_OnTermination:performConstraint]  Error:  "
      << "Constraining Destroyed Node." << endl;
    return TCM_Error;
  }

  if ( theReferenceTreeNode == (_TDL_SpawnStatementTreeNode *) NULL )
  {
    TDL::getLogStream()
      << "[_TDL_OnTermination:performConstraint]  Error:  "
      << "NULL value for theReferenceTreeNode "
      << "(The Task [treeNode] to run if/when termination occurs)."
      << endl;
    return TCM_Error;
  }

	/* If the reference node has been started, we have a problem! */
  if ( theReferenceTreeNode -> isNotStarted() == FALSE )
  {
    TDL::getLogStream()
      << "[_TDL_OnTermination:performConstraint]  Error:  "
      << "theReferenceTreeNode (The Task [treeNode] to run if/when termination"
      << " occurs) has already been started (or possibly destroyed).  ("
      << int4(theReferenceTreeNode -> getState())      << ",\""
      <<      theReferenceTreeNode -> getStateString() << "\")"
      << endl;
    return TCM_Error;
  }

	/* Mark reference node as now being in the RUNNING state, *
	 * so we can't use it again (Ie: twice).                  */
  theReferenceTreeNode -> getSpawnStatementData() . startRunning();


	/* Constraining running node is questionable... */
  if ( theNodeToConstrain . getState() == _TDL_SpawnStatementData::RUNNING )
  {
    TDL::getLogStream()
      << "[_TDL_OnTermination:performConstraint]  Warning:  Constraining "
      << "node that has already started running is questionable." << endl;
  }


	/* Lets be verbose */
  if ( TDL::getIsReporting ( TDL::VERBOSE_BRIEF ) )
  {
    TDL::getLogStream()
      << "_TDL_OnTermination ( Constrain=" << theNodeToConstrain . getName();
    theNodeToConstrain . printArrayIndexes ( TDL::getLogStream() );
    TDL::getLogStream()
      << " , Ref=" << theReferenceTreeNode -> getName();
    theReferenceTreeNode -> printArrayIndexes ( TDL::getLogStream() );
    TDL::getLogStream()
      << " [Ref_flags: " << theReferenceTreeNode -> getStateString() << "]"
      << " )" << endl;
  }
  if ( TDL::getIsReporting ( TDL::VERBOSE_FULL ) )
  {
    printObject ( TDL::getLogStream(), "" );
    TDL::getLogStream() << endl;
  }

#ifndef _TDL_DISABLE_CONSTRAINTS_FOR_TESTING
  return _TDL_DO_TCM_ON_TERMINATION (   theNodeToConstrain,
				      * theReferenceTreeNode );
#else
  _TDL_MARKUSED ( theNodeToConstrain   );
  _TDL_MARKUSED ( theReferenceTreeNode );
  return TCM_Ok;
#endif
}

	/* Deal with the NULL reference case internally as an error! */
/*virtual*/ BOOLEAN
_TDL_OnTermination::getCanUseNullReference() const
{
  return TRUE;
}

/*virtual*/ const char *
_TDL_OnTermination::getConstraintName() const
{
  return "_TDL_OnTermination";
}

/*virtual*/ ostream &
_TDL_OnTermination::printObject ( ostream    & theOstream,
				  const char * theIndentString /*=""*/ ) const
{
  theOstream << theIndentString << "Constraint:  _TDL_OnTermination  ("
	     << ((void *)this)  << ")" << endl;
  return _TDL_ConstraintWithReference::printObject ( theOstream,
						     theIndentString );
}





/*** class _TDL_OnAgent ***/

/*virtual*/
_TDL_OnAgent::~_TDL_OnAgent()
{}

/*virtual*/ _TDL_Constraint *
_TDL_OnAgent::clone() const
{
  return new _TDL_OnAgent ( * this );
}


/*virtual*/ TCM_Return_Type
_TDL_OnAgent::performConstraint (
		     _TDL_SpawnStatementTreeNode & theNodeToConstrain,
		     _TDL_SpawnStatementTreeNode * theReferenceTreeNode ) const
{
  BOOLEAN  constraintOverriden;

  if ( theNodeToConstrain . isDestroyed() == TRUE )
  {
    TDL::getLogStream()
      << "[_TDL_OnAgent:performConstraint]  Error:  "
      << "Constraining Destroyed Node." << endl;
    return TCM_Error;
  }

	/* Disable Compiler warnings about unused variables */
  _TDL_MARKUSED ( theReferenceTreeNode );


	/* Deal with being overriden. */
  constraintOverriden
    = shouldOverrideConstraint ( theNodeToConstrain,
				 _TDL_Constraint::ON_AGENT );

	/* Lets be verbose */
  if (   (     TDL::getIsReporting ( TDL::VERBOSE_BRIEF ) )
      || (     TDL::getIsReporting ( TDL::VERBOSE       )
	   &&  ( constraintOverriden == TRUE            ) ) )
  {
    if ( constraintOverriden == TRUE )
      TDL::getLogStream() << "Constraint overriden (and canceled):    ";      

    TDL::getLogStream()
      << "_TDL_OnAgent ( Constrain=" << theNodeToConstrain . getName();
    theNodeToConstrain . printArrayIndexes ( TDL::getLogStream() );
    TDL::getLogStream()
      << ", Agent-Name=\"" << getAgentName() << "\" )" << endl;
  }

	/* Abort if we are being overriden. */
  if ( constraintOverriden == TRUE )
    return TCM_Ok;

  if ( TDL::getIsReporting ( TDL::VERBOSE_FULL ) )
  {
    printObject ( TDL::getLogStream(), "" );
    TDL::getLogStream() << endl;
  }

#ifndef _TDL_DISABLE_CONSTRAINTS_FOR_TESTING
    /* OnAgent actually has no direct effects.
     * We search out on-agent constraints when constructing
     * the TCM_Task_Tree_Ref in _TDL_SpawnStatementData.
     */
  return TCM_Ok;
#else
  _TDL_MARKUSED ( theNodeToConstrain );
  return TCM_Ok;
#endif
}


/*virtual*/ BOOLEAN 
_TDL_OnAgent::isOnAgentConstraint() const
{
  return TRUE;
}


/*virtual*/ const char *
_TDL_OnAgent::getConstraintName() const
{
  return "_TDL_OnAgent";
}


/*virtual*/ ostream &
_TDL_OnAgent::printObject ( ostream    & theOstream,
			    const char * theIndentString /*=""*/ ) const
{
  theOstream << theIndentString
	     << "Constraint:  _TDL_OnAgent  ("
	     << ((void *)this) << ")" << endl

	     << theIndentString
	     << " Agent-Name = \"" << getAgentName() << "\"" << endl;
  return _TDL_Constraint::printObject( theOstream, theIndentString );
}
 

