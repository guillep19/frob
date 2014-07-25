/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

#include "_TDL_Dlist.H"

/*virtual*/
_TDL_Dlist::~_TDL_Dlist()
{
  emptyAllElementsInList();
}


	/* Note:  Deletes all elements if deletesContainedObjects == TRUE */
void
_TDL_Dlist::emptyAllElementsInList()
{
  _TDL_Dnode * node = getFirstNode();
  _TDL_Dnode * nextNode;

  while (   ( getDeletesContainedObjects() )
	 && ( node != (_TDL_Dnode *) NULL  ) )
  {
    nextNode = node -> getNextNode();
    delete node;
    node = nextNode;
  }

  setFirstNode ( (_TDL_Dnode *) NULL );
  setLastNode  ( (_TDL_Dnode *) NULL );
}



/* ASSUMES that theDnode can be deleted  *
 * when this Dlist is deleted!!!         *
 *  (if deletesContainedObjects == TRUE) */
status_t
_TDL_Dlist::appendNode ( _TDL_Dnode * theDnode )
{
  if (   (     getFirstNode()     != (_TDL_Dnode *) NULL   )
      && (   ( getLastNode()      == (_TDL_Dnode *) NULL )
	  || ( getLastNode()
	         -> getNextNode() != (_TDL_Dnode *) NULL ) ) )
  {
    TDL::getLogStream()
	  << "[_TDL_Dlist::appendNode]  Warning:  "
	  << "LastNode inconsistent.   Repairing..."
	  << endl;
    repairList();
  }


  if ( getFirstNode() == (_TDL_Dnode *) NULL )
  {
    setFirstNode ( theDnode );
    setLastNode  ( (_TDL_Dnode *) NULL );
  }

  else if ( getLastNode() == (_TDL_Dnode *) NULL )
  {
    TDL::getLogStream()
	  << "[_TDL_Dlist::appendNode]  Warning:  "
	  << "LastNode inconsistent.   Failed to Append Node!"
	  << endl;
    return FAILURE;
  }

  else
  {
    getLastNode() -> setNextNode ( theDnode );
  }

  theDnode -> setNextNode ( (_TDL_Dnode *) NULL );
  theDnode -> setPreviousNode ( getLastNode() );
  setLastNode ( theDnode );
  return SUCCESS;
}



/* ASSUMES that theDnode can be deleted  *
 * when this Dlist is deleted!!!         *
 *  (if deletesContainedObjects == TRUE) */
status_t
_TDL_Dlist::prependNode ( _TDL_Dnode * theDnode )
{
  if (   (     getFirstNode()     != (_TDL_Dnode *) NULL   )
      && (   ( getLastNode()      == (_TDL_Dnode *) NULL )
	  || ( getLastNode()
	         -> getNextNode() != (_TDL_Dnode *) NULL ) ) )
  {
    TDL::getLogStream()
	  << "[_TDL_Dlist::prependNode]  Warning:  "
	  << "LastNode inconsistent.   Repairing..."
	  << endl;
    repairList();
  }


  if ( getFirstNode() == (_TDL_Dnode *) NULL )
  {
    theDnode -> setNextNode     ( (_TDL_Dnode *) NULL );
    theDnode -> setPreviousNode ( (_TDL_Dnode *) NULL );
    setFirstNode ( theDnode );
    setLastNode  ( theDnode );
  }

  else
  {
    getFirstNode() -> setPreviousNode ( theDnode );
    theDnode -> setNextNode     ( getFirstNode() );
    theDnode -> setPreviousNode ( (_TDL_Dnode *) NULL );
    setFirstNode ( theDnode );
  }

  return SUCCESS;
}



	/* Note:  Does NOT delete theDnode. *
	 * Returns NULL for failure.        */
_TDL_Dnode *
_TDL_Dlist::removeNode( _TDL_Dnode *           theDnode,
			_TDL_Dlist::DIRECTION  theSearchDiretion /*=BACKWARD*/)
{
  _TDL_Dnode * dnode;
  _TDL_Dnode * lastDnode = (_TDL_Dnode *) NULL;

	/* Error check */
  if (   (     getFirstNode()     != (_TDL_Dnode *) NULL   )
      && (   ( getLastNode()      == (_TDL_Dnode *) NULL )
	  || ( getLastNode()
	         -> getNextNode() != (_TDL_Dnode *) NULL ) ) )
  {
    TDL::getLogStream()
	  << "[_TDL_Dlist::removeNode]  Warning:  "
	  << "LastNode inconsistent.   Repairing..."
	  << endl;
    repairList();
  }


  if ( theSearchDiretion == _TDL_Dlist::FORWARD )
    dnode = getFirstNode();
  else
    dnode = getLastNode();

  while ( dnode != (_TDL_Dnode *) NULL )
  {
    if ( dnode == theDnode )
    {
      if ( lastDnode == (_TDL_Dnode *) NULL )
      {
	if ( theSearchDiretion == _TDL_Dlist::FORWARD )
	{
	  setFirstNode ( dnode -> getNextNode() );
	  if ( dnode -> getNextNode() != (_TDL_Dnode *) NULL )
	    dnode -> getNextNode() -> setPreviousNode ( (_TDL_Dnode *) NULL );
	}
	else
	{
	  setLastNode ( dnode -> getPreviousNode() );
	  if ( dnode -> getPreviousNode() != (_TDL_Dnode *) NULL )
	    dnode -> getPreviousNode() -> setNextNode ( (_TDL_Dnode *) NULL );
	}
      }
      else
      {
	if ( theSearchDiretion == _TDL_Dlist::FORWARD )
	{
	  lastDnode -> setNextNode ( dnode -> getNextNode() );
	  if ( dnode -> getNextNode() != (_TDL_Dnode *) NULL )
	    dnode -> getNextNode() -> setPreviousNode ( lastDnode );
	}
	else
	{
	  lastDnode -> setPreviousNode ( dnode -> getPreviousNode() );
	  if ( dnode -> getPreviousNode() != (_TDL_Dnode *) NULL )
	    dnode -> getPreviousNode() -> setNextNode ( lastDnode );
	}
      }

      if (   ( theSearchDiretion == _TDL_Dlist::FORWARD )
	  && (   ( getLastNode()          == dnode               )
	      || ( dnode -> getNextNode() == (_TDL_Dnode *) NULL ) ) )
      {
	setLastNode ( lastDnode );
	if ( lastDnode != (_TDL_Dnode *) NULL )
	  lastDnode -> setNextNode ( (_TDL_Dnode *) NULL );
      }

      if (   ( theSearchDiretion != _TDL_Dlist::FORWARD )
	  && (   ( getFirstNode()             == dnode               )
	      || ( dnode -> getPreviousNode() == (_TDL_Dnode *) NULL ) ) )
      {
	setFirstNode ( lastDnode );
	if ( lastDnode != (_TDL_Dnode *) NULL )
	  lastDnode -> setPreviousNode ( (_TDL_Dnode *) NULL );
      }

      dnode -> setNextNode     ( (_TDL_Dnode *) NULL );
      dnode -> setPreviousNode ( (_TDL_Dnode *) NULL );
      return dnode;
    }

    else
    {
      lastDnode = dnode;

      if ( theSearchDiretion == _TDL_Dlist::FORWARD )
	dnode  = dnode -> getNextNode();
      else
	dnode  = dnode -> getPreviousNode();
    }
  } /* WHILE ( dnodes ) */


  return (_TDL_Dnode *) NULL;
}



BOOLEAN
_TDL_Dlist::contains (
	      const _TDL_Dnode *     theDnode,
	      _TDL_Dlist::DIRECTION  theSearchDiretion /* = BACKWARD */ ) const
{
  _TDL_Dnode * dnode;

  if ( theSearchDiretion == _TDL_Dlist::FORWARD )
    dnode = getFirstNode();
  else
    dnode = getLastNode();

  while ( dnode != (_TDL_Dnode *) NULL )
  {
    if ( dnode == theDnode )
      return TRUE;

    if ( theSearchDiretion == _TDL_Dlist::FORWARD )
      dnode  = dnode -> getNextNode();
    else
      dnode  = dnode -> getPreviousNode();
  }

  return FALSE;
}



u_int4
_TDL_Dlist::count() const
{
  u_int4       ourCount = 0;
  _TDL_Dnode * dnode;

  for ( dnode  = getFirstNode();
	dnode != (_TDL_Dnode *) NULL;
	dnode  = dnode -> getNextNode() )
  {
    ourCount++;
  }

  return ourCount;
}



_TDL_Dnode *
_TDL_Dlist::getNodeAtIndex ( u_int4 theIndex ) const
{
  _TDL_Dnode * dnode = getFirstNode();

  while (   ( theIndex >  0                   )
	 && ( dnode    != (_TDL_Dnode *) NULL ) )
  {
    theIndex --;
    dnode = dnode -> getNextNode();
  }

  return dnode;
}



void
_TDL_Dlist::repairList()
{
  _TDL_Dnode * lastNode = (_TDL_Dnode *) NULL;

  for ( _TDL_Dnode * node = getFirstNode();
	node != (_TDL_Dnode *) NULL;
	node = node -> getNextNode()
       )
  {
    node -> setPreviousNode ( lastNode );
    lastNode = node;
  }

  setLastNode ( lastNode );
}



_TDL_Dnode *
_TDL_Dlist::pop()
{
	/* Error check */
  if (   (     getFirstNode()     != (_TDL_Dnode *) NULL   )
      && (   ( getLastNode()      == (_TDL_Dnode *) NULL )
	  || ( getLastNode()
	         -> getNextNode() != (_TDL_Dnode *) NULL ) ) )
  {
    TDL::getLogStream()
	  << "[_TDL_Dlist::pop]  Warning:  "
	  << "LastNode inconsistent.   Repairing..."
	  << endl;
    repairList();
  }

  if ( getLastNode() == (_TDL_Dnode *)NULL )
  {
    return (_TDL_Dnode *)NULL;
  }
  else
  {
    return removeNode ( getLastNode(), _TDL_Dlist::BACKWARD );
  }
}



/*virtual*/ ostream &
_TDL_Dlist::printObject ( ostream    & theOstream,
			  const char * theIndentString /*=""*/ ) const
{
  char  subIndentString [ iostreamBase::MAXIMUM_INDENT ];
  iostreamBase::createSubIndentString ( subIndentString, theIndentString );

  theOstream << theIndentString << "-Begin- Dlist: "
	     << ((void *)this)  << endl;

  for ( _TDL_Dnode * node = getFirstNode();
	node != (_TDL_Dnode *) NULL;
	node = node -> getNextNode()
       )
  {
    node -> printObject ( theOstream, subIndentString );
  }

  theOstream << theIndentString << "--End-- Dlist: "
	     << ((void *)this)  << endl;

  return theOstream;
}


