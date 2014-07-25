/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

#include "_TDL_Slist.H"

/*virtual*/
_TDL_Slist::~_TDL_Slist()
{
  emptyAllElementsInList();
}


	/* Note:  Deletes all elements if deletesContainedObjects == TRUE */
void
_TDL_Slist::emptyAllElementsInList()
{
  _TDL_Snode * node = getFirstNode();
  _TDL_Snode * nextNode;

  while (   ( getDeletesContainedObjects() )
	 && ( node != (_TDL_Snode *) NULL  ) )
  {
    nextNode = node -> getNextNode();
    delete node;
    node = nextNode;
  }

  setFirstNode ( (_TDL_Snode *) NULL );
  setLastNode  ( (_TDL_Snode *) NULL );
}



/* ASSUMES that theSnode can be deleted  *
 * when this Slist is deleted!!!         *
 *  (if deletesContainedObjects == TRUE) */
status_t
_TDL_Slist::appendNode ( _TDL_Snode * theSnode )
{
  if (   (     getFirstNode()     != (_TDL_Snode *) NULL   )
      && (   ( getLastNode()      == (_TDL_Snode *) NULL )
	  || ( getLastNode()
	         -> getNextNode() != (_TDL_Snode *) NULL ) ) )
  {
    TDL::getLogStream()
	  << "[_TDL_Slist::appendNode]  Warning:  "
	  << "LastNode inconsistent.   Repairing..."
	  << endl;
    repairList();
  }


  if ( getFirstNode() == (_TDL_Snode *) NULL )
  {
    setFirstNode ( theSnode );
  }

  else if ( getLastNode() == (_TDL_Snode *) NULL )
  {
    TDL::getLogStream()
	  << "[_TDL_Slist::appendNode]  Warning:  "
	  << "LastNode inconsistent.   Failed to Append Node!"
	  << endl;
    return FAILURE;
  }

  else
  {
    getLastNode() -> setNextNode ( theSnode );
  }

  theSnode -> setNextNode ( (_TDL_Snode *) NULL );
  setLastNode ( theSnode );
  return SUCCESS;
}



/* ASSUMES that theSnode can be deleted  *
 * when this Slist is deleted!!!         *
 *  (if deletesContainedObjects == TRUE) */
status_t
_TDL_Slist::prependNode ( _TDL_Snode * theSnode )
{
  if (   (     getFirstNode()     != (_TDL_Snode *) NULL   )
      && (   ( getLastNode()      == (_TDL_Snode *) NULL )
	  || ( getLastNode()
	         -> getNextNode() != (_TDL_Snode *) NULL ) ) )
  {
    TDL::getLogStream()
	  << "[_TDL_Slist::prependNode]  Warning:  "
	  << "LastNode inconsistent.   Repairing..."
	  << endl;
    repairList();
  }


  if ( getFirstNode() == (_TDL_Snode *) NULL )
  {
    theSnode -> setNextNode ( (_TDL_Snode *) NULL );
    setFirstNode ( theSnode );
    setLastNode  ( theSnode );
  }

  else
  {
    theSnode -> setNextNode ( getFirstNode() );
    setFirstNode ( theSnode );
  }

  return SUCCESS;
}



	/* Note:  Does NOT delete theSnode. *
	 * Returns NULL for failure.        */
_TDL_Snode *
_TDL_Slist::removeNode ( _TDL_Snode * theSnode )
{
  _TDL_Snode * snode;
  _TDL_Snode * lastSnode = (_TDL_Snode *) NULL;

	/* Error check */
  if (   (     getFirstNode()     != (_TDL_Snode *) NULL   )
      && (   ( getLastNode()      == (_TDL_Snode *) NULL )
	  || ( getLastNode()
	         -> getNextNode() != (_TDL_Snode *) NULL ) ) )
  {
    TDL::getLogStream()
	  << "[_TDL_Slist::removeNode]  Warning:  "
	  << "LastNode inconsistent.   Repairing..."
	  << endl;
    repairList();
  }


  for ( snode  = getFirstNode();
	snode != (_TDL_Snode *) NULL;
	snode  = snode -> getNextNode() )
  {
    if ( snode == theSnode )
    {
      if ( lastSnode == (_TDL_Snode *) NULL )
      {
	setFirstNode ( snode -> getNextNode() );
      }
      else
      {
	lastSnode -> setNextNode ( snode -> getNextNode() );
      }

      if (   ( getLastNode()          == snode               )
	  || ( snode -> getNextNode() == (_TDL_Snode *) NULL ) )
      {
	setLastNode ( lastSnode );
	if ( lastSnode != (_TDL_Snode *) NULL )
	  lastSnode -> setNextNode ( (_TDL_Snode *) NULL );
      }

      snode -> setNextNode ( (_TDL_Snode *) NULL );
      return snode;
    }

    else
    {
      lastSnode = snode;
    }
  } /* FOR ( snodes ) */

  return (_TDL_Snode *) NULL;
}



BOOLEAN
_TDL_Slist::contains ( const _TDL_Snode * theSnode ) const
{
  _TDL_Snode * snode;

  for ( snode  = getFirstNode();
	snode != (_TDL_Snode *) NULL;
	snode  = snode -> getNextNode() )
  {
    if ( snode == theSnode )
      return TRUE;
  }

  return FALSE;
}



u_int4
_TDL_Slist::count() const
{
  u_int4       ourCount = 0;
  _TDL_Snode * snode;

  for ( snode  = getFirstNode();
	snode != (_TDL_Snode *) NULL;
	snode  = snode -> getNextNode() )
  {
    ourCount++;
  }

  return ourCount;
}



_TDL_Snode *
_TDL_Slist::getNodeAtIndex ( u_int4 theIndex ) const
{
  _TDL_Snode * snode = getFirstNode();

  while (   ( theIndex >  0                   )
	 && ( snode    != (_TDL_Snode *) NULL ) )
  {
    theIndex --;
    snode = snode -> getNextNode();
  }

  return snode;
}



void
_TDL_Slist::repairList()
{
  _TDL_Snode * lastNode = (_TDL_Snode *) NULL;

  for ( _TDL_Snode * node = getFirstNode();
	node != (_TDL_Snode *) NULL;
	node = node -> getNextNode()
       )
  {
    lastNode = node;
  }

  setLastNode ( lastNode );
}



_TDL_Snode *
_TDL_Slist::pop()
{
	/* Error check */
  if (   (     getFirstNode()     != (_TDL_Snode *) NULL   )
      && (   ( getLastNode()      == (_TDL_Snode *) NULL )
	  || ( getLastNode()
	         -> getNextNode() != (_TDL_Snode *) NULL ) ) )
  {
    TDL::getLogStream()
	  << "[_TDL_Slist::pop]  Warning:  "
	  << "LastNode inconsistent.   Repairing..."
	  << endl;
    repairList();
  }

  if ( getLastNode() == (_TDL_Snode *)NULL )
  {
    return (_TDL_Snode *)NULL;
  }
  else
  {
    return removeNode ( getLastNode() );
  }
}


	/* iostreamBase interface */
/*virtual*/ ostream &
_TDL_Slist::printObject ( ostream    & theOstream,
			  const char * theIndentString /*=""*/ ) const
{
  char  subIndentString [ iostreamBase::MAXIMUM_INDENT ];
  iostreamBase::createSubIndentString ( subIndentString, theIndentString );

  theOstream << theIndentString << "-Begin- Slist: "
	     << ((void *)this)  << endl;

  for ( _TDL_Snode * node = getFirstNode();
	node != (_TDL_Snode *) NULL;
	node = node -> getNextNode()
       )
  {
    node -> printObject ( theOstream, subIndentString );
  }

  theOstream << theIndentString << "--End-- Slist: "
	     << ((void *)this)  << endl;

  return theOstream;
}



