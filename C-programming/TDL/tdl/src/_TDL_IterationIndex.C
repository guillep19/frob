/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

#include "_TDL_IterationIndex.H"

/*static*/ ostream &
_TDL_IterationIndex::printIndexList (
				 ostream          & theOstream,
				 const _TDL_Dlist & theListOfIterationIndexes,
				 _TDL_Dnode       * theLastNodeToPrint,
						      /*= (_TDL_Dnode *) NULL*/
				 _TDL_Dnode       * theFirstNodeToPrint       )
{						      /*= (_TDL_Dnode *) NULL*/
  _TDL_Dnode * node;

  theOstream << "[ ";

  for ( node  = theListOfIterationIndexes . getFirstNode();
	node != (_TDL_Dnode *) NULL;
	node  = node -> getNextNode()
       )
  {
    if ( theFirstNodeToPrint == node )
      theFirstNodeToPrint = (_TDL_Dnode *) NULL;

    if ( theFirstNodeToPrint == (_TDL_Dnode *) NULL )
      theOstream << ((_TDL_IterationIndex *) node) -> getCurrentIndex() << " ";

    if ( theLastNodeToPrint == node )
      break;
  }

  theOstream << " ]";
  return theOstream;
}



/*virtual*/
_TDL_IterationIndex::~_TDL_IterationIndex()
{
  currentIndex        = _TDL_IterationIndex::INVALID_INDEX;
  ignoreNextIncrement = TRUE;
}

void
_TDL_IterationIndex::incrementIndex()
{
  if ( getIgnoreNextIncrement() == TRUE )
  {
    setIgnoreNextIncrement ( FALSE );
  }
  else
  {
    setCurrentIndex ( getCurrentIndex() + 1 );
  }
}


	/* iostreamBase interface */
/*virtual*/ ostream &
_TDL_IterationIndex::printObject ( ostream    & theOstream,
				   const char * theIndentString /*=""*/ ) const
{
  theOstream << theIndentString << "IterationIndex:  " << getCurrentIndex()
	     << "  " << ( (getIgnoreNextIncrement() == TRUE)
			 ? "[IgnoringNextIncrement]"
			 : "[NextIncrementWillHappen]" )
	     << endl;

  return theOstream;
}

