/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

#ifndef _TDL_IterationIndex_H
#define _TDL_IterationIndex_H

#define _TDL_INTERNAL_
#include "tdl.H"
#include "_TDL_Dnode.H"
#include "_TDL_Dlist.H"

/*
 * Trivial class to keep track of the current indices of iteration loops.
 */

class _TDL_IterationIndex :
#ifndef DISABLE_ISTREAMBASE
		            public virtual iostreamBase,
#endif /* ! DISABLE_ISTREAMBASE */
		            public _TDL_Dnode
{
public:
  enum CONSTANTS
  {
    INVALID_INDEX = -1
  };

public:
  static ostream & printIndexList (
			       ostream          & theOstream,
			       const _TDL_Dlist & theListOfIterationIndexes,
			       _TDL_Dnode       * theLastNodeToPrint
						       = (_TDL_Dnode *) NULL,
			       _TDL_Dnode       * theFirstNodeToPrint
						       = (_TDL_Dnode *) NULL );

protected:
  int4     currentIndex;
  BOOLEAN  ignoreNextIncrement;

public:
  _TDL_IterationIndex ( int4    theCurrentIndex        = 0,
			BOOLEAN theIgnoreNextIncrement = TRUE )
    : _TDL_Dnode          (                        ),
      currentIndex        ( theCurrentIndex        ),
      ignoreNextIncrement ( theIgnoreNextIncrement )
  {}

  virtual ~_TDL_IterationIndex();


  int4    getCurrentIndex() const { return currentIndex; }
  void    setCurrentIndex( int4 theCurrentIndex )
		  { currentIndex = theCurrentIndex; }

  BOOLEAN getIgnoreNextIncrement() const { return ignoreNextIncrement; }
  void    setIgnoreNextIncrement( BOOLEAN theIgnoreNextIncrement )
		  { ignoreNextIncrement = theIgnoreNextIncrement; }


  void    incrementIndex();
  void    reset() { setCurrentIndex ( 0 ); setIgnoreNextIncrement ( TRUE ); }


	/* iostreamBase interface */
  virtual ostream & printObject ( ostream    & theOstream,
				  const char * theIndentString = "") const;


	/* These operations are disabled (prohibited) & unimplemented */
private:
  _TDL_IterationIndex            (const _TDL_IterationIndex & theObjectToCopy);
  _TDL_IterationIndex & operator=(const _TDL_IterationIndex & theObjectToCopy);
};

#endif /* _TDL_IterationIndex_H */

