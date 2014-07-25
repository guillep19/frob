/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

#ifndef _TDL_NamesList_H
#define _TDL_NamesList_H

#define _TDL_INTERNAL_
#include "tdl.H"
#include "_TDL_ArrayList.H"


/*
 * Contains a set of names in an arbitrary sized array maintained in
 * the base class _TDL_ArrayList.
 *
 * These char-strings are NOT deleted upon destruction of this object.
 * (Since they are most likely coming in as C-char-string-constants.)
 */

class _TDL_NamesList : 
#ifndef DISABLE_ISTREAMBASE
		       public virtual iostreamBase,
#endif /* ! DISABLE_ISTREAMBASE */
		       public _TDL_ArrayList
{
public:
  enum  CONSTANTS
  {
    DEFAULT_NAME_INDEX = 0
  };

  _TDL_NamesList ( u_int4        theInitialCapacity
			            = _TDL_ArrayList::DEFAULT_INITIAL_CAPACITY,
		   const char * theFirstName
			            = (const char *)NULL )
    :  _TDL_ArrayList ( theInitialCapacity )
  {
    if ( theFirstName != (const char *) NULL )
      addName ( theFirstName );
  }

  virtual ~_TDL_NamesList();

  	/* Note: theName will *NOT* be deleted when this object is destroyed!*/
  _TDL_NamesList &  addName ( const char * theName );
  BOOLEAN           hasName ( const char * theName ) const;
  const char *      getName ( u_int4 theIndex
			          = _TDL_NamesList::DEFAULT_NAME_INDEX ) const;

	/* iostreamBase interface */
  virtual ostream & printObject ( ostream    & theOstream,
				  const char * theIndentString = "") const;

protected:
	/* _TDL_ArrayList Interface */
  virtual void deleteElement ( void * theElement );


	/* These operations are disabled (prohibited) & unimplemented */
private:
  _TDL_NamesList (              const _TDL_NamesList & theObjectToCopy );
  _TDL_NamesList & operator = ( const _TDL_NamesList & theObjectToCopy );
};


	/**************************************/
	/* Inlined Methods for _TDL_NamesList */
	/**************************************/

inline const char *
_TDL_NamesList::getName ( u_int4 theIndex /* = DEFAULT_NAME_INDEX */ ) const
{
  if ( theIndex < getCount() )
    return (const char *) _TDL_ArrayList::getElement ( theIndex );
  else
  {
    TDL::getLogStream()
      << "[_TDL_NamesList:getName]  Warning: Illegal Index:  " << theIndex
      << endl;
    return (const char *) NULL;
  }
}


#endif /* _TDL_NamesList_H */

