/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */


#include <string.h>

#include "_TDL_NamesList.H"

/*virtual*/
_TDL_NamesList::~_TDL_NamesList()
{
	/* Clear the arraylist */
	/* Note:  This can't happen in _TDL_ArrayList since that base
	 * class can't access _TDL_NamesList's version of deleteElement().
	 */
  clearList();
}


	/* Note: theName will *NOT* be deleted when this object is destroyed!*/
_TDL_NamesList &
_TDL_NamesList::addName ( const char * theName )
{
	/* _TDL_ArrayList::addElement() method... */
  addElement ( (void *) theName );
  return *this;
}


BOOLEAN
_TDL_NamesList::hasName ( const char * theName ) const
{
  for ( u_int4 i=0;  i < getCount();  i++ )
  {
    if ( strcmp ( theName, getName ( i ) ) == 0 )
      return TRUE;
  }
  return FALSE;
}


	/* iostreamBase interface */
/*virtual*/ ostream &
_TDL_NamesList::printObject ( ostream    & theOstream,
			      const char * theIndentString /*=""*/ ) const
{
  char  subIndentString [ iostreamBase::MAXIMUM_INDENT ];
  iostreamBase::createSubIndentString ( subIndentString, theIndentString );

  theOstream << theIndentString << "-Begin- _TDL_NamesList: "
	     << ((void *)this)  << endl

	     << theIndentString
	     << " count               = "  << getCount()               << endl

	     << theIndentString
	     << " capacity            = "  << getCapacity()            << endl

	     << theIndentString
	     << " allocationBlockSize = "  << getAllocationBlockSize() << endl;

  for ( u_int4 i=0;  i < getCount();  i++ )
  {
    theOstream << theIndentString
	       << " name [ " << setw ( 2 ) << i << " ]         = \""
	       << getName ( i ) << "\"" << endl;
  }

  theOstream << theIndentString << "--End-- _TDL_NamesList: "
	     << ((void *)this)  << endl;

  return theOstream;
}


	/* _TDL_ArrayList Interface */
/*protected*/
/*virtual*/ void
_TDL_NamesList::deleteElement ( void * theElement )
{
  _TDL_MARKUSED ( theElement );
  /* Don't delete the strings (const char *)'s.
   * They are most likely coming in as string-constants...
   */
}


