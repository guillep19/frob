/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

#include "_TDL_ConstraintsList.H"
#include "_TDL_Constraints.H"

/*virtual*/
_TDL_ConstraintsList::~_TDL_ConstraintsList()
{
}

        /* Assumes that theConstraint has been dynamically allocated off
         * the heap, and that theConstraint should be deleted when this
         * object is deleted!
         */
status_t
_TDL_ConstraintsList::addConstraint ( _TDL_Constraint * theConstraint )
{
  return getConstraintSlistNonConst() . appendNode ( theConstraint );
}


	/* iostreamBase interface */
/*virtual*/ ostream &
_TDL_ConstraintsList::printObject( ostream    & theOstream,
				   const char * theIndentString /*=""*/ ) const
{
  char  subIndentString [ iostreamBase::MAXIMUM_INDENT ];
  iostreamBase::createSubIndentString ( subIndentString, theIndentString );

  theOstream << theIndentString << "_TDL_ConstraintsList:" 
	     << ((void *)this)  << endl;

  getConstraintSlist() . printObject ( theOstream, subIndentString );

  return theOstream;
};
