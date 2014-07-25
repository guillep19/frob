/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

#ifndef _TDL_ConstraintsList_H
#define _TDL_ConstraintsList_H

#define _TDL_INTERNAL_
#include "tdl.H"
#include "_TDL_Snode.H"
#include "_TDL_Slist.H"


/*
 * _TDL_ConstraintsList is an base class abstracting away the concept of
 * containing a list of WITH-Statement-style constraints.
 *
 * Originally part of _TDL_WithStatementData.
 *
 * Extend to be part of _TDL_TreeNodeBranch to support iteration constraints.
 *
 * Extended again (and made an independent base class) to support constraints 
 * applied to a DISTRIBUTED task.  (Due to the need to apply such constraints
 * after the last on-agent constraint is applied.)
 */

class _TDL_Constraint;

class _TDL_ConstraintsList
#ifndef DISABLE_ISTREAMBASE
		 : public virtual iostreamBase
#endif /* ! DISABLE_ISTREAMBASE */
{
protected:
   _TDL_Slist   constraintSlist;
 
public:
  _TDL_ConstraintsList ( )
    : constraintSlist ( TRUE /* Delete contained objects */ )
  {}

  virtual ~_TDL_ConstraintsList();


  const _TDL_Slist & getConstraintSlist() const { return constraintSlist; }

        /* Assumes that theConstraint has been dynamically allocated off
         * the heap, and that theConstraint should be deleted when this
         * object is deleted!
         */
  status_t addConstraint ( _TDL_Constraint * theConstraint );


	/* Abstract iostreamBase-interface method */
  virtual ostream & printObject (
	     ostream    & theOstream,
	     const char * theIndentString = "") const TCM_PURE_VIRTUAL_METHOD ;


protected:
  _TDL_Slist & getConstraintSlistNonConst() { return constraintSlist; }


	/* These operations are disabled (prohibited) & unimplemented */
private:
  _TDL_ConstraintsList (        const _TDL_ConstraintsList & theObjectToCopy );
  _TDL_ConstraintsList & operator = (
			        const _TDL_ConstraintsList & theObjectToCopy );
};

#endif /* _TDL_ConstraintsList_H */
