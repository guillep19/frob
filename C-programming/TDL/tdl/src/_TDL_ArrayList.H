/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

#ifndef _TDL_ArrayList_H
#define _TDL_ArrayList_H

#define _TDL_INTERNAL_
#include "tdl.H"

  /*
   * _TDL_ArrayList:  A Simple Dynamic Array-List class.
   *
   * Note:  This class exists to simplify certain other classes
   * that need to use this functionality.
   */
class _TDL_ArrayList
{
public:
  enum CONSTANTS
  {
    DEFAULT_INITIAL_CAPACITY      = 10,
    DEFAULT_ALLOCATION_BLOCK_SIZE = 10,
    MAX_ALLOCATION_BLOCK_SIZE     = 100
  };


protected:
  void * * elements;
  u_int4   count, capacity, allocationBlockSize;


public:
  _TDL_ArrayList ( u_int4 theInitialCapacity
			          = _TDL_ArrayList::DEFAULT_INITIAL_CAPACITY );
  virtual ~_TDL_ArrayList();

  void   clearList();

  u_int4 getCount()               const { return count; }
  u_int4 getCapacity()            const { return capacity; }
  u_int4 getAllocationBlockSize() const { return allocationBlockSize; }
  void   setAllocationBlockSize( u_int4 theAllocationBlockSize );
  void   increaseCapacityBy    ( u_int4 theAmountToIncreaseCapacityBy = 1 );

protected:
    /* This method *MUST* be implemented in subclasses to perform *
     * appropriate deletion of the cast version of theElement.    *
     * Otherwise the element will *NOT* get deleted, and the      *
     * appropriate destructor will *NOT* get invoked.             */
  virtual void deleteElement ( void * theElement );


  void    addElement ( void * theElement );
  void *  getElement ( u_int4  theIndex ) const;


	/* These operations are disabled (prohibited) & unimplemented */
private:
  _TDL_ArrayList              ( const _TDL_ArrayList & theObjectToCopy );
  _TDL_ArrayList & operator = ( const _TDL_ArrayList & theObjectToCopy );
};

	/**************************************/
	/* Inlined Methods for _TDL_ArrayList */
	/**************************************/

/*protected*/
inline void *
_TDL_ArrayList::getElement ( u_int4  theIndex ) const
{
  if ( theIndex < getCount() )
    return elements [ theIndex ];
  else
  {
    TDL::getLogStream()
      << "[_TDL_ArrayList:getElement]  Warning: Illegal Index:  " << theIndex
      << endl;
    return (void *)NULL;
  }
}


#endif /* _TDL_ArrayList_H */

