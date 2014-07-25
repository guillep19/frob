/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

#include "_TDL_ArrayList.H"


_TDL_ArrayList::_TDL_ArrayList ( u_int4 theInitialCapacity )
					       /* = DEFAULT_INITIAL_CAPACITY */
  : elements            ( (void * *) NULL                               ),
    count               ( 0                                             ),
    capacity            ( 0                                             ),
    allocationBlockSize ( _TDL_ArrayList::DEFAULT_ALLOCATION_BLOCK_SIZE )
{
  if ( theInitialCapacity > 0 )
    increaseCapacityBy ( theInitialCapacity );
}



/*virtual*/
_TDL_ArrayList::~_TDL_ArrayList()
{
	/* Note:  Good 'old C++.  Can't use virtual methods *
	 * in constructors OR destructors.  *sigh*.         *
	 * Cleanup has to occur in derived classes...       */
  if ( getCount() > 0 )
  {
    TDL::getLogStream()
      << "[_TDL_ArrayList:~_TDL_ArrayList]  Error:  "
      << "destructor invoked before clearList().  "
      << "Unable to delete contained data." << endl;
  }

	/* Clear the list anyway.  Might as well delete elements[]... */
  clearList();
}



void
_TDL_ArrayList::clearList()
{
  if ( elements != ( (void * *) NULL ) )
  {
    for ( u_int4 i = 0;   i < getCount();  i++ )
    {
      deleteElement ( elements [ i ] );
    }
    delete [] elements;
    elements = ( (void * *) NULL );
  }
	/* And reset these values */
  count    = 0;
  capacity = 0;
}



void
_TDL_ArrayList::setAllocationBlockSize( u_int4 theAllocationBlockSize )
{
	/* Idiocy check... */
  if ( theAllocationBlockSize == 0 )
  {
    TDL::getLogStream()
      << "[_TDL_ArrayList:setAllocationBlockSize] Warning:  "
      << "theAllocationBlockSize (" <<  theAllocationBlockSize
      << ")  == 0  [Assuming a value of 1]." << endl;

    theAllocationBlockSize = 1;
  }

	/* More Idiocy.  (Perhaps a calculation has gone negative???) */
  if (   theAllocationBlockSize
       > _TDL_ArrayList::MAX_ALLOCATION_BLOCK_SIZE )
  {
    TDL::getLogStream()
      << "[_TDL_ArrayList:setAllocationBlockSize] Warning:  "
      << "theAllocationBlockSize (" << theAllocationBlockSize << ")  > MAX ("
      << u_int4(_TDL_ArrayList::MAX_ALLOCATION_BLOCK_SIZE)
      << ")   [Assuming a value of MAX]." << endl;

    theAllocationBlockSize
      = _TDL_ArrayList::MAX_ALLOCATION_BLOCK_SIZE;
  }

  allocationBlockSize = theAllocationBlockSize;
}



void
_TDL_ArrayList::increaseCapacityBy ( u_int4 theAmountToIncreaseCapacityBy )
{								      /* = 1 */
  void  * * newElements;
  u_int4    newCapacity, i;
  

	/* Idiocy check... */
  if ( theAmountToIncreaseCapacityBy == 0 )
  {
    TDL::getLogStream()
      << "[_TDL_ArrayList:increaseCapacityBy] Warning:  "
      << "theAmountToIncreaseCapacityBy ("
      <<  theAmountToIncreaseCapacityBy
      << ")  == 0  [Assuming a value of 1]." << endl;

    theAmountToIncreaseCapacityBy = 1;
  }

	/* More Idiocy.  (Perhaps a calculation has gone negative???) */
  if (   theAmountToIncreaseCapacityBy
       > _TDL_ArrayList::MAX_ALLOCATION_BLOCK_SIZE )
  {
    TDL::getLogStream()
      << "[_TDL_ArrayList:increaseCapacityBy] Warning:  "
      << "theAmountToIncreaseCapacityBy ("
      <<  theAmountToIncreaseCapacityBy << ")  > MAX ("
      << u_int4(_TDL_ArrayList::MAX_ALLOCATION_BLOCK_SIZE)
      << ")   [Assuming a value of MAX]." << endl;

    theAmountToIncreaseCapacityBy
      = _TDL_ArrayList::MAX_ALLOCATION_BLOCK_SIZE;
  }

  newCapacity = getCapacity() + theAmountToIncreaseCapacityBy;
  newElements = new void * [ newCapacity ];

  i = 0;

  if ( elements != ( (void * *) NULL ) )
  {
    for ( ;  i < getCapacity();  i++ )
      newElements [ i ] = elements [ i ];

    delete [] elements;
  }

  for ( ;  i < newCapacity;  i++ )
    newElements [ i ] = (void *)NULL;

  elements = newElements;
  capacity = newCapacity;
}




/*virtual*/ void
_TDL_ArrayList::deleteElement ( void * theElement )
{
  _TDL_MARKUSED ( theElement );
  TDL::getLogStream()
      << "[_TDL_ArrayList:deleteElement]  Error:  "
      << "Default deleteElement() has been invoked.  "
      << "deleteElement() should have been overriden via a subclass.  Or "
      << "perhaps the destructor was invoked without calling clearList first?"
      << "  [Deletion action aborted.]" << endl;
}



void
_TDL_ArrayList::addElement    ( void * theElement )
{
	/* Do we need more space? */
  if ( getCount() >= getCapacity() )
  {
    increaseCapacityBy (   getAllocationBlockSize()
			 + getCount() - getCapacity() );
  }

  if ( getCount() < getCapacity() )
  {
    elements [ count++ ] = theElement;
  }
  else
  {
    TDL::getLogStream()
      << "[_TDL_ArrayList:addElement] Error:  "
      << "Insufficient space to store additional element.  "
      << "(increaseCapacityBy failure?)"
      << endl;
  }
}


