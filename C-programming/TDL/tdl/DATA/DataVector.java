
/*
 * Originally, TDL started life as a Visual-Design-Tool for visual-programming.
 * As a consequence, we used netscape's IFC library, and netscape's Vector
 * utility class.  As we phase out IFC support, it is useful to have our
 * own Vector interface class, where we can add additional methods as
 * necessary.
 *
 * Addendum:  So the clueless idiots over at Sun went and made every method
 * of the java.util.Vector class "final".  WHY???  I don't know.  They didn't
 * do it for the Hashtable class...  Well, anyways, that's while we are now
 * wrapping the Vector class. (And adding new wrapped methods as needed.)
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 *
 */

import java.util.Vector;

public class DataVector extends Object
{
	/* Instance variables */
  java.util.Vector   actualVector;

	/* Instance methods */
  public DataVector ( )
  {
    actualVector = new java.util.Vector();
  }

  public DataVector ( int theInitialSize )
  {
    actualVector = new java.util.Vector ( theInitialSize );
  }

  protected java.util.Vector getActualVector() { return actualVector; }


  public void addElement ( Object theElement )
  {
    if ( theElement == null )
      throw new java.lang.NullPointerException (
		  "[DataVector:addElement]  "
		  + "Attempted to add a NULL element." );
    else
      getActualVector() . addElement ( theElement );
  }

  public void insertElementAt ( Object theElement, int theIndex )
  {
    if ( theElement == null )
      throw new java.lang.NullPointerException (
		  "[DataVector:insertElementAt]  "
		  + "Attempted to insert a NULL element." );
    else
      getActualVector() . insertElementAt ( theElement, theIndex );
  }

  public Object replaceElementAt ( int theIndex, Object theElement )
  {
    if ( theElement == null )
      throw new java.lang.NullPointerException (
		  "[DataVector:replaceElementAt]  "
		  + "Attempted to replace with a NULL element." );
    else
    {
      Object returnObject = getActualVector() . elementAt ( theIndex );
      getActualVector() . setElementAt ( theElement, theIndex );
      return returnObject;
    }
  }

  public Object removeElementAt ( int theIndex )
  {
    Object returnObject = getActualVector() . elementAt ( theIndex );
    getActualVector() . removeElementAt ( theIndex );
    return returnObject;
  }

  public Object removeLastElement()
  {
    if ( size() <= 0 )
      throw new java.lang.ArrayIndexOutOfBoundsException (
		  "[DataVector:removeLastElement]  "
		  + "Vector is empty." );
    else
      return removeElementAt ( size() - 1 );
  }

  public boolean contains ( Object theElement )
  {
    for ( int i = 0;   i < size();   i++ )
    {
      if ( theElement . equals ( getActualVector() . elementAt ( i ) ) )
	return true;
    }
    return false;
  }

  public boolean containsIdentical ( Object theElement )
  {
    for ( int i = 0;   i < size();   i++ )
    {
      if ( theElement == getActualVector() . elementAt ( i ) )
	return true;
    }
    return false;
  }

    /* Implement this here -- since it's trivial -- to force a shallow-copy. */
  public Object clone()
  {
    DataVector returnVector = new DataVector ( getActualVector().capacity() );

    for ( int i = 0;   i < getActualVector() . size();   i++ )
      returnVector . addElement ( getActualVector() . elementAt ( i ) );

    return returnVector;
  }


  public int count()
  {
    return getActualVector() . size();
  }

  public int size()
  {
    return getActualVector() . size();
  }

  public Object elementAt ( int theIndex )
  {
    return getActualVector() . elementAt ( theIndex );
  }

  public boolean removeElement( Object theObject )
  {
    return getActualVector() . removeElement ( theObject );
  }

  public void removeAllElements()
  {
    getActualVector() . removeAllElements();
  }

  public boolean isEmpty()
  {
    return getActualVector() . isEmpty();
  }

  public Object lastElement()
  {
    return getActualVector() . lastElement();
  }

  public String toString()
  {
    return getActualVector() . toString();
  }

}
