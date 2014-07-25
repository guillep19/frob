/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

public class DataDestinationStringBuffer
				extends DataDestinationAbstractBaseClass
{
	/* Instance Variables */
  protected StringBuffer  stringBuffer;


	/* Instance Methods */
  public DataDestinationStringBuffer ( )
  {
    stringBuffer = new StringBuffer ( );
  }

  protected StringBuffer getStringBuffer ( ) { return stringBuffer; }
  public    String       getString ( ) { return getStringBuffer().toString(); }


  public int getLength ( )
  {
    if ( getStringBuffer() . length()  !=  super . getLength() )
    {
      System.err.println (
             "[DataDestinationStringBuffer:writeWithoutIndenting]  Warning:  "
	   + "Internal Data Inconsistency detected.  BufferLength=" 
	   + getStringBuffer() . length() + "   internalLength="
	   + super . getLength() + "   Assuming BufferLength correct.  "
	   + "Attempting to recover..." );
      super . length = getStringBuffer() . length();
      return getStringBuffer() . length();
    }
    else
    {
      return super . getLength();
    }
  }


  public void writeWithoutIndenting ( String theString )
  {
    	/* Trivial case */
    if (   ( theString == null )
	|| ( theString . equals ( "" ) ) )
    {
      return;
    }

	/* Error case */
    if ( getHasClosed() )
    {
      System.err.println (
             "[DataDestinationStringBuffer:writeWithoutIndenting]  Error:  "
	   + "Attempted to write to a close'd DataDestinationStringBuffer "
	   + "detected and aborted" );
      return;
    }

	/* General case */
    getStringBuffer() . append ( theString );
    super . writeWithoutIndenting ( theString );
  }
}
