/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

public class DataSourceString extends DataSource
{
	/* Instance Variables */
  protected String   ourString;


	/* Instance Methods */
  public DataSourceString ( String theDataString )
  {
    ourString = theDataString;
  }

  public String getString       ( )  { return ourString; }
  public int    getStringLength ( )  { return getString().length(); }

	/* Note:  Use of this method could really mess up any Parsers */
  public void   setString ( String theNewString )
  {
    ourString = theNewString;
    super . reset();
  }


  public char read ( int thePosition ) throws java.io.IOException
  {
    if (  ( thePosition >= 0 )  &&  ( thePosition < getStringLength() )  )
    {
      return getString().charAt ( thePosition );
    }
    else if ( thePosition >= getStringLength() )
    {
      throw new java.io.IOException ( "END-OF-FILE (" + getStringLength()
				      + ", " + thePosition + ")" );
    }
    else
    {
      throw new java.io.IOException ( "Invalid Index (" + thePosition + ")" );
    }
  }

	/* Note: Reads substring:  StartPosition <= position < EndPosition */
  public String readSubString ( int theStartPosition, int theEndPosition )
  {
    if ( theStartPosition < 0 )
    {
      System.err.println ( "[DataSourceString:readSubString]  Warning:  "
			   + "theStartPosition(" + theStartPosition 
			   + ") < 0" );
      theStartPosition = 0;
    }

    if ( theStartPosition >= getStringLength() )
    {
      System.err.println ( "[DataSourceString:readSubString]  Warning:  "
			   + "theStartPosition(" + theStartPosition 
			   + ") >= getStringLength(" + getStringLength()
			   + ")" );
      theStartPosition = getStringLength() - 1;
    }

    if ( theEndPosition < 0 )
    {
      System.err.println ( "[DataSourceString:readSubString]  Warning:  "
			   + "theEndPosition(" + theEndPosition 
			   + ") < 0" );
      theEndPosition = 0;
    }

    if ( theEndPosition > getStringLength() )
    {
      System.err.println ( "[DataSourceString:readSubString]  Warning:  "
			   + "theEndPosition(" + theEndPosition 
			   + ") > getStringLength(" + getStringLength()
			   + ")" );
      theEndPosition = getStringLength();
    }

    if ( theStartPosition >= theEndPosition )
    {
      System.err.println ( "[DataSourceString:readSubString]  Warning:  "
			   + "theStartPosition(" + theStartPosition 
			   + ") >= theEndPosition(" + theEndPosition + ")" );
      return "";
    }

    return getString().substring ( theStartPosition, theEndPosition );
  }


}
