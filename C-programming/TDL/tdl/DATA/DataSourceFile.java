/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

import java.io.*;

public class DataSourceFile extends DataSource
{
	/* Instance Variables */
  protected RandomAccessFile   file;

	/* Instance Methods */
  public DataSourceFile ( )
  {
    file = null;
  }

  public DataSourceFile ( File theFile )
  {
    setFile ( theFile );
  }

  public DataSourceFile ( String thePathname )
  {
    setFile ( thePathname, true );
  }

	/* Changing the file while parsing could really muck things up.. */
  public void setFile ( File  theFile )
  {
    if ( theFile . exists() == false )
    {
      System.err.println ( "[DataSourceFile:setFile]  Error: "
			   + "theFile does not exist.  (theFile = \""
			   + theFile + "\")." );
      file = null;
      return;
    }

    if ( theFile . isDirectory() )
    {
      System.err.println ( "[DataSourceFile:setFile]  Error: "
			   + "theFile is a directory.  (theFile = \""
			   + theFile + "\")." );
      file = null;
      return;
    }

    setFile ( theFile . getAbsolutePath(), false );
    setFilenameWithoutPathDefault ( theFile . getName() );
  }

	/* Changing the file while parsing could really muck things up.. */
  public void setFile ( String thePathname, boolean theSetFilenameValue )
  {
    super.reset();
    try
    {
      file = new RandomAccessFile ( thePathname, "r" );
    }
    catch ( IllegalArgumentException  theException )
    {
      System.err.println ( "[DataSourceFile:setFile]  Error: "
			   + "Unable to open file \"" + thePathname
			   + "\".  IllegalArgumentException = "
			   + theException );
      file = null;
    }
    catch ( IOException  theException )
    {
      System.err.println ( "[DataSourceFile:setFile]  Error: "
			   + "Unable to open file \"" + thePathname
			   + "\". IOException  = " + theException );
      file = null;
    }
    catch ( SecurityException  theException )
    {
      System.err.println ( "[DataSourceFile:setFile]  Error: "
			   + "Unable to open file \"" + thePathname
			   + "\".  SecurityException = " + theException );
      file = null;
    }

    if ( theSetFilenameValue == true )
    {
      setFilenameWithoutPathDefault ( new File ( thePathname ) . getName() );
    }
  }


  public RandomAccessFile  getFile() { return file; }
  public boolean           hasFile() { return getFile() != null; }

  public char read ( int thePosition ) throws java.io.IOException
  {
    if ( hasFile() == false )
      throw new java.io.IOException ("[DataSourceFile] File not initialized.");

    getFile() . seek ( thePosition );

    int  readChar = getFile() . read();

    if ( readChar != -1 )
      return (char) (readChar);
    else
      throw new java.io.IOException ( "END-OF-FILE REACHED" );
  }

	/* Note: Reads substring:  StartPosition <= position < EndPosition */
  public String readSubString ( int theStartPosition, int theEndPosition )
  {
    if ( hasFile() == false )
    {
      System.err.println ( "[DataSourceFile:readSubString]  Warning:  "
			   + "File was never initialized." );
      return "";
    }

    if ( theStartPosition < 0 )
    {
      System.err.println ( "[DataSourceFile:readSubString]  Warning:  "
			   + "theStartPosition(" + theStartPosition 
			   + ") < 0" );
      theStartPosition = 0;
    }

    if ( theEndPosition < 0 )
    {
      System.err.println ( "[DataSourceFile:readSubString]  Warning:  "
			   + "theEndPosition(" + theEndPosition 
			   + ") < 0" );
      theEndPosition = 0;
    }

    if ( theStartPosition >= theEndPosition )
    {
      System.err.println ( "[DataSourceFile:readSubString]  Warning:  "
			   + "theStartPosition(" + theStartPosition 
			   + ") >= theEndPosition(" + theEndPosition + ")" );
      return "";
    }

    byte[]   data = new byte [ theEndPosition - theStartPosition ];

    try
    {
      getFile() . seek ( theStartPosition );
      getFile() . read ( data, 0, theEndPosition - theStartPosition );
      return new String ( data );
    }
    catch ( IOException  theException )
    {
      System.err.println ( "[DataSourceFile:readSubString]  Error:  "
			   + "Unable to read data.  Failing..." );
      return "";
    }
  }

}
