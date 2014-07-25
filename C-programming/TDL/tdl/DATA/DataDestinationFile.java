/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

import java.io.*;

public class DataDestinationFile extends DataDestinationAbstractBaseClass
{
	/* Instance Variables */
  protected OutputStream   outputStream;


	/* Instance Methods */
  public DataDestinationFile ( )
  {
    outputStream = null;
  }

  public DataDestinationFile ( String theFilename )
  {
    this();
    setFile ( theFilename );
  }

  public OutputStream getFile ()  { return outputStream; }

  public boolean      setFile ( String theFilename )
  {
    try
    {
      setFileWithoutExceptionHandling ( theFilename );
    }
    catch ( IOException  theException )
    {
      System.err.println ( "[DataDestinationFile]  Error:  "
			   + "Unable to open file \"" + theFilename
			   + "\".  IOException = " + theException );
      outputStream = null;
    }
    catch ( SecurityException  theException )
    {
      System.err.println ( "[DataDestinationFile]  Error:  "
			   + "Unable to open file \"" + theFilename
			   + "\".  SecurityException = " + theException );
      outputStream = null;
    }

    return hasFile();
  }

  public void setFileWithoutExceptionHandling ( String theFilename )
    throws IOException
  {
    super . reset();
    outputStream = null;
    outputStream = new FileOutputStream ( theFilename );    
  }


  public boolean hasFile()  { return outputStream != null; }    


  public void writeWithoutIndenting ( String theString )
  {
	/* Check for idiocy. */
    if ( hasFile() == false )
    {
      System.err.println ( "[DataDestinationFile:writeWithoutIndenting]  "
			   + "Error:  File is not initialized." );
      return;
    }

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
             "[DataDestinationFile:writeWithoutIndenting]  Error:  "
	   + "Attempted to write to a close'd DataDestinationStringBuffer "
	   + "dectected and aborted" );
      return;
    }

	/* General case */
    try
    {
	/* Write to the file */
      getFile() . write ( theString.getBytes() );

	/* Keep track of the bookkeeping... */
      super . writeWithoutIndenting ( theString );
    }
    catch ( IOException  theException )
    {
      System.err.println ( "[DataDestinationFile:writeWithoutIndenting]  "
			   + "Error:  Unable to write to file.  IOException = "
			   + theException );
      return;
    }
  }

  public void close ( )
  {
    try
    {
      closeWithoutExceptionHandling();
    }
    catch ( IOException  theException )
    {
      System.err.println ( "[DataDestinationFile:close]  "
			   + "Error:  Unable to close file.  IOException = "
			   + theException );
    }

  }

  public void closeWithoutExceptionHandling ( ) throws IOException
  {
    getFile() . close();

	/* Keep track of the bookkeeping... */
    super.close();
  }

}
