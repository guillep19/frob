/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

import java.io.IOException;


public abstract class DataSource extends Object implements CharStream
{
  public final static int START_POSITION   = 0;
  public final static int UNKNOWN_POSITION = -1;


  String filenameWithoutPathPlusColonDefault;

  int   totalReadSoFar,       column,       row,
        totalReadSoFarAtMark, columnAtMark, rowAtMark;
        


  public DataSource ( )
  {
    setFilenameWithoutPathDefault ( DataComponent.DEFAULT_DATA_SOURCE_NAME );
    reset();
  }

  public DataSource ( String theFilename )
  {
    if ( DataComponent.isEmptyString ( theFilename ) )
      setFilenameWithoutPathDefault ( DataComponent.DEFAULT_DATA_SOURCE_NAME );
    else
      setFilenameWithoutPathDefault ( theFilename );
    reset();
  }

  public void reset ( )
  {
    totalReadSoFar = 0;
    column         = 0;
    row            = 0;
    totalReadSoFarAtMark = 0;
    columnAtMark         = 0;
    rowAtMark            = 0;
  }

	/* These *MUST* be implemented in the derived class */
  abstract public char read ( int thePosition ) throws java.io.IOException;

	/* Note: Reads substring:  StartPosition <= position < EndPosition */
  abstract public String readSubString ( int theStartPosition,
					 int theEndPosition   );

	/* This can/should be overriden with a more efficient approach */
	/* whenever possible... */
  public char[] readCharArray ( int theStartPosition, int theEndPosition )
  {
    return readSubString ( theStartPosition, theEndPosition ) . toCharArray();
  }

	/* This is invoked when we are done reading...  It may be overriden. */
  public void Done()
  {
  }

  public void setFilenameWithoutPathDefault( String theFilenameWithoutPath )
  {
    filenameWithoutPathPlusColonDefault = theFilenameWithoutPath + ":";
  }

  public String getFilenameWithoutPathPlusColonDefault()
  {
    return filenameWithoutPathPlusColonDefault;
  }
	/* And this is the version that *should* be invoked! */
  public String getFilenameWithoutPathPlusColon()
  {
    if ( DataFile.getOverrideInMessagesFilenamesWithLine() == true )
      return DataComponent.DEFAULT_MESSAGE_FILENAME_LEAD;

    return getFilenameWithoutPathPlusColonDefault();
  }



  public int getColumn()      { return column;       }
  public int getRow()         { return row;          }
  public int getLine()        { return row;          }
  public int getEndColumn()   { return getColumn();  }
  public int getEndRow()      { return getRow();     }
  public int getEndLine()     { return getRow();     }
  public int getBeginColumn() { return columnAtMark; }
  public int getBeginRow()    { return rowAtMark;    }
  public int getBeginLine()   { return rowAtMark;    }


  private void setColumn ( int theColumn )  { column = theColumn; }
  private void setRow    ( int theRow    )  { row    = theRow;    }

  private void incrementColumn ( ) { column++; }
  private void decrementColumn ( ) { column--; }
  private void incrementRow    ( ) { row++; }
  private void decrementRow    ( ) { row--; }

  public  int  getTotalReadSoFar ( )    { return totalReadSoFar; }
  private void setTotalReadSoFar ( int theTotalReadSoFar )
					{ totalReadSoFar = theTotalReadSoFar; }
  private void incrementTotalReadSoFar ( ) { totalReadSoFar++; }
  private void decrementTotalReadSoFar ( ) { totalReadSoFar--; }

  public  int  getTotalReadSoFarAtMark ( ) { return totalReadSoFarAtMark; }
  public  int  getColumnAtMark         ( ) { return columnAtMark;         }
  public  int  getRowAtMark            ( ) { return rowAtMark;            }

  private void setMark ( )
  {
    totalReadSoFarAtMark = totalReadSoFar;
    columnAtMark         = getColumn();
    rowAtMark            = getRow();
  }

  private void restoreToMark ( )
  {
    setTotalReadSoFar ( getTotalReadSoFarAtMark() );
    setColumn         ( getColumnAtMark() );
    setRow            ( getRowAtMark() );
  }


  public  int  getTotalReadFromMark ( )
		    { return getTotalReadSoFar() - getTotalReadSoFarAtMark(); }



  public char readChar() throws java.io.IOException
  {
    char  aChar = read ( getTotalReadSoFar() );

    if ( aChar == '\n' )
    {
      incrementRow();
      setColumn ( 0 );
    }
    else
    {
      incrementColumn();
    }

    incrementTotalReadSoFar();

    return aChar;
  }


  public void backup ( int theAmount )
  {
    int  moveForward, moveBackward;

	/* Error case. */
    if ( theAmount <= 0 )
    {
      System.err.println ( "[DataSource:backup]  Warning:  "
			   + "theAmount(" + theAmount + ") <= 0" );
      return;
    }

	/* Trivial Case */
    if ( theAmount <= getTotalReadFromMark() )
    {
      moveForward = getTotalReadFromMark() - theAmount;
      restoreToMark();
      for ( ; moveForward > 0;  moveForward-- )
      {
	try {
	  readChar();
	} catch ( java.io.IOException theException ) {
	  System.err.println ( "[DataSource:backup]  Warning:  "
			       + "move-forward readChar() failed. " );
	}
      }
      return;
    }

	/* We are not supposed to hit the non-trivial case... */
    System.err.println ( "[DataSource:backup]  Warning:  "
			 + "Non-trivial backup case invoked.  "
			 + "(Should this be happening?)" );

	/* Non-trivial case */
    for ( moveBackward = ( getTotalReadSoFar() - 1 );
	  (   ( moveBackward >= ( getTotalReadSoFar() - theAmount ) )
	   && ( moveBackward >= 0                                   ) );
	  moveBackward -- )
    {
	try {
	  if ( read ( moveBackward ) == '\n' )
	    decrementRow();
	} catch ( java.io.IOException theException ) {
	  System.err.println ( "[DataSource:backup]  Warning:  "
			       + "look-backward [row] read() failed. " );
	}
    }

    setColumn ( 0 );

    setTotalReadSoFar ( getTotalReadSoFar() - theAmount );
    if ( getTotalReadSoFar() < 0 )
    {
      setTotalReadSoFar ( 0 );
      if ( getRow() != 0 )
      {
	System.err.println ( "[DataSource:backup]  Warning:  "
			     + "Internal consistency failure.  Row("
			     + getRow() + ") != 0" );
	setRow ( 0 );
      }
    }
    else
    {
      for ( moveBackward = ( getTotalReadSoFar() - 1 );
	    moveBackward >= 0;
	    moveBackward -- )
      {
	try
	{
	  if ( read ( moveBackward ) == '\n' )
	    break;
	  else
	    incrementColumn();
	}
	catch ( java.io.IOException theException )
	{
	  System.err.println ( "[DataSource:backup]  Warning:  "
			       + "look-backward [col] read() failed. " );
	} /* try ... catch */

      } /* for ( all previous characters until we hit a newline ) */

    } /* if we went past the beginning of this DataSource  ...  else */

  }


  public char BeginToken() throws java.io.IOException
  {
    setMark();
    return readChar();
  }

  public String getImage()  { return GetImage(); }
  public String GetImage()
  {
    return readSubString ( getTotalReadSoFarAtMark(), getTotalReadSoFar() );
  }


  public char[] GetSuffix ( int theLength )
  {
	/* Error case */
    if ( theLength <= 0 )
      return new char [ 0 ];

	/* Problems? */
    if ( theLength > getTotalReadSoFar() )
    {
      System.err.println ( "[DataSource:GetSuffix]  Warning:  "
			   + "theLength(" + theLength + ")  >  totalReadSoFar("
			   + getTotalReadSoFar() + ").  Truncating..." );
      theLength = getTotalReadSoFar();
    }

	/* Possible problems? */
    if ( theLength > getTotalReadFromMark() )
    {
      System.err.println ( "[DataSource:GetSuffix]  Warning:  "
			   + "theLength(" + theLength + ")  >  tokenLength("
			   + getTotalReadFromMark() + ")." );
    }

    return readCharArray ( getTotalReadSoFar() - theLength,
			   getTotalReadSoFar() );
  }


	/* Convert line/col to file position... */
  public int getPositionFromLineAndColumn ( int theLine, int theColumn )
  {
    int  position = DataSource.START_POSITION;

    try
    {
      while ( (theLine > 0)  ||  (theColumn > 0) )
      {
	if (   ( read ( position++ ) == '\n' )
	    && ( theLine > 0 ) )
	  theLine--;
	else if ( theLine <= 0 )
	  theColumn--;
      }
      return position;
    }
    catch ( java.io.IOException  theException )
    {
      System.err.println ( "[DataSource:getPositionFromLineAndColumn]  "
			   + "Warning:  read(" + position + ") Failed.  "
			   + "Exception: " + theException );
      return DataSource.UNKNOWN_POSITION;
    }
  }

}
