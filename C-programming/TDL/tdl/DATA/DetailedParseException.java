/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

public class DetailedParseException extends Exception
{
  public static final int UNKNOWN_VALUE = -1;

  protected Throwable  originalException;


  public DetailedParseException ( ParseException theException )
  {
    originalException = theException;
	 /* Kludge: First line is #1 not #0. */
    theException . currentToken . next . beginLine ++;
  }

  public DetailedParseException ( TokenMgrError theException )
  {
    originalException = theException;
  }

  public DetailedParseException ( DetailedParseException theException )
  {
    originalException = theException;
  }

  public DetailedParseException ( String theException )
  {
    super ( theException );
    originalException = null;
  }

  public DetailedParseException ( Throwable theException )
  {
    originalException = theException;
  }


  public Throwable getOriginalException ()
  {
    return originalException;
  }


  public boolean getHasErrorLocation()
  {
    return (   ( getOriginalException() instanceof ParseException )
	    && ( ( ((ParseException) getOriginalException()) . currentToken )
		 != null ) );
  }

  public int getErrorLineStart()
  {
    if ( getHasErrorLocation() )
      return ( ((ParseException) getOriginalException())
	       . currentToken . next . beginLine );
    else
      return DetailedParseException.UNKNOWN_VALUE;
  }

  public int getErrorColumnStart()
  {
    if ( getHasErrorLocation() )
      return ( ((ParseException) getOriginalException())
	       . currentToken . next . beginColumn );
    else
      return DetailedParseException.UNKNOWN_VALUE;
  }

  public int getErrorLength()
  {
    if ( getHasErrorLocation() )
      return ( ((ParseException) getOriginalException())
	       . currentToken . next . image . length() );
    else
      return DetailedParseException.UNKNOWN_VALUE;
  }


  public String getMessage()
  {
    if ( getOriginalException() == null )
      return super . getMessage();
    else
      return getOriginalException() . getMessage();
  }

  public String getLocalizedMessage()
  {
    return getOriginalException() . getLocalizedMessage();
  }

  public String toString()
  {
	/* Do this transparently for throwing error messages. */
    if ( getOriginalException() == null )
      return getMessage();

    if ( getOriginalException() instanceof ParseException )
      return getOriginalException() . toString();

    if ( getOriginalException() instanceof TokenMgrError )
      return "Unknown Token Manager Error:\n\n"
	  + getOriginalException() . toString();

    if ( getOriginalException() instanceof DetailedParseException )
      return getOriginalException() . toString();

    if ( getOriginalException() instanceof Throwable )
      return getOriginalException() . toString();

    return "Unknown Error:\n\n" + getOriginalException() . toString();
  }

  public String toClippedString ()  { return toClippedString ( 30 ); }

  public String toClippedString ( int theMaxLines )
  {
    String  string = toString();
    int     lineCount, stringIndex = 0;

    for ( lineCount = 0;  lineCount < theMaxLines;  lineCount++ )
    {
      stringIndex = string . indexOf ( "\n", stringIndex );
      if ( stringIndex == -1 )
	return string;
      else
	stringIndex ++;
    }

    return string . substring ( 0, stringIndex )
      + ".........................";
  }

}

