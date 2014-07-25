/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

public class DataDestinationAbstractBaseClass  extends    Object
					       implements DataDestination
{
	/* Class constants */
  public final static int NO_MAKE_NEXT_LINE_NUMBER = -101;  /*Arbitrary Value*/
  public final static int RESET_TO_CURRENT_ROW     = -1001; /*Arbitrary Value*/

	/* Instance Variables */
  protected boolean  hasClosed, needsToIndent, secondaryIndentEnabled,
		     isStrippingLeadingWhitespace, isStrippingLeadingSpaces,
		     isPersistentlyStrippingLeadingSpaces;
  protected int      indent, type, length, row, column,
		     secondaryIndent;
  protected String   newlineText;
  protected boolean  enableLineMacros, usingTdlFileName;
  protected String   tdlFileName, cxxFileName;
  protected int      rowOffset, makeNextLineNumber;
  protected boolean  isNestingComments,
                     isConcatenatingStrings, hasStartingQuote, hasEndingQuote;

  public DataDestinationAbstractBaseClass ( )
  {
    reset();
  }

  public boolean getNeedsToIndent()  { return needsToIndent; }
  public void    setNeedsToIndent ( boolean  theNeedsToIndent ) 
					{ needsToIndent = theNeedsToIndent; }

  public void reset ( )
  {
    hasClosed                            = false;
    needsToIndent                        = false;
    isStrippingLeadingWhitespace         = false;
    isStrippingLeadingSpaces             = false;
    isPersistentlyStrippingLeadingSpaces = false;
    secondaryIndentEnabled               = false;

    indent          = 0;
    type            = DataDestination.NORMAL_TYPE;
    length          = 0;
    row             = 0;
    column          = 0;
    secondaryIndent = 0;

    newlineText = "";

    enableLineMacros = false;
    usingTdlFileName = true;
    tdlFileName      = null;
    cxxFileName      = null;
    rowOffset        = 0;

    clearMakeNextLineNumber();

    isNestingComments      = false;
    isConcatenatingStrings = false;
    hasStartingQuote       = false;
    hasEndingQuote         = false;
  }


  public boolean getHasClosed ( )   { return hasClosed; }

  public void    close   ( )  { hasClosed = true; }


  public int     getType ( )  { return type; }

  public boolean setType ( int theType ) 
  {
    if (   ( theType >= DataDestination.MIN_TYPE )
	&& ( theType <= DataDestination.MAX_TYPE ) )
    {
      type = theType;
      return true;
    }
    else
    {
      return false;
    }
  }


  public int  getIndent ( ) { return indent; }

  public void addIndent ( int theDeltaIndent )
  {
    indent += theDeltaIndent;
    if ( indent < 0 )
      indent = 0;
  }

  public void removeIndent ( int theDeltaIndent )
  {
    addIndent ( - theDeltaIndent );
  }

  public int indentToCurrentColumn ( )
  {
    int newIndent = getColumn() - getIndent();

	/* In very rare circumstances, we may have established an indent,
	 * but not yet written anything.  Subsequent indentToCurrentColumn
	 * *MUST* not be negative, thereby moving the indentation backwards.
	 * Rather, it should keep the status-quo.
	 *
	 * Somewhat more commonplace was the previous case,
	 * of getColumn() being zero, and getIndent() > 0.
	 * (E.g. Having not yet applied the indent.)
	 * This case, too, is dealt with herein.
	 */
    if ( newIndent < 0 )
      newIndent = 0;

    addIndent ( newIndent );
    return newIndent;
  }



  public int getSecondaryIndent     ( )  { return secondaryIndent; }

  public void addSecondaryIndent     ( int theDeltaIndent )
  {
    secondaryIndent += theDeltaIndent;
    if ( secondaryIndent < 0 )
      secondaryIndent = 0;
  }

  public void removeSecondaryIndent  ( int theDeltaIndent )
  {
    addSecondaryIndent ( - theDeltaIndent );
  }

  public boolean getIsSecondaryIndentEnabled ( )
					     { return secondaryIndentEnabled; }

  public void enableSecondaryIndent  ( )
  {
    if ( getIsSecondaryIndentEnabled() == false )
    {
      secondaryIndentEnabled = true;
      addIndent ( getSecondaryIndent() );
    }
    else
    {
      System.err.println (
	  "[DataDestinationAbstractBaseClass:enableSecondaryIndent]  "
	  + "Warning:  SecondaryIndent Already ENABLED." );
    }
  }

  public void disableSecondaryIndent ( )
  {
    if ( getIsSecondaryIndentEnabled() )
    {
      secondaryIndentEnabled = false;
      removeIndent ( getSecondaryIndent() );
    }
    else
    {
      System.err.println (
	  "[DataDestinationAbstractBaseClass:disableSecondaryIndent]  "
	  + "Warning:  SecondaryIndent Already DISABLED." );
    }
  }


  public void    setStripLeadingWhitespace ( )
                       { setStripLeadingWhitespace ( true ); }

  public void    setStripLeadingWhitespace (
				      boolean theShouldStripLeadingWhitespace )
	    { isStrippingLeadingWhitespace = theShouldStripLeadingWhitespace; }

  public boolean getIsStrippingLeadingWhitespace ( )
				      { return isStrippingLeadingWhitespace; }


  public void    setStripLeadingSpaces ( )
                       { setStripLeadingSpaces ( true ); }

  public void    setStripLeadingSpaces ( boolean theShouldStripLeadingSpaces )
	    { isStrippingLeadingSpaces = theShouldStripLeadingSpaces; }

  public boolean getIsStrippingLeadingSpaces ( )
				      { return isStrippingLeadingSpaces; }


  public void    setPersistentlyStripLeadingSpaces ( )
                       { setPersistentlyStripLeadingSpaces ( true ); }

  public void    setPersistentlyStripLeadingSpaces (
			      boolean theShouldPersistentlyStripLeadingSpaces )
  {
    isPersistentlyStrippingLeadingSpaces
      = theShouldPersistentlyStripLeadingSpaces;
    setStripLeadingSpaces ( theShouldPersistentlyStripLeadingSpaces );
  }

  public boolean getIsPersistentlyStrippingLeadingSpaces ( )
			       { return isPersistentlyStrippingLeadingSpaces; }


  public String getNewlineText ( )    { return newlineText; }

  public void   setNewlineText ( String theNewlineText )
				      { newlineText = theNewlineText; }

  public void   clearNewlineText ( )  { setNewlineText ( "" ); }



  public int getLength ( )
  {
    return length;
  }

  public int getRow    ( )
  {
    return row;
  }

  public int getColumn ( )
  {
    return column;
  }


  public void spaceToColumn ( int theColumn )
  {
    if ( getIsStrippingLeadingWhitespace() )
      setStripLeadingWhitespace ( false );

    while ( getColumn() < theColumn )
      write ( " " );
  }



  public void setEnableLineMacros ( boolean theEnableLineMacros )
  {
    enableLineMacros = theEnableLineMacros;
  }

  public boolean getEnableLineMacros()
  {
    return enableLineMacros;
  }

  public void setUsingTdlFileName ( boolean theUsingTdlFilename )
  {
	/* Automatically reset us back to our current line number
	 * if we are now using the C++ (non-TDL) filename.   (Unless
	 * getEnableLineMacros() == false.  see copyLineMacroSettingsFrom() ).
	 */
    if (   ( getEnableLineMacros() == true  )
	&& ( usingTdlFileName      == true  )
	&& ( theUsingTdlFilename   == false ) )
    {
	/* Delay this computation until later to calculate it correctly. */
      makeNextLineNumber (
		       DataDestinationAbstractBaseClass.RESET_TO_CURRENT_ROW );
    }

    usingTdlFileName = theUsingTdlFilename;
  }

  public boolean getUsingTdlFileName()
  {
    return usingTdlFileName;
  }

  public void setTdlFileName( String theTdlFileName )
  {
    tdlFileName = theTdlFileName;
  }

  public String getTdlFileName()
  {
    return tdlFileName;
  }

  public void setCxxFileName( String theCxxFileName )
  {
    cxxFileName = theCxxFileName;
  }

  public String getCxxFileName()
  {
    return cxxFileName;
  }

  public String getCurrentFileName()
  {
    if ( getUsingTdlFileName() == true )
      return getTdlFileName();
    else
      return getCxxFileName();
  }

  protected void setRowOffset( int theRowOffset )
  {
    rowOffset = theRowOffset;
  }

  protected int  getRowOffset()
  {
    return rowOffset;
  }

	/* Note:  Causes #line to be written after next newline.     * 
         *        Write("\n") occurs separately to facilitate        *
	 *        keeping track of newlines in text-generation code. */
  public void makeNextLineNumber ( int theMakeNextLineNumber )
  {
    makeNextLineNumber = theMakeNextLineNumber;
  }

	/* Simplify things, and try to compensate for common problems... */
  public void makeNextLineNumber ( DataComponent theDataComponent )
  {
    makeNextLineNumber(   theDataComponent . getLineNumber()
		        - theDataComponent . countLeadingSubcomponentNewlines()
		       );
  }

  public int getMakeNextLineNumber()
  {
    return makeNextLineNumber;
  }

  public void clearMakeNextLineNumber()
  {
    makeNextLineNumber (
		   DataDestinationAbstractBaseClass.NO_MAKE_NEXT_LINE_NUMBER );
  }

  public boolean hasMakeNextLineNumber()
  {
    return getMakeNextLineNumber()
       !=  DataDestinationAbstractBaseClass.NO_MAKE_NEXT_LINE_NUMBER;
  }



  public void copyLineMacroSettingsFrom ( DataDestination theDataDestination )
  {
    setEnableLineMacros ( false );
    setUsingTdlFileName ( theDataDestination . getUsingTdlFileName() );
    setEnableLineMacros ( theDataDestination . getEnableLineMacros() );
    setTdlFileName      ( theDataDestination . getTdlFileName()      );
    setCxxFileName      ( theDataDestination . getCxxFileName()      );
    row                 = theDataDestination . getRow();
  }


  public boolean getIsNestingComments()
  {
    return isNestingComments;
  }

  public void setIsNestingComments( boolean theIsNestingComments )
  {
    isNestingComments = theIsNestingComments;
  }

  public boolean getIsConcatenatingStrings()
  {
    return isConcatenatingStrings;
  }

  public void setIsConcatenatingStrings( boolean theIsConcatenatingStrings )
  {
    isConcatenatingStrings = theIsConcatenatingStrings;

	/* Fencepost case:  Flush any pending quote */
    if ( getHasEndingQuote() )
    {
      write ( "\"" );
    }

    clearHasStartingQuote();
    clearHasEndingQuote();
  }

	/* Finite state machine flags.
	 * In a nutshell, we are seeking a ending-quote followed by a
	 * starting-quote.  A starting-quote alone won't do it.
	 */
  protected boolean getHasStartingQuote() { return hasStartingQuote;  }
  protected void    setHasStartingQuote() { hasStartingQuote = true;  }
  protected void  clearHasStartingQuote() { hasStartingQuote = false; }

  protected boolean getHasEndingQuote() { return hasEndingQuote;  }
  protected void    setHasEndingQuote() { hasEndingQuote = true;  }
  protected void  clearHasEndingQuote() { hasEndingQuote = false; }



  public int getAdjustedCurrentRow()
  {
    return getRow() - getRowOffset();
  }

  public void writeNewline()
  {
	/* Deal with this here to calculate the next line correctly. */
    if (    getMakeNextLineNumber()
	 == DataDestinationAbstractBaseClass.RESET_TO_CURRENT_ROW )
    {
      makeNextLineNumber (   getRow()
			   + 1 /* Start counting from 1 not zero   */
			   + 1 /* We want the next row after #line */
			       /* Deal with leading newline: */
			   + ( ( getColumn() != 0 ) ? 1 : 0 )
			  );
    }

	/* If necessary, do the #line thing. */
    if (   ( getEnableLineMacros()   == true )
	&& ( hasMakeNextLineNumber() == true )
	&& ( getAdjustedCurrentRow() != ( getMakeNextLineNumber() - 1 ) )
	)
    {
      if ( getCurrentFileName() == null )
      {
	System.err.println ( "[DataDestinationAbstractBaseClass:writeNewline] "
			     + " Error:  Current Filename is unknown." );
	writeWithoutIndenting ( "\n" );
      }
      else
      {      
	if ( getColumn() != 0 )
	  writeWithoutIndenting ( "\n" );

	writeWithoutIndenting ( "#line " + getMakeNextLineNumber()
				+ " \"" + getCurrentFileName() + "\"\n" );

	setRowOffset ( getRow() - getMakeNextLineNumber() );
      }
    }
    else
    {
      writeWithoutIndenting ( "\n" );
    }

  	/* Stop doing the #line thing */
    clearMakeNextLineNumber();

	/* Deal with persistently stripping leading spaces */
    if ( getIsPersistentlyStrippingLeadingSpaces() )
      setStripLeadingSpaces ( true );
  }


  public DataDestination write ( String theString )
  {
    int      i, stringStart=0, stringEnd=0, stringComment, stringQuote;

	/* Remove any leading whitespace... */
    if (   ( getIsStrippingLeadingWhitespace() == true )
	|| ( getIsStrippingLeadingSpaces()     == true ) )
    {
	/* Find the first non-whitespace character */
      for ( i=0
	      ;
	    (   ( i < theString.length() )

	     && (   (   ( getIsStrippingLeadingWhitespace() == true )
		     && (   ( theString.charAt ( i ) == ' '  )
			 || ( theString.charAt ( i ) == '\t' )
			 || ( theString.charAt ( i ) == '\n' )
			 || ( theString.charAt ( i ) == '\r' )
			 || ( theString.charAt ( i ) == '\f' )      ) )

		 || (   ( getIsStrippingLeadingSpaces()     == true )
		     && (   ( theString.charAt ( i ) == ' '  )      ) )
		 )
	     )
	      ;
	    i++ )
      {
	 ; /* NULL (EMPTY) FOR LOOP BODY */
      }

	/* Is this entire string whitespace? */
      if ( i >= theString.length() )
      {
	  /* Keep any bookkeeping happy... */
	writeWithoutIndenting ( "" );
	return this;
      }

	/* Did we strip part of this string? */
      if ( i > 0 )
      {
	    /* This is equivalent to:  theString = theString.substring( i ); */
	stringStart = i;
	stringEnd   = i;
      }

	/* And stop stripping (leading) whitespace/spaces.. */
      setStripLeadingWhitespace ( false );
      setStripLeadingSpaces     ( false );
    }



	/* Write out the string... */
    while ( stringEnd < theString . length() )
    {
	/* If we have a delayed indentation... */
	/* (Note: Skip indentation if first character is a newline.) */
      if (   ( getNeedsToIndent()                         )
	  && ( theString . charAt ( stringStart ) != '\n' ) )
      {
	    /* Write the indent */
	for ( i=0;  i < getIndent();  i++ )
	  writeWithoutIndenting ( " " );

	    /* Stop indenting... */
	setNeedsToIndent ( false );
      }

      stringEnd = theString . indexOf ( "\n", stringStart );

	/* Perturb the string to deal with comments inside comments. */
      if ( getIsNestingComments() )
      {
	stringComment = theString . indexOf ( "/*", stringStart );
	if (   (      stringComment != -1          )
	    && (    ( stringEnd     == -1        )
		 || ( stringComment <  stringEnd ) ) )
	  stringEnd = stringComment;

	stringComment = theString . indexOf ( "*/", stringStart );
	if (   (      stringComment != -1          )
	    && (    ( stringEnd     == -1        )
		 || ( stringComment <  stringEnd ) ) )
	  stringEnd = stringComment;
      } /* if ( getIsNestingComments() ) */


	/* Perturb the string to deal with removing ""  *
	 * Caveat:  Don't remove start-quote end-quote, *
	 *          remove end-quote start-quote.       */
      if ( getIsConcatenatingStrings() )
      {
	if ( getHasEndingQuote() )
	{
	  if ( theString . charAt ( stringStart ) == '\"' )
	  {
	    clearHasEndingQuote();
	    stringStart++;
	  }
	  else
	  {
	    clearHasStartingQuote();
	    clearHasEndingQuote();
	    writeWithoutIndenting ( "\"" );
	  }
	} /* if ( getHasEndingQuote() ) */

	stringQuote = theString . indexOf ( "\"", stringStart );
	if (   (      stringQuote != -1          )
	    && (    ( stringEnd   == -1        )
		 || ( stringQuote <  stringEnd ) ) )
	  stringEnd = stringQuote;

      } /* if ( getIsConcatenatingStrings() ) */



      if ( stringEnd == -1 )
	stringEnd = theString . length();

      writeWithoutIndenting( theString . substring( stringStart, stringEnd ) );


	/* Perturb the string to deal with comments inside comments. */
      if ( getIsNestingComments() )
      {
	if (   ( ( stringEnd + 1 )  < theString . length()   )
	    && ( theString . charAt ( stringEnd     ) == '/' )
	    && ( theString . charAt ( stringEnd + 1 ) == '*' ) )
	{
	  writeWithoutIndenting ( "/_*" );
	  stringEnd += 2;
	}

	if (    ( ( stringEnd + 1 )  < theString . length()   )
	     && ( theString . charAt ( stringEnd     ) == '*' )
	     && ( theString . charAt ( stringEnd + 1 ) == '/' ) )
	{
	  writeWithoutIndenting ( "*_/" );
	  stringEnd += 2;
	}
      } /* if ( getIsNestingComments() ) */


	/* Perturb the string to deal with removing ""  *
	 * Caveat:  Don't remove start-quote end-quote, *
	 *          remove end-quote start-quote.       */
      if ( getIsConcatenatingStrings() )
      {
	if (   ( stringEnd < theString . length()        )
	    && ( theString . charAt ( stringEnd ) == '"' ) )
	{
	  stringEnd ++;

	  if ( getHasStartingQuote() == false )
	  {
	    setHasStartingQuote();
	    clearHasEndingQuote();
	    writeWithoutIndenting ( "\"" );
	  }

	  else if ( getHasEndingQuote() == false )
	  {
	    setHasEndingQuote();
	  }

	  else /* Both true */
	  {
		/* Something is seriously wrong. */
	    System.err.println ( "[DataDestinationAbstractBaseClass:write]  "
				 + "Internal Error:  "
				 + "Bad state for Concatenating-Strings." );
	    setHasStartingQuote();
	    clearHasEndingQuote();
	  }
	}
      } /* if ( getIsConcatenatingStrings() ) */



      if (   ( stringEnd < theString . length()         )
	  && ( theString . charAt ( stringEnd ) == '\n' ) )
      {
	    /* write the newline "\n" character */
	writeNewline();

	   /* The newline character at stringEnd has been dealt with... */
	stringEnd ++;

	    /* write any new-line text that we are producing... */
	if ( getNewlineText() . length() > 0 )
	{
	    /* Write the indent */
	  for ( i=0;  i < getIndent();  i++ )
	    writeWithoutIndenting ( " " );

	  writeWithoutIndenting ( getNewlineText() );

	    /* Do NOT delay-indent if we have leading text... */
	  setNeedsToIndent ( false );
	}
	else
	{
	   /* And we need to indent... */
	  setNeedsToIndent ( true );
	}

	    /* Skip over any new leading spaces, if necessary */
	if ( getIsStrippingLeadingSpaces() == true )
	{
	  for ( ;
		(   ( stringEnd < theString.length() )
		 && ( theString . charAt ( stringEnd ) == ' ' ) )
		  ;
		stringEnd ++ )
	  {
	    ; /* NULL (EMPTY) FOR LOOP BODY */
	  } /* FOR ( ; stringEnd < theString.length; stringEnd ++ ) */

		/* If we found a non-space character... */
	  if ( stringEnd < theString.length() )
	    setStripLeadingSpaces ( false );
	} /* IF ( getIsStrippingLeadingSpaces() == true ) */


      } /* if ( we needed to write a newline character ) */

      stringStart = stringEnd;

    } /* while ( stringEnd < theString . length() ) */

    return this;
  }


  public void writeWithoutIndenting ( String theString )
  {
    for ( int i = 0;  i < theString.length();  i++ )
    {
      if (   ( theString.charAt ( i ) == '\n' )
	  || ( theString.charAt ( i ) == '\f' )
	  || ( theString.charAt ( i ) == '\r' ) )
      {
	column = 0;
	row++; /* Not entirely accurate since \f will do a form feed   */
      }	       /* which will translate to many rows...  But for now... */
      else
	column ++;
    }
    length += theString.length();
  }
}
