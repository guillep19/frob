
    /* This is the interface used by Data* classes that "generate" output
     *
     * Copyright (c) 2008, Carnegie Mellon University
     *     This software is distributed under the terms of the 
     *     Simplified BSD License (see tdl/LICENSE.TXT)
     *
     */

public interface DataDestination
{
	/* Class Constants */
  public static final int NORMAL_TYPE   = 0;
  public static final int KEYWORD_TYPE  = 1;
  public static final int COMMENT_TYPE  = 2;
  public static final int FUNCTION_TYPE = 3;
  public static final int MACRO_TYPE    = 4;
  public static final int UNKNOWN_TYPE  = 5;

  public static final int MIN_TYPE = NORMAL_TYPE;
  public static final int MAX_TYPE = UNKNOWN_TYPE;


  public boolean          getHasClosed ( );
  public void             close ( );

  public int              getType ( );
  public boolean          setType ( int theType );

  public int              getIndent             ( );
  public void             addIndent             ( int theDeltaIndent );
  public void             removeIndent          ( int theDeltaIndent );
  public int              indentToCurrentColumn ( );

  public int              getSecondaryIndent          ( );
  public void             addSecondaryIndent          ( int theDeltaIndent );
  public void             removeSecondaryIndent       ( int theDeltaIndent );
  public boolean          getIsSecondaryIndentEnabled ( );
  public void             enableSecondaryIndent       ( );
  public void             disableSecondaryIndent      ( );

    /* These only affect the current line.  Subsequent lines are unaffected */
  public void             setStripLeadingWhitespace ( );
  public void             setStripLeadingWhitespace (
				     boolean theShouldStripLeadingWhitespace );
  public boolean          getIsStrippingLeadingWhitespace ( );

    /* These only affect the current line.  Subsequent lines are unaffected */
  public void             setStripLeadingSpaces ( );
  public void             setStripLeadingSpaces (
					 boolean theShouldStripLeadingSpaces );
  public boolean          getIsStrippingLeadingSpaces ( );

    /* These affect the current line and all subsequent lines until canceled.*/
  public void             setPersistentlyStripLeadingSpaces ( );
  public void             setPersistentlyStripLeadingSpaces (
			     boolean theShouldPersistentlyStripLeadingSpaces );
  public boolean          getIsPersistentlyStrippingLeadingSpaces ( );


  public String           getNewlineText   ( );
  public void             setNewlineText   ( String theNewlineText );
  public void             clearNewlineText ( );

  public int              getLength ( );
  public int              getRow    ( );
  public int              getColumn ( );

  public void             spaceToColumn ( int theColumn );

  public void             setEnableLineMacros ( boolean theEnableLineMacros );
  public boolean          getEnableLineMacros();
  public void             setUsingTdlFileName(boolean theUsingTdlFileName );
  public boolean          getUsingTdlFileName();
  public void             setTdlFileName( String thePrimaryFileName );
  public String           getTdlFileName();
  public void             setCxxFileName( String theOutputFileName );
  public String           getCxxFileName();
  public void             makeNextLineNumber ( int theNextLineNumber );
  public void             makeNextLineNumber ( DataComponent theDataComponent);
  public int              getMakeNextLineNumber();
  public void             clearMakeNextLineNumber();
  public boolean          hasMakeNextLineNumber();
  public int              getAdjustedCurrentRow();
  public void             copyLineMacroSettingsFrom (
					  DataDestination theDataDestination );

  public boolean          getIsNestingComments();
  public void             setIsNestingComments( boolean theIsNestingComments );

  public boolean          getIsConcatenatingStrings();
  public void             setIsConcatenatingStrings(
					   boolean theIsConcatenatingStrings );

  public DataDestination  write                 ( String theString );
  public void             writeWithoutIndenting ( String theString );
}

