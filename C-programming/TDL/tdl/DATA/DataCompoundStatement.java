
/*
 * A Compound Statement is basically "{" 0-or-more-statements "}".
 *
 * (But it forms the basis for multiple statements in C/C++/TDL programs.)
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 *
 */

public class DataCompoundStatement extends DataStatement
{
	/* Class Constants */
  public final static String   OPEN_BRACE     = "{";
  public final static String   CLOSE_BRACE    = "}";
  public final static String   NEW_LINE       = "\n";

	/* Instance Variables */
  protected int     lineNumberOfCloseBrace;


	/* Instance Methods */
  public DataCompoundStatement()
  {
    lineNumberOfCloseBrace = DataComponent.INVALID_LINE_NUMBER;
  }

  public DataCompoundStatement ( Object theComponent )
  {
    this();

	/* Add the open-brace */
    addSubcomponent ( DataCompoundStatement.NEW_LINE );
    setIndex ( DataCompoundStatement.OPEN_BRACE );

	/* Add the statement... */
    if ( theComponent != null )
    {
      if ( theComponent instanceof String )
	addSubcomponent ( (String) theComponent );

      else if ( theComponent instanceof DataComponent )
	addSubcomponent ( (DataComponent) theComponent );

      else
      {
	System.err.println ( "[DataCompoundStatement:DataCompoundStatement]  "
		     + "Warning:  theComponent has an UNACCEPTABLE class (\""
		     + theComponent . getClass() . getName() + "\")." );
      }
    }

	/* Add the close-brace */
    addSubcomponent ( DataCompoundStatement.NEW_LINE );

	/* Note our new close-brace location... */
    setIndex ( DataCompoundStatement.CLOSE_BRACE );
  }



  public void setLineNumberOfCloseBrace ( int theLineNumberOfCloseBrace )
		   { lineNumberOfCloseBrace = theLineNumberOfCloseBrace; }

  public int  getLineNumberOfCloseBrace() { return lineNumberOfCloseBrace; }

  public boolean hasValidLineNumberOfCloseBrace()
  {
    return DataComponent.isValidLineNumber ( getLineNumberOfCloseBrace() );
  }



  public void addStringAfterOpenBrace ( String theStringToAdd )
  {
    addSubcomponent ( theStringToAdd,
		      getIndex ( DataCompoundStatement.OPEN_BRACE ) );

    setIndex (            DataCompoundStatement.CLOSE_BRACE,
	       getIndex ( DataCompoundStatement.CLOSE_BRACE ) + 1 );
  }

  public void addSubcomponentAfterOpenBrace ( DataComponent theDataComponent )
  {
    addSubcomponent ( theDataComponent,
		      getIndex ( DataCompoundStatement.OPEN_BRACE ) );

    setIndex (            DataCompoundStatement.CLOSE_BRACE,
	       getIndex ( DataCompoundStatement.CLOSE_BRACE ) + 1 );
  }

  public void removeStringAfterOpenBrace ( )
  {
    setIndex (            DataCompoundStatement.CLOSE_BRACE,
	       getIndex ( DataCompoundStatement.CLOSE_BRACE ) - 1 );

    removeSubcomponent ( getIndex ( DataCompoundStatement.OPEN_BRACE ) );
  }

  public void removeSubcomponentAfterOpenBrace ( )
  {
    removeStringAfterOpenBrace();
  }

  public void addStringBeforeCloseBrace ( String theStringToAdd )
  {
    addSubcomponent ( theStringToAdd,
		      getIndex ( DataCompoundStatement.CLOSE_BRACE ) );

    setIndex (            DataCompoundStatement.CLOSE_BRACE,
	       getIndex ( DataCompoundStatement.CLOSE_BRACE ) + 1 );
  }

  public void addSubcomponentBeforeCloseBrace( DataComponent theDataComponent )
  {
    addSubcomponent ( theDataComponent,
		      getIndex ( DataCompoundStatement.CLOSE_BRACE ) );

    setIndex (            DataCompoundStatement.CLOSE_BRACE,
	       getIndex ( DataCompoundStatement.CLOSE_BRACE ) + 1 );
  }

  public void removeStringBeforeCloseBrace ( )
  {
    setIndex (            DataCompoundStatement.CLOSE_BRACE,
	       getIndex ( DataCompoundStatement.CLOSE_BRACE ) - 1 );

    removeSubcomponent ( getIndex ( DataCompoundStatement.CLOSE_BRACE ) );
  }

  public void removeSubcomponentBeforeCloseBrace ( )
  {
    removeStringBeforeCloseBrace();
  }


  public int getChildStatementCount()
  {
    int i, count=0;

    for ( i=0;  i < getSubcomponentsCount();  i++ )
    {
      if ( isSubcomponentADataStatement ( i ) )
      {
	count++;
      }
    }

    return super . getChildStatementCount()  +  count;
  }

  public DataStatement  getChildStatement ( int theIndex )
  {
    int i, count=0;

    for ( i=0;  i < getSubcomponentsCount();  i++ )
    {
      if ( isSubcomponentADataStatement ( i ) )
      {
	if ( theIndex == count )
	  return getDataStatementSubcomponent ( i );
	else
	  count++;
      }
    }

    return super . getChildStatement ( theIndex - count );
  }


  public boolean addPrimaryChild ( DataComponent theChildToAdd,
				   DataComponent theAddChildAfterThisComponent)
  {
    return addChild ( theChildToAdd, theAddChildAfterThisComponent );
  }

  public boolean addChild ( DataComponent theChildToAdd,
			    DataComponent theAddChildAfterThisComponent )
  {
    int i;

	/* Find where to add the child */
    if ( theAddChildAfterThisComponent == null )
    {
      i = getIndex ( DataCompoundStatement.OPEN_BRACE );
    }
    else
    {
      for ( i = getIndex ( DataCompoundStatement.OPEN_BRACE );
	    i < getSubcomponentsCount();
	    i++ )
      {
	if (   (    isSubcomponentADataComponent ( i ) )
	    && (    getDataComponentSubcomponent ( i )
	         == theAddChildAfterThisComponent      ) )
	{
	  break;
	}
      }

      i++; /* Add after this component */
    }

	/* Add the child */
    addSubcomponent ( theChildToAdd, i );

	/* Adjust child's parent */
    theChildToAdd . setParent ( this );

	/* Tweak the } index */
    setIndex ( DataCompoundStatement.CLOSE_BRACE,
	       getIndex ( DataCompoundStatement.CLOSE_BRACE ) + 1 );

    return true;
  }


  public boolean removeChild ( DataComponent theChildToRemove )
  {
    int   index = getIndexOfSubcomponent ( theChildToRemove );

    if (   ( index != DataComponent.INVALID_INDEX )
	&& ( removeSubcomponent ( index )         ) )
    {
	/* Adjust child's parent */
      theChildToRemove . setParent ( null );

	/* Tweak the } index */
      setIndex ( DataCompoundStatement.CLOSE_BRACE,
		 getIndex ( DataCompoundStatement.CLOSE_BRACE ) - 1 );

      return true;
    }
    else
    {
      System.err.println ( "[DataCompoundStatement:removeChild]  Warning:  "
			   + "Unable to remove child (\""
			   + theChildToRemove . getClass() . getName()
			   + "\")." );
      return false;
    }
  }


  public void runOnSubcomponentFraction (
			String                      theString,
			RunOnSubcomponentInterface  theRunOnSubcomponentObject,
			Object                      theArgumentObject )
  {
	/* Don't search children by default. */
    runOnSubcomponentFraction ( theString,         theRunOnSubcomponentObject,
				theArgumentObject, false );
  }


  public void generate ( DataDestination  theOutputDestination,
			 int              theObjectSubsetToGenerate )
  {
    String  subcomponent = null;


       	/* Print a warning if necessary... */
    warnIfInvalidObjectSubset ( theObjectSubsetToGenerate, "generate" );

    	/* Initialize us to generate non-significant tokens... */
    initializeGenerateSubcomponentIndex();

	/* Generate any labels that we have... */
    generateLabels ( theOutputDestination, theObjectSubsetToGenerate,
		     DataCompoundStatement.OPEN_BRACE );

	/* Write any pre-open-brace non-significant tokens */
    generateSubcomponents ( DataCompoundStatement.OPEN_BRACE,
			    theOutputDestination,
			    theObjectSubsetToGenerate, false );

	/* Write our open-brace */
    theOutputDestination . write ( DataCompoundStatement.OPEN_BRACE );

	/* Increase our indenting... */
    theOutputDestination . addIndent ( DataComponent.STANDARD_INDENT );

	/* Write all the tokens up to the next-to-close-brace token... */
    generateSubcomponents ( getIndex ( DataCompoundStatement.CLOSE_BRACE ) - 1,
			    theOutputDestination,
			    theObjectSubsetToGenerate, false );


	/* At this point, we are going to do some fancy footwork.
	 * This is specifically to deal with the case where the close-brace
	 * is preceeded by a newline followed by spaces.  This causes
	 * the close-brace to be improperly indented.  The solution is to
	 * un-indent prior to writing this string, IF it exists...
	 */

	/* Is there another token before the CLOSE_BRACE,
	 * and it is a string consisting of just whitespace??
	 */
    if (    getHasSubcomponentsToGenerateBefore (
					  DataCompoundStatement.CLOSE_BRACE )
	 && isSubcomponentAString (
			 getIndex ( DataCompoundStatement.CLOSE_BRACE ) - 1 ) )
    {
      subcomponent = getStringSubcomponent (
			 getIndex ( DataCompoundStatement.CLOSE_BRACE ) - 1 );

      for ( int i=0; i < subcomponent.length();  i++ )
      {
	if ( Character.isWhitespace ( subcomponent . charAt ( i ) ) == false )
	{
	  subcomponent = null;
	  break;
	}
      }
    }


	/* If there was a string of whitespace before the close-brace */
    if ( subcomponent != null )
    {
	/* Write the line number of the close-brace */
      generateRelevantLineNumberMacros ( theOutputDestination,
					 theObjectSubsetToGenerate,
					 getLineNumberOfCloseBrace(),
					 DataCompoundStatement.CLOSE_BRACE );
        /* Un-indent now */
      theOutputDestination . removeIndent ( DataComponent.STANDARD_INDENT );
    }

	/* Write any remaining next-to-close-brace token... */
    generateSubcomponents ( DataCompoundStatement.CLOSE_BRACE,
			    theOutputDestination,
			    theObjectSubsetToGenerate, false );


	/* If there was NOT a string of whitespace before the close-brace, */
    if ( subcomponent == null )
    {
	/* Write the line number of the close-brace */
      generateRelevantLineNumberMacros ( theOutputDestination,
					 getLineNumberOfCloseBrace(),
					 0 );
	/* We should still un-indent. */
      theOutputDestination . removeIndent ( DataComponent.STANDARD_INDENT );
    }


	/* Write our close-brace */
    theOutputDestination . write ( DataCompoundStatement.CLOSE_BRACE );

	/* Write any remaining non-significant tokens */
	/* (There *SHOULD* *NOT* be any...  But just in case...) */
    generateAllRemainingSubcomponents ( theOutputDestination,
					theObjectSubsetToGenerate, false );
  }

}
