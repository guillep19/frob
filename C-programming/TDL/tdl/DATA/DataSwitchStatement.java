
/*
 * This represents a C "switch" statement.
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 *
 */

public class DataSwitchStatement extends DataSelectionStatement
{
	/* Class variables */
  public final static String   SWITCH           = "switch";
  public final static String   OPEN_PAREN       = "(";
  public final static String   CLOSE_PAREN      = ")";
  public final static String   EXPRESSION_INDEX = "ExpressionIndex";
  public final static String   STATEMENT_INDEX  = "StatementIndex";
  public final static String   SEMICOLON        = ";";

  public final static int      SWITCH_EXPRESSION_ONLY = 5;


	/* Instance Variables */
  protected DataExpression         expression;


	/* These are used solely by addSpawnDestroys() */
  private DataStatement lastStatementWithCaseOrDefaultLabel;
  private boolean       hasFoundSpawn;



	/* Instance Methods */
  public DataSwitchStatement()
  {
    expression = null;
  }

  public DataExpression getExpression () { return expression; }

  public void           setExpression ( DataExpression theExpression )
  {
    if ( expression != null )
      expression . setParent ( null );

    expression = theExpression;

    if ( expression != null )
      expression . setParent ( this );
  }


	/* Allow searching of our expression... */
  public boolean hasSubcomponentFraction ( String theString )
  {
    return super               . hasSubcomponentFraction ( theString )
      || (   ( getExpression() != null )
	  && ( getExpression() . hasSubcomponentFraction ( theString ) ) );
  }

	/* Allow searching of our expressions... */
  public void runOnSubcomponentFraction (
			String                      theString,
			RunOnSubcomponentInterface  theRunOnSubcomponentObject,
			Object                      theArgumentObject )
  {
    super . runOnSubcomponentFraction ( theString,
					theRunOnSubcomponentObject,
					theArgumentObject );

    if ( getExpression() != null )
      getExpression() . runOnSubcomponentFraction ( theString,
						    theRunOnSubcomponentObject,
						    theArgumentObject );
  }


  public String getWarnString ( int theObjectSubset )
  {
    return super . getWarnString ( theObjectSubset )
      + " or DataSwitchStatement.SWITCH_EXPRESSION_ONLY ("
      + DataSwitchStatement.SWITCH_EXPRESSION_ONLY + ")";
  }

  public boolean isValidObjectSubset ( int theObjectSubset )
  {
    if ( theObjectSubset == DataSwitchStatement.SWITCH_EXPRESSION_ONLY )
      return true;
    else
      return super . isValidObjectSubset ( theObjectSubset );
  }


  public boolean isValid ( int theObjectSubsetToValidate )
  {
    if (    theObjectSubsetToValidate
	 == DataSwitchStatement.SWITCH_EXPRESSION_ONLY )
    {
      return getExpression() != null;
    }

	/* Simple checks that we can do here.. */
    if (   ( getExpression() == null )
	|| ( getStatement()  == null ) )
      return false;

    return super.isValid ( theObjectSubsetToValidate );
  }


  public void generateExpression ( DataDestination  theOutputDestination,
				   int              theObjectSubsetToGenerate,
				   boolean          theIsPartOfSwitchStatement)
  {
    boolean  needsLeadingSpace  = theIsPartOfSwitchStatement,
             needsTrailingSpace = theIsPartOfSwitchStatement;

    if ( getExpression() != null )
    {
      if ( getExpression() . hasLeadingWhitespaceSubcomponent() )
	needsLeadingSpace = false;
      if ( getExpression() . hasTrailingWhitespaceSubcomponent() )
	needsTrailingSpace = false;
    }

    	/* Initialize us to generate non-significant tokens... */
    initializeGenerateSubcomponentIndex ( DataSwitchStatement.OPEN_PAREN );

	/* Write any pre-EXPRESSION non-significant tokens */
    generateSubcomponents ( DataSwitchStatement.EXPRESSION_INDEX,
			    theOutputDestination,
			    theObjectSubsetToGenerate,
			    needsLeadingSpace );

	/* Write out expression, if we have one... */
    if ( getExpression() != null )
      getExpression() . generate ( theOutputDestination,
				   theObjectSubsetToGenerate );


	/* Write any pre-")" tokens */
    generateSubcomponents ( DataSwitchStatement.CLOSE_PAREN,
			    theOutputDestination,
			    theObjectSubsetToGenerate,
			    needsTrailingSpace );
  }


  public void generate ( DataDestination  theOutputDestination,
			 int              theObjectSubsetToGenerate )
  {
    String  spawnDestroyClauseString;

       	/* Print a warning if necessary... */
    warnIfInvalidObjectSubset ( theObjectSubsetToGenerate, "generate" );

		/* Handle subset case... */
    if (    theObjectSubsetToGenerate
	 == DataSwitchStatement.SWITCH_EXPRESSION_ONLY )
    {
      generateExpression ( theOutputDestination, DataComponent.ENTIRE_OBJECT,
			   false );
      return;
    }

    	/* Initialize us to generate non-significant tokens... */
    initializeGenerateSubcomponentIndex();

	/* Generate any labels that we have... */
    generateLabels ( theOutputDestination, theObjectSubsetToGenerate,
		     DataSwitchStatement.SWITCH );

	/* Write any pre-switch non-significant tokens */
    generateSubcomponents ( DataSwitchStatement.SWITCH,
			    theOutputDestination,
			    theObjectSubsetToGenerate, false );

	/* Write our SWITCH keyword */
    theOutputDestination . write ( DataSwitchStatement.SWITCH );

	/* Write any pre-"(" non-significant tokens */
    generateSubcomponents ( DataSwitchStatement.OPEN_PAREN,
			    theOutputDestination,
			    theObjectSubsetToGenerate, false );

	/* Write "(" */
    theOutputDestination . write ( DataSwitchStatement.OPEN_PAREN );


	/* Write pre-EXPRESSION tokens, the expression, and pre-")" tokens */
    generateExpression ( theOutputDestination, theObjectSubsetToGenerate,
			 true );


	/* Write ")" */
    theOutputDestination . write ( DataSwitchStatement.CLOSE_PAREN );


	/* Write any pre-STATEMENT non-significant tokens */
    generateSubcomponents ( DataSwitchStatement.STATEMENT_INDEX,
			    theOutputDestination,
			    theObjectSubsetToGenerate, false );


	/* Write the statement, if we have one... */
    if ( getStatement() != null )
    {
      if ( isCxxSubset ( theObjectSubsetToGenerate ) )
	addSpawnDestroys();

      theOutputDestination
	. addSecondaryIndent ( DataComponent.STANDARD_INDENT );

      getStatement() . generate ( theOutputDestination,
				  theObjectSubsetToGenerate );

      theOutputDestination
	. removeSecondaryIndent ( DataComponent.STANDARD_INDENT );

      if ( isCxxSubset ( theObjectSubsetToGenerate ) )
      {
	removeSpawnDestroys ( getStatement() );

	    /* Destroy everything that's not yet started... */
	spawnDestroyClauseString
	  = getStatement() . getSpawnDestroyClause ( true );

		/* Deal with #line macros... */
	if ( DataComponent.isEmptyString( spawnDestroyClauseString ) == false )
	{
	  theOutputDestination . setUsingTdlFileName ( false );
	  theOutputDestination . write ( "\n" );
	}

	    /* Destroy everything that's not yet started... */
	theOutputDestination . enableSecondaryIndent();
	theOutputDestination . write ( spawnDestroyClauseString );
	theOutputDestination . disableSecondaryIndent();
      }
    }
    else  /* Try to recover gracefully if we have a problem... */
    {
	/* Write ";" */
      theOutputDestination . write ( DataSwitchStatement.SEMICOLON );
    }

	/* Write any remaining non-significant tokens */
	/* (There *SHOULD* *NOT* be any...  But just in case...) */
    generateAllRemainingSubcomponents ( theOutputDestination,
					theObjectSubsetToGenerate, false );
  }


	/* If and only If there are spawns after the last case/default label,*
	 * do we add a spawn-destroy clause for preceeding spawns.           */
  protected void addSpawnDestroys()
  {
    String  spawnDestroyClauseString;

    lastStatementWithCaseOrDefaultLabel = null;
    hasFoundSpawn = false;
    if ( getStatement() != null )
      privateAddSpawnDestroys ( getStatement(), false );

	/* If we have found a spawn since the last case/default label, *
	 * add destroys after the last case label...                   */
    if (    hasFoundSpawn
	 && ( lastStatementWithCaseOrDefaultLabel != null ) )
    {
      spawnDestroyClauseString
	= getSpawnDestroyClause ( null, 
				  lastStatementWithCaseOrDefaultLabel,
				  true );

      if ( DataComponent.isEmptyString ( spawnDestroyClauseString ) == false )
      {
	lastStatementWithCaseOrDefaultLabel
	  . setPostLabelString ( "\n" + spawnDestroyClauseString );
      }
    }
  }

  protected void privateAddSpawnDestroys ( DataStatement theCurrentStatement,
					   boolean       theJustLookForSpawns )
  {
    String  spawnDestroyClauseString;

	/* If we have found a spawn since the last case/default label, *
	 * add destroys after the last case label...                   *
	 * (But ignore case/default labels inside switch statements.)  */
    if (   ( theCurrentStatement . hasCaseOrDefaultLabel() )
	&& ( theJustLookForSpawns == false                 ) )
    {
      if (    hasFoundSpawn
	   && ( lastStatementWithCaseOrDefaultLabel != null ) )
      {
	spawnDestroyClauseString
	  = getSpawnDestroyClause ( null, 
				    lastStatementWithCaseOrDefaultLabel,
				    true );

	if ( DataComponent.isEmptyString( spawnDestroyClauseString ) == false )
	{
	  lastStatementWithCaseOrDefaultLabel
	    . setPostLabelString ( "\n" + spawnDestroyClauseString );
	}
      }
	/* And record that we are now working on this case/default label */
      lastStatementWithCaseOrDefaultLabel = theCurrentStatement;
      hasFoundSpawn = false;
    }

	/* Did we find a spawn? */
    if ( theCurrentStatement instanceof DataSpawnTask )
      hasFoundSpawn = true;

	/* Don't deal with case/default labels inside switch statements */
    if ( theCurrentStatement instanceof DataSwitchStatement )
      theJustLookForSpawns = true;

	/* Descend (recursively, depth-first) into children */
    for ( int i=0;  i < theCurrentStatement . getChildStatementCount();  i ++ )
    {
      privateAddSpawnDestroys ( theCurrentStatement . getChildStatement ( i ),
				theJustLookForSpawns );
    }
  }



  protected void removeSpawnDestroys ( DataStatement  theDataStatement )
  {
    if ( theDataStatement != this )
      theDataStatement . setPostLabelString ( null );

	/* Don't descend into switch statements... */
	/* (Let them take care of themselves...)   */
    if ( ( theDataStatement instanceof DataSwitchStatement ) == false )
    {
	/* Descend (recursively, depth-first) into children */
      for ( int i=0;  i < theDataStatement . getChildStatementCount();  i ++ )
      {
	removeSpawnDestroys ( theDataStatement . getChildStatement ( i ) );
      }
    }
  }

}

