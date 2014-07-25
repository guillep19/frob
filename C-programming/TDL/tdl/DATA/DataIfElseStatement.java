
/*
 * This represents a C "if-else" statement.
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 *
 */

public class DataIfElseStatement extends DataSelectionStatement
{
	/* Class variables */
  public final static String   IF                       = "if";
  public final static String   ELSE                     = "else";
  public final static String   OPEN_PAREN               = "(";
  public final static String   CLOSE_PAREN              = ")";
  public final static String   EXPRESSION_INDEX         = "ExpressionIndex";
  public final static String   IF_STATEMENT_INDEX       = "IfStatementIndex";
  public final static String   ELSE_STATEMENT_INDEX     = "ElseStatementIndex";
  public final static String   SEMICOLON                = ";";
  public final static String   EMPTY_COMPOUND_STATEMENT = "\n;";

  public final static int      IF_EXPRESSION_ONLY       = 4;


	/* Instance Variables */
  protected DataExpression          expression;
  protected DataStatement           elseStatement;
  protected int                     lineNumberOfElse;

	/* Instance Methods */
  public DataIfElseStatement()
  {
    expression       = null;
    elseStatement    = null;
    lineNumberOfElse = DataComponent.INVALID_LINE_NUMBER;
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

  public DataCompoundStatement getIfStatement ()     { return getStatement(); }
  public void                  setIfStatement ( DataComponent theIfStatement )
  {
    setStatement ( theIfStatement );
  }

  public DataStatement getElseStatement ()     { return elseStatement; }
  public void          setElseStatement ( DataComponent theElseStatement )
  {
    if ( elseStatement != null )
      elseStatement . setParent ( null );

    if ( theElseStatement == null )
      elseStatement = null;

    else if (   ( theElseStatement instanceof DataCompoundStatement )
	     || ( theElseStatement instanceof DataIfElseStatement   ) )
      elseStatement = (DataStatement) theElseStatement;

    else
      elseStatement = new DataCompoundStatement ( theElseStatement );

    if ( elseStatement != null )
      elseStatement . setParent ( this );
  }


  public void setLineNumberOfElse ( int theLineNumberOfElse )
		   { lineNumberOfElse = theLineNumberOfElse; }

  public int  getLineNumberOfElse() { return lineNumberOfElse; }

  public boolean hasValidLineNumberOfElse()
  {
    return DataComponent.isValidLineNumber ( getLineNumberOfElse() );
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


  public int getChildStatementCount()
  {
    return super . getChildStatementCount()
      +  ( getElseStatement() != null ? 1 : 0 );
  }

  public DataStatement  getChildStatement ( int theIndex )
  {
    if ( theIndex == 0 )
    {
      if ( getIfStatement()        != null )
	return getIfStatement();
      else if ( getElseStatement() != null )
	return getElseStatement();
      else
	return super . getChildStatement ( theIndex );
    }

    if (   ( theIndex           == 1    )
	&& ( getIfStatement()   != null )
	&& ( getElseStatement() != null ) )
    {
      return getElseStatement();
    }

    return super . getChildStatement (
			  theIndex
			  - ( (   ( getIfStatement()   != null )
			       && ( getElseStatement() != null ) ) ? 1 : 0 ) );
  }



  public boolean addSecondaryChild(DataComponent theChildToAdd,
				   DataComponent theAddChildAfterThisComponent)
  {
	/* addSecondaryChild() only gets invoked from the GUI interface...
         * (So, if someone just dropped a compound-statement,
	 *  do the right thing...)
	 */
    if ( getElseStatement() == null )
    {
      if ( theChildToAdd instanceof DataIfElseStatement )
      {
	setElseStatement ( theChildToAdd );
	return true;
      }
      else
      {
	setElseStatement ( new DataCompoundStatement ( null ) );
      }
    }

    if ( getElseStatement() instanceof DataIfElseStatement )
    {
      setElseStatement ( new DataCompoundStatement ( getElseStatement() ) );
    }

    return ( (DataCompoundStatement) getElseStatement() )
               . addChild ( theChildToAdd, theAddChildAfterThisComponent );
  }


  public boolean removeChild ( DataComponent theChildToRemove )
  {
	/*  If it IS the Else statement... */
    if ( getElseStatement() == theChildToRemove )
    {
      setElseStatement ( null );
      return true;
    }

	/* If it is IN the Else statement */
    if (   ( getElseStatement() != null                          )
	&& ( getElseStatement() instanceof DataCompoundStatement )
	&& ( ( (DataCompoundStatement) getElseStatement() )
	       . getIndexOfSubcomponent ( theChildToRemove )
	     != DataComponent.INVALID_INDEX                    ) )

    {
      return getElseStatement() . removeChild ( theChildToRemove );
    }

	/* Otherwise... */
    return super . removeChild ( theChildToRemove );
  }


	/* Overriden from DataStatementWithSubstatements */
  public boolean isPartOfThisStatement ( DataComponent  theDataComponent )
  {
    return (   ( super . isPartOfThisStatement ( theDataComponent ) )
	    || (   ( theDataComponent   == getElseStatement() )
		&& ( getElseStatement() != null               )     ) );
  }




  public String getWarnString ( int theObjectSubset )
  {
    return super . getWarnString ( theObjectSubset )
      + " or DataIfElseStatement.IF_EXPRESSION_ONLY ("
      + DataIfElseStatement.IF_EXPRESSION_ONLY + ")";
  }

  public boolean isValidObjectSubset ( int theObjectSubset )
  {
    if ( theObjectSubset == DataIfElseStatement.IF_EXPRESSION_ONLY )
      return true;
    else
      return super . isValidObjectSubset ( theObjectSubset );
  }


  public boolean isValid ( int theObjectSubsetToValidate )
  {
    if ( theObjectSubsetToValidate == DataIfElseStatement.IF_EXPRESSION_ONLY )
      return getExpression() != null;

	/* Otherwise... */

	/* Simple checks that we can do here.. */
    if (   ( getExpression()  == null )
	|| ( getIfStatement() == null ) )
      return false;

    return super.isValid ( theObjectSubsetToValidate );
  }


  public void generateExpression ( DataDestination  theOutputDestination,
				   int              theObjectSubsetToGenerate,
				   boolean          theIsPartOfIfElseStatement)
  {
    boolean  needsLeadingSpace  = theIsPartOfIfElseStatement,
             needsTrailingSpace = theIsPartOfIfElseStatement;

    if ( getExpression() != null )
    {
      if ( getExpression() . hasLeadingWhitespaceSubcomponent() )
	needsLeadingSpace = false;
      if ( getExpression() . hasTrailingWhitespaceSubcomponent() )
	needsTrailingSpace = false;
    }

    	/* Initialize us to generate non-significant tokens... */
    initializeGenerateSubcomponentIndex ( DataIfElseStatement.OPEN_PAREN );

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
    generateSubcomponents ( DataIfElseStatement.CLOSE_PAREN,
			    theOutputDestination,
			    theObjectSubsetToGenerate, 
			    needsTrailingSpace );
  }


  public void generate ( DataDestination  theOutputDestination,
			 int              theObjectSubsetToGenerate )
  {
    int                    i;
    DataCompoundStatement  dataCompoundStatement;
    String                 spawnDestroyClauseString;

       	/* Print a warning if necessary... */
    warnIfInvalidObjectSubset ( theObjectSubsetToGenerate, "generate" );

		/* Handle subset case... */
    if ( theObjectSubsetToGenerate == DataIfElseStatement.IF_EXPRESSION_ONLY )
    {
      generateExpression ( theOutputDestination, DataComponent.ENTIRE_OBJECT,
			   false );
      return;
    }

    	/* Initialize us to generate non-significant tokens... */
    initializeGenerateSubcomponentIndex();

	/* Generate any labels that we have... */
    generateLabels ( theOutputDestination, theObjectSubsetToGenerate,
		     DataIfElseStatement.IF );

	/* Write any pre-IF non-significant tokens */
    generateSubcomponents ( DataIfElseStatement.IF,
			    theOutputDestination,
			    theObjectSubsetToGenerate, false );

	/* Write our IF keyword */
    theOutputDestination . write ( DataIfElseStatement.IF );

	/* Write any pre-"(" non-significant tokens */
    generateSubcomponents ( DataIfElseStatement.OPEN_PAREN,
			    theOutputDestination,
			    theObjectSubsetToGenerate, false );

	/* Write "(" */
    theOutputDestination . write ( DataIfElseStatement.OPEN_PAREN );


	/* Write the pre-expression tokens, the expression, */
	/* and the post-expression-pre-close-paren tokens   */
    generateExpression ( theOutputDestination, theObjectSubsetToGenerate,
			 true );


	/* Write ")" */
    theOutputDestination . write ( DataIfElseStatement.CLOSE_PAREN );


	/* Write any pre--IF-STATEMENT non-significant tokens */
    generateSubcomponents ( DataIfElseStatement.IF_STATEMENT_INDEX,
			    theOutputDestination,
			    theObjectSubsetToGenerate, false );


	/* Try to recover gracefully if we have a problem... */
    if ( getIfStatement() == null )
    {
      setIfStatement ( new DataCompoundStatement ( null ) );
    }

	/* Add in the else-spawn-destroy clause */
    if (   ( isCxxSubset ( theObjectSubsetToGenerate ) )
	&& ( getElseStatement() != null                ) )
    {
      spawnDestroyClauseString
	= getElseStatement() . getSpawnDestroyClause ( false );

      if ( DataComponent.isEmptyString ( spawnDestroyClauseString ) == false )
      {
	getIfStatement() . addSubcomponentBeforeCloseBrace (
	   new DataComponentPlaceholder (
		 "\n"
		 + DataComponent.getSpaceString (
				    theOutputDestination.getSecondaryIndent() )
		 + spawnDestroyClauseString ) );
      }
    }

	/* Write the IF-STATEMENT */
    getIfStatement() . generate ( theOutputDestination,
				  theObjectSubsetToGenerate );

	/* Remove the else-spawn-destroy clause */
    if (   ( isCxxSubset ( theObjectSubsetToGenerate ) )
	&& ( getElseStatement() != null                ) )
    {
      getIfStatement() . removeSubcomponentBeforeCloseBrace();
    }



	/* Do we have an else clause? */
    if (   ( getElseStatement() != null )
	|| ( isCxxSubset ( theObjectSubsetToGenerate ) ) )
    {
	/* Write the line number of the else keyword */
      generateRelevantLineNumberMacros ( theOutputDestination,
					 theObjectSubsetToGenerate,
					 getLineNumberOfElse(),
					 DataIfElseStatement.ELSE );

	/* Write any pre-ELSE non-significant tokens */
      generateSubcomponents ( DataIfElseStatement.ELSE,
			      theOutputDestination,
			      theObjectSubsetToGenerate, false );

	/* Write our ELSE keyword */
      theOutputDestination . write ( DataIfElseStatement.ELSE );

	/* Write any pre--ELSE-STATEMENT non-significant tokens */
      generateSubcomponents ( DataIfElseStatement.ELSE_STATEMENT_INDEX,
			      theOutputDestination,
			      theObjectSubsetToGenerate, false );


	/* Handle Cxx case. */
      if ( isCxxSubset ( theObjectSubsetToGenerate ) )
      {
	    /* Find us a compound-statement for the else clause */
	if ( getElseStatement() != null )
	{
	  if ( getElseStatement() instanceof DataCompoundStatement )
	    dataCompoundStatement = (DataCompoundStatement) getElseStatement();
	  else
	  {
	    dataCompoundStatement
	      = new DataCompoundStatement ( getElseStatement() );
	    dataCompoundStatement . setParent ( this );
	  }
	}
	else /* getElseStatement() == null */
	{
	  dataCompoundStatement = new DataCompoundStatement ( null );
	  dataCompoundStatement . setParent ( this );
	}
	
	  /* Add in our if-spawn-destroy clause */
	spawnDestroyClauseString
	  = getIfStatement() . getSpawnDestroyClause ( false );

	if ( DataComponent.isEmptyString( spawnDestroyClauseString ) == false )
	{
	  dataCompoundStatement . addSubcomponentAfterOpenBrace (
	     new DataComponentPlaceholder (
		   "\n" 
		 + DataComponent.getSpaceString (
				    theOutputDestination.getSecondaryIndent() )
		 + spawnDestroyClauseString ) );
	}


	dataCompoundStatement . generate ( theOutputDestination,
					   theObjectSubsetToGenerate );

	  /* Remove the else-spawn-destroy clause */
	dataCompoundStatement . removeSubcomponentAfterOpenBrace();

      } /* IF ( isCxxSubset ( theObjectSubsetToGenerate ) ) */

      else /* getElseStatement() != null && NOT a CxxSubset */
      {
	  /* Write our ELSE-STATEMENT */
	getElseStatement() . generate ( theOutputDestination,
				      theObjectSubsetToGenerate );

      }

    } /* Generating ELSE clause */


	/* Write any remaining non-significant tokens */
	/* (There *SHOULD* *NOT* be any...  But just in case...) */
    generateAllRemainingSubcomponents ( theOutputDestination,
					theObjectSubsetToGenerate, false );
  }

}
