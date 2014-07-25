
/*
 * This represents a C "while" statement.
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 *
 */

public class DataWhileStatement extends DataIterationStatement
{
	/* Class variables */
  public final static String   WHILE            = "while";
  public final static String   OPEN_PAREN       = "(";
  public final static String   CLOSE_PAREN      = ")";
  public final static String   EXPRESSION_INDEX = "ExpressionIndex";
  public final static String   STATEMENT_INDEX  = "StatementIndex";
  public final static String   SEMICOLON        = ";";

  public final static int      WHILE_EXPRESSION_ONLY = 6;


	/* Instance Variables */
  protected DataExpression          expression;


	/* Instance Methods */
  public DataWhileStatement()
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
      + " or DataWhileStatement.WHILE_EXPRESSION_ONLY ("
      + DataWhileStatement.WHILE_EXPRESSION_ONLY + ")";
  }

  public boolean isValidObjectSubset ( int theObjectSubset )
  {
    if ( theObjectSubset == DataWhileStatement.WHILE_EXPRESSION_ONLY )
      return true;
    else
      return super . isValidObjectSubset ( theObjectSubset );
  }


  public boolean isValid ( int theObjectSubsetToValidate )
  {
    if ( theObjectSubsetToValidate == DataWhileStatement.WHILE_EXPRESSION_ONLY)
      return getExpression() != null;

	/* Simple checks that we can do here.. */
    if (   ( getExpression() == null )
	|| ( getStatement()  == null ) )
      return false;

    return super.isValid ( theObjectSubsetToValidate );
  }



  public void generateExpression ( DataDestination  theOutputDestination,
				   int              theObjectSubsetToGenerate,
				   boolean          theIsPartOfWhileStatement )
  {
    boolean  needsLeadingSpace  = theIsPartOfWhileStatement,
             needsTrailingSpace = theIsPartOfWhileStatement;

    if ( getExpression() != null )
    {
      if ( getExpression() . hasLeadingWhitespaceSubcomponent() )
	needsLeadingSpace = false;
      if ( getExpression() . hasTrailingWhitespaceSubcomponent() )
	needsTrailingSpace = false;
    }

    	/* Initialize us to generate non-significant tokens... */
    initializeGenerateSubcomponentIndex ( DataWhileStatement.OPEN_PAREN );

	/* Write any pre-EXPRESSION non-significant tokens */
    generateSubcomponents ( DataWhileStatement.EXPRESSION_INDEX,
			    theOutputDestination,
			    theObjectSubsetToGenerate,
			    needsLeadingSpace );

	/* Write out expression, if we have one... */
    if ( getExpression() != null )
      getExpression() . generate ( theOutputDestination,
				   theObjectSubsetToGenerate );


	/* Write any pre-")" tokens */
    generateSubcomponents ( DataWhileStatement.CLOSE_PAREN,
			    theOutputDestination,
			    theObjectSubsetToGenerate,
			    needsTrailingSpace );
  }


  public void generate ( DataDestination  theOutputDestination,
			 int              theObjectSubsetToGenerate )
  {
    int i;

       	/* Print a warning if necessary... */
    warnIfInvalidObjectSubset ( theObjectSubsetToGenerate, "generate" );

		/* Handle subset case... */
    if ( theObjectSubsetToGenerate == DataWhileStatement.WHILE_EXPRESSION_ONLY)
    {
      generateExpression ( theOutputDestination, DataComponent.ENTIRE_OBJECT,
			   false );
      return;
    }

    	/* Initialize us to generate non-significant tokens... */
    initializeGenerateSubcomponentIndex();

	/* Generate any labels that we have... */
    generateLabels ( theOutputDestination,
		     theObjectSubsetToGenerate,
		     DataWhileStatement.WHILE,
		     getLabel() == null /* Disable #line macros if no labels.*/
		    );

	/* Write any pre-while non-significant tokens */
    generateSubcomponents ( DataWhileStatement.WHILE,
			    theOutputDestination,
			    theObjectSubsetToGenerate, false );

	/* Generate extra pre-iteration code. */
    generatePreIterationCode ( theOutputDestination,
			       theObjectSubsetToGenerate );

	/* Generate relevant #line-number macros */
    generateRelevantLineNumberMacros ( theOutputDestination,
				       getLineNumber(),
				       0 );

	/* Write our WHILE keyword */
    theOutputDestination . write ( DataWhileStatement.WHILE );

	/* Write any pre-"(" non-significant tokens */
    generateSubcomponents ( DataWhileStatement.OPEN_PAREN,
			    theOutputDestination,
			    theObjectSubsetToGenerate, false );

	/* Write "(" */
    theOutputDestination . write ( DataWhileStatement.OPEN_PAREN );


	/* Write pre-Expression tokens, the expression, and pre-")" tokens */
    generateExpression ( theOutputDestination, theObjectSubsetToGenerate,
			 true );


	/* Write ")" */
    theOutputDestination . write ( DataWhileStatement.CLOSE_PAREN );


	/* Write any pre-STATEMENT non-significant tokens */
    generateSubcomponents ( DataWhileStatement.STATEMENT_INDEX,
			    theOutputDestination,
			    theObjectSubsetToGenerate, false );


	/* Write the statement, if we have one... */
    if ( getStatement() != null )
    {
	/* Generate extra inside-iteration code */
      addInsideIterationCode ( theObjectSubsetToGenerate, getStatement() );

      getStatement() . generate ( theOutputDestination,
				  theObjectSubsetToGenerate );

	/* Remove extra inside-iteration code */
      removeInsideIterationCode ( theObjectSubsetToGenerate, getStatement() );
    }
    else  /* Try to recover gracefully if we have a problem... */
    {
	/* Generate empty body statement with extra inside-iteration code */
      generateEmptyIterationBodyStatement ( theOutputDestination,
					    theObjectSubsetToGenerate );
    }

	/* Generate extra post-iteration code. */
    generatePostIterationCode ( theOutputDestination,
				theObjectSubsetToGenerate );


	/* Write any remaining non-significant tokens */
	/* (There *SHOULD* *NOT* be any...  But just in case...) */
    generateAllRemainingSubcomponents ( theOutputDestination,
					theObjectSubsetToGenerate, false );
  }

}

