
/*
 * This represents a C "do {} while (...);" statement.
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 *
 */

public class DataDoWhileStatement extends DataIterationStatement
{
	/* Class variables */
  public final static String   DO               = "do";
  public final static String   WHILE            = "while";
  public final static String   OPEN_PAREN       = "(";
  public final static String   CLOSE_PAREN      = ")";
  public final static String   EXPRESSION_INDEX = "ExpressionIndex";
  public final static String   STATEMENT_INDEX  = "StatementIndex";
  public final static String   SEMICOLON        = ";";

  public final static int      DO_WHILE_EXPRESSION_ONLY = 7;


	/* Instance Variables */
  protected DataExpression           expression;


	/* Instance Methods */
  public DataDoWhileStatement()
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
      + " or DataDoWhileStatement.DO_WHILE_EXPRESSION_ONLY ("
      + DataDoWhileStatement.DO_WHILE_EXPRESSION_ONLY + ")";
  }

  public boolean isValidObjectSubset ( int theObjectSubset )
  {
    if ( theObjectSubset == DataDoWhileStatement.DO_WHILE_EXPRESSION_ONLY )
      return true;
    else
      return super . isValidObjectSubset ( theObjectSubset );
  }


  public boolean isValid ( int theObjectSubsetToValidate )
  {
    if (    theObjectSubsetToValidate
	 == DataDoWhileStatement.DO_WHILE_EXPRESSION_ONLY )
    {
      return getExpression() != null;
    }

	/* Simple checks that we can do here.. */
    if (   ( getExpression() == null )
	|| ( getStatement()  == null ) )
      return false;

    return super.isValid ( theObjectSubsetToValidate );
  }


  public void generateExpression ( DataDestination theOutputDestination,
				   int             theObjectSubsetToGenerate,
				   boolean         theIsPartOfDoWhileStatement)
  {
    boolean  needsLeadingSpace  = theIsPartOfDoWhileStatement,
             needsTrailingSpace = theIsPartOfDoWhileStatement;

    if ( getExpression() != null )
    {
      if ( getExpression() . hasLeadingWhitespaceSubcomponent() )
	needsLeadingSpace = false;
      if ( getExpression() . hasTrailingWhitespaceSubcomponent() )
	needsTrailingSpace = false;
    }

    	/* Initialize us to generate non-significant tokens... */
    initializeGenerateSubcomponentIndex ( DataDoWhileStatement.OPEN_PAREN );

	/* Write any pre-EXPRESSION non-significant tokens */
    generateSubcomponents ( DataDoWhileStatement.EXPRESSION_INDEX,
			    theOutputDestination,
			    theObjectSubsetToGenerate,
			    needsLeadingSpace );

	/* Write out expression, if we have one... */
    if ( getExpression() != null )
      getExpression() . generate ( theOutputDestination,
				   theObjectSubsetToGenerate );


	/* Write any pre-")" tokens */
    generateSubcomponents ( DataDoWhileStatement.CLOSE_PAREN,
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
    if (    theObjectSubsetToGenerate
	 == DataDoWhileStatement.DO_WHILE_EXPRESSION_ONLY )
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
		     DataDoWhileStatement.DO,
		     getLabel() == null /* Disable #line macros if no labels.*/
		    );

	/* Write any pre-do non-significant tokens */
    generateSubcomponents ( DataDoWhileStatement.DO,
			    theOutputDestination,
			    theObjectSubsetToGenerate, false );

	/* Generate extra pre-iteration code. */
    generatePreIterationCode ( theOutputDestination,
			       theObjectSubsetToGenerate );

	/* Generate relevant #line-number macros */
    generateRelevantLineNumberMacros ( theOutputDestination,
				       getLineNumber(),
				       0 );

	/* Write our DO keyword */
    theOutputDestination . write ( DataDoWhileStatement.DO );



	/* Write any pre-STATEMENT non-significant tokens */
    generateSubcomponents ( DataDoWhileStatement.STATEMENT_INDEX,
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


	/* Write any pre-while non-significant tokens */
    generateSubcomponents ( DataDoWhileStatement.WHILE,
			    theOutputDestination,
			    theObjectSubsetToGenerate, true );

	/* Write our WHILE keyword */
    theOutputDestination . write ( DataDoWhileStatement.WHILE );


	/* Write any pre-"(" non-significant tokens */
    generateSubcomponents ( DataDoWhileStatement.OPEN_PAREN,
			    theOutputDestination,
			    theObjectSubsetToGenerate, true );

	/* Write "(" */
    theOutputDestination . write ( DataDoWhileStatement.OPEN_PAREN );


	/* Write pre-Expression tokens, the expression, and pre-")" tokens */
    generateExpression ( theOutputDestination, theObjectSubsetToGenerate,
			 true );


	/* Write ")" */
    theOutputDestination . write ( DataDoWhileStatement.CLOSE_PAREN );


	/* Write any pre-";" tokens */
    generateSubcomponents ( DataDoWhileStatement.SEMICOLON,
			    theOutputDestination,
			    theObjectSubsetToGenerate, false );

	/* Write ";" */
    theOutputDestination . write ( DataDoWhileStatement.SEMICOLON );



	/* Generate extra post-iteration code. */
    generatePostIterationCode ( theOutputDestination,
				theObjectSubsetToGenerate );

	/* Write any remaining non-significant tokens */
	/* (There *SHOULD* *NOT* be any...  But just in case...) */
    generateAllRemainingSubcomponents ( theOutputDestination,
					theObjectSubsetToGenerate, false );
  }

}
