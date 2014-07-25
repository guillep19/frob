
/*
 * This represents a C "for" statement.
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 *
 */

public class DataForStatement extends DataIterationStatement
{
	/* Class variables */
  public final static String   FOR                        = "for";
  public final static String   OPEN_PAREN                 = "(";
  public final static String   CLOSE_PAREN                = ")";
  public final static String   INITIAL_EXPRESSION_INDEX   = "InitialExprIndex";
  public final static String   TEST_EXPRESSION_INDEX      = "TestExprIndex";
  public final static String   ITERATION_EXPRESSION_INDEX = "IterExprIndex";
  public final static String   STATEMENT_INDEX            = "StatementIndex";
  public final static String   SEMICOLON                  = ";";

  public final static int      FOR_INIT_ONLY              = 8;
  public final static int      FOR_TEST_ONLY              = 9;
  public final static int      FOR_ITERATION_ONLY         = 10;


	/* Instance Variables */
  protected DataStatement            initialExpressionOrDeclarationStatement;
  protected DataExpression           testExpression, iterationExpression;


	/* Instance Methods */
  public DataForStatement()
  {
    initialExpressionOrDeclarationStatement = null;
    testExpression                          = null;
    iterationExpression                     = null;
  }

  public DataStatement  getInitialExpressionOrDeclarationStatement ()
  {
    return initialExpressionOrDeclarationStatement;
  }

  public void setInitialExpressionOrDeclarationStatement (
						  DataStatement  theStatement )
  {
    if (   ( theStatement instanceof DataExpressionStatement  )
	|| ( theStatement instanceof DataDeclarationStatement ) )
    {
      if ( initialExpressionOrDeclarationStatement != null )
	initialExpressionOrDeclarationStatement . setParent ( null );

      initialExpressionOrDeclarationStatement = theStatement;

      if ( initialExpressionOrDeclarationStatement != null )
	initialExpressionOrDeclarationStatement . setParent ( this );
    }
    else
    {
      initialExpressionOrDeclarationStatement = null;
      System.err.println (
         "[DataForStatement:setInitialExpressionOrDeclarationStatement]  "
	 + "Error:  Initial \"statement\" must be a DataExpressionStatement "
	 + "or a DataDeclarationStatement, not a \""
	 + theStatement . getClass() . getName()
	 + "\".   initialExpressionOrDeclarationStatement SET TO NULL INSTEAD!"
	 );
    }
  }


  public DataExpression getTestExpression () { return testExpression; }

  public void           setTestExpression ( DataExpression theExpression )
  {
    if ( testExpression != null )
      testExpression . setParent ( null);

    testExpression = theExpression;

    if ( testExpression != null )
     testExpression  . setParent ( this );
  }

  public DataExpression getIterationExpression () {return iterationExpression;}

  public void           setIterationExpression ( DataExpression theExpression )
  {
    if ( iterationExpression != null )
      iterationExpression . setParent ( null);

    iterationExpression = theExpression;

    if ( iterationExpression != null )
      iterationExpression . setParent ( this );
  }


	/* Allow searching of our expression... */
  public boolean hasSubcomponentFraction ( String theString )
  {
    return super . hasSubcomponentFraction ( theString )

      || (   ( getInitialExpressionOrDeclarationStatement() != null )
	  && ( getInitialExpressionOrDeclarationStatement()
	         . hasSubcomponentFraction ( theString ) ) )

      || (   ( getTestExpression() != null )
	  && ( getTestExpression()
	         . hasSubcomponentFraction ( theString ) ) )

      || (   ( getIterationExpression() != null )
	  && ( getIterationExpression()
	         . hasSubcomponentFraction ( theString ) ) );
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

    if ( getInitialExpressionOrDeclarationStatement() != null )
      getInitialExpressionOrDeclarationStatement()
	. runOnSubcomponentFraction ( theString,
				      theRunOnSubcomponentObject,
				      theArgumentObject );

    if ( getTestExpression() != null )
      getTestExpression()
	. runOnSubcomponentFraction ( theString,
				      theRunOnSubcomponentObject,
				      theArgumentObject );

    if ( getIterationExpression() != null )
      getIterationExpression()
	. runOnSubcomponentFraction ( theString,
				      theRunOnSubcomponentObject,
				      theArgumentObject );
  }


  public String getWarnString ( int theObjectSubset )
  {
    return super . getWarnString ( theObjectSubset )
      + " or DataForStatement.FOR_INIT_ONLY ("
      +  DataForStatement.FOR_INIT_ONLY
      + ") or DataForStatement.FOR_TEST_ONLY ("
      +  DataForStatement.FOR_TEST_ONLY
      + ") or DataForStatement.FOR_ITERATION_ONLY ("
      +  DataForStatement.FOR_ITERATION_ONLY + ")";
  }

  public boolean isValidObjectSubset ( int theObjectSubset )
  {
    if (   ( theObjectSubset == DataForStatement.FOR_INIT_ONLY      )
	|| ( theObjectSubset == DataForStatement.FOR_TEST_ONLY      )
	|| ( theObjectSubset == DataForStatement.FOR_ITERATION_ONLY ) )
      return true;
    else
      return super . isValidObjectSubset ( theObjectSubset );
  }


  public boolean isValid ( int theObjectSubsetToValidate )
  {
	/* Not much I can do for these right now... */
    if (   ( theObjectSubsetToValidate == DataForStatement.FOR_INIT_ONLY     )
	|| ( theObjectSubsetToValidate == DataForStatement.FOR_TEST_ONLY     )
	|| ( theObjectSubsetToValidate == DataForStatement.FOR_ITERATION_ONLY))
      return true;


	/* Simple checks that we can do here.. */
    if ( getStatement()  == null )
      return false;

    return super.isValid ( theObjectSubsetToValidate );
  }


  public void generateInit ( DataDestination  theOutputDestination,
			     int              theObjectSubsetToGenerate,
			     boolean          theIsPartOfEntireForStatement )
  {
    boolean  needsLeadingSpace  = theIsPartOfEntireForStatement;

    if ( getInitialExpressionOrDeclarationStatement() != null )
    {
      if ( getInitialExpressionOrDeclarationStatement()
	     . hasLeadingWhitespaceSubcomponent()  )
	needsLeadingSpace = false;
    }

    	/* Initialize us to generate non-significant tokens... */
    initializeGenerateSubcomponentIndex ( DataForStatement.OPEN_PAREN );

	/* Write any pre-INITIAL-EXPRESSION non-significant tokens */
    generateSubcomponents ( DataForStatement.INITIAL_EXPRESSION_INDEX,
			    theOutputDestination,
			    theObjectSubsetToGenerate,
			    needsLeadingSpace );

	/* Write out the initial-expression/declaration statement, */
	/* (if we have one...) */
    if ( getInitialExpressionOrDeclarationStatement() != null )
    {
      boolean  oldTheOutputDestinationGetEnableLineMacros
		 = theOutputDestination . getEnableLineMacros();

	/* Disable line macros here, inside the for statement. */
      theOutputDestination . setEnableLineMacros ( false );

      getInitialExpressionOrDeclarationStatement()
	. generate ( theOutputDestination, theObjectSubsetToGenerate );

      theOutputDestination
	. setEnableLineMacros ( oldTheOutputDestinationGetEnableLineMacros );
    }
    else /* Try to recover gracefully if we have a problem */
    {
      theOutputDestination . write ( DataForStatement.SEMICOLON );
    }

  }

  public void generateTest ( DataDestination  theOutputDestination,
			     int              theObjectSubsetToGenerate,
			     boolean          theIsPartOfEntireForStatement )
  {
    boolean  needsLeadingSpace  = theIsPartOfEntireForStatement;

    if ( getTestExpression() != null )
    {
      if ( getTestExpression() . hasLeadingWhitespaceSubcomponent() )
	needsLeadingSpace = false;
    }

    	/* Initialize us to generate non-significant tokens... */
    if ( hasIndex ( DataForStatement.INITIAL_EXPRESSION_INDEX ) )
      initializeGenerateSubcomponentIndex (
				   DataForStatement.INITIAL_EXPRESSION_INDEX );
    else
      initializeGenerateSubcomponentIndex ( DataForStatement.OPEN_PAREN );


	/* Write any pre-TEST-EXPRESSION non-significant tokens */
    generateSubcomponents ( DataForStatement.TEST_EXPRESSION_INDEX,
			    theOutputDestination,
			    theObjectSubsetToGenerate,
			    needsLeadingSpace );

	/* Write out expression, if we have one... */
    if ( getTestExpression() != null )
      getTestExpression() . generate ( theOutputDestination,
				       theObjectSubsetToGenerate );


	/* Write any pre-";" tokens */
    generateSubcomponents ( DataForStatement.SEMICOLON,
			    theOutputDestination,
			    theObjectSubsetToGenerate, false );
  }


  public void generateIteration( DataDestination theOutputDestination,
				 int             theObjectSubsetToGenerate,
				 boolean         theIsPartOfEntireForStatement)
  {
    boolean  needsLeadingSpace  = theIsPartOfEntireForStatement,
             needsTrailingSpace = theIsPartOfEntireForStatement;

    if ( getIterationExpression() != null )
    {
      if ( getIterationExpression() . hasLeadingWhitespaceSubcomponent() )
	needsLeadingSpace = false;
      if ( getIterationExpression() . hasTrailingWhitespaceSubcomponent() )
	needsTrailingSpace = false;
    }

    	/* Initialize us to generate non-significant tokens... */
    initializeGenerateSubcomponentIndex ( DataForStatement.SEMICOLON);

	/* Write any pre-iteration-EXPRESSION non-significant tokens */
    generateSubcomponents ( DataForStatement.ITERATION_EXPRESSION_INDEX,
			    theOutputDestination,
			    theObjectSubsetToGenerate,
			    needsLeadingSpace );

	/* Write out the iteration-expression, if we have one... */
    if ( getIterationExpression() != null )
      getIterationExpression() . generate( theOutputDestination,
					   theObjectSubsetToGenerate );


	/* Write any pre-")" tokens */
    generateSubcomponents ( DataForStatement.CLOSE_PAREN,
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
    if ( theObjectSubsetToGenerate == DataForStatement.FOR_INIT_ONLY )
    {
      generateInit ( theOutputDestination, DataComponent.ENTIRE_OBJECT, false);
      return;
    }
    if ( theObjectSubsetToGenerate == DataForStatement.FOR_TEST_ONLY )
    {
      generateTest ( theOutputDestination, DataComponent.ENTIRE_OBJECT, false);
      return;
    }
    if ( theObjectSubsetToGenerate == DataForStatement.FOR_ITERATION_ONLY )
    {
      generateIteration ( theOutputDestination, DataComponent.ENTIRE_OBJECT,
			  false );
      return;
    }


    	/* Initialize us to generate non-significant tokens... */
    initializeGenerateSubcomponentIndex();

	/* Generate any labels that we have... */
    generateLabels ( theOutputDestination,
		     theObjectSubsetToGenerate,
		     DataForStatement.FOR,
		     getLabel() == null /* Disable #line macros if no labels.*/
		    );

	/* Write any pre-for non-significant tokens */
    generateSubcomponents ( DataForStatement.FOR,
			    theOutputDestination,
			    theObjectSubsetToGenerate, false );

	/* Generate extra pre-iteration code. */
    generatePreIterationCode ( theOutputDestination,
			       theObjectSubsetToGenerate );

	/* Generate relevant #line-number macros */
    generateRelevantLineNumberMacros ( theOutputDestination,
				       getLineNumber(),
				       0 );

	/* Write our FOR keyword */
    theOutputDestination . write ( DataForStatement.FOR );

	/* Write any pre-"(" non-significant tokens */
    generateSubcomponents ( DataForStatement.OPEN_PAREN,
			    theOutputDestination,
			    theObjectSubsetToGenerate, false );

	/* Write "(" */
    theOutputDestination . write ( DataForStatement.OPEN_PAREN );

	/* Write pre-Init tokens, and the init expression */
    generateInit ( theOutputDestination, theObjectSubsetToGenerate, true );

	/* Write pre-Test tokens, the test expression, and pre-; tokens */
    generateTest ( theOutputDestination, theObjectSubsetToGenerate, true );

	/* Write ";" */
    theOutputDestination . write ( DataForStatement.SEMICOLON );

	/* Write pre-Iter. tokens, the Iter. expression, and pre-) tokens */
    generateIteration ( theOutputDestination, theObjectSubsetToGenerate, true);


	/* Write ")" */
    theOutputDestination . write ( DataForStatement.CLOSE_PAREN );



	/* Write any pre-STATEMENT non-significant tokens */
    generateSubcomponents ( DataForStatement.STATEMENT_INDEX,
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
