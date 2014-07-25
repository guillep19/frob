
/*
 * This represents an arbitrary C/C++ Expression Statement
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 *
 */

public class DataExpressionStatement extends DataStatement
{
	/* Class Constants. */
  public final static String   EXPRESSION_INDEX = "expressionIndex";
  public final static String   SEMICOLON        = ";";

	/* Class Variables */

	/* Instance Variables */
  protected DataExpression   expression;


	/* Instance Methods */
  public DataExpressionStatement()
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


  public void generate ( DataDestination  theOutputDestination,
			 int              theObjectSubsetToGenerate )
  {
       	/* Print a warning if necessary... */
    warnIfInvalidObjectSubset ( theObjectSubsetToGenerate, "generate" );

    	/* Initialize us to generate non-significant tokens... */
    initializeGenerateSubcomponentIndex();

	/* Generate any labels that we have... */
    generateLabels ( theOutputDestination, theObjectSubsetToGenerate,
		     DataExpressionStatement.EXPRESSION_INDEX,
		     getExpression(), DataExpression.FIRST_TOKEN_INDEX );

	/* Write any pre-expression non-significant tokens */
	/* (just in case... There *should* not be any...)  */
    generateSubcomponents ( DataExpressionStatement.EXPRESSION_INDEX,
			    theOutputDestination,
			    theObjectSubsetToGenerate, false );

	/* Write our expression */
    if ( getExpression() != null )
      getExpression() . generate ( theOutputDestination,
				   theObjectSubsetToGenerate );

	/* Write any non-significant tokens before the semicolon... */
    generateSubcomponents ( DataExpressionStatement.SEMICOLON,
			    theOutputDestination,
			    theObjectSubsetToGenerate, false );

	/* Write our semicolon */
    theOutputDestination . write ( DataExpressionStatement.SEMICOLON );

	/* Write any remaining non-significant tokens */
	/* (There *SHOULD* *NOT* be any...  But just in case...) */
    generateAllRemainingSubcomponents ( theOutputDestination,
					theObjectSubsetToGenerate, false );
  }

}
