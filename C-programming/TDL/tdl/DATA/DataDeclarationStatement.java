
/*
 * This represents an arbitrary C/C++ Expression.
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 *
 */

public class DataDeclarationStatement extends DataStatement
{
	/* Class Constants. */
  public final static String   FIRST_TOKEN_INDEX = "First";

	/* Class Variables */

	/* Instance Variables */

	/* Instance Methods */
  public DataDeclarationStatement()
  {
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
		     DataDeclarationStatement.FIRST_TOKEN_INDEX );

	/* And generate all of our tokens, since we are (currently)
	 * just a token-list
	 */
    generateAllRemainingSubcomponents ( theOutputDestination,
					theObjectSubsetToGenerate, false );
  }

}

