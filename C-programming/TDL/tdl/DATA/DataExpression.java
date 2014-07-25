
/*
 * This represents an arbitrary C/C++ Expression.
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 *
 */

public class DataExpression extends DataComponent
{
	/* Class variables */
  public final static String   FIRST_TOKEN_INDEX = "First";

	/* Instance Variables */

	/* Instance Methods */
  public DataExpression()
  {
  }

	/* Convenience method. */
  public DataExpression( String theString )
  {
    addSubcomponent ( theString );
  }


  public void generate ( DataDestination  theOutputDestination,
			 int              theObjectSubsetToGenerate )
  {
       	/* Print a warning if necessary... */
    warnIfInvalidObjectSubset ( theObjectSubsetToGenerate, "generate" );

    initializeGenerateSubcomponentIndex();
    generateAllRemainingSubcomponents ( theOutputDestination,
					theObjectSubsetToGenerate, false );
  }

}
