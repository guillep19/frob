
/*
 * This represents a 'extern "H" { ...whatever... }' block.
 * The '...whatever...' contents are only reproduced in the Header file.
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 *
 */

public class DataExternH extends DataComponent
{
	/* Class variables */
  public final static String   EXTERN        = "EXTERN";
  public final static String   HEADER_STRING = "\"H\"";
  public final static String   OPEN_BRACE    = "{";
  public final static String   CLOSE_BRACE   = "}";

	/* Instance Variables */

	/* Instance Methods */
  public DataExternH()
  {
  }


  public void generate ( DataDestination  theOutputDestination,
			 int              theObjectSubsetToGenerate )
  {
       	/* Print a warning if necessary... */
    warnIfInvalidObjectSubset ( theObjectSubsetToGenerate, "generate" );

	/* Code/html is a no-op. */
    if (   ( isCxxCodeSubset ( theObjectSubsetToGenerate )                 )
	|| ( theObjectSubsetToGenerate == DataComponent.HTML_DOCUMENTATION ) )
      return;

    initializeGenerateSubcomponentIndex();

    generateSubcomponents ( DataExternH.EXTERN, theOutputDestination,
			    theObjectSubsetToGenerate, false );
    if ( isCxxHeaderSubset ( theObjectSubsetToGenerate ) )
      theOutputDestination . write ( "/*" );
    theOutputDestination . write ( DataExternH.EXTERN );
    if ( isCxxHeaderSubset ( theObjectSubsetToGenerate ) )
      theOutputDestination . write ( "*/" );

    generateSubcomponents ( DataExternH.HEADER_STRING, theOutputDestination,
			    theObjectSubsetToGenerate, false );
    if ( isCxxHeaderSubset ( theObjectSubsetToGenerate ) )
      theOutputDestination . write ( "/*" );
    theOutputDestination . write ( DataExternH.HEADER_STRING );
    if ( isCxxHeaderSubset ( theObjectSubsetToGenerate ) )
      theOutputDestination . write ( "*/" );

    generateSubcomponents ( DataExternH.OPEN_BRACE, theOutputDestination,
			    theObjectSubsetToGenerate, false );
    if ( isCxxHeaderSubset ( theObjectSubsetToGenerate ) )
      theOutputDestination . write ( "/*" );
    theOutputDestination . write ( DataExternH.OPEN_BRACE );
    if ( isCxxHeaderSubset ( theObjectSubsetToGenerate ) )
      theOutputDestination . write ( "*/" );


    generateSubcomponents ( DataExternH.CLOSE_BRACE, theOutputDestination,
			    theObjectSubsetToGenerate, false );
    if ( isCxxHeaderSubset ( theObjectSubsetToGenerate ) )
      theOutputDestination . write ( "/*" );
    theOutputDestination . write ( DataExternH.CLOSE_BRACE );
    if ( isCxxHeaderSubset ( theObjectSubsetToGenerate ) )
      theOutputDestination . write ( "*/" );

    generateAllRemainingSubcomponents ( theOutputDestination,
					theObjectSubsetToGenerate, false );
  }

}
