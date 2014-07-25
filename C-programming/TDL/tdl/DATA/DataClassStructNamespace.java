
/*
 * Handles Class/Struct/Namespace constructs.
 * The interior of these constructs may be located in the .H file,
 * rather than the typical .C file for all non-Task code.
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 *
 */

public class DataClassStructNamespace extends DataComponent
{
  protected boolean  isStart;
  protected String   correspondingStartString;

  public DataClassStructNamespace() { isStart = false;
				      correspondingStartString = null; }

  public boolean getIsStart() { return (isStart == true ); }
  public boolean getIsEnd()   { return (isStart == false); }
  public void    setIsStart() { isStart = true;  }
  public void    setIsEnd()   { isStart = false; }

  public void      setCorrespondingStartString(
					  String theCorrespondingStartString)
		    { correspondingStartString = theCorrespondingStartString;}
  public String    getCorrespondingStartString()
	     { return correspondingStartString; }

  public void generate ( DataDestination  theOutputDestination,
			 int              theObjectSubsetToGenerate )
  {
       	/* Print a warning if necessary... */
    warnIfInvalidObjectSubset ( theObjectSubsetToGenerate, "generate" );

	/* html is a no-op. */
    if ( theObjectSubsetToGenerate == DataComponent.HTML_DOCUMENTATION )
      return;

    initializeGenerateSubcomponentIndex();

    generateAllRemainingSubcomponents ( theOutputDestination,
					theObjectSubsetToGenerate, false );

    if (      getIsEnd()
	 && ( getCorrespondingStartString() != null ) )
    {
      theOutputDestination . write ( " /* " );
      theOutputDestination . write ( getCorrespondingStartString() );
      theOutputDestination . write ( " */ " );
    }
  }
}
