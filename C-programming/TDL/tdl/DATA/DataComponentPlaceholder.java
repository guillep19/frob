
/* Placeholder class.
 * Invokes appropriate functions to deal with delayed generate()
 * in appropriate places.  This class will be expanded as necessary.
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 *
 */

public class DataComponentPlaceholder extends DataComponent
{
	/* Class constants */
  public final static int ARGUMENTS_DECLARATION     = 0;
  public final static int HANDLEMANAGER_DECLARATION = 1;

	/* Instance variables */
  protected boolean             markDeclarationsUsed;
  protected DataTaskDefinition  ourDataTaskDefinition;
  protected int                 ourGenerationType;
  protected DataVector          ourNonUniqueNames;
  protected DataVector          ourStatements;
  protected DataHashtable       ourOnAgentHashtable;
  protected String              stringToGenerate;
  protected boolean             isPartOfTdlFile;
  protected int                 currentLineNumber;


	/* Instance methods */
  public DataComponentPlaceholder( DataTaskDefinition theDataTaskDefinition,
				   boolean            theMarkDeclarationsUsed )
  {
    clear();
    ourDataTaskDefinition = theDataTaskDefinition;
    markDeclarationsUsed  = theMarkDeclarationsUsed;
    ourGenerationType     = DataComponentPlaceholder.ARGUMENTS_DECLARATION;
  }

  public DataComponentPlaceholder ( DataTaskDefinition theDataTaskDefinition,
				    DataVector         theNonUniqueNames,
				    DataVector         theStatements,
				    DataHashtable      theOnAgentHashtable )
  {
    clear();
    ourDataTaskDefinition = theDataTaskDefinition;
    ourGenerationType     = DataComponentPlaceholder.HANDLEMANAGER_DECLARATION;
    ourNonUniqueNames     = theNonUniqueNames;
    ourStatements         = theStatements;
    ourOnAgentHashtable   = theOnAgentHashtable;
  }

  public DataComponentPlaceholder ( String theCxxStringToGenerate )
  {
	/* Check for idiocy */
    if ( theCxxStringToGenerate == null )
    {
      System.err.println ("[DataComponentPlaceholder] Warning:  Null string.");
      theCxxStringToGenerate = "";
    }

    clear();
    stringToGenerate = theCxxStringToGenerate;
    isPartOfTdlFile  = false;

    if ( theCxxStringToGenerate . equals ( "\n" ) )
    {
      System.err.println ( "DataComponentPlaceholder initialized with "
			   + "newline string:" );
      Thread.dumpStack();
    }
    if ( DataComponent.isEmptyString ( theCxxStringToGenerate ) )
    {
      System.err.println ( "DataComponentPlaceholder initialized with "
			   + "empty string:" );
      Thread.dumpStack();
    }
  }

  public DataComponentPlaceholder ( String  theTdlStringToGenerate,
				    int     theCurrentLineNumber   )

  {
	/* Check for idiocy */
    if ( theTdlStringToGenerate == null )
    {
      System.err.println ("[DataComponentPlaceholder] WARNING:  Null string.");
      theTdlStringToGenerate = "";
    }

    clear();
    stringToGenerate  = theTdlStringToGenerate;
    currentLineNumber = theCurrentLineNumber;
    isPartOfTdlFile   = true;
  }


  protected void clear()
  {
    markDeclarationsUsed  = false;
    ourGenerationType     = DataComponent.INVALID_INDEX;
    ourDataTaskDefinition = null;
    ourNonUniqueNames     = null;
    ourStatements         = null;
    ourOnAgentHashtable   = null;
    stringToGenerate      = null;
    isPartOfTdlFile       = false;
    currentLineNumber     = -1;
  }


  public void generate ( DataDestination  theOutputDestination,
			 int              theObjectSubsetToGenerate )
  {
    if ( stringToGenerate != null )
    {
      if ( isPartOfTdlFile == false )
      {
	theOutputDestination . setUsingTdlFileName ( false );
	    /* If necessary, force Cxx file #line macros here (via newline). */
	if ( stringToGenerate . charAt ( 0 ) != '\n' )
	  theOutputDestination . write ( "\n" );
      }
      else
      {
	theOutputDestination . setUsingTdlFileName ( true );
	theOutputDestination . makeNextLineNumber  ( currentLineNumber );
	if ( theOutputDestination . getEnableLineMacros() )
	  theOutputDestination . write ( "\n" ); /* Flush #line macro. */
      }

      theOutputDestination . write ( stringToGenerate );
      return;
    }


    if ( ourDataTaskDefinition != null )
    {
      if (    ourGenerationType
	   == DataComponentPlaceholder.HANDLEMANAGER_DECLARATION )
      {
	ourDataTaskDefinition
	  . generateHandleManagerDeclaration ( theOutputDestination,
					       ourNonUniqueNames,
					       ourStatements,
					       ourOnAgentHashtable );
      }
      else
      {
	ourDataTaskDefinition
	  . generateArgumentsDeclarationCode ( markDeclarationsUsed,
					       theOutputDestination );
      }

      return;
    }

    System.err.println ( "[DataComponentPlaceholder:generate]  Error:  "
			 + "Unknown generation encountered." );
  }
}
