
/*
 * This represents a scope (class/struct/namespace) before a corresponding
 * identifier.  It does not include that trailing indentifier!
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 *
 */

public class DataScope extends DataComponent
{
	/* Class Constants */
  public    final static String   IDENTIFIER_INDEX  = "Id";
  public    final static String   SCOPE             = "::";

	/* Instance Data */
  protected DataVector            scopes;

  public DataScope()
  {
    this ( 10 /* Arbitrary number */ );
  }

  public DataScope ( int theExpectedNumberOfScopes )
  {
    scopes = new DataVector ( theExpectedNumberOfScopes );
  }


  public DataVector getScopes()      { return scopes; }

  public boolean    hasScope ( )     { return ( getScopeCount() > 0 );  }

  public int        getScopeCount( ) { return getScopes() . count(); }

  public String getScopeString ( int theIndex )
  {
    if (   ( theIndex >= 0                 )
	&& ( theIndex <  getScopeCount() ) )
      return getScopes() . elementAt ( theIndex ) . toString();
    else
    {
      System.err.println ( "[DataScope:getScopeString]  Warning: "
			   + " Invalid index (" + theIndex + ").  ["
			   + getScopeCount() + "]" );
      return null;
    }
  }

  public String getScopeString  ( ) { return getAllScopeStrings(); }
  public String getScopeStrings ( ) { return getAllScopeStrings(); }

  public String getAllScopeStrings ()
  {
    StringBuffer  stringBuffer = new StringBuffer ( 1000 /* Arbitrary size */);

    for ( int scopeCount = 0; scopeCount < getScopeCount(); scopeCount++ )
    {
      stringBuffer . append ( getScopeString ( scopeCount ) );
      stringBuffer . append ( DataScope.SCOPE               );
    }

    return stringBuffer . toString();
  }

  public void writeScope ( DataDestination theOutputDestination )
  {
    for ( int scopeCount = 0; scopeCount < getScopeCount(); scopeCount++ )
    {
      theOutputDestination . write ( getScopeString ( scopeCount ) );
      theOutputDestination . write ( DataScope.SCOPE               );
    }
  }


  public void addScope ( String theScope )
  {
    getScopes() . addElement ( theScope );
  }

  public void clearAllScopes()
  {
    getScopes() . removeAllElements();
  }


  public void generate ( DataDestination  theOutputDestination,
			 int              theObjectSubsetToGenerate )
  {
      	/* Print a warning if necessary... */
    warnIfInvalidObjectSubset ( theObjectSubsetToGenerate, "generate" );

    	/* Initialize us to generate non-significant tokens... */
    initializeGenerateSubcomponentIndex();

    for ( int scopeCount = 0; scopeCount < getScopeCount(); scopeCount++ )
    {
      generateSubcomponents ( DataScope.IDENTIFIER_INDEX + scopeCount,
			      theOutputDestination,
			      theObjectSubsetToGenerate, false );

      theOutputDestination . write ( getScopeString ( scopeCount ) );

      generateSubcomponents ( DataScope.SCOPE + scopeCount,
			      theOutputDestination,
			      theObjectSubsetToGenerate, false );

      theOutputDestination . write ( DataScope.SCOPE );
    }

	/* Write any remaining non-significant tokens */
    generateAllRemainingSubcomponents ( theOutputDestination,
					theObjectSubsetToGenerate, false );
  }

}
