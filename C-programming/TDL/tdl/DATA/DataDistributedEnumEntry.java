
/*
 * This represents one entry in a distributed enum.
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 *
 */

public class DataDistributedEnumEntry extends DataComponent
{
	/* Class variables */
  public final static String   ID           = "ID";
  public final static String   EQUALS       = "=";
  public final static String   EQUALS_VALUE = "VALUE";

	/* Instance Variables */
  protected String           id;
  protected String           positiveIntegerEqualsString;
  protected DataExpression   generalEqualsExpression;
  protected boolean          usingPositiveIntegerEqualsString;


	/* Instance Methods */
  public DataDistributedEnumEntry()
  {
    id                               = null;
    positiveIntegerEqualsString      = null;
    generalEqualsExpression          = null;
    usingPositiveIntegerEqualsString = true;
  }

  public String getId() { return id; }
  public void   setId ( String theId ) { id = theId; }

  public String          getPositiveIntegerEqualsString()
				   { return positiveIntegerEqualsString; }

  public DataExpression  getGeneralEqualsExpression()
				   { return generalEqualsExpression; }

  public boolean         getUsingPositiveIntegerEqualsString()
				   { return usingPositiveIntegerEqualsString; }


  public void    setPositiveIntegerEqualsString ( String theEqualsString )
  {
    positiveIntegerEqualsString      = theEqualsString;
    usingPositiveIntegerEqualsString = true;
  }

  public void    setGeneralEqualsExpression ( DataExpression theExpression )
  {
    generalEqualsExpression          = theExpression;
    usingPositiveIntegerEqualsString = false;
  }


  public boolean hasEquals()
  {
    if ( getUsingPositiveIntegerEqualsString() == true )
      return getPositiveIntegerEqualsString()  != null;
    else
      return getGeneralEqualsExpression()      != null;
  }


  public void generate ( DataDestination  theOutputDestination,
			 int              theObjectSubsetToGenerate )
  {
       	/* Print a warning if necessary... */
    warnIfInvalidObjectSubset ( theObjectSubsetToGenerate, "generate" );

    initializeGenerateSubcomponentIndex();


	/* Write out ID */
    generateSubcomponents ( DataDistributedEnumEntry.ID, theOutputDestination,
			    theObjectSubsetToGenerate, false );
    theOutputDestination . write ( getId() );

	/* Deal with equals */
    if ( hasEquals() == true )
    {
      generateSubcomponents ( DataDistributedEnumEntry.EQUALS,
			      theOutputDestination,
			      theObjectSubsetToGenerate, false );
      theOutputDestination . write ( DataDistributedEnumEntry.EQUALS );



      generateSubcomponents ( DataDistributedEnumEntry.EQUALS_VALUE,
			      theOutputDestination,
			      theObjectSubsetToGenerate, false );

      if ( getUsingPositiveIntegerEqualsString() == true )
      {
	theOutputDestination . write ( getPositiveIntegerEqualsString() );
      }
      else
      {
	getGeneralEqualsExpression() . generate ( theOutputDestination,
						  theObjectSubsetToGenerate );
      }
    } /* if ( hasEquals() == true ) */

	/* Deal with any leftovers. */
    generateAllRemainingSubcomponents ( theOutputDestination,
					theObjectSubsetToGenerate, false );
  }

}
