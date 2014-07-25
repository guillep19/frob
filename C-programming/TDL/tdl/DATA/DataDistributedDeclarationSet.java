
/*
 * This represents:
 *      TYPE  declaration-1, declaration-2, declaration-3, ... ;
 *
 * The TYPE and declaration's are represented by DataTaskArguments,
 * for programmer convenience...
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 *
 */

public class DataDistributedDeclarationSet extends DataComponent
{
	/* Class Variables */
  public final static String   DECLARATION_INDEX  = "DECLARATION_";
  public final static String   COMMA              = ",";
  public final static String   SEMICOLON          = ";";


	/* Instance Variables */
  protected DataVector  taskArgumentsVector;

	/* Instance Methods */
  public DataDistributedDeclarationSet()
  {
    taskArgumentsVector = new DataVector ( 10 /* Arbitrary number */ );
  }

  protected DataVector getTaskArgumentsVector() { return taskArgumentsVector; }

  public void clear()
  {
    getTaskArgumentsVector() . removeAllElements();
  }

  public DataTaskArgument getFirstDeclarationCreatingIfNecessary()
  {
    if ( getTaskArgumentsVector() . size() <= 0 )
      getTaskArgumentsVector() . addElement ( new DataTaskArgument() );

    return (DataTaskArgument) ( getTaskArgumentsVector() . elementAt ( 0 ) );
  }

  public DataTaskArgument createNextDeclaration()
  {
    DataTaskArgument returnValue = new DataTaskArgument();

    getTaskArgumentsVector() . addElement ( returnValue );

    returnValue . setDistributedType (
      getFirstDeclarationCreatingIfNecessary() . getDistributedType() );

    returnValue . setDistributedId (
      getFirstDeclarationCreatingIfNecessary() . getDistributedId() );

    returnValue . setDistributedStructNotSubcomponent (
      getFirstDeclarationCreatingIfNecessary() . getDataDistributedStruct() );

    returnValue . setDistributedEnumNotSubcomponent (
      getFirstDeclarationCreatingIfNecessary() . getDataDistributedEnum() );

    return returnValue;
  }

  public DataTaskArgument getLastExistingDeclaration()
  {
    return (DataTaskArgument) ( getTaskArgumentsVector() . lastElement() );
  }

  public DataTaskArgument getTaskArgument ( int theIndex )
  {
    if ( (theIndex >= 0)  &&  (theIndex < getTaskArgumentsVector().size()) )
      return
	(DataTaskArgument) ( getTaskArgumentsVector() . elementAt (theIndex) );
    else
    {
      System.err.println ( "[DataDistributedDeclarationSet:getTaskArgument]  "
			   + "Internal Error:  Invalid index (" + theIndex
			   + ").  Only " + getTaskArgumentsVector().size()
			   + " DataTaskArgument's are in this object." );
      return null;
    }
  }

  public boolean getDistributedFormatRequiresDynamicString()
  {
    for ( int i = 0;   i < getTaskArgumentsVector() . size();   i++ )
    {
      if ( getTaskArgument ( i )
	     . getDistributedFormatRequiresDynamicString() )
	return true;
    }
    return false;
  }


  public void generate ( DataDestination  theOutputDestination,
			 int              theObjectSubsetToGenerate )
  {
       	/* Print a warning if necessary... */
    warnIfInvalidObjectSubset ( theObjectSubsetToGenerate, "generate" );

    initializeGenerateSubcomponentIndex();

    for ( int i=0;   i < getTaskArgumentsVector() . size();   i++ )
    {
      generateSubcomponents (
			  DataDistributedDeclarationSet.DECLARATION_INDEX + i,
			  theOutputDestination,
			  theObjectSubsetToGenerate, false );

      getTaskArgument ( i ) . generate ( theOutputDestination,
					 theObjectSubsetToGenerate );

      if ( (i+1) < getTaskArgumentsVector() . size() )
	theOutputDestination
	  . write ( DataDistributedDeclarationSet.COMMA );
      else
	theOutputDestination
	  . write ( DataDistributedDeclarationSet.SEMICOLON );
    }

    generateAllRemainingSubcomponents ( theOutputDestination,
					theObjectSubsetToGenerate, false );
  }

}
