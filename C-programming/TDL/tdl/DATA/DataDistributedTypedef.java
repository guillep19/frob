
/*
 * This represents:
 *      DISTRIBUTED typedef declaration-set
 *
 * Uses DataTaskArgument to hold the distributed-type
 * via DataDistributedDeclarationSet.
 *
 * DataDistributedDeclarationSet also contains the trailing semicolon,
 * to facilitate correct behavior in the parsing engine.
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 *
 */

public class DataDistributedTypedef extends DataComponent
{
	/* Class Variables */
  public final static String  DISTRIBUTED = "DISTRIBUTED";
  public final static String  TYPEDEF     = "typedef";
  public final static String  DECLARATION = "DECLARATION";

	/* Instance Variables */
  protected DataDistributedDeclarationSet  dataDistributedDeclarationSet;

	/* Instance Methods */
  public DataDistributedTypedef()
  {
    dataDistributedDeclarationSet = null;
  }

  public DataDistributedDeclarationSet getDataDistributedDeclarationSet()
           { return dataDistributedDeclarationSet; }
  public void setDataDistributedDeclarationSet (
					 DataDistributedDeclarationSet theSet )
	   { dataDistributedDeclarationSet = theSet; }



  public void generate ( DataDestination  theOutputDestination,
			 int              theObjectSubsetToGenerate )
  {
    int i;

       	/* Print a warning if necessary... */
    warnIfInvalidObjectSubset ( theObjectSubsetToGenerate, "generate" );

	/* Code/html is a no-op. */
    if (   ( isCxxCodeSubset ( theObjectSubsetToGenerate )                 )
	|| ( theObjectSubsetToGenerate == DataComponent.HTML_DOCUMENTATION ) )
      return;

    initializeGenerateSubcomponentIndex();


	/* Write out DISTRIBUTED */
    generateSubcomponents ( DataDistributedTypedef.DISTRIBUTED,
			    theOutputDestination,
			    theObjectSubsetToGenerate, false );

    if ( theObjectSubsetToGenerate != ENTIRE_OBJECT )
      theOutputDestination . write ( "/*" );

    theOutputDestination . write ( DataDistributedTypedef.DISTRIBUTED );

    if ( theObjectSubsetToGenerate != ENTIRE_OBJECT )
      theOutputDestination . write ( "*/" );



	/* Write out TYPEDEF */
    generateSubcomponents ( DataDistributedTypedef.TYPEDEF,
			    theOutputDestination,
			    theObjectSubsetToGenerate, false );

    theOutputDestination . write ( DataDistributedTypedef.TYPEDEF );



	/* Write out DataDistributedDeclarationSet */
    generateSubcomponents ( DataDistributedTypedef.DECLARATION,
			    theOutputDestination,
			    theObjectSubsetToGenerate, false );

    getDataDistributedDeclarationSet() . generate ( theOutputDestination,
						    theObjectSubsetToGenerate);

	/* Deal with and leftovers. */
    generateAllRemainingSubcomponents ( theOutputDestination,
					theObjectSubsetToGenerate, false );


	/* If this is a header file, create the format #defs */
    if ( isCxxHeaderSubset ( theObjectSubsetToGenerate ) )
    {
	/* Start with a clean (cleared) list of macros that we  *
	 * are verifying the existence of via #ifndef #error... */
      DataTaskArgument.writeDistributedMacroRequirements( theOutputDestination,
							  null, true );
      DataTaskArgument.writeDistributedMacroRequirements (
	theOutputDestination,
	getDataDistributedDeclarationSet() . getTaskArgument ( 0 ) );


      for ( i = 0;
	    i < getDataDistributedDeclarationSet()
		  . getTaskArgumentsVector()
		  . size();
	    i++ )
      {
	if ( theOutputDestination . getColumn() != 0 )
	  theOutputDestination . write ( "\n" );      

	theOutputDestination . write ( "#define " );

	theOutputDestination . write ( getDataDistributedDeclarationSet()
				         . getTaskArgument ( i )
				         . getArgumentName()     );

	theOutputDestination
	  . write ( DataComponent.CXX_DISTRIBUTED_FORMAT_TRAILER );

	theOutputDestination . write ( " " );

	if ( getDataDistributedDeclarationSet()
	       . getTaskArgument ( i )
	       . getDistributedFormatRequiresDynamicString() )
	  theOutputDestination . write ( "( " );

	theOutputDestination . setIsConcatenatingStrings ( true );

	theOutputDestination . write (
	  DataTaskArgument.getDistributedFormatString (

	    getDataDistributedDeclarationSet() . getTaskArgument ( i ),

	    this,

	    "Distributed Typedef \""
	    + getDataDistributedDeclarationSet()
	        . getTaskArgument ( i )
	        . getArgumentName()
	    + "\""

	    ) );

	theOutputDestination . setIsConcatenatingStrings ( false );

	if ( getDataDistributedDeclarationSet()
	       . getTaskArgument ( i )
	       . getDistributedFormatRequiresDynamicString() )
	  theOutputDestination . write ( " )" );

      } /* FOR ( 0 <= i < number of DataTaskArguments ) */


	/*********************************/
	/* Deal with embeded constructs. */
	/*********************************/
      DataTaskArgument  dataTaskArgument = null;
      int               distributedType  = DataComponent.INVALID_INDEX;
      if (   getDataDistributedDeclarationSet()
	       . getTaskArgumentsVector()
	       . size()
	   > 0 )
      {
	dataTaskArgument = getDataDistributedDeclarationSet()
			     . getTaskArgument ( 0 );
	distributedType  = dataTaskArgument . getDistributedType();
      }

      if ( distributedType == DataTaskArgument.DATA_DISTRIBUTED_STRUCT )
      {
	if ( dataTaskArgument . getDataDistributedStruct() != null )
	{
	  dataTaskArgument
	    . getDataDistributedStruct()
	    . generateFormatDefineMacro ( theOutputDestination, null, null,
					  false /* theClearMacrosSoFar */  );

	  for ( i = 0;
		i < getDataDistributedDeclarationSet()
		      . getTaskArgumentsVector()
		      . size();
		i++ )
	  {
	    dataTaskArgument
	      . getDataDistributedStruct()
	      . generateFormatDefineMacro( theOutputDestination,
					   null,
					   getDataDistributedDeclarationSet()
					     . getTaskArgument ( i )
					     . getArgumentName(),
					   false /* theClearMacrosSoFar */ );
	  }
	}
	else
	{
	  System.err.println (
	    "[DataDistributedTypedef:generateFormatDefineMacro]  Warning: "
	      + "Internal programmer error encountered:  No "
	      + "DataDistributedStruct object available for struct." );
	}
      }

      else if ( distributedType == DataTaskArgument.DATA_DISTRIBUTED_ENUM )
      {
	if ( dataTaskArgument . getDataDistributedEnum() != null )
	{
	  dataTaskArgument
	    . getDataDistributedEnum()
	    . generateFormatDefineMacro ( theOutputDestination, null, null,
					  false /* theClearMacrosSoFar */  );
	}
	else
	{
	  System.err.println (
	    "[DataDistributedTypedef:generateFormatDefineMacro]  Warning: "
	      + "Internal programmer error encountered:  No "
	      + "DataDistributedEnum object available for enum." );
	}
      }

      else
      {
	theOutputDestination . write ( "\n" );
      }

    } /* if ( isCxxHeaderSubset ( theObjectSubsetToGenerate ) ) */
  }

}
