
/*
 * This is an abstract class.
 * It's used to refer to statements (of any kind) in a generic way.
 * It also stores anything common to all of the statement classes,
 * such as common functionality, Labels, and the postLabelString (which
 * is used to generated CXX spawn-destroys inside switch statements).
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 *
 */

public abstract class DataStatement extends DataComponent
				 implements DataValidateCode
{
	/* Class Constants */
  public static int  FOUND_NOTHING                 = 0;
  public static int  FOUND_SPAWN_RELATED_STATEMENT = 1;
  public static int  FOUND_END                     = 2;


	/* Class methods */


//////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////


	/* Used by DataTaskDefintion for on-agent (Local/Either/Distributed)
	 *  and below when printing overridden warnings.
	 *
	 * Searches for a constraint statement of the specified type that is
	 *      (*) Not conditionalized.  (Always applied to Spawn.)
	 *  And (*) Applies to all iteration indexes.
	 *
	 * (We are looking to deterministically say whether a particular
	 *  constraint is being applied to a particular Task.  Specifically
	 *  for use with ON-AGENT constraints.  Or for reporting particular
	 *  error messages.)
	 */
  public static boolean hasNonconditionalConstraintOfType (
					DataSpawnTask     theDataSpawnTask,
					int               theConstraintType,
					DataVector        theTaskStatements  )
  {
    int                   i;
    DataComponent         dataComponent;
    DataWithDoStatement   dataWithDoStatement;

    for ( i = 0;  i < theDataSpawnTask.getConstraintCount();  i++ )
    {
      if (    theDataSpawnTask . getConstraint ( i ) . getConstraintType()
	   == theConstraintType )
	return true;
    }

    for ( dataComponent  = theDataSpawnTask . getParent();
	  dataComponent != null;
	  dataComponent  = dataComponent . getParent() )
    {
      if ( dataComponent instanceof DataWithDoStatement )
      {
	dataWithDoStatement = (DataWithDoStatement) dataComponent;

	for ( i = 0;   i < dataWithDoStatement . getConstraintCount();   i++ )
	{
	  if (    dataWithDoStatement . getConstraint (i) . getConstraintType()
	       == theConstraintType )
	    return true;
	}
      }
    }

    return false;
  }


	/* This is used by DataSpawnStatement, DataWithStatement
	 * and DataTaskDefinition...   So it also goes here too.
	 */
  public static void validateAnyOverridenConstraints (
				 DataComponent               theDataComponent,
			         DataValidateCodeReturnValue theReturnValue   )
  {
    return;
  }

//////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////




	/* This is used by DataSpawnTask & DataWithDoStatement.
	 * Without multiple inheritance, it needs to go someplace
	 * common to both.  So it will live here.
	 */
  public static void validateInternalConstrainedObject (
			     int                         theReference,
			     DataValidateCodeReturnValue theReturnValue,
			     DataConstrainedObject       theConstrainedObject )
  {
    boolean                 hasParallel        = false,
                            hasSerial          = false,
                            hasSequential      = false;
    DataComponent           serialLocation     = null,
                            sequentialLocation = null;
    int                     i;

    for ( i=0;   i < theConstrainedObject . getConstraintCount();   i++ )
    {
      if ( theConstrainedObject . getConstraint ( i )
	                        . getHasEventTagTaskOfPrevious() )
      {
	switch ( theConstrainedObject . getConstraint ( i )
                                      . getConstraintType() )
	{
	  case DataConstraint.SERIAL:
	    hasSerial = true;
	    serialLocation = theConstrainedObject . getConstraint ( i );
	    break;

	  case DataConstraint.SEQUENTIAL_HANDLING:
	  case DataConstraint.SEQUENTIAL_EXPANSION:
	  case DataConstraint.SEQUENTIAL_EXECUTION:
	    hasSequential = true;
	    sequentialLocation = theConstrainedObject . getConstraint ( i );
	    break;

	    /* Ignore the other "PREVIOUS" constraints for now... */

	} /* switch ( getConstraint ( i ) . getConstraintType() ) */
      } /* if ( getConstraint ( i ) . getHasEventTagTaskOfPrevious() */

      if (    theConstrainedObject . getConstraint ( i ) . getConstraintType()
	   == DataConstraint.PARALLEL )
      {
	hasParallel = true;
      }
    } /* FOR ( 0 <= i < theConstrainedObject . getConstraints() . count() ) */

    if ( hasParallel && hasSerial )
    {
      theReturnValue
	. addWarning ( serialLocation )
	. write ( "SERIAL constraint overrides PARALLEL constraint.\n" );
    }

    if ( hasParallel && hasSequential )
    {
      theReturnValue
	. addWarning ( sequentialLocation )
	. write ( "SEQUENTIAL constraint overrides PARALLEL constraint.\n");
    }
  }


	/* Used by DataConstraint and DataConstraintStatement. */
  public static void generateTagTaskCxxReference (
					DataDestination  theOutputDestination,
					String           theTagTaskName,
					Object[]         theTagTaskIndexes,
					DataComponent    theDataComponent  )
    throws CompilationException
  {
    DataTaskDefinition parentTask;
    DataComponent      tagTaskDataComponent;
    int                iterationCount;
    int                indent;

	/* Idiocy checks */
    if ( theDataComponent == null )
    {
      throw new CompilationException (
			"Line:??: Programmer Error:  Missing DataComponent." );
    }

    if ( DataComponent.isEmptyString ( theTagTaskName ) == true )
    {
      throw new CompilationException (
				 theDataComponent.getMessageFilenameLead()
			       + theDataComponent.getLineNumberString()
			       + ":  Error:  Missing Task-Tag." );
    }

	/* Find our DataTaskDefinition Ancestor */
    parentTask = theDataComponent . getParentTaskDefinition();
    if ( parentTask == null )
    {
      throw new CompilationException (
	    theDataComponent.getMessageFilenameLead()
	  + theDataComponent.getLineNumberString() + ":  "
	  + "Programmer Error:  Unable to find DataTaskDefinition ancestor." );
    }

	/* Find the tagTaskDataComponent */
    tagTaskDataComponent
      = parentTask . getDataComponentWithName ( theTagTaskName );
    if ( tagTaskDataComponent == null )
    {
      throw new CompilationException (
	      theDataComponent.getMessageFilenameLead()
	    + theDataComponent.getLineNumberString()
	    + ":  Programmer Error:  Unable to find tag-Task-DataComponent." );
    }

	/* Find out how many iteration loops enclose the tagTask Object */
	/* Note:  This can throw a CompilationException.                */
    iterationCount
      = parentTask . getIterationParentCount ( tagTaskDataComponent );
    
	/* Idiocy check */
    if (   ( theTagTaskIndexes        != null          )
	&& ( theTagTaskIndexes.length > iterationCount ) )
    {
      throw new CompilationException (
	       theDataComponent.getMessageFilenameLead()
	     + theDataComponent.getLineNumberString()
	     + ":  Error:  Too many iteration indexes for Task/With Reference."
	     + "  (Task/With is enclosed in " + iterationCount
	     + " iteration loop(s) and there is (are) "
	     + theTagTaskIndexes.length + " array index(es) specified.)" );
    }


	/* Generate the CXX Tag Task Reference. */
    theOutputDestination
      . write ( DataComponent.CXX_TDL_TASK_HANDLER_INSTANCE );
    theOutputDestination . write ( " [ \"" );
    theOutputDestination . write ( theTagTaskName );
    theOutputDestination . write ( "\" ]" );

	/* Indent remaining text... */
    theOutputDestination . addIndent ( DataComponent.STANDARD_INDENT );


    if ( iterationCount <= 0 ) 
    {
      theOutputDestination . write ( "\n. " );
      theOutputDestination
	. write ( DataComponent.CXX_TDL_CACHE_DESCEND_INTO_NODE );
      theOutputDestination . write ( " ( " );
      theOutputDestination
	. write ( DataComponent.CXX_TDL_DEFAULT_ARRAY_INDEX );
      theOutputDestination . write ( " )" );

    }

    else if (   ( theTagTaskIndexes        == null )
	     || ( theTagTaskIndexes.length <= 0    ) )
    {
      theOutputDestination . write ( "\n. " );
      theOutputDestination
	. write ( DataComponent.CXX_TDL_GET_CACHED_TOPMOST_TREE_NODE_BRANCH );
      theOutputDestination . write ( " ()" );
    }

    else
    {
      for ( int i = 0;  i < theTagTaskIndexes.length;  i++ )
      {
	theOutputDestination . write ( "\n. " );

	if ( i >= ( iterationCount - 1 ) )
	{
	  theOutputDestination
	    . write ( DataComponent.CXX_TDL_CACHE_DESCEND_INTO_NODE );
	}
	else
	{
	  theOutputDestination
	    . write ( DataComponent.CXX_TDL_CACHE_DESCEND_INTO_BRANCH );
	}

	theOutputDestination . write ( " ( " );

	if (   ( theTagTaskIndexes [ i ] instanceof String               )
	    && ( theTagTaskIndexes [ i ] . equals (
		   DataConstraintTagTaskIndexes.DEFAULT_TAG_TASK_INDEX ) ) )
	{
	  theOutputDestination
	    . write ( DataComponent.CXX_TDL_TASK_HANDLER_INSTANCE);
	  theOutputDestination . write ( " . ");
	  theOutputDestination
	    . write ( DataComponent.CXX_TDL_GET_CURRENT_ARRAY_INDEX );
	  theOutputDestination . write ( " ( " );
	  theOutputDestination . write ( "" + i );
	  theOutputDestination . write ( " )" );
	}
	else
	{
		/* Indent this properly */
	  indent = theOutputDestination . indentToCurrentColumn();

	  if ( theTagTaskIndexes [ i ] instanceof DataComponent )
	  {
	    if ( theOutputDestination . getEnableLineMacros() == true )
	    {
	      theOutputDestination . setUsingTdlFileName ( true );
	      theOutputDestination
		. makeNextLineNumber( ((DataComponent) (theTagTaskIndexes [i]))
				          . getLineNumber() );
	      theOutputDestination . write ( "\n" ); /* Flush #line macro */
	    }

	    theOutputDestination . setStripLeadingWhitespace();
	    theOutputDestination . setPersistentlyStripLeadingSpaces ( true );

	    ((DataComponent) (theTagTaskIndexes [ i ]))
	      . generate ( theOutputDestination, DataComponent.ENTIRE_OBJECT );

	    theOutputDestination . setPersistentlyStripLeadingSpaces ( false );

	    if ( theOutputDestination . getEnableLineMacros() == true )
	    {
	      theOutputDestination . setUsingTdlFileName ( false );
	      theOutputDestination . write ( "\n" ); /* Flush #line macro */
	    }
	  }
	  else
	  {
	    theOutputDestination
	      . write ( theTagTaskIndexes [ i ] . toString() );
	  }

		/* And we can stop indenting this now... */
	  theOutputDestination . removeIndent ( indent );
	}

	theOutputDestination . write ( " )" );
      } /* for ( int i = 0;  i < theTagTaskIndexes.length;  i++ ) */

      if ( theTagTaskIndexes.length < iterationCount )
      {
	theOutputDestination . write ( "\n. " );
	theOutputDestination
	  . write ( DataComponent.CXX_TDL_GET_CACHED_TREE_NODE );
	theOutputDestination . write ( " ()" );
      }
    }

	/* Stop indenting... */
    theOutputDestination . removeIndent ( DataComponent.STANDARD_INDENT );
  }




	/* Instance variables */
  protected DataLabelStatement  label;
  protected String              postLabelString;

	/* Instance methods */
  public DataStatement ( )
  {
    label           = null;
    postLabelString = null;
  }

  public DataLabelStatement getLabel ( ) { return label; }
  public void               setLabel ( DataLabelStatement theLabel )
  {
    if ( label != null )
      label . setParent ( null );

    label = theLabel;

    if ( label != null )
      label . setParent ( this );
  }

  public boolean hasCaseOrDefaultLabel()
  {
    for ( DataLabelStatement  currentLabel = getLabel();
	  currentLabel != null;
	  currentLabel = currentLabel . getLabel() )
    {
	/* There are three kinds of labels.  ID, case, & default. *
	 * Neither case nor default labels have an id...          */
      if ( currentLabel . hasId() == false )
	return true;
    }
    return false;
  }

	/* Note:  Overriden by DataSpawnTask() */
  public boolean hasName ( String theName )
  {
    for ( DataLabelStatement  currentLabel = getLabel();
	  currentLabel != null;
	  currentLabel = currentLabel . getLabel() )
    {
      if (   ( currentLabel . hasId() == true                   )
	  && ( currentLabel . getId() . equals ( theName ) ) )
	return true;
    }    
    return false;
  }


  public String getPostLabelString ( ) { return postLabelString; }
  public void   setPostLabelString ( String thePostLabelString )
				       { postLabelString = thePostLabelString;}
  public void   eraseAllPostLabelStrings ( )
  {
    setPostLabelString ( null );

	/* Descend (recursively, depth-first) into children */
    for ( int i=0;  i < getChildStatementCount();  i ++ )
    {
      getChildStatement ( i ) . eraseAllPostLabelStrings();
    }
  }


  public boolean isSubcomponentADataStatement ( int theIndex )
  {
    return (     isSubcomponentADataComponent ( theIndex )
	    && ( getDataComponentSubcomponent ( theIndex )
		 instanceof DataStatement
	        )
	    );
  }

  public DataStatement getDataStatementSubcomponent ( int theIndex )
  {
    if ( isSubcomponentADataStatement ( theIndex ) )
      return (DataStatement) getSubcomponent ( theIndex );
    else
    {
      System.err.println (
		 "[DataStatement:getDataStatementSubcomponent]  Warning:  "
		 + "Subcomponent ( " + theIndex
		 + " ) is NOT a DataStatement!  It is a \""
		 + getSubcomponent ( theIndex ) . getClass() . getName()
		 + "\"." );
      return null;
    }
  }


  public void validateExternalCode( int                         theReference,
				    DataValidateCodeReturnValue theReturnValue)
    throws CompilationException
  {
    throw new CompilationException ( "Internal Error:  DataStatement "
				   + "object may not exist outside a Task." );
  }


	/* Validates code that is inside of a Task */
  public void validateInternalCode( int                         theReference,
				    DataValidateCodeReturnValue theReturnValue)
    throws CompilationException
  {
    	/* Check for case/default labels... */
    for ( DataLabelStatement  currentLabel = getLabel();
	  currentLabel != null;
	  currentLabel = currentLabel . getLabel() )
    {
	/* If case or default label. */
      if ( currentLabel . hasId() == false )
      {
	if (   (    ( getParent() instanceof DataSwitchStatement   ) == false )
	    && (   (( getParent() instanceof DataCompoundStatement ) == false)
		|| (( getParent() . getParent()
		                  instanceof DataSwitchStatement   ) == false))
	    )
	{
	  theReturnValue
	    . addError ( currentLabel )
	    . write ( "Case or Default label is not contained by an immediate "
		      + "switch statement.\n" );
	}
	break; /* After the first case/default label, exit our for loop. */
      }
    }

    	/* Descend (recursively, depth-first) into children */
    for ( int i=0;  i < getChildStatementCount();  i ++ )
    {
      getChildStatement ( i )
	. validateInternalCode ( theReference, theReturnValue );
    }
  }





	/* Used to generate subsequent-to-first-token line numbers */
  public void generateRelevantLineNumberMacros (
				    DataDestination  theOutputDestination,
				    int              theObjectSubsetToGenerate,
				    int              theLineNumber,
				    String           theNextStopIndex )

  {
    generateRelevantLineNumberMacros (
		     theOutputDestination, theLineNumber,
		     countSubcomponentNewlines ( theNextStopIndex,
						 theObjectSubsetToGenerate,
						 false /* Don't stop at first*
						        * non-whitespace char*/
						) );
				       
  }

  public void generateRelevantLineNumberMacros (
					 DataDestination  theOutputDestination,
					 int              theLineNumber,
					 int              theNewlineCount )
  {
	/* Don't write out extra newlines if we aren't using line macros */
    if ( theOutputDestination . getEnableLineMacros() == false )
      return;

	/* There's no TDL-file equivalent if we have no line numbers */
    if ( DataComponent.isValidLineNumber ( theLineNumber ) == false )
    {
        /* If necessary, makeNextLineNumber() will be set automatically here.*/
      theOutputDestination . setUsingTdlFileName ( false );
    }
    else
    {
      theOutputDestination . setUsingTdlFileName ( true );

      if ( theNewlineCount <= 0 )
      {
	theOutputDestination . makeNextLineNumber (   theLineNumber
						    - theNewlineCount );
      }
      else
      {
	theOutputDestination . makeNextLineNumber (   theLineNumber
						    - theNewlineCount
						    + 1 /*After next newline*/
						   );
      }
    } /* IF ( DataComponent.isValidLineNumber ( theLineNumber ) */


    if ( theNewlineCount <= 0 )
    {
	    /* Case where we're already where we want to be. */
      if (   (    theOutputDestination . hasMakeNextLineNumber() )
	  && (    theOutputDestination . getAdjustedCurrentRow()
	       == theOutputDestination . getMakeNextLineNumber() ) )
      {
	theOutputDestination . clearMakeNextLineNumber();
      }
      else
      {
	    /* Handle stripping leading whitespace gracefully */
	boolean  wasStrippingLeadingWhitespace
		   = theOutputDestination . getIsStrippingLeadingWhitespace();

	theOutputDestination . setStripLeadingWhitespace ( false );

	theOutputDestination . write ( "\n" );

	    /* Restore things back however they were. */
	theOutputDestination
	  . setStripLeadingWhitespace ( wasStrippingLeadingWhitespace );
      }
    } /* IF ( theNewlineCount <= 0 ) */
  }


	/* Note: theExtraLeadingNewlines are only used if there are NO labels,
	 *       And typically when there's a leading subcomponent, such as a
	 *       DataExpression object, that can contain leading whitespace.
	 */
  public void generateRelevantLineNumberMacros (
				    DataDestination  theOutputDestination,
				    int              theObjectSubsetToGenerate,
				    int              theLabelSubsetToGenerate,
				    int              theFirstStopIndex,
				    int              theExtraLeadingNewlines,
				    boolean          theIsPartOfTdlFile,
				    boolean          theIncludeLabels )
  {
    int  lineNumber, newlineCount;


	/* Don't write out extra newlines if we aren't using line macros */
    if ( theOutputDestination . getEnableLineMacros() == false )
      return;


    if ( theIsPartOfTdlFile == false )
    {
	/* This may not ever actually get invoked. */
      if (   ( theOutputDestination . getEnableLineMacros() == true )
	  && ( theOutputDestination . getUsingTdlFileName() == true ) )
      {
	theOutputDestination . setUsingTdlFileName ( false );
	theOutputDestination . write ( "\n" ); /* Flush #line macro */
      }
      return;
    }

	/* Else: theIsPartOfTdlFile == true */

    if (   ( getLabel()       != null )
	&& ( theIncludeLabels == true ) )
    {
      lineNumber = getLabel() . getLineNumber();
    }
    else
    {
      lineNumber = getLineNumber();
    }


	/* Handle stripping leading whitespace gracefully */
    if ( theOutputDestination . getIsStrippingLeadingWhitespace() )
    {
      newlineCount = 0;
    }
    else if (   ( getLabel()       != null )
	     && ( theIncludeLabels == true ) )
    {
      newlineCount
	=   countSubcomponentNewlines ( DataComponent.FIRST_TOKEN_INDEX,
					theObjectSubsetToGenerate,
					false /* Don't stop at first *
					       * non-whitespace char */ )
	  + getLabel() . countSubcomponentNewlines (
				        DataLabelStatement.FIRST_TOKEN_INDEX,
					theLabelSubsetToGenerate,
					false /* Don't stop at first *
					       * non-whitespace char */ );
    }
    else
    {
      newlineCount
	=   theExtraLeadingNewlines
	  + countSubcomponentNewlines (
				Math.max (
				  getIndex ( DataComponent.FIRST_TOKEN_INDEX ),
				  theFirstStopIndex ),
				theObjectSubsetToGenerate,
				false /* Don't stop at first *
				       * non-whitespace char */ );
    }

    generateRelevantLineNumberMacros ( theOutputDestination,
				       lineNumber, newlineCount );
  }


	/* Convenience method */
  public void generateLabels ( DataDestination  theOutputDestination,
			       int              theObjectSubsetToGenerate,
			       String           theFirstStopIndex )
  {
    generateLabels ( theOutputDestination,
		     theObjectSubsetToGenerate,
		     theObjectSubsetToGenerate,
		     getIndex ( theFirstStopIndex ),
		     0,
		     true  /* This is TDL (not autogenerated C++) code. */,
		     false /* Include line Number Macros */ );
  }

	/* Convenience method */
  public void generateLabels ( DataDestination  theOutputDestination,
			       int              theObjectSubsetToGenerate,
			       String           theFirstStopIndex,
			       boolean          theDisableLineNumberMacros )
  {
    generateLabels ( theOutputDestination,
		     theObjectSubsetToGenerate,
		     theObjectSubsetToGenerate,
		     getIndex ( theFirstStopIndex ),
		     0,
		     true  /* This is TDL (not autogenerated C++) code. */,
		     theDisableLineNumberMacros );
  }

	/* Convenience method */
  public void generateLabels ( DataDestination  theOutputDestination,
			       int              theObjectSubsetToGenerate,
			       int              theLabelSubsetToGenerate,
			       String           theFirstStopIndex )
  {
    generateLabels ( theOutputDestination,
		     theObjectSubsetToGenerate,
		     theLabelSubsetToGenerate,
		     getIndex ( theFirstStopIndex ),
		     0,
		     true  /* This is TDL (not autogenerated C++) code. */,
		     false /* Include line Number Macros */ );
  }

	/* Convenience method */
  public void generateLabels (
		     DataDestination  theOutputDestination,
		     int              theObjectSubsetToGenerate,
		     String           theFirstStopIndex,
		     DataComponent    theExtraNewlinesComponent,
		     String           theExtraNewlinesComponentFirstStopIndex )
  {
    generateLabels ( theOutputDestination,
		     theObjectSubsetToGenerate,
		     theObjectSubsetToGenerate,
		     getIndex ( theFirstStopIndex ),
		     (   ( theExtraNewlinesComponent != null )
		       ? ( theExtraNewlinesComponent
			     . countSubcomponentNewlines (
				       theExtraNewlinesComponentFirstStopIndex,
				       theObjectSubsetToGenerate,
				       false /* Don't stop at first *
					      * non-whitespace char */ ) )
		       : 0 ),
		     true  /* This is TDL (not autogenerated C++) code. */,
		     false /* Include line Number Macros */ );
  }


  public void generateLabels ( DataDestination  theOutputDestination,
			       int              theObjectSubsetToGenerate,
			       int              theLabelSubsetToGenerate,
			       int              theFirstStopIndex,
			       int              theExtraLeadingNewlines,
			       boolean          theIsPartOfTdlFile,
			       boolean          theDisableLineNumberMacros )
  {
	/* Deal with line number macros */
    if ( theDisableLineNumberMacros == false )
    {
      generateRelevantLineNumberMacros ( theOutputDestination,
					 theObjectSubsetToGenerate,
					 theLabelSubsetToGenerate,
					 theFirstStopIndex,
					 theExtraLeadingNewlines,
					 theIsPartOfTdlFile,
					 true /* Include labels */ );
    }

	/* Write any pre-first-token non-significant tokens */
    generateSubcomponents ( DataComponent.FIRST_TOKEN_INDEX,
			    theOutputDestination,
			    theObjectSubsetToGenerate,
			    false );

	/* Write any labels... */
    for ( DataLabelStatement  currentLabel = getLabel();
	  currentLabel != null;
	  currentLabel = currentLabel . getLabel() )
    {
      currentLabel . generate ( theOutputDestination,
				theLabelSubsetToGenerate );
    }

	/* Write any post-labels string */
    if ( DataComponent.isEmptyString ( getPostLabelString() ) == false )
    {
	/* Deal with line number macros */
      theOutputDestination . setUsingTdlFileName ( false );
      if ( getPostLabelString() . charAt ( 0 ) != '\n' )
	theOutputDestination . write ( "\n" );

      theOutputDestination . enableSecondaryIndent();
      theOutputDestination . write ( getPostLabelString() );
      theOutputDestination . disableSecondaryIndent();

	/* Deal with line number macros */
      if ( theDisableLineNumberMacros == false )
      {
	generateRelevantLineNumberMacros ( theOutputDestination,
					   theObjectSubsetToGenerate,
					   theLabelSubsetToGenerate,
					   theFirstStopIndex,
					   theExtraLeadingNewlines,
					   theIsPartOfTdlFile,
					   false /* Don't include labels */ );
      }
    }
  }


	/** Generates a string containing spawn-destroys for every spawn
	  * in this DataStatement that occurs on or after theStartComponent
	  * and before theEnd.  With theStart being inclusive, and theEnd
	  * being exclusive.  If theStart or theEnd is null, they are
	  * considered to be the first and (last+1) statements contained in
	  * this DataStatement respectively.
	  */
  public String getSpawnDestroyClause ( boolean theCouldBeRunning )
	    { return getSpawnDestroyClause ( null, null, theCouldBeRunning ); }

  public String getSpawnDestroyClause ( DataStatement theStart,
					DataStatement theEnd,
					boolean       theCouldBeRunning )
  {
    StringBuffer        stringBuffer;
    DataTaskDefinition  theParentTask = getParentTaskDefinition();

    if ( theParentTask == null )
    {
      System.err.println ( "[DataStatement:getSpawnDestroyClause]  Error:  "
			   + "Unable to find parent task.  "
			   + "Unable to compute spawn-destroy clause." );
      return DataComponent.EMPTY_STRING;
    }

    stringBuffer = new StringBuffer();
    getSpawnDestroyClause ( theStart, theEnd, theParentTask, stringBuffer,
			    false,    theCouldBeRunning );

    return stringBuffer . toString();
  }

  protected boolean getSpawnDestroyClause( DataStatement      theStart,
					   DataStatement      theEnd,
					   DataTaskDefinition theParentTask,
					   StringBuffer       theStringBuffer,
					   boolean            theFoundStart,
					   boolean            theCouldBeRunning
					  )
  {
	/* Note: This is buggy if theStart is a grandchild... */
    if (   ( theStart == null )
	|| ( theStart == this ) )
    {
      theFoundStart = true;
    }

    if (   ( theEnd != null )
	&& ( theEnd == this ) )
    {	    /* True indicates that we have found theEnd, and are done... */
      return true;
    }

	/* Destruction situation */
    if (   ( theFoundStart == true         )
	&& ( this instanceof DataSpawnTask ) )
    {
	/* (Caveat:  Ideally, this method should have been overriden in
	 * DataSpawnTask, and this termination code should have gone there.
	 * But Java will let me put it here, and I think it reads better
	 * when it's all in one place...
	 */
      theStringBuffer . append ( DataComponent.CXX_TDL_TASK_HANDLER_INSTANCE );
      theStringBuffer . append ( " . " );
      if ( theCouldBeRunning )
	theStringBuffer . append (
	    DataComponent.CXX_TDL_TASK_HANDLER_DESTROY_IF_NOT_RUNNING_METHOD );
      else
	theStringBuffer . append (
			   DataComponent.CXX_TDL_TASK_HANDLER_DESTROY_METHOD );
      theStringBuffer . append ( " ( \"" );
      theStringBuffer
	. append ( theParentTask
		     . getIdentifierForSubtask ( (DataSpawnTask) this ) );
      theStringBuffer . append ( "\" );\n" );
    }

	/* Descend (recursively, depth-first) into children */
    for ( int i=0;  i < getChildStatementCount();  i ++ )
    {
      if ( getChildStatement ( i )
	     . getSpawnDestroyClause ( theStart,      theEnd,
				       theParentTask, theStringBuffer,
				       theFoundStart, theCouldBeRunning )
	   == true )
      {
	    /* True indicates that we have found theEnd, and are done... */
	return true;
      }
    }

	/* False indicates that we have ***NOT*** found theEnd yet... */
    return false;
  }


	/* Note:  Overriden by DataSpawnTask, DataWithDoStatement, *
	 *        and DataBindTaskStatement.                       *
	 *        Indicates activities that create/use spawn/with  *
	 *        entries in the C++ HandleManager.                */
  public boolean isSpawnRelatedStatement ( )
  {
	/* Appearance of TDL_REF indicates SPAWN-related activity. */
    return hasSubcomponentFraction ( DataComponent.CXX_TDL_REF );
  }


  public boolean containsSpawnRelatedStatement ( )
		{ return containsSpawnRelatedStatement ( null, null, false ); }

  public boolean containsSpawnRelatedStatement ( DataStatement theStart,
						 DataStatement theEnd    )
	  { return containsSpawnRelatedStatement ( theStart, theEnd, false ); }

  public boolean containsSpawnRelatedStatement ( DataStatement theStart,
						 DataStatement theEnd,
						 boolean       theFoundStart )
  {
    int  containsValue = privateContainsSpawnRelatedStatement ( theStart,
								theEnd,
								false    );

    if ( containsValue == DataStatement.FOUND_SPAWN_RELATED_STATEMENT )
      return true;
    else
      return false;
  }


  protected int privateContainsSpawnRelatedStatement (
						  DataStatement theStart,
						  DataStatement theEnd,
						  boolean       theFoundStart )
  {
	/* Note: This is buggy if theStart is a grandchild... */
    if (   ( theStart == null )
	|| ( theStart == this ) )
    {
      theFoundStart = true;
    }

    if (   ( theEnd != null )
	&& ( theEnd == this ) )
    {	/* Indicate that we have found theEnd, and are done. */
      return DataStatement.FOUND_END;
    }

	/* Found a spawn-related statement situation */
    if (   ( theFoundStart             == true )
	&& ( isSpawnRelatedStatement() == true ) )
    {	/* Indicate that we have found a spawn-related statement.*/
      return DataStatement.FOUND_SPAWN_RELATED_STATEMENT;
    }

	/* Descend (recursively, depth-first) into children */
    for ( int i=0;  i < getChildStatementCount();  i ++ )
    {
      int  containsValue
	     = getChildStatement ( i )
		 . privateContainsSpawnRelatedStatement ( theStart,
							  theEnd,
							  theFoundStart );
      if (   ( containsValue == DataStatement.FOUND_END                     )
	  || ( containsValue == DataStatement.FOUND_SPAWN_RELATED_STATEMENT ) )
      {
	return containsValue; /* Return whatever we have found. */
      }
    }

	/* Indicate that we have not found anything yet. */
    return DataStatement.FOUND_NOTHING;
  }




	/* These next two methods -- getChildStatementCount() and 
	 * getChildStatement() -- provide a general mechanism for
	 * "walking" the DataStatement "tree" of sub-statements.
	 *
	 * They *SHOULD* be overriden in any subclasses that contain
	 * explicit sub-statements.
	 */
  public int  getChildStatementCount()
  {
    return 0;
  }

  public DataStatement  getChildStatement ( int theIndex )
  {
    System.err.println ( "[DataComponent:getChild]  Warning:  "
	       + "theIndex [" + theIndex + "]  is invalid.  Returning NULL!" );
    return null;
  }


	/* Rather than writing this code in DataSpawnTask, DataWithDoStatement,
	 * AND DataConstraintStatement, we'll just put it here, in one place.
	 * (And invoke it from those other classes.)
	 */
  public int getConstraintChildStatementCount()
  {
    DataConstrainedObject  constrainedObject             = null;
    int                    constraintChildStatementCount = 0;

    if ( this instanceof DataConstrainedObject )
      constrainedObject = (DataConstrainedObject)this;
    else
      return 0;

    for ( int i=0;   i < constrainedObject . getConstraintCount();   i++ )
    {
      if ( constrainedObject . getConstraint ( i )
			     . hasChildStatement() == true )
      {
	constraintChildStatementCount ++;
      }
    }
    return constraintChildStatementCount;
  }

  public DataStatement getConstraintChildStatement ( int theIndex )
  {
    DataConstrainedObject  constrainedObject = null;

    if ( this instanceof DataConstrainedObject )
      constrainedObject = (DataConstrainedObject)this;
    else
      return null;

    for ( int i=0;   i < constrainedObject . getConstraintCount();   i++ )
    {
      if ( constrainedObject . getConstraint ( i )
			     . hasChildStatement() == true )
      {
	if ( theIndex <= 0 )
	  return constrainedObject . getConstraint ( i )
				   . getChildStatement();
	else
	  theIndex --;
      }
    }
    return null;
  }


	/* This was in DataTaskDefintion.generateStatementsVector(),
	 * But seeing as of how we are going to need it for DISTRIBUTED
	 * ON-AGENT constraint implementation, it has been migrated here.
	 */
  public static DataVector getChildrenInFlatVector (
						DataVector    theVector,
						DataStatement theTopStatement )
  {
    int            vectorIndex, childrenIndex;
    DataStatement  dataStatement;

    if ( theVector == null )
      theVector = new DataVector();


	/* Start with an empty list... */
    theVector . removeAllElements();

	/* If we have no theTopStatement, return an empty list... */
    if ( theTopStatement == null )
      return theVector;

	/* Start us off with theTopStatement... */
    theVector . addElement ( theTopStatement );

	/* Build up a complete (flat) listing of all the *
	 * statements in theTopStatement...              */
    for ( vectorIndex = 0;
	  vectorIndex < theVector . count();
	  vectorIndex ++ )
    {
      dataStatement = (DataStatement) (theVector . elementAt ( vectorIndex ) );

      for ( childrenIndex = 0;
	    childrenIndex < dataStatement . getChildStatementCount();
	    childrenIndex ++ )
      {
	if ( theVector . contains (
		        dataStatement . getChildStatement ( childrenIndex ) ) )
	{
	  System.err.println (
		      "[DataStatement:getChildrenInFlatVector]  "
		      + "Warning:  Found statement twice:  "
		      + dataStatement . getChildStatement ( childrenIndex )
		                      . toString() );
	}
	else
	{
	  theVector . insertElementAt (
			   dataStatement . getChildStatement ( childrenIndex ),
			   vectorIndex + 1 + childrenIndex );
	}
      }
    }

    return theVector;
  }

}

