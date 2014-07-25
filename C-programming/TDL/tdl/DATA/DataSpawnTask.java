
/*
 * This represents a Task being spawned off.
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 *
 */

public class DataSpawnTask extends DataStatement
                        implements DataValidateCode, DataConstrainedObject
{
	/* Class variables */
  public final static String   SPAWN            = "SPAWN";
  public final static String   TASK_NAME_INDEX  = "taskNameIndex";
  public final static String   OPEN_PAREN       = "(";
  public final static String   CLOSE_PAREN      = ")";
  public final static String   COMMA            = ",";
  public final static String   WITH             = "WITH";
  public final static String   SEMICOLON        = ";";

  public final static int      TASK_ONLY        = 3;
  public final static int      CONSTRAINTS_ONLY = 2;

  protected    static int      ARBITRARY_SUBTASK_INDEX = 0;

	/* Sometimes we need to generate() a wait constraint... */
  protected    static DataConstraint   WAIT_CONSTRAINT
				  = new DataConstraint ( DataConstraint.WAIT );

	/* Class Methods... */
  public synchronized static int getArbitrarySubTaskIndex()
  {
    return ++ ARBITRARY_SUBTASK_INDEX;
  }



	/* Instance Variables */
  protected DataExpression  taskExpression;
  protected DataScope       taskScope;
  protected String          taskname;
  protected DataVector      arguments;
  protected DataVector      constraints;
  protected int             extraIndent;


	/* Instance Methods */
  public DataSpawnTask()
  {
    taskExpression   = null;
    taskScope        = new DataScope();
    taskname         = DataComponent.EMPTY_STRING;
    arguments        = new DataVector();
    constraints      = new DataVector();
    extraIndent      = 0;
  }

	/* An initial leading expression that specifies an instance of a
	 * class or struct in which this task resides.
	 */
  public DataExpression getTaskExpression()         { return taskExpression;  }
  public boolean        hasTaskExpression(){ return null!=getTaskExpression();}
  public void           setTaskExpression( DataExpression theTaskExpression )
				       { taskExpression = theTaskExpression;  }

	/* An initial leading scoping for the taskname */
  public DataScope      getTaskScope() { return taskScope; }


  public String getTaskName(                    )  { return taskname;        }
  public void   setTaskName( String theTaskname )  { taskname = theTaskname; }

  public DataVector getArguments()   { return arguments;   }
  public DataVector getConstraints() { return constraints; }

  public int    getConstraintCount() { return getConstraints() . count(); }


  public void   addArgument ( DataExpression theExpression )
  {
    getArguments() . addElement ( theExpression );
    theExpression . setParent ( this );
  }

  public void   addArgument ( DataExpression theExpression,
			      int            theAtIndex     )
  {
    getArguments() . insertElementAt ( theExpression, theAtIndex );
    theExpression . setParent ( this );
  }

  public void   addArgument ( String theArgument,
			      int    theAtIndex   )
  {
    DataExpression  dataExpression = new DataExpression();
    dataExpression . addSubcomponent ( theArgument );
    addArgument ( dataExpression, theAtIndex );
  }

  public void   removeArgument ( int theAtIndex )
  {
    getArguments() . removeElementAt ( theAtIndex );
  }


  public DataExpression getArgument ( int theIndex )
  {
    if ( ( theIndex >= 0 )   &&   ( theIndex < getArguments() . count() ) )
      return (DataExpression) ( getArguments() . elementAt ( theIndex ) ); 
    else
      return null;
  }

  public void   addConstraint ( DataConstraint theConstraint )
  {
    getConstraints() . addElement ( theConstraint );
    theConstraint . setParent ( this );
  }

  public DataConstraint getConstraint ( int theIndex )
  {
    if ( ( theIndex >= 0 )   &&   ( theIndex < getConstraints() . count() ) )
      return (DataConstraint) ( getConstraints() . elementAt ( theIndex ) ); 
    else
      return null;
  }

  public int  getExtraIndent ()               { return extraIndent; }
  public void setExtraIndent ( int theExtraIndent ) 
					      { extraIndent = theExtraIndent; }



	/* Overriden from DataStatement. */
	/* Includes the spawn-task-name when looking for names. */
  public boolean hasName ( String theName )
  {
    if (   ( getTaskName() != null              )
	&& ( getTaskName() . equals ( theName ) ) )
      return true;
    else
      return super.hasName ( theName );
  }


	/* Allow searching for contained expression subparts... */
  public boolean hasSubcomponentFraction ( String theString )
  {
    int i;

    if ( super . hasSubcomponentFraction ( theString ) )
      return true;

    for ( i=0;   i < getArguments() . count();   i++ )
      if ( getArgument ( i ) . hasSubcomponentFraction ( theString ) )
	return true;

    for ( i=0;   i < getConstraintCount();   i++ )
      if ( getConstraint ( i ) . hasSubcomponentFraction ( theString ) )
	return true;

    return false;
  }

	/* Allow searching of our expressions... */
  public void runOnSubcomponentFraction (
			String                      theString,
			RunOnSubcomponentInterface  theRunOnSubcomponentObject,
			Object                      theArgumentObject )
  {
    int i;

    super . runOnSubcomponentFraction ( theString,
					theRunOnSubcomponentObject,
					theArgumentObject );

    for ( i=0;   i < getArguments() . count();   i++ )
      getArgument ( i )
	. runOnSubcomponentFraction ( theString,
				      theRunOnSubcomponentObject,
				      theArgumentObject );


    for ( i=0;   i < getConstraintCount();   i++ )
      getConstraint ( i )
	. runOnSubcomponentFraction ( theString,
				      theRunOnSubcomponentObject,
				      theArgumentObject );
  }



	/* Validates code that is outside of any Task */
  public void validateExternalCode( int                         theReference,
				    DataValidateCodeReturnValue theReturnValue)
    throws CompilationException
  {
	/* Validate our constraints */
    for ( int i=0;  i < getConstraints().count();  i++ )
    {
      getConstraint ( i )
	. validateExternalCode( theReference, theReturnValue );
    }

	/* Check for overriden constraints */
    DataStatement.validateAnyOverridenConstraints ( this, theReturnValue );
  }

	/* Validates code that is inside of a Task */
  public void validateInternalCode( int                         theReference,
				    DataValidateCodeReturnValue theReturnValue)
    throws CompilationException
  {
	/* Perform super-class validation */
    super . validateInternalCode ( theReference, theReturnValue );


	/* Validate our constraints */
    for ( int i=0;  i < getConstraints() . count();  i++ )
    {
      getConstraint ( i )
	. validateInternalCode ( theReference, theReturnValue );

      if ( getConstraint( i ) . getConstraintType() == DataConstraint.PARALLEL)
      {
	theReturnValue
	  . addWarning ( getConstraint ( i ) )
	  . write ( "PARALLEL constraint in a SPAWN statement WITH clause "
		    + "is meaningless.\n" );
      }
    }

        /* Validate mutually inconsistent constraints */
    DataStatement.validateInternalConstrainedObject ( theReference,
						      theReturnValue,
						      this );

	/* Check for overriden constraints */
    DataStatement.validateAnyOverridenConstraints ( this, theReturnValue );
  }



  public String getWarnString ( int theObjectSubset )
  {
    return super . getWarnString ( theObjectSubset )
      + " or DataSpawnTask.TASK_ONLY ("
      + DataSpawnTask.TASK_ONLY
      + ") or DataSpawnTask.CONSTRAINTS_ONLY ("
      + DataSpawnTask.CONSTRAINTS_ONLY + ")";
  }

  public boolean isValidObjectSubset ( int theObjectSubset )
  {
    if (   ( theObjectSubset == DataSpawnTask.TASK_ONLY        )
	|| ( theObjectSubset == DataSpawnTask.CONSTRAINTS_ONLY ) )
      return true;
    else
      return super . isValidObjectSubset ( theObjectSubset );
  }



  public void generateTask ( DataDestination  theOutputDestination,
			     int              theObjectSubsetToGenerate,
			     boolean          theIsFollowingSpawn,
			     boolean          thePrintPreTaskNameSubcomponents,
			     boolean          theCorrectIndentation,
			     boolean          theStartIndentationOnNextLine,
			     boolean          theIsCxxSubsetGeneration,
			     String           theFunctionLeadText )
  {
    int      i, indent = 0;
    boolean  needsToUndoLineMacros = false;


    if ( hasTaskExpression() )
    {
      if ( theIsCxxSubsetGeneration )
      {    /* Deal with #line macros */
	theOutputDestination . setUsingTdlFileName ( true );
	theOutputDestination . makeNextLineNumber ( getTaskExpression() );
	theOutputDestination . write ( "\n" );
      }

      getTaskExpression() . generate ( theOutputDestination,
				       DataComponent.ENTIRE_OBJECT );
      if ( theIsCxxSubsetGeneration )
	theOutputDestination . setUsingTdlFileName ( false );
    }

    if ( thePrintPreTaskNameSubcomponents == true )
    {
      getTaskScope() . generate ( theOutputDestination,
				  DataComponent.ENTIRE_OBJECT );

	/* Initialize us to generate non-significant tokens... */
      initializeGenerateSubcomponentIndex ( DataSpawnTask.SPAWN );


	/* Write any pre-task-name non-significant tokens */
      generateSubcomponents ( DataSpawnTask.TASK_NAME_INDEX,
			      theOutputDestination,
			      theObjectSubsetToGenerate, theIsFollowingSpawn );
    }
    else
    {
      getTaskScope() . writeScope ( theOutputDestination );

	/* Initialize us to generate non-significant tokens... */
      initializeGenerateSubcomponentIndex ( DataSpawnTask.TASK_NAME_INDEX );
    }

    if ( theFunctionLeadText != null )
    {
      theOutputDestination . write ( theFunctionLeadText );
      theOutputDestination . setStripLeadingWhitespace();
    }


	/* Write our task name */
    theOutputDestination . write ( getTaskName() );

	/* Write any pre-"(" non-significant tokens */
    generateSubcomponents ( DataSpawnTask.OPEN_PAREN,
			    theOutputDestination,
			    theObjectSubsetToGenerate, false );

	/* Write "(" */
    theOutputDestination . write ( DataSpawnTask.OPEN_PAREN );


	/* Are we trying to compensate for non-standard argument indentation?*/
    if ( theCorrectIndentation == true )
    {
      if ( theStartIndentationOnNextLine == true )
	theOutputDestination . addIndent ( DataComponent.STANDARD_INDENT );
      else
      {
	theOutputDestination . write ( " " ); /* Add a space in here */
	indent = theOutputDestination . indentToCurrentColumn();
      }
    }


	/* Write arguments */
    for ( i=0;  i < getArguments().count();  i++ )
    {
	/* Deal with #line macros */
      if (   ( theIsCxxSubsetGeneration                     == true )
	  && ( getArgument ( i ) . hasValidLineNumber()     == true )
	  && ( theOutputDestination . getEnableLineMacros() == true ) )
      {
	theOutputDestination . setUsingTdlFileName ( true );
	    /* Note: If ( theCorrectIndentation == true ),              *
	     *       adjust for skipping leading whitespace (newlines). */
	theOutputDestination
	  . makeNextLineNumber(  getArgument ( i ) . getLineNumber()
			       + (   ( theCorrectIndentation == true )
				   ? getArgument ( i )
				       . countLeadingSubcomponentNewlines()
				   : 0 ) );
	theOutputDestination . write ( "\n" ); /* Flush #line macro */

	needsToUndoLineMacros = true;
      }
      else if ( theCorrectIndentation == true )
      {
	if (   ( i > 0                                 )
	    || ( theStartIndentationOnNextLine == true ) )
	{
	  theOutputDestination . write ( "\n" );
	}
      }

      if ( theCorrectIndentation == true )
      {
	theOutputDestination . setStripLeadingWhitespace();
	theOutputDestination . setPersistentlyStripLeadingSpaces ( true );
      }

      getArgument ( i ) . generate ( theOutputDestination,
				     theObjectSubsetToGenerate );

      if ( theCorrectIndentation == true )
      {
	theOutputDestination . setPersistentlyStripLeadingSpaces ( false );
      }

	/* If there are more arguments */
      if ( (i+1) < getArguments().count() )
      {
	    /* Write "," */
	theOutputDestination . write ( DataSpawnTask.COMMA );
      }

    } /* FOR ( i=0;  i < getArguments().count();  i++ ) */


	/* Deal with #line macros */
    if ( needsToUndoLineMacros == true )
    {
      theOutputDestination . setUsingTdlFileName ( false );
      theOutputDestination . write ( "\n" ); /* Flush #line macro */
    }


	/* Write any pre-")" non-significant tokens */
    generateSubcomponents ( DataSpawnTask.CLOSE_PAREN,
			    theOutputDestination,
			    theObjectSubsetToGenerate, false );

	/* Write ")" */
    theOutputDestination . write ( DataSpawnTask.CLOSE_PAREN );


	/* Are we trying to compensate for non-standard argument indentation?*/
    if ( theCorrectIndentation == true )
    {
      if ( theStartIndentationOnNextLine == true )
	theOutputDestination . removeIndent ( DataComponent.STANDARD_INDENT );
      else
	theOutputDestination . removeIndent ( indent );
    }
  }


  public void generateConstraints ( DataDestination  theOutputDestination,
				    int              theObjectSubsetToGenerate)
  {
    int i;

	/* Write constraints */
    for ( i=0;  i < getConstraints().count();  i++ )
    {
      getConstraint ( i ) . generate ( theOutputDestination,
				       theObjectSubsetToGenerate );

	  /* If there are more constraints */
      if ( (i+1) < getConstraints().count() )
      {
	    /* Write "," */
	theOutputDestination . write ( DataSpawnTask.COMMA );
      }
    }
  }


  public void generate ( DataDestination  theOutputDestination,
			 int              theObjectSubsetToGenerate )
  {
	/* Do any extra indenting that we may require... */
	/* (Typically used for indenting spawns outside of Task definitions).*/
    theOutputDestination . addIndent ( getExtraIndent() );

       	/* Print a warning if necessary... */
    warnIfInvalidObjectSubset ( theObjectSubsetToGenerate, "generate" );

	/* Are we just doing the TASK_ONLY ? */
    if ( theObjectSubsetToGenerate == DataSpawnTask.TASK_ONLY )
    {
      generateTask ( theOutputDestination,
		     DataComponent.ENTIRE_OBJECT,
		     false,  /* No extra leading space   */
		     true,   /* Include leading comments */
		     false,  /* No extra indentation.    */
		     false,  /* No extra indentation.    */
		     false,  /* Is [not] Cxx generation. */
		     null ); /* theFunctionLeadText      */

		/* Stop any extra indenting... */
      theOutputDestination . removeIndent ( getExtraIndent() );
      return;
    }

	/* Are we just doing the CONSTRAINTS_ONLY ? */
    if ( theObjectSubsetToGenerate == DataSpawnTask.CONSTRAINTS_ONLY )
    {
      generateConstraints ( theOutputDestination, DataComponent.ENTIRE_OBJECT);
		/* Stop any extra indenting... */
      theOutputDestination . removeIndent ( getExtraIndent() );
      return;
    }

    	/* Initialize us to generate non-significant tokens... */
    initializeGenerateSubcomponentIndex();

	/* Generate any labels that we have... */
    generateLabels ( theOutputDestination,
		     theObjectSubsetToGenerate,
		     (   (   ( isCxxSubset ( theObjectSubsetToGenerate ) )
			  && ( getParentTaskDefinition() != null         ) )
		       ? DataLabelStatement.COMMENT_OUT_ID_LABELS
		       : DataComponent.ENTIRE_OBJECT ),
		     DataSpawnTask.SPAWN );

	/* Write any pre-spawn non-significant tokens */
    generateSubcomponents ( DataSpawnTask.SPAWN,
			    theOutputDestination,
			    theObjectSubsetToGenerate, false );

	/* If we are doing CXX generation... */
    if ( isCxxSubset ( theObjectSubsetToGenerate ) )
    {
      theOutputDestination . setNewlineText ( "    // " );
      theOutputDestination . write          ( "    // " );
    }

	/* Write our spawn keyword */
    theOutputDestination . write ( DataSpawnTask.SPAWN );


	/* Write our task-name & arguments... */
    generateTask ( theOutputDestination,
		   theObjectSubsetToGenerate,
		   true,    /* Add extra leading space if necessary. */
		   true,    /* Include leading comments.             */
		   false,   /* No extra indentation.                 */
		   false,   /* No extra indentation.                 */
		   false,   /* Disable special CXX generation code.  */
		   null );  /* theFunctionLeadText */

	/* Do we have constraints? */
    if ( getConstraints() . count() > 0 )
    {
	/* Write any pre-WITH non-significant tokens */
      generateSubcomponents ( DataSpawnTask.WITH,
			      theOutputDestination,
			      theObjectSubsetToGenerate, true );

	/* Write our WITH keyword */
      theOutputDestination . write ( DataSpawnTask.WITH );

	/* Write the constraints */
      generateConstraints ( theOutputDestination, DataComponent.ENTIRE_OBJECT);

    } /* IF ( we have constraints ) */

	/* Write any pre-";" non-significant tokens */
    generateSubcomponents ( DataSpawnTask.SEMICOLON,
			    theOutputDestination,
			    theObjectSubsetToGenerate, false );

	/* Write ";" */
    theOutputDestination . write ( DataSpawnTask.SEMICOLON );


	/* Write any remaining non-significant tokens */
	/* (There *SHOULD* *NOT* be any...  But just in case...) */
    generateAllRemainingSubcomponents ( theOutputDestination,
					theObjectSubsetToGenerate, false );

	/* If we are doing CXX generation... */
    if ( isCxxSubset ( theObjectSubsetToGenerate ) )
    {
      theOutputDestination . clearNewlineText();
      theOutputDestination . write ( "\n" );
      generateCxx ( theOutputDestination, theObjectSubsetToGenerate );
    }

		/* Stop any extra indenting... */
    theOutputDestination . removeIndent ( getExtraIndent() );
  }



  public void generateCxx ( DataDestination  theOutputDestination,
			    int              theSubsetToProduce   )
    throws CompilationException
  {
    int                  i, argIndent, invokeIndent;
    DataTaskDefinition   task;
    DataConstraint       waitConstraint  = null;
    String               spawnIdentifier;

	/* We need to establish any ON-AGENT constraints before the
	 * SPAWN statement, due to the TCM requirements that the AGENT
	 * be specified when the TCM-node is constructed, and the TDL aspect
	 * of delayed node creation.
	 */
    final int onAgentIndex = DataConstraint.getLastIndexOfConstraintOfType (
			       DataConstraint.ON_AGENT,
			       this );
	/* And much the same for the TCM-Task-Tree PARENT constraint. */
    final int parentIndex  = DataConstraint.getLastIndexOfConstraintOfType (
			       DataConstraint.TCM_TASK_TREE_PARENT,
			       this );


	/* Deal with #line macros */
    theOutputDestination . setUsingTdlFileName ( false );
    theOutputDestination . write ( "\n" ); /* This newline will be skipped.*/


	/* Find our top-most object -- the Task that we are a part of */
    task = getParentTaskDefinition();

	/* If we don't have a parent */
    if ( task == null )
    {
	/* Error check */
      for ( i=0;  i < getConstraints() . count();  i++ )
      {
	   /* If we need a parent... Ie: have a constrainer-tag */
	if ( getConstraint ( i ) . getHasNonStandardEventTagTask() )
	{
	  throw new CompilationException (
	       "Internal Error in [DataSpawnTask:generateCxx]:  "
	       + "Spawn statement is not contained in a task.  "
	       + "Unable to satisfy constraint \""
	       + getConstraint ( i ) . toString()
	       + "\"." );
	}
      }

      DataSpawnTask.generateCxxOutsideOfTask ( theOutputDestination,
					       theSubsetToProduce,
					       this );
      return;
    }
  

	/* Find our default spawn-identifier */
    spawnIdentifier = task.getIdentifierForSubtask ( this );


	/** Deal with any switch-idents... **/
    theOutputDestination . enableSecondaryIndent();


	/***************************************/
	/* Begin (massive) invocation the Task */
	/***************************************/

	/* Note:  do-start-invoking must be a complete stand-alone statement,
	 * Otherwise we can't access the current Task from the handle-manager
	 */
    theOutputDestination
      . write ( DataComponent.CXX_TDL_TASK_HANDLER_INSTANCE );
    theOutputDestination . write ( "\n" );
    theOutputDestination . addIndent (  DataComponent.STANDARD_INDENT );


    theOutputDestination . write ( ". " );
    theOutputDestination
      . write ( DataComponent.CXX_TDL_TASK_HANDLER_DO_START_INVOKING );
    theOutputDestination . write ( " ( " );
    argIndent = theOutputDestination . indentToCurrentColumn();
    theOutputDestination . write ( "\"" );
    theOutputDestination . write ( spawnIdentifier );
    theOutputDestination . write ( "\"" );
	/* Add the ON-AGENT constraint */
    if ( onAgentIndex >= 0 )
    {
//da0g todo: Fix line numbers here.
      theOutputDestination . write ( ",\n" );
      getConstraint ( onAgentIndex )
	. generateCxxTaskInternal ( theOutputDestination );
    }
	/* Add the PARENT constraint */
    if ( parentIndex >= 0 )
    {
	    /* Deal with #line macros */
      theOutputDestination . setUsingTdlFileName ( true );
      theOutputDestination . makeNextLineNumber (
	getConstraint ( parentIndex )
	  . getTcmTaskTreeParentExpression() );

      theOutputDestination . write ( ",\n" );
      getConstraint ( parentIndex )
	. getTcmTaskTreeParentExpression()
	. generate ( theOutputDestination, DataComponent.ENTIRE_OBJECT );

	/* Deal with #line macros */
      theOutputDestination . setUsingTdlFileName ( false );
    }
    theOutputDestination . removeIndent ( argIndent );
    theOutputDestination . write ( " );\n" );
    theOutputDestination . removeIndent ( DataComponent.STANDARD_INDENT );


	/*********************/
	/* Handle Set-Action */
	/*********************/

	/* Write "task-spawn-manager . doSetAction ( " */
    theOutputDestination
      . write ( DataComponent.CXX_TDL_TASK_HANDLER_INSTANCE );
    theOutputDestination . write ( "\n" );
    theOutputDestination . addIndent (  DataComponent.STANDARD_INDENT );

    theOutputDestination . write ( ". " );
    theOutputDestination
      . write ( DataComponent.CXX_TDL_TASK_HANDLER_DO_SET_ACTION );
    theOutputDestination . write ( " (" );
    theOutputDestination . addIndent ( DataComponent.STANDARD_INDENT * 2);
    
//da0g marker
    if ( hasTaskExpression() )
    {	    /* Deal with #line macros */
      theOutputDestination . setUsingTdlFileName ( true );
      theOutputDestination . makeNextLineNumber ( getTaskExpression() );
      theOutputDestination . write ( "\n" );
      getTaskExpression()  . generate ( theOutputDestination,
					DataComponent.ENTIRE_OBJECT );
      theOutputDestination . setUsingTdlFileName ( false );
    }
    else
    {
      theOutputDestination . write ( "\n" );
    }
    getTaskScope() . writeScope ( theOutputDestination );
    theOutputDestination
      . write ( DataComponent.CXX_CREATEACTION_FUNCTION_LEAD );
    theOutputDestination . write ( getTaskName() );
    theOutputDestination . write ( " (\n" );

	/* Indent arguments... */
    theOutputDestination . addIndent ( DataComponent.STANDARD_INDENT * 2);

	/* Write: "* ( _TDL_SpawnedTasks . getCurrentTaskTcmTaskTreeRef() ),"*/
    theOutputDestination
      . write ( DataComponent.CXX_TDL_TASK_HANDLER_INSTANCE );
    theOutputDestination . write ( " . " );
    theOutputDestination
      . write ( DataComponent.CXX_TDL_TASK_HANDLER_GET_CURRENT_TASK_REF );
    theOutputDestination . write ( "(),\n" );


	/* Note:  This is assuming our parent "task" has      *
	 * correctly populated it's internal data-structures. */
    switch ( task . getDataSpawnTaskLocalOrDistributed ( this ) )
    {
      case DataComponent.DISTRIBUTED_ONLY:
	theOutputDestination . write (
	  DataComponent.CXX_INVOKE_AS_DISTRIBUTED_ONLY_DELAYED_ALLOCATION );
	break;
 
      case DataComponent.EITHER_LOCAL_OR_DISTRIBUTED:
	theOutputDestination . write (
	  DataComponent
	    . CXX_INVOKE_AS_EITHER_LOCAL_OR_DISTRIBUTED_DELAYED_ALLOCATION );
	break;

      case DataComponent.LOCAL_NONDISTRIBUTED_ONLY:
	theOutputDestination . write (
	  DataComponent
	    . CXX_INVOKE_AS_LOCAL_NONDISTRIBUTED_ONLY_DELAYED_ALLOCATION );
	break;

      default:
	System.err.println (
		  "[DataSpawnTask:generateCxx] Internal Error:  "
		  + "getDataSpawnTaskLocalOrDistributed() returned an "
		  + "invalid value. Assuming local non-distributed case.\n" );
	theOutputDestination . write (
	  DataComponent
	    . CXX_INVOKE_AS_LOCAL_NONDISTRIBUTED_ONLY_DELAYED_ALLOCATION );
	break;
    } /* switch ( getDataSpawnTaskLocalOrDistributed (...) ) */
    theOutputDestination . write ( " ( " );
    theOutputDestination
      . write ( DataComponent.CXX_TDL_TASK_HANDLER_INSTANCE );
    theOutputDestination . write ( " )" );


	  /* Write the task-argument-names as arguments to the setaction... */
    for ( i=0;  i < getArguments() . count();  i++ )
    {
	/* Deal with #line macros */
      theOutputDestination . setUsingTdlFileName ( true );
      theOutputDestination
	. makeNextLineNumber (   getArgument ( i ) . getLineNumber()
			       + getArgument ( i )
				   . countLeadingSubcomponentNewlines() );

	/* Write the argument */
      theOutputDestination . write ( ",\n" );
      theOutputDestination . setStripLeadingWhitespace();
      theOutputDestination . setPersistentlyStripLeadingSpaces ( true  );
      getArgument ( i ) . generate( theOutputDestination, theSubsetToProduce );
      theOutputDestination . setPersistentlyStripLeadingSpaces ( false );
    }

	/* Deal with #line macros */
    theOutputDestination . setUsingTdlFileName ( false );

    theOutputDestination . write ( " ) )\n" );


	/* Stop indenting Arguments... */
    theOutputDestination . removeIndent ( DataComponent.STANDARD_INDENT * 4 );
    


	/**************************************/
	/*** SPAWN Constraints, pre-insert  ***/
	/**************************************/

    for ( i=0;  i < getConstraints() . count();  i++ )
    {
      if ( DataConstraint.getShouldConstraintBeCxxGenerated (
					getConstraint ( i ),
					getConstraints(),
					false, /* theIsPostInsert      */
					true   /* theIsInternalToTasks */ ) )
      {
	theOutputDestination . write ( ". " );
	theOutputDestination
	  . write ( DataComponent.CXX_TDL_TASK_HANDLER_DO_APPLY_CONSTRAINT );
	theOutputDestination . write ( " (\n" );
	theOutputDestination . addIndent ( DataComponent.STANDARD_INDENT * 2);
	getConstraint ( i ) . generateCxxTaskInternal ( theOutputDestination );
	theOutputDestination . removeIndent(DataComponent.STANDARD_INDENT * 2);
	theOutputDestination . write ( " )\n" );
      }
    }



	/**********************/
	/* Handle insert-node */
	/**********************/

    theOutputDestination . write ( ". " );
    theOutputDestination
      . write ( DataComponent.CXX_TDL_TASK_HANDLER_DO_INSERT );
    theOutputDestination . write ( " ()\n" );



	/***************************************/
	/*** SPAWN Constraints, post-insert  ***/
	/***************************************/

    for ( i=0;  i < getConstraints() . count();  i++ )
    {
      if ( DataConstraint.getShouldConstraintBeCxxGenerated (
					getConstraint ( i ),
					getConstraints(),
					true, /* theIsPostInsert      */
					true  /* theIsInternalToTasks */ ) )
      {
	    /* Only do WAIT constraints once. */
	if ( getConstraint ( i ) . getConstraintType() == DataConstraint.WAIT )
	{
	  if ( waitConstraint == null )
	    waitConstraint = getConstraint ( i );

	  continue;  /* Skip this constraint. */
	}

	theOutputDestination . write ( ". " );
	theOutputDestination
	  . write ( DataComponent.CXX_TDL_TASK_HANDLER_DO_APPLY_CONSTRAINT );
	theOutputDestination . write ( " ( " );
	theOutputDestination . addIndent ( DataComponent.STANDARD_INDENT * 2);
	getConstraint ( i ) . generateCxxTaskInternal ( theOutputDestination );
	theOutputDestination . removeIndent(DataComponent.STANDARD_INDENT * 2);
	theOutputDestination . write ( " )\n" );
      }
    }
	/* WAIT constraints also need to be last... */
    if ( waitConstraint != null )
    {
	theOutputDestination . write ( ". " );
	theOutputDestination
	  . write ( DataComponent.CXX_TDL_TASK_HANDLER_DO_APPLY_CONSTRAINT );
	theOutputDestination . write ( " ( " );
	theOutputDestination . addIndent ( DataComponent.STANDARD_INDENT * 2);
	waitConstraint . generateCxxTaskInternal ( theOutputDestination );
	theOutputDestination . removeIndent(DataComponent.STANDARD_INDENT * 2);
	theOutputDestination . write ( " )\n" );
    }



	/******************************/
	/* END the spawn statement... */
	/******************************/

    theOutputDestination . write ( ". " );
    theOutputDestination
      . write ( DataComponent.CXX_TDL_TASK_HANDLER_FINISH_INVOKING );
	/* Stop indenting now... */
    theOutputDestination . removeIndent ( DataComponent.STANDARD_INDENT );
    theOutputDestination . write ( "();\n" );


	/** Stop Dealing with any switch-idents... **/
    theOutputDestination . disableSecondaryIndent();


  } /* public void generateCxx ( ... ) */


  public static void generateCxxOutsideOfTask (
					  DataDestination theOutputDestination,
					  int             theSubsetToProduce,
					  DataSpawnTask   theDataSpawnTask    )
  {
    privateGenerateCxxOutsideOfTask ( theOutputDestination,
				      theSubsetToProduce,
				      theDataSpawnTask,
				      "" );
  }

  public static void generateCxxOutsideOfTask (
				     DataDestination    theOutputDestination,
				     int                theSubsetToProduce,
				     DataTaskDefinition theDataTaskDefinition,
				     String             theUniqueString       )
  {
    privateGenerateCxxOutsideOfTask ( theOutputDestination,
				      theSubsetToProduce,
				      theDataTaskDefinition,
				      theUniqueString );
  }

	/** This method is static so that it can serve two purposes.
	  * (1) Generating the CXX version of SPAWNs outside of Tasks
	  * And (2), Generating the _TDL_SpawnAndWait_* function for
	  * DataTaskDefinition objects.
	  */
  protected static void privateGenerateCxxOutsideOfTask (
				         DataDestination  theOutputDestination,
					 int              theSubsetToProduce,
					 DataComponent    theTaskSource,
					 String           theUniqueString )
  {
    DataConstraint     waitConstraint        = null;
    DataSpawnTask      theDataSpawnTask      = null;
    DataTaskDefinition theDataTaskDefinition = null;
    boolean            isDataSpawnTask;
    DataExpression     taskExpression;
	/* We need to use scope in text-strings for identification in places *
	 * where we don't want to be scoped.  Thus, the two scopes...        */
    DataScope          taskScopeObject;
    String             taskScopeString, taskName, spawnObject,
                       errorLocation, returnValueString;
    int                i, parentIndex, ifIndent;


	/* Find what flavor we are generating... */
    if ( theTaskSource instanceof DataSpawnTask )
    {
      isDataSpawnTask  = true;
      theDataSpawnTask = (DataSpawnTask) theTaskSource;
    }

    else if ( theTaskSource instanceof DataTaskDefinition )
    {
      isDataSpawnTask  = false;
      theDataTaskDefinition = (DataTaskDefinition) theTaskSource;
    }

    else
    {
      System.err.println ( "[DataSpawnTask:privateGenerateCxxOutsideOfTask]  "
			   + "Programmer Error:  theTaskSource is not a "
			   + "DataSpawnTask or a DataTaskDefinition. "
			   + "Aborting..." );
      return;
    }
    

	/* Choose our spawn-object-name... */
    if ( isDataSpawnTask )
    {
      taskExpression  =  theDataSpawnTask . getTaskExpression();
      taskScopeObject =  theDataSpawnTask . getTaskScope();
      taskScopeString =  taskScopeObject  . getAllScopeStrings();
      taskName        =  theDataSpawnTask . getTaskName();
      spawnObject     = (  DataComponent.CXX_TDL_ARBITRARY_SPAWN_TASK_NAME_LEAD
		         + DataSpawnTask.getArbitrarySubTaskIndex() );
      errorLocation   = "\"[:\'" + taskScopeString + taskName + "\' - "
	                 + spawnObject + "]\"";
      returnValueString = "";

	/* We run into an interesting problems with:
	 * if ( ) spawn foo(); -- there is no compound statement...
	 */
      theOutputDestination . write        ( "{\n" );
      theOutputDestination . addIndent    ( DataComponent.STANDARD_INDENT );
    }
    else
    {
      taskExpression  = null;
      taskScopeObject = null;
      taskScopeString = theDataTaskDefinition . getTaskScope()
					      . getAllScopeStrings();
      taskName        = theDataTaskDefinition . getTaskName();
      spawnObject     = DataComponent.CXX_SPAWNWAIT_CHILD_NODE_NAME;
      errorLocation   = ( "\"[" + taskScopeString 
			 + DataComponent.CXX_SPAWNWAIT_FUNCTION_LEAD
			 + taskName + "]\"" );
      returnValueString = DataComponent.CXX_SPAWNWAIT_RETURN_VARIABLE + " = ";

	/* Write "return-value-type  returnValue;" */
      theOutputDestination
        . write ( DataComponent.CXX_SPAWNWAIT_RETURN_VALUE );
      theOutputDestination . write ( "    " );
      theOutputDestination
	. write ( DataComponent.CXX_SPAWNWAIT_RETURN_VARIABLE );
      theOutputDestination . write ( " = " );
      theOutputDestination . write ( DataComponent.CXX_TCM_ERROR );
      theOutputDestination . write ( ";\n" );
    }

	/* Declare/Allocate the spawnObject */
    theOutputDestination . write ( DataComponent.CXX_ALLOCATE_RETURN_VALUE );
    theOutputDestination . write ( "  " );
    theOutputDestination . write ( spawnObject );
    if ( taskExpression != null )
    {
      theOutputDestination . write ( ";\n" );
    }
    else
    {
      theOutputDestination . write ( " = " );
      if ( taskScopeObject != null )
	taskScopeObject . writeScope ( theOutputDestination );
      theOutputDestination . write ( DataComponent.CXX_ALLOCATE_FUNCTION_LEAD);
      theOutputDestination . write ( taskName );
      theOutputDestination . write ( theUniqueString );
      theOutputDestination . write ( " ( " );
      privateGenerateCxxOutsideOfTask_internalGenerateName (
							theOutputDestination,
							isDataSpawnTask,
							theDataSpawnTask,
							taskScopeString,
							taskName );
      theOutputDestination . write ( " ) ;\n" );
    }


	/*******************************************/
	/* Some Constraints, such as ON-TERMINATE, */
	/* need to allocate secondary subtasks.    */
	/*******************************************/

    if ( isDataSpawnTask )
    {
      for ( i=0;  i < theDataSpawnTask . getConstraintCount();  i++ )
      {
	theDataSpawnTask
	  . getConstraint ( i )
	  . generateCxxExternalSubtaskDeclarations ( theOutputDestination,
						     false /* Add new line*/ );
      }
    }


	/*****************************************************/
	/* Add in any TDL_REF_IN constraint assignments here */
	/*****************************************************/
    privateGenerateCxxOutsideOfTask_internalGenerateTDLREF ( 
							theOutputDestination,
							theSubsetToProduce,
							isDataSpawnTask,
							theDataSpawnTask,
							spawnObject );


	/**********************************/
	/* Begin the massive IF statement */
	/**********************************/

    theOutputDestination . write ( "\nif (" );
    
	  /* Indent Master if-statement expression */
    ifIndent = theOutputDestination . indentToCurrentColumn();


	/**********************************************************/
	/* Handle allocation check -- Regular (Not Delayed) case. */
	/**********************************************************/
    theOutputDestination . write ( "   " );
    if ( taskExpression == null )
    {
      privateGenerateCxxOutsideOfTask_internalAllocationCheck (
							theOutputDestination,
							errorLocation,
							spawnObject );
      theOutputDestination . write ( "\n&& " );
    }


	/*********************/
	/* Handle set-action */
	/*********************/

    theOutputDestination . write ( DataComponent.CXX_VERIFY_SET_ACTION );
    theOutputDestination . write ( " (\n" );
    
	  /* Indent arguments...   (3 for the "   " / "&& " bit.) */
    theOutputDestination . addIndent ( 3 + DataComponent.STANDARD_INDENT * 2 );

    theOutputDestination . write ( errorLocation );
    theOutputDestination . write ( ",\n" );

    theOutputDestination . write ( returnValueString );

    theOutputDestination . write ( DataComponent.CXX_TCM_SET_ACTION_FUNCTION );
    theOutputDestination . write ( " (\n" );

	/* Indent next arguments to this column ... */
    theOutputDestination . addIndent ( DataComponent.STANDARD_INDENT * 2 );

    theOutputDestination . write ( spawnObject );
    theOutputDestination . write ( "," );


	/* Write the Create-Action argument */
    if ( taskExpression != null )
    {	    /* Deal with #line macros */
      theOutputDestination . setUsingTdlFileName ( true );
      theOutputDestination . makeNextLineNumber ( taskExpression );
      theOutputDestination . write ( "\n" );
      taskExpression . generate ( theOutputDestination,
				  DataComponent.ENTIRE_OBJECT );
      theOutputDestination . setUsingTdlFileName ( false );
    }
    else
    {
      theOutputDestination . write ( "\n" );
    }
    if ( taskScopeObject != null )
      taskScopeObject . writeScope ( theOutputDestination );
    theOutputDestination
      . write ( DataComponent.CXX_CREATEACTION_FUNCTION_LEAD );
    theOutputDestination . write ( taskName );
    theOutputDestination . write ( " (\n" );


	/* Indent remaining arguments to this column ... */
    theOutputDestination . addIndent ( DataComponent.STANDARD_INDENT * 2 );

	/* Write the spawnObject (TCM_Task_Tree_Ref) */
    theOutputDestination . write ( spawnObject );
    theOutputDestination . write ( ",\n" );


	/* Write Local Delayed-Allocation argument to create-Action. */
    theOutputDestination . write (
      DataComponent.CXX_INVOKE_AS_LOCAL_NONDISTRIBUTED_ONLY_DELAYED_ALLOCATION
      );
    theOutputDestination . write ( " (\n" );
    theOutputDestination . addIndent ( DataComponent.STANDARD_INDENT * 2 );

    privateGenerateCxxOutsideOfTask_internalGenerateName( theOutputDestination,
							  isDataSpawnTask,
							  theDataSpawnTask,
							  taskScopeString,
							  taskName );
    theOutputDestination . write ( ", " );
    if ( taskExpression != null )
      theOutputDestination . write ( "TRUE" );
    else
      theOutputDestination . write ( "FALSE" );


    theOutputDestination . removeIndent ( DataComponent.STANDARD_INDENT * 2 );
    theOutputDestination . write ( " )" );



	  /* Write the task-argument-names as arguments to createaction... */
    if ( isDataSpawnTask )
    {
      for ( i=0;  i < theDataSpawnTask . getArguments() . count();  i++ )
      {
	    /* Deal with #line macros */
	theOutputDestination . setUsingTdlFileName ( true );
	theOutputDestination . makeNextLineNumber (
	       theDataSpawnTask . getArgument ( i ) . getLineNumber()
	     + theDataSpawnTask . getArgument ( i )
	         . countLeadingSubcomponentNewlines() );

	    /* Write the argument */
	theOutputDestination . write ( ",\n" );
	theOutputDestination . setStripLeadingWhitespace();
	theOutputDestination . setPersistentlyStripLeadingSpaces ( true  );
	theDataSpawnTask . getArgument ( i ) . generate ( theOutputDestination,
							  theSubsetToProduce );
	theOutputDestination . setPersistentlyStripLeadingSpaces ( false );
      }

      	/* Deal with #line macros */
      theOutputDestination . setUsingTdlFileName ( false );
    }
    else
    {
      for ( i=0;  i < theDataTaskDefinition . getTaskArgumentCount();  i++ )
      {
	theOutputDestination . write ( ",\n" );
	theOutputDestination . setStripLeadingWhitespace();
	theOutputDestination
	  . write ( theDataTaskDefinition . getTaskArgument ( i )
				          . getArgumentName()     );
      }
    }

    theOutputDestination . write ( " ) ) )\n" );

	/* Stop indenting Arguments... */
    theOutputDestination
      . removeIndent ( 3 + DataComponent.STANDARD_INDENT * 6 );



	/*******************************************************/
	/* Handle allocation check -- Delayed Allocation case. */
	/*******************************************************/
    if ( taskExpression != null )
    {
      theOutputDestination . write ( "\n&& " );
      privateGenerateCxxOutsideOfTask_internalAllocationCheck (
							theOutputDestination,
							errorLocation,
							spawnObject );
    }


	/**************************************/
	/*** SPAWN Constraints, pre-insert  ***/
	/**************************************/
    if ( isDataSpawnTask )
    {
      for ( i=0;  i < theDataSpawnTask . getConstraints() . count();  i++ )
      {
	if ( DataConstraint.getShouldConstraintBeCxxGenerated (
				     theDataSpawnTask . getConstraint ( i ),
				     theDataSpawnTask . getConstraints(),
				     false, /* theIsPostInsert      */
				     false  /* theIsInternalToTasks */ ) )
	{
	  theDataSpawnTask . getConstraint ( i )
			   . generateCxxTaskExternal ( theOutputDestination,
						       errorLocation,
						       spawnObject,
						       returnValueString,
						       true, /* Inside IF */
						       false,/*Not MONITOR*/
						       false /*Don't clear*/ );
	}
      }
    }



	/**********************/
	/* Handle insert-node */
	/**********************/

    theOutputDestination . write ( "\n&& " );
    theOutputDestination . write ( DataComponent.CXX_VERIFY_INSERT_NODE );
    theOutputDestination . write ( " (\n" );
    
	  /* Indent arguments... */
    theOutputDestination . addIndent ( 3 + DataComponent.STANDARD_INDENT * 2 );

    theOutputDestination . write ( errorLocation );
    theOutputDestination . write ( ",\n" );

    theOutputDestination . write ( returnValueString );
    theOutputDestination . write ( DataComponent.CXX_TCM_INSERT_NODE_FUNCTION);
    theOutputDestination . write ( " (\n" );

	/* Indent remaining arguments to this column ... */
    theOutputDestination . addIndent ( DataComponent.STANDARD_INDENT * 2 );

	/* When spawning Outside-of-Tasks, we use the parent value of
	 * CXX_TCM_ROOT_NODE, or the PARENT-constraint if it is specified.
	 * But for generating _TDL_SpawnAndWait_* functions, this is specified.
	 */
    if ( isDataSpawnTask )
    {
      parentIndex = DataConstraint.getLastIndexOfConstraintOfType (
					  DataConstraint.TCM_TASK_TREE_PARENT,
					  theDataSpawnTask );
      if ( parentIndex >= 0 )
      {
	    /* Deal with #line macros */
	if ( theOutputDestination . getEnableLineMacros() )
	{
	  theOutputDestination . setUsingTdlFileName ( true );
	  theOutputDestination . makeNextLineNumber (
	    theDataSpawnTask
	      . getConstraint ( parentIndex )
	      . getTcmTaskTreeParentExpression() );
	  theOutputDestination . write ( "\n" );
	}

	theDataSpawnTask
	  . getConstraint ( parentIndex )
	  . getTcmTaskTreeParentExpression()
	  . generate ( theOutputDestination, DataComponent.ENTIRE_OBJECT );

      	  /* Deal with #line macros */
	if ( theOutputDestination . getEnableLineMacros() )
	  theOutputDestination . setUsingTdlFileName ( false );
      }
      else
	theOutputDestination . write ( DataComponent.CXX_TCM_ROOT_NODE );
    }
    else
      theOutputDestination . write (DataComponent.CXX_SPAWNWAIT_ARGUMENT_NAME);

    theOutputDestination . write ( ",\n" );
    theOutputDestination . write ( spawnObject );
    theOutputDestination . write ( ",\n" );
    if ( isDataSpawnTask )
      theOutputDestination . write ( "TRUE" );
    else
      theOutputDestination . write ( "FALSE" );
    theOutputDestination . write ( " ) )\n" );

	/* Stop indenting Arguments... */
    theOutputDestination
      . removeIndent ( 3 + DataComponent.STANDARD_INDENT * 4 );



	/***************************************/
	/*** SPAWN Constraints, post-insert  ***/
	/***************************************/
    if ( isDataSpawnTask )
    {
      for ( i=0;  i < theDataSpawnTask . getConstraints() . count();  i++ )
      {
	if ( DataConstraint.getShouldConstraintBeCxxGenerated (
				     theDataSpawnTask . getConstraint ( i ),
				     theDataSpawnTask . getConstraints(),
				     true,  /* theIsPostInsert      */
				     false  /* theIsInternalToTasks */ ) )
	{
		/* Only do WAIT constraints once. */
	  if (    theDataSpawnTask . getConstraint ( i ) . getConstraintType()
	       == DataConstraint.WAIT )
	  {
	    if ( waitConstraint == null )
	      waitConstraint = theDataSpawnTask . getConstraint ( i );

	    continue;  /* Skip this constraint. */
	  }

	  theDataSpawnTask . getConstraint ( i )
			   . generateCxxTaskExternal ( theOutputDestination,
						       errorLocation,
						       spawnObject,
						       returnValueString,
						       true, /* Inside IF */
						       false,/*Not MONITOR*/
						       false /*Don't clear*/ );
	}
      }
	/* WAIT constraints also need to be last... */
      if ( waitConstraint != null )
      {
	waitConstraint . generateCxxTaskExternal ( theOutputDestination,
						   errorLocation,
						   spawnObject,
						   returnValueString,
						   true, /* Inside IF */
						   false /*Not MONITOR*/ );
      }
    }


	/*******************************************/
	/* Generate WAIT constraint, if necessary. */
	/* (For DataTaskDefinition case.)          */
	/*******************************************/

    if (   ( waitConstraint  == null  )
	&& ( isDataSpawnTask == false ) )
    {
      DataSpawnTask.WAIT_CONSTRAINT
	. generateCxxTaskExternal ( theOutputDestination,
				    errorLocation,
				    spawnObject,
				    returnValueString,
				    true, /* Inside IF */
				    false /*Not MONITOR*/ );
    }



	/**************************************/
	/* Terminate the massive IF statement */
	/**************************************/

    theOutputDestination . write ( ")" );

	/* Stop indenting the Master if-statement expression */
    theOutputDestination . removeIndent ( ifIndent  );

	/* Write the body. */
    theOutputDestination . write        ( "\n{\n" );
    theOutputDestination . addIndent    ( DataComponent.STANDARD_INDENT );
    theOutputDestination . write        ( 
	"; /* Null Statement!  (Task has been successfully SPAWN'ed) */\n" );
    theOutputDestination . removeIndent ( DataComponent.STANDARD_INDENT );
    theOutputDestination . write        ( "}\n" );


	/***************************************************************/
	/* Add in any TDL_REF_IN constraint assignments here -- AGAIN! */
        /* (Handles any delayed-allocation assignments.)               */
	/***************************************************************/
    privateGenerateCxxOutsideOfTask_internalGenerateTDLREF ( 
							theOutputDestination,
							theSubsetToProduce,
							isDataSpawnTask,
							theDataSpawnTask,
							spawnObject );


	/* Manually clear constraints external subtask name. */
    if ( isDataSpawnTask )
    {
      for ( i=0;  i < theDataSpawnTask . getConstraints() . count();  i++ )
	theDataSpawnTask . getConstraint ( i )
	                 . setCxxExternalSubtaskName ( null );
    }


	/********************************************************/
	/* Write the final return statement or end indenting... */
        /********************************************************/
    if ( isDataSpawnTask )
    {
      	/* We run into an interesting problems with:
	 * if ( ) spawn foo(); -- there is no compound statement...
	 */
      theOutputDestination . removeIndent ( DataComponent.STANDARD_INDENT );
      theOutputDestination . write        ( "}\n" );
    }

    else /* E.g: isDataSpawnTask == false */
    {
      theOutputDestination . write ( "\nreturn " );
      theOutputDestination
	. write ( DataComponent.CXX_SPAWNWAIT_RETURN_VARIABLE );
      theOutputDestination . write ( ";\n" );
    }

  } /* public void privateGenerateCxxOutsideOfTask ( ... ) */


  private static void privateGenerateCxxOutsideOfTask_internalAllocationCheck (
				       DataDestination  theOutputDestination,
				       String           errorLocation,
				       String           spawnObject )
  {
	/***************************/
	/* Handle allocation check */
	/***************************/
    theOutputDestination . write ( DataComponent.CXX_VERIFY_ALLOCATION );
    theOutputDestination . write ( " (\n" );
    
	  /* Indent arguments... */
    theOutputDestination . addIndent ( 3 + DataComponent.STANDARD_INDENT * 2 );

    theOutputDestination . write ( errorLocation );
    theOutputDestination . write ( ",\n" );

    theOutputDestination . write ( spawnObject );
    theOutputDestination . write ( " )\n" );

	/* Stop indenting Arguments... */
    theOutputDestination
      . removeIndent ( 3 + DataComponent.STANDARD_INDENT * 2 );
  }


	/* Lets not write this code twice. */
  private static void privateGenerateCxxOutsideOfTask_internalGenerateName (
					  DataDestination theOutputDestination,
					  boolean         isDataSpawnTask,
					  DataSpawnTask   theDataSpawnTask,
					  String          taskScopeString,
					  String          taskName )
  {
    int   nameConstraintIndex;

	/* Check to see if we have a NAME constraint. */
    if ( isDataSpawnTask == true )
    {
      nameConstraintIndex = DataConstraint.getLastIndexOfConstraintOfType (
					     DataConstraint.TCM_TASK_TREE_NAME,
					     theDataSpawnTask );
    }
    else
    {
      nameConstraintIndex = DataComponent.INVALID_INDEX;
    }


    if ( nameConstraintIndex >= 0 )
    {
		/* Deal with #line macros */
      theOutputDestination . setUsingTdlFileName ( true );
      theOutputDestination . makeNextLineNumber (
	theDataSpawnTask
	  . getConstraint ( nameConstraintIndex )
	  . getTcmTaskTreeNameExpression() );

      if ( theOutputDestination . getEnableLineMacros() )
      {
	theOutputDestination . write ( "\n" );
      }

      theDataSpawnTask
	. getConstraint ( nameConstraintIndex )
	. getTcmTaskTreeNameExpression()
	. generate ( theOutputDestination,
		     DataComponent.ENTIRE_OBJECT );

		/* Deal with #line macros */
      theOutputDestination . setUsingTdlFileName ( false );
    }
    else
    {
      theOutputDestination . write ( "\"" );
      theOutputDestination . write ( taskScopeString );
      theOutputDestination . write ( taskName );

	/* Identify whether this is a outside-Task or a auto-Task */
      if ( isDataSpawnTask )
	theOutputDestination . write ( "-outsideTask\"" );
      else
	theOutputDestination . write ( "-auto,wait\"" );
    }
  }

	/* Lets not write this code twice. */
  private static void privateGenerateCxxOutsideOfTask_internalGenerateTDLREF (
					 DataDestination theOutputDestination,
					 int             theSubsetToProduce,
					 boolean         isDataSpawnTask,
					 DataSpawnTask   theDataSpawnTask,
					 String          spawnObject )
  {
    DataLabelStatement currentLabel;
    int                i;

	/*****************************************************/
	/* Add in any TDL_REF_IN constraint assignments here */
	/*****************************************************/
    if ( isDataSpawnTask )
    {
      for ( currentLabel  = theDataSpawnTask . getLabel();
	    currentLabel != null;
	    currentLabel  = currentLabel . getLabel() )
      {
	if ( currentLabel . hasId() )
	{
	  theOutputDestination . write ( currentLabel . getId() );
	  theOutputDestination . write ( " = " );
	  theOutputDestination . write ( spawnObject );
	  theOutputDestination . write ( ";\n" );
	}
      }

      for ( i=0;  i < theDataSpawnTask . getConstraints() . count();  i++ )
      {
	if (    theDataSpawnTask . getConstraint ( i ) . getConstraintType()
	     == DataConstraint.TDL_REF_IN )
	{
	  theOutputDestination . setStripLeadingWhitespace();
	  theOutputDestination . setPersistentlyStripLeadingSpaces ( true );
	  theDataSpawnTask . getConstraint ( i ) . getTdlRefInExpression()
	    . generate ( theOutputDestination, theSubsetToProduce );
	  theOutputDestination . setPersistentlyStripLeadingSpaces ( false );

	  theOutputDestination . write ( " = " );
	  theOutputDestination . write ( spawnObject );
	  theOutputDestination . write ( ";\n" );
	}

	   /* getCxxExternalSubtaskName is set to a non-null value up above, *
	    * where available, in generateCxxExternalSubtaskDeclarations().  *
	    * It is then cleared down below when we generate the constraint. */
	if ( theDataSpawnTask . getConstraint ( i )
			      . getCxxExternalSubtaskName() != null )
	{
	  for ( currentLabel  = theDataSpawnTask
			          . getConstraint ( i )
			          . getCxxExternalSubtaskDeclarationLabel();
		currentLabel != null;
		currentLabel  = currentLabel . getLabel() )
	  {
	    if ( currentLabel . hasId() )
	    {
	      theOutputDestination . write ( currentLabel . getId() );
	      theOutputDestination . write ( " = " );
	      theOutputDestination . write ( theDataSpawnTask
					       . getConstraint ( i )
					       . getCxxExternalSubtaskName() );
	      theOutputDestination . write ( ";\n" );
	    }
	  } /* FOR ( currentLabel ) */
	} /* IF ( getCxxExternalSubtaskName() != null ) */
      } /* FOR ( 0 <= i < theDataSpawnTask . getConstraints() . count() ) */
    } /* if ( isDataSpawnTask ) */
  }





  public boolean isSpawnRelatedStatement ( )
  {
    return true;
  }

  public int  getChildStatementCount()
  {
    return super . getChildStatementCount()
      + getConstraintChildStatementCount();
  }

  public DataStatement  getChildStatement ( int theIndex )
  {
    if ( theIndex < getConstraintChildStatementCount() )
      return getConstraintChildStatement ( theIndex );
    else
      return super . getChildStatement(   theIndex
					- getConstraintChildStatementCount() );
  }

}

