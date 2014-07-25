/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

public class DataConstraintStatement extends DataStatement
				  implements DataConstrainedObject,
					     DataConstraintTagTaskIndexes
{
	/* Class variables */
  public final static String  BEGIN_TAG_TASK_INDEX
			 = DataConstraintTagTaskIndexes.BEGIN_TAG_TASK_INDEX;
  public final static String  DEFAULT_TAG_TASK_INDEX
			 = DataConstraintTagTaskIndexes.DEFAULT_TAG_TASK_INDEX;
  public final static String  END_TAG_TASK_INDEX
			 = DataConstraintTagTaskIndexes.END_TAG_TASK_INDEX;
  public final static String  TAG_TASK
			 = DataConstraintTagTaskIndexes.TAG_TASK;
  public final static String  THIS
			 = DataComponent.THIS;
  public final static String  SEMICOLON            = ";";


	/* Instance Variables */
  protected String           taskTag;
  protected Object[]         taskTagIndexes;
  protected DataConstraint   constraint;

	/* Instance Methods */
  public DataConstraintStatement ( )
  {
    taskTag        = null;
    taskTagIndexes = null;
    constraint     = new DataConstraint();
    constraint . setParent ( this );
  }

  public boolean getHasTaskTag()
	      { return DataComponent.isEmptyString ( getTaskTag() ) == false; }

  public String getTaskTag ( )    { return taskTag == null ? "" : taskTag; }
  public void   setTaskTag ( String theTaskTag )    { taskTag = theTaskTag; }

  public boolean getHasTaskTagOfThis()
	      { return (    getHasTaskTag()
			 && getTaskTag() . equals ( DataComponent.THIS ) ); }

  public boolean getHasTaskIndexes ( )
  {
    return (   ( getTaskIndexes()          != null )
	    && ( getTaskIndexes() . length >  0    ) );
  }

  public Object[] getTaskIndexes () { return taskTagIndexes; }

  public void     setTaskIndexesWithoutParsing ( Object[] theTaskIndexes )
  {
    taskTagIndexes = theTaskIndexes;
  }

	/* DataConstraintTagTaskIndexes Interface */
  public boolean  getHasTagTask()                { return getHasTaskTag();    }
  public String   getTagTask()                   { return getTaskTag();       }
  public void     setTagTask( String theTagTask ){ setTaskTag (theTagTask);   }
  public boolean  getHasTagTaskIndexes()         { return getHasTaskIndexes();}
  public Object[] getTagTaskIndexes()            { return getTaskIndexes();   }
  public void     setTagTaskIndexesWithoutParsing (Object[] theTagTaskIndexes)
			{ setTaskIndexesWithoutParsing ( theTagTaskIndexes ); }




  public DataConstraint  getConstraint()     { return constraint; }
  public int             getConstraintType()
			      { return getConstraint() . getConstraintType(); }

	/* DataConstrainedObject Interface */
  public int getConstraintCount() { return 1; }
  public DataConstraint  getConstraint( int theIndex )
  {
    if ( theIndex == 0 )
    {
      return getConstraint();
    }
    else
    {
      System.err.println ( "[DataConstraintStatement:getConstraint]  Error:  "
			   + "Invalid Index (" + theIndex + ")" );
      return null;
    }
  }
  public void addConstraint ( DataConstraint theConstraint )
  {
    if ( theConstraint != null )
    {
      constraint = theConstraint;
      theConstraint . setParent ( this );
    }
    else
    {
      System.err.println ( "[DataConstraintStatement:addConstraint]  Error:  "
			   + "NULL Constraint." );
    }
  }


	/* Convenience method. */
  public boolean  getConstrainsSpawnOrWithStatement (
				    DataStatement theDataSpawnOrWithStatement )
  {
    if ( theDataSpawnOrWithStatement instanceof DataSpawnTask )
    {
      if ( getTaskTag() . equals (
	      ((DataSpawnTask) theDataSpawnOrWithStatement) . getTaskName() ) )
	return true;
    }

    for ( DataLabelStatement dataLabelStatement
	    = theDataSpawnOrWithStatement . getLabel();
	  dataLabelStatement != null;
	  dataLabelStatement  = dataLabelStatement . getLabel() )
    {
      if (   ( dataLabelStatement . hasId()                           )
	  && ( getTaskTag() . equals ( dataLabelStatement . getId() ) ) )
	return true;
    } /* FOR ( dataLabelStatement attached to theDataSpawnTask ) */

    return false;
  }






	/* Allow searching for contained expression subparts... */
  public boolean hasSubcomponentFraction ( String theString )
  {
    if ( getHasTaskIndexes() )
    {
      for ( int i=0;  i < getTaskIndexes() . length;  i++ )
      {
	if (   (   ( getTaskIndexes() [ i ] instanceof DataComponent == true  )
		&& ( ((DataComponent) (getTaskIndexes() [ i ]))
		        . hasSubcomponentFraction ( theString ) ) )
	    || (   ( getTaskIndexes() [ i ] instanceof DataComponent == false )
		&& ( getTaskIndexes() [ i ]
		       . toString() . indexOf ( theString ) != -1 ) ) )
	{
	  return true;
	}
      }
    }

    return super               . hasSubcomponentFraction ( theString )
      ||   getConstraint ( 0 ) . hasSubcomponentFraction ( theString );
  }


	/* Allow searching of our expressions... */
  public void runOnSubcomponentFraction (
			String                      theString,
			RunOnSubcomponentInterface  theRunOnSubcomponentObject,
			Object                      theArgumentObject )
  {
    super . runOnSubcomponentFraction ( theString,
					theRunOnSubcomponentObject,
					theArgumentObject );

    if ( getHasTaskIndexes() )
    {
      for ( int i=0;  i < getTaskIndexes() . length;  i++ )
      {
	if ( getTaskIndexes() [ i ] instanceof DataComponent )
	  ((DataComponent) (getTaskIndexes() [ i ]))
	     . runOnSubcomponentFraction ( theString,
					   theRunOnSubcomponentObject,
					   theArgumentObject );
	else
	  DataComponent.staticRunOnStringFraction (
				  getTaskIndexes() [ i ] . toString(),
				  theString,
				  theRunOnSubcomponentObject,
				  theArgumentObject );
      }
    }

    getConstraint ( 0 )
      . runOnSubcomponentFraction ( theString,
				    theRunOnSubcomponentObject,
				    theArgumentObject );
  }




  	/* Validates code that is inside of a Task */
  public void validateInternalCode( int                         theReference,
				    DataValidateCodeReturnValue theReturnValue)
    throws CompilationException
  {
    DataComponent       ancestor, taskTagDataComponent;
    DataTaskDefinition  ancestorDataTaskDefinition;
    int                 iterationCount;

    if ( getHasTaskTag() )
    {
    	/* Find our DataTaskDefinition Ancestor */
      for ( ancestor  = this;
	    ancestor != null;
	    ancestor  = ancestor . getParent()
	   )
      {
	if ( ancestor instanceof DataTaskDefinition )
	  break;
      }
      if ( ancestor == null )
      {
	throw new CompilationException (
	   getMessageFilenameLead() + getLineNumberString()
	 + ": Programmer Error:  Unable to find DataTaskDefinition ancestor.");
      }
      ancestorDataTaskDefinition = (DataTaskDefinition) ancestor;

	/* Find the taskTagDataComponent */
      taskTagDataComponent = ancestorDataTaskDefinition
			      . getDataComponentWithName ( getTaskTag() );
	/* If this is null, the error will be picked up elsewhere. */
      if ( taskTagDataComponent != null )
      {
	  /* Find out how many iteration loops enclose the tagTask Object */
	  /* Note:  This can throw a CompilationException.                */
	iterationCount = ancestorDataTaskDefinition
			   . getIterationParentCount ( taskTagDataComponent );


	  /* And lets double-check this stuff. */
	if ( taskTagDataComponent instanceof DataBindTaskStatement )
	{
	  if ( getHasTaskIndexes() == true )
	  {
	    theReturnValue
	      . addError ( this )
	      . write ( "Too many Iteration-Indexes for TDL_BIND "          )
	      . write ( "Constraint Statement Reference.  (There is (are) " )
	      . write ( "" + getTaskIndexes() . length                      )
	      . write ( " array index(es) specified.  "                     )
	      . write ( "There should be zero.)\n"                          );
	  }
	}
	else
	{
	  if (   ( getHasTaskIndexes()       == true           )
	      && ( getTaskIndexes() . length >  iterationCount ) )
	  {
	    theReturnValue
	      . addError ( this )
	      . write ( "Too many Iteration-Indexes for Constraint "        )
	      . write ( " Statement Reference.  (Reference is enclosed in " )
	      . write ( "" + iterationCount                                 )
	      . write ( " iteration loop(s) and there is (are) "            )
	      . write ( "" + getTaskIndexes() . length                      )
	      . write ( " array index(es) specified.)\n"                    );
	  }

	  if (   (     iterationCount            >  0                )
	      && (   ( getTaskIndexes()          == null           )
		  || ( getTaskIndexes() . length <  iterationCount ) ) )
	  {
	    theReturnValue
	      . addWarning ( this )
	      . write ( "Using an Iteration-Set of References for Constraint")
	      . write ( " Statement Reference.  (Reference is enclosed in "  )
	      . write ( "" + iterationCount                                  )
	      . write ( " iteration loop(s) and there is (are) "             )
	      . write (   ( getTaskIndexes() == null )
			? "0" : ("" + getTaskIndexes() . length)             )
	      . write ( " array index(es) specified.)\n"                     );
	  }
	} /* IF (tagTaskDataComponent instanceof DataBindTaskStatement) ELSE */
      } /* if ( taskTagDataComponent != null ) */
    } /* if ( getHasTaskTag() ) */



    for ( int i=0;
	  getHasTaskIndexes() && (i < getTaskIndexes().length);
	  i++ )
    {
      if (   ( getTaskIndexes()[i] . toString() . trim() . charAt (0) == '0')
	  && ( getTaskIndexes()[i] . toString() . trim() . length()   >   1 ) )
      {
	theReturnValue
	  . addWarning ( this )
	  . write ( "Task Index is specified in octal.\n" );
	break; /* Don't test any more indexes... */
      }
    }

	/* Perform super-class validation */
    super . validateInternalCode ( theReference, theReturnValue );

    if ( getConstraint() . getConstraintType() == DataConstraint.PARALLEL )
    {
      theReturnValue
	. addWarning ( getConstraint() )
	. write ( "PARALLEL constraint in a CONSTRAINT STATEMENT is "
		  + "meaningless.\n" );
    }

    getConstraint() . validateInternalCode ( theReference, theReturnValue );
  }



  protected void generateTaskTag ( DataDestination  theOutputDestination,
				   int              theObjectSubsetToGenerate )
    throws CompilationException
  {
    Object[] taskIndexes = getTaskIndexes();

	/* Idiocy check */
    if ( getHasTaskTag() == false )
    {
      throw new CompilationException ( 
			getMessageFilenameLead() + getLineNumberString()
		      + ":  [DataConstraintStatement:generateEventTagTask]  "
		      + "Error:  No Task-Tag available to generate..." );
    }

	/* Write our task-tag */
    theOutputDestination . write ( getTaskTag() );


	/* If we do have indexes... */
    if ( getHasTaskIndexes() )
    {
      for ( int i=0;   i < taskIndexes.length;   i++ )
      {
	  /* Write non-significant tokens & open-index */
	generateSubcomponents (
			     DataConstraintStatement.BEGIN_TAG_TASK_INDEX + i,
			     theOutputDestination,
			     theObjectSubsetToGenerate, false );
	theOutputDestination
	  . write ( DataConstraintStatement.BEGIN_TAG_TASK_INDEX );


	  /* Write non-significant tokens & index */      
	generateSubcomponents ( "" + i,
				theOutputDestination,
				theObjectSubsetToGenerate, false );
	if ( taskIndexes [ i ] != null )
	{
	  if ( taskIndexes [ i ] instanceof DataComponent )
	  {
	    ((DataComponent) (taskIndexes [ i ]))
	      . generate ( theOutputDestination, DataComponent.ENTIRE_OBJECT );
	  }
	  else
	  {
	    theOutputDestination . write ( taskIndexes [ i ] . toString() );
	  }
	}

	  /* Write non-significant tokens & close-index */
	generateSubcomponents ( DataConstraintStatement.END_TAG_TASK_INDEX + i,
				theOutputDestination,
				theObjectSubsetToGenerate, false );
	theOutputDestination . write ( DataConstraint.END_TAG_TASK_INDEX );
      }
    }
  }




  public void generate ( DataDestination  theOutputDestination,
			 int              theObjectSubsetToGenerate )
  {
       	/* Print a warning if necessary... */
    warnIfInvalidObjectSubset ( theObjectSubsetToGenerate, "generate" );

        	/* Initialize us to generate non-significant tokens... */
    initializeGenerateSubcomponentIndex();

	/* Generate any labels that we have... */
    generateLabels ( theOutputDestination, theObjectSubsetToGenerate,
		     DataConstraintStatement.TAG_TASK );

	/* Write any pre-task-tag non-significant tokens */
    generateSubcomponents ( DataConstraintStatement.TAG_TASK,
			    theOutputDestination,
			    theObjectSubsetToGenerate, false );

	/* If we are doing CXX generation... */
    if ( isCxxSubset ( theObjectSubsetToGenerate ) )
    {
      theOutputDestination . setNewlineText ( "    // " );
      theOutputDestination . write          ( "    // " );
    }

	/* Write our task-tag */
    generateTaskTag ( theOutputDestination, DataComponent.ENTIRE_OBJECT );


	/* Write our constraint */
    getConstraint() . generate ( theOutputDestination,
				 DataComponent.ENTIRE_OBJECT );


	/* Write any pre-";" non-significant tokens */
    generateSubcomponents ( DataConstraintStatement.SEMICOLON,
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
      return;
    }
  }


  public void generateCxx ( DataDestination  theOutputDestination,
			    int              theSubsetToProduce   )
    throws CompilationException
  {
    int  indent;

    if ( getHasTaskTag() == false )
    {
      throw new CompilationException (   getMessageFilenameLead()
				       + getLineNumberString()
				       + ":  Error:  Missing Task-Tag." );
    }

	/* Deal with #line macros */
    theOutputDestination . setUsingTdlFileName ( false );
    theOutputDestination . write ( "\n" ); /* This newline will be skipped.*/


	/* Write "_TDL_SpawnedTasks . applyConstraintTo ( " */
    theOutputDestination
      . write ( DataComponent.CXX_TDL_TASK_HANDLER_INSTANCE );
    theOutputDestination . write ( " . " );
    theOutputDestination
      . write ( DataComponent.CXX_TDL_TASK_HANDLER_APPLY_CONSTRAINT_TO );
    theOutputDestination . write ( " ( " );

    indent = theOutputDestination . indentToCurrentColumn();


	/* Write the corresponding CXX generated string. */
    if ( getHasTaskTagOfThis() )
      theOutputDestination
	. write ( DataComponent.CXX_ENCLOSING_TASK_CONSTRAINT_OBJECT );
    else
      DataStatement.generateTagTaskCxxReference ( theOutputDestination,
						  getTaskTag(),
						  getTaskIndexes(),
						  this );
    theOutputDestination . write ( ",\n" );



	/* Generate constraint in the inside-task style. */
    getConstraint() . generateCxxTaskInternal ( theOutputDestination );

    theOutputDestination . removeIndent ( indent );

    theOutputDestination . write ( " );\n" );
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

