
/*
 * This represents an arbitrary C/C++ Expression Statement
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 *
 */

public class DataBindTaskStatement extends DataStatement
{
	/* Class Constants. */
  public final static String   BIND_INDEX  = "BindIndex";
  public final static String   OPEN_PAREN  = "(";
  public final static String   TASK_VALUE  = "TaskValueIndex";
  public final static String   COMMA       = ",";
  public final static String   TASK_NAME   = "TaskNameIndex";
  public final static String   CLOSE_PAREN = ")";
  public final static String   SEMICOLON   = ";";


	/* Instance Variables */
  protected String           taskValueToBind;
  protected String           taskNameToBind;
  protected int              lineNumberOfTaskValueToBind;


	/* Instance Methods */
  public DataBindTaskStatement()
  {
    taskValueToBind             = null;
    taskNameToBind              = null;
    lineNumberOfTaskValueToBind = DataComponent.INVALID_LINE_NUMBER;
  }


  public boolean hasNullTaskValueToBind()
  {
    return taskValueToBind == null;
  }

  public String getTaskValueToBind()
  {
    if ( hasNullTaskValueToBind() )
      return "";
    else
      return taskValueToBind;
  }

  public void   setTaskValueToBind( String theTaskValueToBind )
  {
    taskValueToBind = theTaskValueToBind;
  }


  public boolean hasNullTaskNameToBind()
  {
    return taskNameToBind == null;
  }

  public String getTaskNameToBind()
  {
    if ( hasNullTaskNameToBind() )
      return getTaskValueToBind();
    else
      return taskNameToBind;
  }

  public void   setTaskNameToBind( String theTaskNameToBind )
  {
    taskNameToBind = theTaskNameToBind;
  }


  public void setLineNumberOfTaskValueToBind (
					  int theLineNumberOfTaskValueToBind )
	      { lineNumberOfTaskValueToBind = theLineNumberOfTaskValueToBind; }

  public int  getLineNumberOfTaskValueToBind()
					{ return lineNumberOfTaskValueToBind; }

  public boolean hasValidLineNumberOfTaskValueToBind()
  {
    return DataComponent.isValidLineNumber( getLineNumberOfTaskValueToBind() );
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
		     DataBindTaskStatement.BIND_INDEX );

	/* Write any non-significant tokens before the task-registration... */
    generateSubcomponents ( DataBindTaskStatement.BIND_INDEX,
			    theOutputDestination,
			    theObjectSubsetToGenerate, false );

	/* If we are doing CXX generation... */
    if ( isCxxSubset ( theObjectSubsetToGenerate ) )
    {
      theOutputDestination . setNewlineText ( "    // " );
      theOutputDestination . write          ( "    // " );
    }

	/* Write out TDL_BIND */
    theOutputDestination . write ( DataComponent.TDL_BIND );


	/* Write any non-significant tokens before the open-paren */
    generateSubcomponents ( DataBindTaskStatement.OPEN_PAREN,
			    theOutputDestination,
			    theObjectSubsetToGenerate, false );

	/* Write out "(" */
    theOutputDestination . write ( DataBindTaskStatement.OPEN_PAREN );


	/* Write any non-significant tokens before the task-value */
    generateSubcomponents ( DataBindTaskStatement.TASK_VALUE,
			    theOutputDestination,
			    theObjectSubsetToGenerate, false );

	/* Write out task-value */
    theOutputDestination . write ( getTaskValueToBind() );


    if ( hasNullTaskNameToBind() == false )
    {
	/* Write any non-significant tokens before the comma */
      generateSubcomponents ( DataBindTaskStatement.COMMA,
			      theOutputDestination,
			      theObjectSubsetToGenerate, false );

	/* Write out "," */
      theOutputDestination . write ( DataBindTaskStatement.COMMA );


	/* Write any non-significant tokens before the task-name */
      generateSubcomponents ( DataBindTaskStatement.TASK_NAME,
			      theOutputDestination,
			      theObjectSubsetToGenerate, false );

	/* Write out task-name */
      theOutputDestination . write ( getTaskNameToBind() );
    }


	/* Write any non-significant tokens before the close-paren */
    generateSubcomponents ( DataBindTaskStatement.CLOSE_PAREN,
			    theOutputDestination,
			    theObjectSubsetToGenerate, false );

	/* Write out ")" */
    theOutputDestination . write ( DataBindTaskStatement.CLOSE_PAREN );


	/* Write any non-significant tokens before the semicolon... */
    generateSubcomponents ( DataBindTaskStatement.SEMICOLON,
			    theOutputDestination,
			    theObjectSubsetToGenerate, false );

	/* Write our semicolon */
    theOutputDestination . write ( DataBindTaskStatement.SEMICOLON );


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
  }

  public void generateCxx ( DataDestination  theOutputDestination,
			    int              theObjectSubsetToGenerate )
  {
    int tmpIndent = 0;

	/* Deal with #line macros */
    theOutputDestination . setUsingTdlFileName ( false );
    theOutputDestination . write ( "\n" ); /* This newline will be skipped.*/

	/* Write _TDL_SpawnedTasks . bindTCMTask ( theTask, "theTask" ); */
    theOutputDestination
      . write ( DataComponent.CXX_TDL_TASK_HANDLER_INSTANCE );
    theOutputDestination . write ( " . " );
    theOutputDestination
      . write ( DataComponent.CXX_TDL_TASK_HANDLER_BIND_TCM_TASK );
    theOutputDestination . write ( " ( " );
    
	/* Deal with #line macros */
    if ( theOutputDestination . getEnableLineMacros() )
    {
      tmpIndent = theOutputDestination . indentToCurrentColumn();
      theOutputDestination . setUsingTdlFileName ( true );
      theOutputDestination
	. makeNextLineNumber ( getLineNumberOfTaskValueToBind() );
      theOutputDestination . write ( "\n" );
    }

    theOutputDestination . write ( getTaskValueToBind() );
    theOutputDestination . write ( " , " );

	/* Deal with #line macros */
    if ( theOutputDestination . getEnableLineMacros() )
    {
      theOutputDestination . setUsingTdlFileName ( false );
      theOutputDestination . write ( "\n\"" );
      theOutputDestination . removeIndent ( tmpIndent );
    }
    else
    {
      theOutputDestination . write ( "\"" );
    }

    theOutputDestination . write ( getTaskNameToBind() );
    theOutputDestination . write ( "\" );\n" );
  }




  public boolean isSpawnRelatedStatement ( )
  {
    return true;
  }
}
