
/*
 * This represents a break, continue, or return C++ statement,
 * and the SUCCESS, FAIL, BYPASS, and POSTPONE TDL statements.
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 *
 */

public class DataJumpStatement extends DataStatement 
{
	/* Class variables */
  public final static String   FIRST_TOKEN_INDEX = "First";
  public final static String   SEMICOLON         = ";";

  public final static int      BREAK_TYPE        = 0;
  public final static int      CONTINUE_TYPE     = 1;
  public final static int      RETURN_TYPE       = 2;
  public final static int      SUCCESS_TYPE      = 3;
  public final static int      FAIL_TYPE         = 4;
  public final static int      BYPASS_TYPE       = 5;
  public final static int      POSTPONE_TYPE     = 6;

  public final static String   JUMP_TYPES[]      = { "break",
						     "continue",
						     "return",
						     "SUCCESS",
						     "FAIL",
						     "BYPASS",
						     "POSTPONE" };

	/* Instance Variables */
  protected int              type;
  protected DataExpression   dataExpression; /* Return value. Should be null.*/

	/* FAIL's have an associated exception.  The DataSpawnTask class  */
	/* has all the features we need to support this exception (here). */
  protected DataSpawnTask          exceptionTask;


	/* Instance Methods */
  public DataJumpStatement()
  {
    type           = DataJumpStatement.RETURN_TYPE;
    dataExpression = null;
    exceptionTask  = null;
  }

  public int  getType() { return type; }

  public void setType ( int theType )
  {
    if (   ( theType == DataJumpStatement.BREAK_TYPE    )
	|| ( theType == DataJumpStatement.CONTINUE_TYPE )
	|| ( theType == DataJumpStatement.RETURN_TYPE   )
	|| ( theType == DataJumpStatement.SUCCESS_TYPE  )
	|| ( theType == DataJumpStatement.FAIL_TYPE     )
	|| ( theType == DataJumpStatement.BYPASS_TYPE   )
	|| ( theType == DataJumpStatement.POSTPONE_TYPE ) )
    {
      type = theType;
    }
    else
    {
      System.err.println( "[DataJumpStatement:setType]  Error:  "
			  + "Illegal type: " + theType + "  Aborting set..." );
    }
  }

  public DataExpression getDataExpression () { return dataExpression; }

  public void           setDataExpression ( DataExpression theDataExpression )
  {
    if ( dataExpression != null )
      dataExpression . setParent ( null );

    dataExpression = theDataExpression;

    if ( dataExpression != null )
      dataExpression . setParent ( this );
  }


  public DataSpawnTask getExceptionTask () { return exceptionTask; }
  public boolean       setExceptionTask ( DataSpawnTask theExceptionTask )
  {
    if (   (   ( theExceptionTask                            != null )
	    && ( theExceptionTask . getTaskName()            != null )
	    && ( theExceptionTask . getTaskName() . length() >  0    )
	    && ( theExceptionTask . getConstraintCount()     == 0    ) )
	|| (     theExceptionTask == null                              ) )
    {
      exceptionTask = theExceptionTask;
      return true;
    }
    else
    {
      System.err.println ( "[DataJumpStatement:setExceptionTask]  Error: "
			   + "Bad object for theExceptionTask:  " 
			   + theExceptionTask . toString() );
      return false;
    }
  }



	/* Allow searching of our expression... */
  public boolean hasSubcomponentFraction ( String theString )
  {
    return super                   . hasSubcomponentFraction ( theString )
      || (   ( getDataExpression() != null )
	  && ( getDataExpression() . hasSubcomponentFraction ( theString ) ) );
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

    if ( getDataExpression() != null )
      getDataExpression()
	. runOnSubcomponentFraction ( theString,
				      theRunOnSubcomponentObject,
				      theArgumentObject );
  }




	/* Validates code that is inside of a Task */
  public void validateInternalCode( int                         theReference,
				    DataValidateCodeReturnValue theReturnValue)
  {
    DataComponent        ancestor;
    DataTaskDefinition   parentTask = getParentTaskDefinition();

    super . validateInternalCode ( theReference, theReturnValue );


    if ( getType() == DataJumpStatement.BREAK_TYPE )
    {
      for ( ancestor = getParent();
	    (   (   ancestor != null )
	     && ( ( ancestor instanceof DataIterationStatement ) == false )
	     && ( ( ancestor instanceof DataSwitchStatement    ) == false ) );
	    ancestor = ancestor . getParent() )
        ; /* NULL (EMPTY) LOOP BODY */

      if ( ancestor == null )
      {
	theReturnValue
	  . addWarning ( this )
	  . write ( "Break statement outside of any enclosing iteration "
		    + "or switch statement.\n" );
      }
    }

    if ( getType() == DataJumpStatement.CONTINUE_TYPE )
    {
      for ( ancestor = getParent();
	    (   (   ancestor != null )
	     && ( ( ancestor instanceof DataIterationStatement ) == false ) );
	    ancestor = ancestor . getParent() )
        ; /* NULL (EMPTY) LOOP BODY */

      if ( ancestor == null )
      {
	theReturnValue
	  . addWarning ( this )
	  . write ( "Continue statement outside of any enclosing iteration "
		    + "statement.\n" );
      }
    }


    if (   ( getType()          == DataJumpStatement.FAIL_TYPE           )
	&& ( getExceptionTask() != null                                  )
	&& (   ( getExceptionTask() . getTaskName()            == null )
	    || ( getExceptionTask() . getTaskName() . length() <= 0    ) ) )
    {
      theReturnValue
	. addError ( this )
	. write ( "Exception specified in FAIL statement has no name.\n" );
    }

    if ( parentTask == null )
    {
      theReturnValue
	. addWarning ( this )
	. write ( "Internal Error -- Unable to locate parent Task in ")
	. write ( "DataJumpStatement.validateInternalCode().\n" );
    }
    else
    {
      if (   ( getType()                  == DataJumpStatement.FAIL_TYPE    )
	  && ( getExceptionTask()         == null                           )
	  && ( parentTask . getTaskType() != DataTaskDefinition.HANDLER_TASK)
	  && (   (    parentTask . getTaskType()
		   != DataTaskDefinition.RESUME_TASK                     )
	      || (    parentTask . getResumeMasterTask()
		   == null                                               )
	      || (    parentTask . getResumeMasterTask() . getTaskType()
		   != DataTaskDefinition.HANDLER_TASK                    )  ) )
      {
	theReturnValue
	  . addWarning ( this )
	  . write ( "FAIL statement lacks an Exception.\n" );
      }

      if (   ( getType()                  == DataJumpStatement.BYPASS_TYPE  )
	  && ( parentTask . getTaskType() != DataTaskDefinition.HANDLER_TASK) )
      {
	theReturnValue
	  . addError ( this )
	  . write ( "BYPASS statement encountered outside of an " )
	  . write ( "Exception-Handler Task.\n" );
      }
    }
  }



  public boolean isValid ( int theObjectSubsetToValidate )
  {
    DataTaskDefinition   parentTask = getParentTaskDefinition();

    if (   ( getType()          == DataJumpStatement.FAIL_TYPE           )
	&& ( getExceptionTask() != null                                  )
	&& (   ( getExceptionTask() . getTaskName()            == null )
	    || ( getExceptionTask() . getTaskName() . length() <= 0    ) ) )
    {
      return false;
    }

    if (   ( getType()          == DataJumpStatement.FAIL_TYPE             )
	&& ( getExceptionTask() == null                                    )
	&& ( parentTask         != null                                    )
	&& ( parentTask . getTaskType() != DataTaskDefinition.HANDLER_TASK ) )
    {
      return false;
    }

    return super . isValid ( theObjectSubsetToValidate );
  }


  public void generateReturn ( DataDestination  theOutputDestination,
			       int              theObjectSubsetToGenerate,
			       boolean          theDoCompletedSuccessfully,
			       boolean          theReturnIsPartOfTdlCode    )
  {
    if ( theDoCompletedSuccessfully )
    {
	/* Deal with #line macros */
      theOutputDestination . setUsingTdlFileName ( false );
      theOutputDestination . write ( "\n" );

      theOutputDestination
	. write ( DataComponent.CXX_TCM_TASK_COMPLETED_SUCCESSFULLY );
      theOutputDestination . write ( " ( " );
      theOutputDestination . write ( DataComponent.CXX_ENCLOSING_TASK_REF );
      theOutputDestination . write ( " );" );
    }

	/* Deal with #line macros */
    if ( theReturnIsPartOfTdlCode )
    {
      theOutputDestination . setUsingTdlFileName ( true );
      theOutputDestination . makeNextLineNumber ( getLineNumber() );
    }
    else
    {
      theOutputDestination . setUsingTdlFileName ( false );
    }
    theOutputDestination . write ( "\n" );


    theOutputDestination . write ( "return" );

    if ( getDataExpression() != null )
      getDataExpression() . generate ( theOutputDestination,
				       theObjectSubsetToGenerate );
  }


  public void generate ( DataDestination  theOutputDestination,
			 int              theObjectSubsetToGenerate )
  {
    int                  indent, unindent;
    DataTaskDefinition   parentTask = getParentTaskDefinition();

       	/* Print a warning if necessary... */
    warnIfInvalidObjectSubset ( theObjectSubsetToGenerate, "generate" );

    	/* Initialize us to generate non-significant tokens... */
    initializeGenerateSubcomponentIndex();

	/* Generate any labels that we have... */
    generateLabels ( theOutputDestination, theObjectSubsetToGenerate,
		     DataJumpStatement.FIRST_TOKEN_INDEX );

	/* Write any pre-first-token non-significant tokens */
    generateSubcomponents ( DataJumpStatement.FIRST_TOKEN_INDEX,
			    theOutputDestination,
			    theObjectSubsetToGenerate, false );


	/* Write the jump-statement's token(s) */

    if (   ( getType() == DataJumpStatement.BREAK_TYPE    )
	|| ( getType() == DataJumpStatement.CONTINUE_TYPE ) )
    {
      theOutputDestination
	. write ( DataJumpStatement.JUMP_TYPES [ getType() ] );
    }


    else if ( getType() == DataJumpStatement.RETURN_TYPE )
    {
      generateReturn ( theOutputDestination, theObjectSubsetToGenerate,
		       isCxxSubset ( theObjectSubsetToGenerate ), true );
    }


    else if (   (     getType()  == DataJumpStatement.SUCCESS_TYPE        )
	     || (     getType()  == DataJumpStatement.BYPASS_TYPE         )
	     || (     getType()  == DataJumpStatement.POSTPONE_TYPE       )

		/* FAIL implements as BYPASS under special circumstances */
	     || (   ( getType()          == DataJumpStatement.FAIL_TYPE )
		 && ( getExceptionTask() == null                        )
		 && ( parentTask         != null                        )
		 && ( isCxxSubset ( theObjectSubsetToGenerate )         )
		 && (   (        parentTask . getTaskType()
			      == DataTaskDefinition.HANDLER_TASK      )
		     || (   (    parentTask . getTaskType()
			      == DataTaskDefinition.RESUME_TASK     )
			 && (    parentTask . getResumeMasterTask()
                              != null                               )
			 && (    parentTask . getResumeMasterTask()
				            . getTaskType()
			      == DataTaskDefinition.HANDLER_TASK    ) ) ) ) )
    {
      if ( isCxxSubset ( theObjectSubsetToGenerate ) )
      {
	theOutputDestination . write ( "// " );
      }

      theOutputDestination
	. write ( DataJumpStatement.JUMP_TYPES [ getType() ] );

      if ( isCxxSubset ( theObjectSubsetToGenerate ) )
      {
	theOutputDestination . write ( " ;\n" );

	if (   ( getType() == DataJumpStatement.BYPASS_TYPE )
	    || ( getType() == DataJumpStatement.FAIL_TYPE   ) )
	{
	  theOutputDestination . write ( DataComponent.CXX_TCM_BYPASS );
	  theOutputDestination . write ( " ( " );
	  theOutputDestination . write ( DataComponent.CXX_ENCLOSING_TASK_REF);
	  theOutputDestination . write ( " );\n" );
	}

	generateReturn ( theOutputDestination, theObjectSubsetToGenerate,
			 getType() == DataJumpStatement.SUCCESS_TYPE, false );
      }
    }


    else if ( getType() == DataJumpStatement.FAIL_TYPE )
    {
      if ( isCxxSubset ( theObjectSubsetToGenerate ) )
      {
	theOutputDestination . setNewlineText ( "// " );
	theOutputDestination . write ( "// " );
      }

      theOutputDestination
	. write ( DataJumpStatement.JUMP_TYPES [ getType() ] );


      if ( getExceptionTask() != null )
      {
	   /* Write the exception-task, properly indenting it... */
	indent = theOutputDestination.indentToCurrentColumn();

	  /* Remove indentation for "// " string */
	if ( indent >= theOutputDestination . getNewlineText() . length() )
	{
	  theOutputDestination
	    . removeIndent ( theOutputDestination . getNewlineText()
			                          . length() );
	  indent -= theOutputDestination . getNewlineText() . length();
	}

	getExceptionTask() . generate ( theOutputDestination,
					DataSpawnTask.TASK_ONLY );
	theOutputDestination . removeIndent ( indent );
      }


      if ( isCxxSubset ( theObjectSubsetToGenerate ) )
      {
	theOutputDestination . write ( " ;" );
	theOutputDestination . clearNewlineText();

	/* Deal with #line macros */
	theOutputDestination . setUsingTdlFileName ( false );
	theOutputDestination . write ( "\n" );

	theOutputDestination . write ( DataComponent.CXX_TCM_FAIL );
	theOutputDestination . write ( " ( " );

	indent = theOutputDestination.indentToCurrentColumn();

	theOutputDestination . write ( DataComponent.CXX_ENCLOSING_TASK_REF);
	theOutputDestination . write ( ",\n" );

		/* Write the create-exception bit. */
	theOutputDestination . addIndent ( 1 );

	indent += 1;

	if ( getExceptionTask() != null )
	{
// da0g marker
		/* Unindent by the exception-tasks's leading whitespace */
	  unindent
	    = getExceptionTask()
	        . getLeadingWhitespaceIndent ( DataSpawnTask.TASK_NAME_INDEX );

	  if ( theOutputDestination . getIndent() > unindent )
	  {
	    theOutputDestination . removeIndent ( unindent );
	    indent -= unindent;
	  }

	  getExceptionTask()
	   . generateTask ( theOutputDestination,
			    DataComponent.ENTIRE_OBJECT,
			    false,  /* No leading space    */
			    false,  /* No leading comments */
			    true,   /* Indent nicely,      */
			    false,  /*   on current line.  */
			    isCxxSubset ( theObjectSubsetToGenerate ),
			    DataComponent.CXX_CREATE_EXCEPTION_FUNCTION_LEAD );
	}
	else  /* Generic exception case... */
	{
	  theOutputDestination
	    . write ( DataComponent.CXX_CREATE_EXCEPTION_FUNCTION_LEAD );

	  theOutputDestination . setStripLeadingWhitespace();

	  theOutputDestination
	    . write ( DataComponent.CXX_EXCEPTION_BASE_CLASS );

	  theOutputDestination . write ( "()" );
	}

	theOutputDestination . removeIndent ( indent );

	theOutputDestination . write ( " );\n" );
	generateReturn ( theOutputDestination, theObjectSubsetToGenerate,
			 false, false );
      }
    }


    else
    {
      System.err.println( "[DataJumpStatement:generate]  Error:  "
			  + "Illegal type: " + getType() );
    }


	/* Write any non-significant tokens before the semicolon... */
    generateSubcomponents ( DataJumpStatement.SEMICOLON,
			    theOutputDestination,
			    theObjectSubsetToGenerate, false );

	/* Write our semicolon */
    theOutputDestination . write ( DataJumpStatement.SEMICOLON );

	/* Write any remaining non-significant tokens */
	/* (There *SHOULD* *NOT* be any...  But just in case...) */
    generateAllRemainingSubcomponents ( theOutputDestination,
					theObjectSubsetToGenerate, false );
  }

}
