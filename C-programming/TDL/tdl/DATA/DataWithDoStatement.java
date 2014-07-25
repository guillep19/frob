
/*
 * This represents a TDL "WITH" statement.
 *   (Caveat:  The "do" in "with-do" has been dropped.)
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 *
 */

public class DataWithDoStatement extends DataStatementWithSubstatements
			      implements DataConstrainedObject
{
	/* Class variables */
  public final static String   WITH             = "WITH";
  public final static String   OPEN_PAREN       = "(";
  public final static String   CLOSE_PAREN      = ")";
  public final static String   COMMA            = ",";
  public final static String   STATEMENT_INDEX  = "StatementIndex";
  public final static String   SEMICOLON        = ";";

  public final static int      CONSTRAINTS_ONLY = 2;


	/* Instance Variables */
  protected DataVector   constraints;


	/* Instance Methods */
  public DataWithDoStatement()
  {
    constraints = new DataVector();
  }

  public DataVector getConstraints()     { return constraints; }
  public int        getConstraintCount() { return getConstraints() . count(); }

  public void addConstraint ( DataConstraint theConstraint )
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


	/* Allow searching for contained expression subparts... */
  public boolean hasSubcomponentFraction ( String theString )
  {
    int i;

    if ( super . hasSubcomponentFraction ( theString ) )
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

    for ( i=0;   i < getConstraintCount();   i++ )
      getConstraint ( i )
	. runOnSubcomponentFraction ( theString,
				      theRunOnSubcomponentObject,
				      theArgumentObject );
  }



	/* Validates code that is inside of a Task */
  public void validateInternalCode( int                         theReference,
				    DataValidateCodeReturnValue theReturnValue)
    throws CompilationException
  {
	/* Validate our constraints */
    for ( int i=0;  i < getConstraints().count();  i++ )
    {
      getConstraint ( i )
	. validateInternalCode( theReference, theReturnValue );

	/* Exception-handlers and on-terminate constraints inside with-do *
	 * statements cause bad things to happen.  (1-N binding issue.)   */
      switch ( getConstraint ( i ) . getConstraintType() )
      {
	case DataConstraint.EXCEPTION_HANDLER:
	  theReturnValue
	    . addError ( getConstraint ( i ) )
	    . write ( "Exception Handlers " )
	    . write ( "are not permitted in WITH statements.\n" );
	  break;

	case DataConstraint.ON_TERMINATE:
	  theReturnValue
	    . addError ( getConstraint ( i ) )
	    . write ( "On-Terminate constraints " )
	    . write ( "are not permitted in WITH statements.\n" );
	  break;

	default:
	  break;
      }
    }

            /* Validate mutually inconsistent constraints */
    DataStatement.validateInternalConstrainedObject ( theReference,
						      theReturnValue,
						      this );

	/* Check for overriden constraints */
    DataStatement.validateAnyOverridenConstraints ( this, theReturnValue );


	/* Perform super-class validation, which will check our children. */
    super . validateInternalCode ( theReference, theReturnValue );
  }



  public String getWarnString ( int theObjectSubset )
  {
    return super . getWarnString ( theObjectSubset )
      + " or DataWithDoStatement.CONSTRAINTS_ONLY ("
      + DataWithDoStatement.CONSTRAINTS_ONLY + ")";
  }

  public boolean isValidObjectSubset ( int theObjectSubset )
  {
    if ( theObjectSubset == DataWithDoStatement.CONSTRAINTS_ONLY )
      return true;
    else
      return super . isValidObjectSubset ( theObjectSubset );
  }


  public boolean isValid ( int theObjectSubsetToValidate )
  {
	/* Check that we have constraints... */
    if ( getConstraints() . count() <= 0 )
      return false;

	/* Check the constraints */
    for ( int i=0;  i < getConstraints().count();  i++ )
      if ( getConstraint ( i ) . isValid() == false )
	return false;

	/* Are we done? */
    if ( theObjectSubsetToValidate == DataWithDoStatement.CONSTRAINTS_ONLY )
      return true;

	/* Check that we have a statement... */
    if ( getStatement() == null )
      return false;

    return super.isValid ( theObjectSubsetToValidate );
  }



  public void generateConstraints ( DataDestination  theOutputDestination,
				    int              theObjectSubsetToGenerate)
  {
    boolean  needsTrailingSpace = true;

    	/* Initialize us to generate non-significant tokens... */
    initializeGenerateSubcomponentIndex ( DataWithDoStatement.OPEN_PAREN );

	/* Write constraints */
    for ( int i=0;  i < getConstraints().count();  i++ )
    {
      getConstraint ( i ) . generate ( theOutputDestination,
				       theObjectSubsetToGenerate );

	  /* If there are more constraints */
      if ( (i+1) < getConstraints().count() )
      {
	    /* Write "," */
	theOutputDestination . write ( DataWithDoStatement.COMMA );
      }
      else
      {
	if ( getConstraint ( i ) . hasTrailingWhitespaceSubcomponent() )
	  needsTrailingSpace = false;
	else
	  needsTrailingSpace = true;
      }
    }

	/* Write any pre-")" non-significant tokens */
    generateSubcomponents ( DataWithDoStatement.CLOSE_PAREN,
			    theOutputDestination,
			    theObjectSubsetToGenerate,
			    needsTrailingSpace );
  }


  public void generate ( DataDestination  theOutputDestination,
			 int              theObjectSubsetToGenerate )
  {
    DataTaskDefinition           parentTask;
    DataDestinationStringBuffer  dataDest;
    int                          commentIndent = 0;
    int                          constraintIndent;

       	/* Print a warning if necessary... */
    warnIfInvalidObjectSubset ( theObjectSubsetToGenerate, "generate" );

	/* Handle subset case... */
    if ( theObjectSubsetToGenerate == DataWithDoStatement.CONSTRAINTS_ONLY )
    {
      generateConstraints( theOutputDestination, DataComponent.ENTIRE_OBJECT );
      return;
    }

    	/* Initialize us to generate non-significant tokens... */
    initializeGenerateSubcomponentIndex();

	/* Generate any labels that we have... */
    generateLabels ( theOutputDestination, theObjectSubsetToGenerate,
		     DataLabelStatement.COMMENT_OUT_ID_LABELS,
		     DataWithDoStatement.WITH );

	/* Write any pre-with non-significant tokens */
    generateSubcomponents ( DataWithDoStatement.WITH,
			    theOutputDestination,
			    theObjectSubsetToGenerate, false );

	/* Comment out the with-do bit for CODE generation */
    if ( isCxxSubset ( theObjectSubsetToGenerate ) )
    {
      theOutputDestination . setNewlineText ( "// " );
      theOutputDestination . write          ( "// " );
    }

	/* Write our WITH keyword */
    theOutputDestination . write ( DataWithDoStatement.WITH );

	/* Write any pre-"(" non-significant tokens */
    generateSubcomponents ( DataWithDoStatement.OPEN_PAREN,
			    theOutputDestination,
			    theObjectSubsetToGenerate, false );

	/* Write "(" */
    theOutputDestination . write ( DataWithDoStatement.OPEN_PAREN );


	/* Write constraints & comments before ")" */
    generateConstraints ( theOutputDestination, DataComponent.ENTIRE_OBJECT );


	/* Write ")" */
    theOutputDestination . write ( DataWithDoStatement.CLOSE_PAREN );



	/* Write any pre-STATEMENT non-significant tokens */
    generateSubcomponents ( DataWithDoStatement.STATEMENT_INDEX,
			    theOutputDestination,
			    theObjectSubsetToGenerate, false );


	/* We are done with the with-do bit.  Stop commenting out... */
    if ( isCxxSubset ( theObjectSubsetToGenerate ) )
    {
      theOutputDestination . clearNewlineText();
      theOutputDestination . write ( "\n" );
      theOutputDestination . setStripLeadingWhitespace();
    }


    if ( getStatement() != null )
    {
      parentTask = getParentTaskDefinition();

      if ( isCxxSubset( theObjectSubsetToGenerate )  &&  (parentTask != null) )
      {
	dataDest = new DataDestinationStringBuffer();

	  /** Deal with any switch-idents... **/
	dataDest . addIndent ( theOutputDestination . getSecondaryIndent() );

	if ( getConstraints() . count() > 0 )
	{
	  dataDest . write ( "\nif ( " );
	}
	dataDest . write ( DataComponent.CXX_TDL_TASK_HANDLER_INSTANCE );
	dataDest . write ( " . " );
	dataDest
	  . write ( DataComponent.CXX_TDL_TASK_HANDLER_PUSH_WITH_METHOD );
	dataDest . write ( " ( \"" );
	dataDest . write ( parentTask.getIdentifierForBranch ( this ) );
	dataDest . write ( "\" )" );

	if ( getConstraints() . count() <= 0 )
	  dataDest . write ( ";\n" );

	else /* generate the constraints... */
	{
	  dataDest . write ( " == " );
	  dataDest . write (
		 DataComponent.CXX_TDL_TASK_HANDLER_PUSH_WITH_SUCCESSFULLY );
	  dataDest . write ( " )\n" );
	  dataDest . addIndent ( DataComponent.getIndent() );

	  dataDest . write ( DataComponent.CXX_TDL_TASK_HANDLER_INSTANCE );
	  dataDest . addIndent ( DataComponent.getIndent() );	  

	    /* Write constraints */
	  for ( int i=0;  i < getConstraints().count();  i++ )
	  {
	    if ( DataConstraint.getIsConstraintOverriden( getConstraint( i ),
							  getConstraints()  ) )
	    {
	      continue; /* Skip this constraint.  It's overriden.  */
	    }

	    dataDest . write ( "\n. " );
	    dataDest
	      . write ( DataComponent.CXX_TDL_TASK_HANDLER_DO_ADD_CONSTRAINT );
	    dataDest . write ( " ( new " );
	    getConstraint ( i ) . generateCxxTaskInternal ( dataDest );
	    dataDest . write ( " )" );
	  }

	  dataDest . removeIndent ( DataComponent.getIndent() );
	  dataDest . removeIndent ( DataComponent.getIndent() );
	  dataDest . write ( ";\n" );
	}

	getStatement() . addSubcomponentAfterOpenBrace ( 
		    new DataComponentPlaceholder ( dataDest . getString() ) );

	getStatement() . addSubcomponentBeforeCloseBrace ( 
	  new DataComponentPlaceholder (
		 "\n"
		+ DataComponent.CXX_TDL_TASK_HANDLER_INSTANCE + " . "
		+ DataComponent.CXX_TDL_TASK_HANDLER_POP_WITH_METHOD
		+ " ( \"" + parentTask.getIdentifierForBranch ( this )
		+ "\" );\n" ) );
      } /* IF isCxxSubset (theObjectSubsetToGenerate) && (parentTask != null)*/


      getStatement() . generate ( theOutputDestination,
				  theObjectSubsetToGenerate );


      if ( isCxxSubset( theObjectSubsetToGenerate )  &&  (parentTask != null) )
      {
	getStatement() . removeSubcomponentBeforeCloseBrace();
	getStatement() . removeSubcomponentAfterOpenBrace();
      }
    }
    else  /* Try to recover gracefully if we have a problem... */
    {
	/* Write ";" */
      theOutputDestination . write ( DataWithDoStatement.SEMICOLON );
    }

	/* Write any remaining non-significant tokens */
	/* (There *SHOULD* *NOT* be any...  But just in case...) */
    generateAllRemainingSubcomponents ( theOutputDestination,
					theObjectSubsetToGenerate, false );

  } /* public void generate ( ... ) */




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
