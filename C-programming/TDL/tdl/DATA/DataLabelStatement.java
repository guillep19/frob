
/*
 * There are three kinds of labels:
 *   case <tokens> :
 *   default :
 *   <ID> :
 *
 * It's the last case that will matter the most to us....
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 *
 */

public class DataLabelStatement extends DataStatement
{
	/* Class variables */
  public final static String FIRST_TOKEN_INDEX = "First"; /*case,default,<ID>*/
  public final static String COLON             = ":";
  public final static String DEFAULT           = "default";

	/* Sometimes we want id-labels to be commented out... */
  public final static int    COMMENT_OUT_ID_LABELS = 100;


	/* Instance Variables */
  protected String  id;
  protected boolean isCase;
  protected boolean isDefault;

	/* Instance Methods */
  public DataLabelStatement ( )
  {
    id        = null;
    isCase    = false;
    isDefault = false;
  }

	/* Used to test whether this is a case/default or <ID> based label */
  public boolean hasId (              ) { return id != null; }
  public String  getId (              ) { return id;         }
  public void    setId ( String theId ) { id = theId;        }


	/* These are just flags -- the actual generation occurs by means *
	 * of 'case' and 'default' being stored as subcomponent strings. */
  public boolean getIsCase()    { return isCase;    }
  public boolean getIsDefault() { return isDefault; }

  public void setIsCase    (boolean theIsCase   ) { isCase    = theIsCase;    }
  public void setIsDefault (boolean theIsDefault) { isDefault = theIsDefault; }


  public String getWarnString ( int theObjectSubset )
  {
    return super . getWarnString ( theObjectSubset )
      + " or DataLabelStatement.COMMENT_OUT_ID_LABELS ("
      + DataLabelStatement.COMMENT_OUT_ID_LABELS + ")";
  }

  public boolean isValidObjectSubset ( int theObjectSubset )
  {
    if ( theObjectSubset == DataLabelStatement.COMMENT_OUT_ID_LABELS )
      return true;
    else
      return super . isValidObjectSubset ( theObjectSubset );
  }



  public void generate ( DataDestination  theOutputDestination,
			 int              theObjectSubsetToGenerate )
  {
    int   objectSubsetToGenerate = theObjectSubsetToGenerate;
    int   currentLine            = 0;

	/* Do ENTIRE_OBJECT when commenting out id labels... */
    if ( objectSubsetToGenerate == DataLabelStatement.COMMENT_OUT_ID_LABELS )
      objectSubsetToGenerate = DataComponent.ENTIRE_OBJECT;

       	/* Print a warning if necessary... */
    warnIfInvalidObjectSubset ( theObjectSubsetToGenerate, "generate" );

    	/* Initialize us to generate non-significant tokens... */
    initializeGenerateSubcomponentIndex();

	/* Write any pre-first-token non-significant tokens */
    generateSubcomponents ( DataLabelStatement.FIRST_TOKEN_INDEX,
			    theOutputDestination,
			    objectSubsetToGenerate, false );

	/* Write our ID if we have one... */
    if ( hasId() )
    {
	/* COMMENT_OUT_ID_LABELS just comments out the ID and the COLON */
      if (    theObjectSubsetToGenerate
	   == DataLabelStatement.COMMENT_OUT_ID_LABELS )
      {
	theOutputDestination . write ( "// " );
	currentLine = theOutputDestination . getRow();
      }

      theOutputDestination . write ( getId() );
    }

	/* Write all the tokens up to the colon... */
    generateSubcomponents ( DataLabelStatement.COLON,
			    theOutputDestination,
			    objectSubsetToGenerate, false );

	/* On the off chance that the ID and the COLON are on different lines*/
    if (    hasId()
	 && (    theObjectSubsetToGenerate
	      == DataLabelStatement.COMMENT_OUT_ID_LABELS )
	 && ( currentLine != theOutputDestination . getRow() ) )
    {
      theOutputDestination . write ( "// " );
    }

	/* Write the colon. */
    theOutputDestination . write ( DataLabelStatement.COLON );

	/* Write any remaining non-significant tokens */
	/* (There *SHOULD* *NOT* be any...  But just in case...) */
    generateAllRemainingSubcomponents ( theOutputDestination,
					objectSubsetToGenerate, false );

	/* Note:  We generate any labels that this label might have
	 * iteratively in DataStatement rather than recursively here...
	 */
  }
}
