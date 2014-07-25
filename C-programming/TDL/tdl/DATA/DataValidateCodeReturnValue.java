
/*
 * This class is a hack to let DataValidateCode return two integers...
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 *
 */

public class DataValidateCodeReturnValue
{
	/* Class Methods */
  public static DataDestination writeWarningBeginning (
				      DataDestination  theDestination,
				      String           theMessageFilenameLead,
				      String           theLineNumberString    )
  {
    theDestination . write ( theMessageFilenameLead );
    theDestination . write ( theLineNumberString );
    theDestination . write ( ":  Warning:  "     );
    return theDestination;
  }

  public static DataDestination writeErrorBeginning (
				      DataDestination  theDestination,
				      String           theMessageFilenameLead,
				      String           theLineNumberString    )
  {
    theDestination . write ( theMessageFilenameLead );
    theDestination . write ( theLineNumberString );
    theDestination . write ( ":  Error:  "       );
    return theDestination;
  }



	/* Instance Variables */
  protected int              warningCount;
  protected int              errorCount;
  protected DataDestination  dataDestination;

	/* Instance Methods */
  public DataValidateCodeReturnValue ( DataDestination  theDataDestination )
  {
    warningCount    = 0;
    errorCount      = 0;
    dataDestination = theDataDestination;
  }


  public DataDestination addWarning ( DataComponent theDataComponent )
  {
    return addWarning ( theDataComponent.getMessageFilenameLead(),
			theDataComponent.getLineNumberString()     );
  }

  public DataDestination addWarning ( String theMessageFilenameLead,
				      String theLineNumberString    )
  {
    warningCount++;
    return DataValidateCodeReturnValue.writeWarningBeginning (
						      getDataDestination(),
						      theMessageFilenameLead,
						      theLineNumberString    );
  }


  public DataDestination addError ( DataComponent theDataComponent )
  {
    return addError ( theDataComponent.getMessageFilenameLead(),
		      theDataComponent.getLineNumberString()    );
  }

  public DataDestination addError ( String theMessageFilenameLead,
				    String theLineNumberString    )
  {
    errorCount++;
    return DataValidateCodeReturnValue.writeErrorBeginning (
						      getDataDestination(),
						      theMessageFilenameLead,
						      theLineNumberString    );
  }


  public void addWarnings ( int theWarnings )  { warningCount += theWarnings; }
  public void addErrors   ( int theErrors   )  { errorCount   += theErrors;   }

  public boolean hasWarnings() { return warningCount > 0; }
  public boolean hasErrors()   { return errorCount   > 0; }

  public boolean hasWarningsOrErrors() { return hasWarnings() || hasErrors(); }

  public int     getWarnings() { return warningCount; }
  public int     getErrors()   { return errorCount;   }

  public DataDestination getDataDestination()  { return dataDestination; }

  public String  toString()
  {
    return "" + getErrors()   + " Error"   + (getErrors()   == 1 ? " " : "s ")
     + "and " + getWarnings() + " Warning" + (getWarnings() == 1 ? ""  : "s" );
  }

}
