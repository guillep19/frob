
/*
 * Note:  For doing "new DataDestinationPrintStream ( System.out )"
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 *
 */

import java.io.PrintStream;

public class DataDestinationPrintStream
			       extends DataDestinationAbstractBaseClass
{
	/* Instance Variables */
  protected PrintStream   printStream;


	/* Instance Methods */
  public DataDestinationPrintStream ( PrintStream  thePrintStream )
  {
    setPrintStream ( thePrintStream );
  }
  

  public PrintStream getPrintStream ( )       { return printStream; }
  public void        setPrintStream ( PrintStream thePrintStream )
					      { printStream = thePrintStream; }

  public void writeWithoutIndenting ( String theString )
  {
    	/* Trivial case */
    if (   ( theString == null )
	|| ( theString . equals ( "" ) ) )
    {
      return;
    }

	/* Error case */
    if ( getHasClosed() )
    {
      System.err.println (
             "[DataDestinationPrintStream:writeWithoutIndenting]  Error:  "
	   + "Attempted to write to a close'd DataDestinationPrintStream "
	   + "detected and aborted" );
      return;
    }

	/* General case */
    getPrintStream() . print ( theString );
    super . writeWithoutIndenting ( theString );
  }
}
