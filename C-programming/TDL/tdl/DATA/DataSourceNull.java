/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

public class DataSourceNull extends DataSource
{
	/* Instance Methods */
  public DataSourceNull ( String theFilename )
  {
    super ( theFilename );
  }

  public DataSourceNull ( )
  {
  }

	/* This *MUST* be implemented in this derived class */
  public char read ( int thePosition ) throws java.io.IOException
  {
    return '\0';
  }

	/* This *MUST* be implemented in this derived class */
  public String readSubString ( int theStartPosition,
					 int theEndPosition   )
  {
    return "";
  }
}
