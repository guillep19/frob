/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

public class DataDestinationNull extends DataDestinationAbstractBaseClass
{
  public void reset ( )
  {
    hasClosed = false;
    indent    = 0;
    type      = DataDestination.NORMAL_TYPE;
    length    = 0;
    row       = 0;
    column    = 0;
  }
}

