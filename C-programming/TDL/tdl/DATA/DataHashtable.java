
/*
 * Originally, TDL started life as a Visual-Design-Tool for visual-programming.
 * As a consequence, we used netscape's IFC library, and netscape's Hashtable
 * utility class.  As we phase out IFC support, it is useful to have our
 * own Hashtable interface class, where we can add additional methods as
 * necessary.
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 *
 */

import java.util.Hashtable;
import java.util.Enumeration;

public class DataHashtable extends java.util.Hashtable
{
  public DataHashtable ( ) { super(); }

  /* Known methods being used from our inherited superclass.
   * (This list is complete at the time of writing, but may go out of date
   *  in the future.)
   *    public Object      clone()
   *    public Object      get(Object)
   *    public Object      put(Object,Object)
   *    public Object      remove(Object)
   *    public Enumeration keys()
   *    public boolean     containsKey(Object)
   *    public void        clear()
   */
}
