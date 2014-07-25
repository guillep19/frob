
    /* This interface is used by DataConstraint and DataConstraintStatement
     * to provide a common interface to dealing with Tag Task Indexes.
     *
     * Copyright (c) 2008, Carnegie Mellon University
     *     This software is distributed under the terms of the 
     *     Simplified BSD License (see tdl/LICENSE.TXT)
     *
     */

public interface DataConstraintTagTaskIndexes
{
	/* Constants */
  public final static String  BEGIN_TAG_TASK_INDEX       = "[";
  public final static String  DEFAULT_TAG_TASK_INDEX     = ".";
  public final static String  END_TAG_TASK_INDEX         = "]";
  public final static String  TAG_TASK                   = "TagTask";


  public boolean  getHasTagTask ( );
  public String   getTagTask ( );
  public void     setTagTask ( String theTagTask );
  public boolean  getHasTagTaskIndexes ( );
  public Object[] getTagTaskIndexes ( );
  public void     setTagTaskIndexesWithoutParsing (Object[] theTagTaskIndexes);
}
