
/*
 * Trivial interface for simplifying Spawn & With-Do accessing code...
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 *
 */
public interface DataConstrainedObject
{
  public int            getConstraintCount();
  public DataConstraint getConstraint ( int theIndex );
  public void           addConstraint ( DataConstraint theConstraint );
}

