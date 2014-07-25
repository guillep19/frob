

    /* This is a callback interface used by Data* classes while searching
     * on a contained string in subcomponents and contained expressions.
     *
     * See DataComponent.runOnSubcomponentFraction().
     *
     * Copyright (c) 2008, Carnegie Mellon University
     *     This software is distributed under the terms of the 
     *     Simplified BSD License (see tdl/LICENSE.TXT)
     *
     */

public interface RunOnSubcomponentInterface
{
  public void foundSubcomponentMatch ( String         theString,
				       String         theStringToSearch,
				       int            theStringIndexOfMatch,
				       DataComponent  theDataComponent,
				       int            theSubcomponentIndex,
				       Object         theArgumentObject );
}

