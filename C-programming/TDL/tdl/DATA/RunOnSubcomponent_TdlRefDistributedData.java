
    /*
     * Container class for distributed TDL_REF testing
     * in DataTaskDefinition.validateTaskForCxxGeneration().
     * with the RunOnSubcomponentObject interface.
     *
     * Passed through: DataComponent.runOnSubcomponentFraction().
     *
     * Copyright (c) 2008, Carnegie Mellon University
     *     This software is distributed under the terms of the 
     *     Simplified BSD License (see tdl/LICENSE.TXT)
     *
     */

public class RunOnSubcomponent_TdlRefDistributedData
{
  protected int                         statementsIndex;
  protected DataVector                  statements;
  protected DataHashtable               onAgentHashtable;
  protected DataValidateCodeReturnValue returnValue;

  public RunOnSubcomponent_TdlRefDistributedData (
			int                         theStatementsIndex,
			DataVector                  theStatements,
			DataHashtable               theOnAgentHashtable,
			DataValidateCodeReturnValue theReturnValue      )
  {
    statementsIndex  = theStatementsIndex;
    statements       = theStatements;
    onAgentHashtable = theOnAgentHashtable;
    returnValue      = theReturnValue;
  }

  public int                   getStatementsIndex() { return statementsIndex; }
  public DataVector            getStatements()      { return statements;      }
  public DataHashtable         getOnAgentHashtable(){ return onAgentHashtable;}
  public DataValidateCodeReturnValue getReturnValue(){return returnValue;     }
}
