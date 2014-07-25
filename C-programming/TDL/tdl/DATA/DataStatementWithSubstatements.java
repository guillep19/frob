
/*
 * A abstract base class for dealing with the details of maintaining
 * a (contained) subset of other data statements...
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 *
 */

public abstract class DataStatementWithSubstatements extends DataStatement
{
	/* Instance Variables */
  private DataCompoundStatement    statement;


	/* Instance Methods */
  public DataStatementWithSubstatements ()
  {
    statement = null;
  }

  public DataCompoundStatement getStatement ()    { return statement; }
  public void                  setStatement ( DataComponent theStatement )
  {
    if ( statement != null )
      statement . setParent ( null );

    if ( theStatement instanceof DataCompoundStatement )
      statement = (DataCompoundStatement) theStatement;
    else
      statement = new DataCompoundStatement ( theStatement );

    if ( statement != null )
      statement . setParent ( this );
  }

  public int getChildStatementCount()
  {
    return super . getChildStatementCount()
      +  ( getStatement() != null ? 1 : 0 );
  }

  public DataStatement  getChildStatement ( int theIndex )
  {
    if ( ( theIndex == 0 )  &&  ( getStatement() != null ) )
      return getStatement();
    else
      return super . getChildStatement ( theIndex
					 - (getStatement() != null ? 1 : 0) );
  }


  public boolean addPrimaryChild ( DataComponent theChildToAdd,
				   DataComponent theAddChildAfterThisComponent)
  {
    if ( getStatement() == null )
      setStatement ( new DataCompoundStatement ( null ) );

    return getStatement() . addChild ( theChildToAdd,
				       theAddChildAfterThisComponent );
  }

  public boolean removeChild ( DataComponent theChildToRemove )
  {
    if ( getStatement() == theChildToRemove )
    {
      setStatement ( null );
      return true;
    }
    else
    {
      return getStatement() . removeChild ( theChildToRemove );
    }
  }


  public boolean isPartOfThisStatement ( DataComponent  theDataComponent )
  {
    return (   (     theDataComponent == this             )
	    || (   ( theDataComponent == getStatement() )
		&& ( getStatement()   != null           ) ) );
  }

}
