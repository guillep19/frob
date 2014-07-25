
/*
 * This is an abstract class.
 *
 * It stores things that are common to all the iteration-statement subclasses.
 * Specifically, the code relating to producing C/C++ code from TDL code.
 *
 * It's also used to refer to iteration-statements in a generic way that
 * excludes non-iteration statements.
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 *
 */

public abstract class DataIterationStatement
					 extends DataStatementWithSubstatements
{
  public void generatePreIterationCode (
			      DataDestination       theOutputDestination,
			      int                   theObjectSubsetToGenerate )
  {
    if (    isCxxSubset                   ( theObjectSubsetToGenerate )
	 && containsSpawnRelatedStatement (                           ) )
    {
	/* Deal with #line macro. */
      theOutputDestination . setUsingTdlFileName ( false );
      theOutputDestination . write ( "\n" ); /* Flush #line macro */

	/* Write "_TDL_SpawnedTasks . pushIteration();" */
      theOutputDestination
	. write ( DataComponent.CXX_TDL_TASK_HANDLER_INSTANCE );
      theOutputDestination . write ( " . " );
      theOutputDestination
	. write ( DataComponent.CXX_TDL_TASK_HANDLER_PUSH_ITERATION_METHOD );
      theOutputDestination . write ( "();\n" );
    }
  }


  public void addInsideIterationCode (
			      int                   theObjectSubsetToGenerate,
			      DataCompoundStatement theBodyStatement          )
  {
    if (    isCxxSubset                   ( theObjectSubsetToGenerate )
	 && containsSpawnRelatedStatement (                           ) )
    {
	/* Add "_TDL_SpawnedTasks . incrementIteration();" */

      theBodyStatement . addSubcomponentAfterOpenBrace (
	  new DataComponentPlaceholder (
		  "\n"
		+ DataComponent.CXX_TDL_TASK_HANDLER_INSTANCE
		+ " . "
		+ DataComponent.CXX_TDL_TASK_HANDLER_INCREMENT_ITERATION_METHOD
		+ "();\n" ) );
    }
  }


  public void removeInsideIterationCode (
			      int                   theObjectSubsetToGenerate,
			      DataCompoundStatement theBodyStatement          )
  {
    if (    isCxxSubset                   ( theObjectSubsetToGenerate )
	 && containsSpawnRelatedStatement (                           ) )
    {
      theBodyStatement . removeSubcomponentAfterOpenBrace();
    }
  }


  public void generateEmptyIterationBodyStatement (
			      DataDestination       theOutputDestination,
			      int                   theObjectSubsetToGenerate )
  {
    if (    isCxxSubset                   ( theObjectSubsetToGenerate )
	 && containsSpawnRelatedStatement (                           ) )
    {
	/* Deal with #line macros */
      theOutputDestination . setUsingTdlFileName ( false );

	/* Write "_TDL_SpawnedTasks . incrementIteration();" */
      theOutputDestination . write ( "\n" );
      theOutputDestination
	. write ( DataComponent.CXX_TDL_TASK_HANDLER_INSTANCE );
      theOutputDestination . write ( " . " );
      theOutputDestination
	. write(DataComponent.CXX_TDL_TASK_HANDLER_INCREMENT_ITERATION_METHOD);
      theOutputDestination . write ( "();\n" );
    }
    else
    {
	/* Write ";" */
      theOutputDestination . write ( DataForStatement.SEMICOLON );
    }
  }


  public void generatePostIterationCode (
			      DataDestination       theOutputDestination,
			      int                   theObjectSubsetToGenerate )
  {
    if (    isCxxSubset                   ( theObjectSubsetToGenerate )
	 && containsSpawnRelatedStatement (                           ) )
    {
	/* Deal with #line macros */
      theOutputDestination . setUsingTdlFileName ( false );

	/* Write "\n_TDL_SpawnedTasks . popIteration();" */
      theOutputDestination . write ( "\n" );
      theOutputDestination
	. write ( DataComponent.CXX_TDL_TASK_HANDLER_INSTANCE );
      theOutputDestination . write ( " . " );
      theOutputDestination
	. write ( DataComponent.CXX_TDL_TASK_HANDLER_POP_ITERATION_METHOD );
      theOutputDestination . write ( "();\n" );

	/* Write "_TDL_SpawnedTasks . destroyIfUnsed(...);" Statements */
      theOutputDestination
	. write ( getStatement() . getSpawnDestroyClause ( true ) );
    }
  }
}

