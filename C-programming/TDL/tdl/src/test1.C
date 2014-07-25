/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

#include <tdl.H>


TCM_Task_Tree_Ref
test_allocate ( STRING theString = STRING(NULL) )
{
  if ( theString == STRING(NULL) )
  {
    theString = "";
  }

  TCM_Task_Tree_Ref allocatedTCMTaskTreeRef = TCM_AllocateGoalNode (theString);
  cerr << endl
       << "************************* test_allocate Invoked.  ( "
       << ((void *) (allocatedTCMTaskTreeRef.operator*()) )
       << " ) ****************"
       << endl << endl;
  return allocatedTCMTaskTreeRef;
}


void
test_conversion ( const TCM_Task_Tree_Ref &  theRef )
{
  cerr << endl << "test_conversion:  " << ((void *) (theRef.operator*()))
       << endl << endl;
}

#define perr(X)   cerr << # X " = " << (X) << endl
#define print(X)  cerr << # X << endl; (X)

int
main()
{
	/* If we don't TCM_Initialize(), we get a core dump when we exit. */
  TCM_Initialize();

  _TDL_Dlist  emptyDlist(FALSE);

  _TDL_SpawnStatement  tdlSpawnStatement ( test_allocate, 3 );
  tdlSpawnStatement
    . addName ( "foo-1" ) -> addName ( "foo-2" ) -> addName ( "foo-3" );
  _TDL_SpawnStatementData * tdlSpawnStatementData
    = tdlSpawnStatement . getSpawnStatementData ( _TDL_Dlist(FALSE) );
  _TDL_SpawnStatementTreeNode * tdlSpawnStatementTreeNode
    = tdlSpawnStatementData -> createAndAttachNewSpawnStatementTreeNode();


  cerr << "test_allocate:  (" << ((void *) test_allocate) << ")" << endl
       << endl << tdlSpawnStatement << endl
       << "Testing Data/Tree/Casting: " << endl;
  perr ( tdlSpawnStatementData );
  perr ( tdlSpawnStatementTreeNode );
  perr ( (const _TDL_TreeNode *) tdlSpawnStatementTreeNode );
  cerr << endl
       << "-----------------------------------------" << endl
       << endl;

  _TDL_HandleManager  SpawnManager( "test1", TCM_AllocateGoalNode("self") );
  
  SpawnManager . addSpawnStatement_Local ( test_allocate, 4 )
               -> addName    ( "foo-1a" )
               -> addName    ( "afoo-2a" )
               -> addName    ( "bfoo-3a" )
               -> addName    ( "cfoo-4a" );
  SpawnManager . addSpawnStatement_Local ( test_allocate, 4 )
               -> addName    ( "bar-1b" )
               -> addName    ( "abar-2b" )
               -> addName    ( "bbar-3b" )
               -> addName    ( "cbar-4b" );
  SpawnManager . addSpawnStatement_Local ( test_allocate, 4 )
               -> addName    ( "baz-1c" )
               -> addName    ( "abaz-2c" )
               -> addName    ( "bbaz-3c" )
               -> addName    ( "cbaz-4c" );


  cerr << SpawnManager
       << endl << "=================================================="
       << endl << endl;

  cerr << SpawnManager [ "cbaz-4c" ] << endl
       << endl << "=========================================" << endl;

  cerr << ( (void *)
	    ( ( (const TCM_Task_Tree_Ref &) (SpawnManager [ "cbaz-4c" ]) )
	      . operator*() ) )
       << endl;

  test_conversion ( SpawnManager [ "baz-1c" ] );

  cerr << SpawnManager [ "cbaz-4c" ] << endl
       << endl << "=========================================" << endl << endl;


  cerr << "Test _TDL_InsertNode:  " << endl;
  perr ( SpawnManager . startInvokingSpawn ( "baz-1c" ) );
  perr ( SpawnManager . insertSpawn() );
  SpawnManager . finishInvokingSpawn();
  cerr << SpawnManager [ "baz-1c" ] << endl
       << endl << "=========================================" << endl << endl;


  cerr << "Test destroy:  " << SpawnManager [ "bar-1b" ] . destroy(emptyDlist) << endl;
  cerr << SpawnManager [ "bar-1b" ] << endl
       << endl << "=========================================" << endl << endl;

  cerr << "Test _TDL_InsertNode(destroyed-node):  " << endl;
  perr ( SpawnManager . startInvokingSpawn ( "bar-1b" ) );
  perr ( SpawnManager . insertSpawn() );
  SpawnManager . finishInvokingSpawn();
  cerr << SpawnManager [ "bar-1b" ] << endl
       << endl << "=========================================" << endl;


  cerr << SpawnManager << endl;


  cerr << endl
       << "----------------------------------------------------------------"
       << endl
       << "================================================================"
       << endl
       << "----------------------------------------------------------------"
       << endl << endl;

  print ( TDL::setReportingLevel (   TDL::EVERYTHING_BASIC
				   | TDL::VERBOSE_BRIEF    ) );

  _TDL_EXPAND_FIRST    ( * ( SpawnManager . findSpawnStatement ( "foo-1a" ) -> getSpawnStatementTreeNode(emptyDlist) ) );

  _TDL_DELAY_EXPANSION ( * ( SpawnManager . findSpawnStatement ( "foo-1a" ) -> getSpawnStatementTreeNode(emptyDlist) ) );

  _TDL_SEQUENTIAL_HANDLING  ( * ( SpawnManager [ "baz-1c" ] . getTreeNode(emptyDlist) ),
			      * ( SpawnManager . findSpawnStatement ( "foo-1a" ) -> getSpawnStatementTreeNode(emptyDlist) ) );

  _TDL_SEQUENTIAL_EXPANSION ( * ( SpawnManager [ "baz-1c" ] . getTreeNode(emptyDlist) ),
			      * ( SpawnManager . findSpawnStatement ( "foo-1a" ) -> getSpawnStatementTreeNode(emptyDlist) ) );

  _TDL_SEQUENTIAL_EXECUTION ( * ( SpawnManager [ "baz-1c" ] . getTreeNode(emptyDlist) ),
			      * ( SpawnManager . findSpawnStatement ( "foo-1a" ) -> getSpawnStatementTreeNode(emptyDlist) ) );

  _TDL_SERIAL               ( * ( SpawnManager [ "baz-1c" ] . getTreeNode(emptyDlist) ),
			      * ( SpawnManager . findSpawnStatement ( "foo-1a" ) -> getSpawnStatementTreeNode(emptyDlist) ) );

  _TDL_PARALLEL ( * ( SpawnManager . findSpawnStatement ( "foo-1a" ) -> getSpawnStatementTreeNode(emptyDlist) ) );

  _TDL_WAIT     ( * ( SpawnManager . findSpawnStatement ( "foo-1a" ) -> getSpawnStatementTreeNode(emptyDlist) ) );

  _TDL_DISABLE_UNTIL_EVENT ( * ( SpawnManager [ "baz-1c" ] . getTreeNode(emptyDlist) ),
			     Handling_Interval,
			     Start_Point,
			     * ( SpawnManager . findSpawnStatement ( "foo-1a" ) -> getSpawnStatementTreeNode(emptyDlist) ),
			     Planning_Interval );

  _TDL_DISABLE_UNTIL       ( * ( SpawnManager . findSpawnStatement ( "foo-1a" ) -> getSpawnStatementTreeNode(emptyDlist) ),
			     Achieving_Interval,
			     1, 2, 3, 0.456 );

  _TDL_DISABLE_FOR         ( * ( SpawnManager . findSpawnStatement ( "foo-1a" ) -> getSpawnStatementTreeNode(emptyDlist) ),
			     Handling_Interval,
			     9, 8, 7, 0.654 );

  _TDL_DISABLE_FOR         ( * ( SpawnManager [ "baz-1c" ] . getTreeNode(emptyDlist) ),
			     Handling_Interval,
			     Start_Point,
			     * ( SpawnManager . findSpawnStatement ( "foo-1a" ) -> getSpawnStatementTreeNode(emptyDlist) ),
			     Handling_Interval,
			     9, 8, 7, 0.654 );

  _TDL_TERMINATE_AT_EVENT  ( * ( SpawnManager [ "baz-1c" ] . getTreeNode(emptyDlist) ),
			     Handling_Interval,
			     Start_Point,
			     * ( SpawnManager . findSpawnStatement ( "foo-1a" ) -> getSpawnStatementTreeNode(emptyDlist) ) );


  _TDL_TERMINATE_AT        ( * ( SpawnManager . findSpawnStatement ( "foo-1a" ) -> getSpawnStatementTreeNode(emptyDlist) ),
			     1, 2, 3, 0.456 );

  _TDL_TERMINATE_IN        ( * ( SpawnManager . findSpawnStatement ( "foo-1a" ) -> getSpawnStatementTreeNode(emptyDlist) ),
			     9, 8, 7, 0.654 );

  _TDL_TERMINATE_IN        ( * ( SpawnManager [ "baz-1c" ] . getTreeNode(emptyDlist) ),
			     Handling_Interval,
			     Start_Point,
			     * ( SpawnManager . findSpawnStatement ( "foo-1a" ) -> getSpawnStatementTreeNode(emptyDlist) ),
			     9, 8, 7, 0.654 );
  

  return 0;
}
