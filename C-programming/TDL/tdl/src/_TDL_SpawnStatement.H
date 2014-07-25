/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

#ifndef _TDL_SpawnStatement_H
#define _TDL_SpawnStatement_H

#define _TDL_INTERNAL_
#include "tdl.H"
#include "_TDL_TDLStatement.H"

/*
 * _TDL_SpawnStatement represents a single TDL "Spawn" operation.
 *
 * Association Relationship:
 *                                1
 *           #####################-------+
 *           # _TDL_TDLStatement #       |
 *           #####################----+  |
 *                     |          1   |  |
 *                    / \             |  |
 *                   .___.            |  |
 *                     |              |  |
 *          #######################   |  |
 *          # _TDL_SpawnStatement #   |  |
 *          #######################   |  |
 *                                    |  |
 *                                    |  |
 *                                    |  |
 *        ########################### |  |
 *        # _TDL_SpawnStatementData #-+  |
 *        ########################### N  |
 *                     | 1               |
 *                     |                 |
 *                     | 2+              |
 *      ###############################  |
 *      # _TDL_SpawnStatementTreeNode #--+
 *      ############################### N/2
 * 
 * Each _TDL_SpawnStatement instance is associated, through its base
 * _TDL_TDLStatement class, with one or more _TDL_SpawnStatementData
 * objects.  With the _TDL_SpawnStatementData object representing the
 * actual data associated with a specific TDL SPAWN invocation.  And
 * multiple _TDL_SpawnStatementData objects being created to implement
 * iteration. 
 * 
 * Each _TDL_SpawnStatementData object is associated with two or more
 * _TDL_SpawnStatementTreeNode objects, which locate that
 * _TDL_SpawnStatementData object inside the _TDL_HandleManager's TDL CODE
 * Tree (*NOT* to be confused with the TCM Task Tree) and the iteration
 * tree maintained by the base _TDL_TDLStatement class of the
 * corresponding _TDL_SpawnStatement.
 */

class _TDL_SpawnStatementData;

class _TDL_SpawnStatement : public _TDL_TDLStatement
{
public:
  static _TDL_SpawnStatement & generateTrivialSpawnStatement (
				     const char              * theName,
				     const TCM_Task_Tree_Ref * theTaskTreeRef
					  = (const TCM_Task_Tree_Ref *) NULL );


protected:
  _TDL_AllocationFunction  allocationFunction;
  STRING                   tcmTaskTreeNodeName;


public:
  _TDL_SpawnStatement ( const _TDL_AllocationFunction &  theAllocationFunction,
			u_int4                           theInitialCapacity
				    = _TDL_ArrayList::DEFAULT_INITIAL_CAPACITY,
			const char *                     theFirstName
				    = (const char *)NULL,
			_TDL_HandleManager *             theHandleManger
				    = (_TDL_HandleManager *) NULL )
    :  _TDL_TDLStatement  ( theInitialCapacity, theFirstName, theHandleManger),
       allocationFunction ( theAllocationFunction                            ),
       tcmTaskTreeNodeName( STRING(NULL)                                     )
  { }

	/* Convenience methods */
  _TDL_SpawnStatement ( u_int4                           theInitialCapacity
				    = _TDL_ArrayList::DEFAULT_INITIAL_CAPACITY,
			const char *                     theFirstName
				    = (const char *)NULL,
			_TDL_HandleManager *             theHandleManger
				    = (_TDL_HandleManager *) NULL )
    :  _TDL_TDLStatement  ( theInitialCapacity, theFirstName, theHandleManger),
       allocationFunction (                                                  ),
       tcmTaskTreeNodeName( STRING(NULL)                                     )
  { }

  _TDL_SpawnStatement (
	_TDL_AllocationFunctionType_Local         theAllocationFunction,
	u_int4                                    theInitialCapacity
				    = _TDL_ArrayList::DEFAULT_INITIAL_CAPACITY,
	const char *                              theFirstName
				    = (const char *)NULL,
	_TDL_HandleManager *                      theHandleManger
				    = (_TDL_HandleManager *) NULL )
    :  _TDL_TDLStatement  ( theInitialCapacity, theFirstName, theHandleManger),
       allocationFunction ( theAllocationFunction                            ),
       tcmTaskTreeNodeName( STRING(NULL)                                     )
  { }

  _TDL_SpawnStatement (
	_TDL_AllocationFunctionType_Either        theAllocationFunction,
	u_int4                                    theInitialCapacity
				    = _TDL_ArrayList::DEFAULT_INITIAL_CAPACITY,
	const char *                              theFirstName
				    = (const char *)NULL,
	_TDL_HandleManager *                      theHandleManger
				    = (_TDL_HandleManager *) NULL )
    :  _TDL_TDLStatement  ( theInitialCapacity, theFirstName, theHandleManger),
       allocationFunction ( theAllocationFunction                            ),
       tcmTaskTreeNodeName( STRING(NULL)                                     )
  { }

  _TDL_SpawnStatement (
	_TDL_AllocationFunctionType_Distributed   theAllocationFunction,
	u_int4                                    theInitialCapacity
				    = _TDL_ArrayList::DEFAULT_INITIAL_CAPACITY,
        const char *                              theFirstName
				    = (const char *)NULL,
	_TDL_HandleManager *                      theHandleManger
				    = (_TDL_HandleManager *) NULL )
    :  _TDL_TDLStatement  ( theInitialCapacity, theFirstName, theHandleManger),
       allocationFunction ( theAllocationFunction                            ),
       tcmTaskTreeNodeName( STRING(NULL)                                     )
  { }

  _TDL_SpawnStatement (
	_TDL_AllocationFunction::TYPES            theType,
	u_int4                                    theInitialCapacity
				    = _TDL_ArrayList::DEFAULT_INITIAL_CAPACITY,
	const char *                              theFirstName
				    = (const char *)NULL,
	_TDL_HandleManager *                      theHandleManger
				    = (_TDL_HandleManager *) NULL )
    :  _TDL_TDLStatement  ( theInitialCapacity, theFirstName, theHandleManger),
       allocationFunction ( theType                                          ),
       tcmTaskTreeNodeName( STRING(NULL)                                     )
  { }


  virtual ~_TDL_SpawnStatement();

  const _TDL_AllocationFunction & getAllocationFunction() const
						 { return allocationFunction; }

	/* _TDL_TDLStatement Interface */
  virtual status_t createObjects ( _TDL_Snode    * & theStatementData,
				   _TDL_TreeNode * & theTreeNode       );
  virtual status_t deleteObjects ( _TDL_Snode    *   theStatementData,
				   _TDL_TreeNode *   theTreeNode       );

	/* Overriden from _TDL_TDLStatement */
  virtual _TDL_TDLStatement * setTcmTaskTreeNodeName ( STRING theNodeName );

  STRING getTcmTaskTreeNodeName() const { return tcmTaskTreeNodeName; }


	/* Creates the SpawnStatementData & TreeNode if it does not exist. */
  _TDL_SpawnStatementTreeNode * getSpawnStatementTreeNode (
				const _TDL_Dlist & theIterationIndexes );

	/* Returns NULL if the specified SpawnStatementData does not exist. */
  _TDL_SpawnStatementTreeNode * getSpawnStatementTreeNodeConst (
				const _TDL_Dlist & theIterationIndexes );


	/* Creates the SpawnStatementData & TreeNode if it does not exist. */
  _TDL_SpawnStatementData * getSpawnStatementData (
				const _TDL_Dlist & theIterationIndexes );

	/* Returns NULL if the specified SpawnStatementData does not exist. */
  _TDL_SpawnStatementData * getSpawnStatementDataConst (
				const _TDL_Dlist & theIterationIndexes );


	/* Lets allow safe-downcasts... (Basic RTTI) */
  virtual operator const _TDL_SpawnStatement * () const;


	/* iostreamBase interface.  Inherited from _TDL_TDLStatement. */
  virtual ostream & printObject ( ostream    & theOstream,
				  const char * theIndentString = "") const;


	/* These operations are disabled (prohibited) & unimplemented */
private:
  _TDL_SpawnStatement (           const _TDL_SpawnStatement & theObjectToCopy);
  _TDL_SpawnStatement & operator=(const _TDL_SpawnStatement & theObjectToCopy);
};

#endif /* _TDL_SpawnStatement_H */

