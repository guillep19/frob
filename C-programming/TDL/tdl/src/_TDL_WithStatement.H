/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

#ifndef _TDL_WithStatement_H
#define _TDL_WithStatement_H

#define _TDL_INTERNAL_
#include "tdl.H"
#include "_TDL_TDLStatement.H"

/*
 * _TDL_WithStatement represents a single TDL "With" set of operations.
 *
 * Association Relationship:
 *                                1
 *           #####################------+
 *           # _TDL_TDLStatement #      |
 *           #####################---+  |
 *                    |           1  |  |
 *                   / \             |  |
 *                  .___.            |  |
 *                    |              |  |
 *          ######################   |  |
 *          # _TDL_WithStatement #   |  |
 *          ######################   |  |
 *                                   |  |
 *                                   |  |
 *                                   |  |
 *            #################      |  |
 *            # _TDL_TreeNode #      |  |
 *            #################      |  |
 *                    |              |  |
 *                   / \             |  |
 *                  .___.            |  |
 *                    |              |  |
 *         #######################   |  |
 *         # _TDL_TreeNodeBranch #   |  |
 *         #######################   |  |
 *                    |              |  |
 *                   / \             |  |
 *                  .___.            |  |
 *                    |              |  |
 *        ########################## |  |
 *        # _TDL_WithStatementData #-+  |
 *        ########################## N  |
 *                    | 1               |
 *                    |                 |
 *                    | 1               |
 *      ##############################  |
 *      # _TDL_WithStatementTreeNode #--+
 *      ############################## N
 * 
 * Each _TDL_WithStatement instance is associated, through its base
 * _TDL_TDLStatement class, with one or more _TDL_WithStatementData
 * objects.  With the _TDL_WithStatementData object representing the
 * actual data associated with a specific TDL WITH invocation.  And
 * multiple _TDL_WithStatementData objects being created to implement
 * iteration. 
 * 
 * Each _TDL_WithStatementData object is associated with a
 * _TDL_WithStatementTreeNode object, which locates that
 * _TDL_WithStatementData object inside the iteration tree
 * maintained by the base _TDL_TDLStatement class of the
 * corresponding _TDL_WithStatement.
 *
 * _TDL_WithStatementData inherits from _TDL_TreeNodeBranch, which
 * inherits from _TDL_TreeNode, thus permitting _TDL_WithStatmentData
 * objects to be directly located inside the _TDL_HandleManager's
 * TDL CODE Tree (*NOT* to be confused with the TCM Task Tree).
 *
 * Constraints are attached at the _TDL_WithStatementData level, as
 * specific array instances can have additional constraints
 * added to just them.
 */

class _TDL_WithStatementData;

class _TDL_WithStatement : public _TDL_TDLStatement
{
public:
  _TDL_WithStatement ( u_int4                       theInitialCapacity
				    = _TDL_ArrayList::DEFAULT_INITIAL_CAPACITY,
		       const char *                 theFirstName
				    = (const char *)NULL,
		       _TDL_HandleManager *         theHandleManger
				    = (_TDL_HandleManager *) NULL )
    :  _TDL_TDLStatement ( theInitialCapacity, theFirstName, theHandleManger )
  { }

  virtual ~_TDL_WithStatement();


	/* _TDL_TDLStatement Interface */
  virtual status_t createObjects ( _TDL_Snode    * & theStatementData,
				   _TDL_TreeNode * & theTreeNode       );
  virtual status_t deleteObjects ( _TDL_Snode    *   theStatementData,
				   _TDL_TreeNode *   theTreeNode       );


	/* Creates the WithStatementData & TreeNode if it does not exist. */
  _TDL_WithStatementTreeNode * getWithStatementTreeNode (
				const _TDL_Dlist & theIterationIndexes );

	/* Returns NULL if the specified WithStatementData does not exist. */
  _TDL_WithStatementTreeNode * getWithStatementTreeNodeConst (
				const _TDL_Dlist & theIterationIndexes );


	/* Creates the WithStatementData & TreeNode if it does not exist. */
  _TDL_WithStatementData * getWithStatementData (
				const _TDL_Dlist & theIterationIndexes );

	/* Returns NULL if the specified WithStatementData does not exist. */
  _TDL_WithStatementData * getWithStatementDataConst (
				const _TDL_Dlist & theIterationIndexes );


	/* Lets allow safe-downcasts... (Basic RTTI) */
  virtual operator const _TDL_WithStatement * () const;


	/* iostreamBase interface.  Inherited from _TDL_TDLStatement. */
  virtual ostream & printObject ( ostream    & theOstream,
				  const char * theIndentString = "") const;


	/* These operations are disabled (prohibited) & unimplemented */
private:
  _TDL_WithStatement (             const _TDL_WithStatement & theObjectToCopy);
  _TDL_WithStatement & operator = (const _TDL_WithStatement & theObjectToCopy);
};

#endif /* _TDL_WithStatement_H */

