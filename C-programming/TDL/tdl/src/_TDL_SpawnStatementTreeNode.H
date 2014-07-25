/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

#ifndef _TDL_SpawnStatementTreeNode_H
#define _TDL_SpawnStatementTreeNode_H

#define _TDL_INTERNAL_
#include "tdl.H"
#include "_TDL_Snode.H"
#include "_TDL_TreeNode.H"
#include "_TDL_SpawnStatementData.H"

/*
 * _TDL_SpawnStatementTreeNode represents a TreeNode that corresponds
 * to a specific _TDL_SpawnStatementData object.
 *
 * (See _TDL_SpawnStatement.H for the Association Relationship.)
 */


class _TDL_SpawnStatementTreeNode :
#ifndef DISABLE_ISTREAMBASE
		          public virtual iostreamBase,
#endif /* ! DISABLE_ISTREAMBASE */
			  public _TDL_Snode,
			  public _TDL_TreeNode
{
protected:
  _TDL_SpawnStatementData &  spawnStatementData;


public:
  _TDL_SpawnStatementTreeNode (_TDL_SpawnStatementData & theSpawnStatementData)
    :  _TDL_Snode         (                       ),
       _TDL_TreeNode      (                       ),
       spawnStatementData ( theSpawnStatementData )
  {}

  virtual ~_TDL_SpawnStatementTreeNode();


  _TDL_SpawnStatementData & getSpawnStatementData() const
						 { return spawnStatementData; }

  _TDL_SpawnStatement     & getSpawnStatement()     const
		      { return getSpawnStatementData() . getSpawnStatement(); }


	/* Convenience Methods */
  BOOLEAN isAllocated()  const {return getSpawnStatementData().isAllocated(); }
  BOOLEAN isNotStarted() const {return getSpawnStatementData().isNotStarted();}
  BOOLEAN isStarted()    const {return getSpawnStatementData().isStarted();   }
  BOOLEAN isDestroyed()  const {return getSpawnStatementData().isDestroyed(); }

  BOOLEAN isDistributable() const
			  { return getSpawnStatementData().isDistributable(); }

  _TDL_SpawnStatementData::FLAGS getState() const
			  { return getSpawnStatementData().getState(); }
  const char * getStateString() const
			  { return getSpawnStatementData().getStateString(); }



        /***************************/
	/* _TDL_TreeNode interface */
        /***************************/


	/* Allow automatic conversion to TCM_Task_Tree_Ref.
	 * (May also transparently allocate() that first
	 *  _TDL_SpawnStatementData, performing lazy allocation.)
	 */
  virtual operator const TCM_Task_Tree_Ref & ();


	/* Names are really useful for identifying (subclassed) *
	 * _TDL_TreeNode instances during debugging/testing...  */
  virtual const char * getName() const;
  virtual BOOLEAN      hasName ( const char * theName ) const;


	/* Performs a depth-first search to discover whether or not this *
	 * object contains any _TDL_SpawnStatementTreeNode objects...    */
  virtual const _TDL_SpawnStatementTreeNode *
				      firstSpawnStatementTreeNodeConst() const;

	/* Common workhorse method to destroy _TDL_SpawnStatementData's *
         * referred to by this subtree.                                 */
  virtual status_t doDestroy ( BOOLEAN   theCanStillBeReferenced,
			       BOOLEAN   theDestroyIfUnused,
			       BOOLEAN & theFoundSpawnStatementData,
			       int4      theDepth                    );

	/* Lets allow safe-downcasts... (Basic RTTI) */
  virtual operator const _TDL_SpawnStatementTreeNode * () const;


	/* iostreamBase interface */
  virtual ostream & printObject ( ostream    & theOstream,
				  const char * theIndentString = "") const;

	/* Convenience method */
  ostream & printArrayIndexes ( ostream & theOstream ) const;


  int operator == (
	  const _TDL_SpawnStatementTreeNode & theSpawnStatementTreeNode ) const
			      { return (& theSpawnStatementTreeNode) == this; }
  int operator != (
	  const _TDL_SpawnStatementTreeNode & theSpawnStatementTreeNode ) const
			      { return (& theSpawnStatementTreeNode) != this; }


  int operator == (const _TDL_SpawnStatementData & theSpawnStatementData) const
		   { return getSpawnStatementData() == theSpawnStatementData; }
  int operator != (const _TDL_SpawnStatementData & theSpawnStatementData) const
		   { return getSpawnStatementData() != theSpawnStatementData; }


	/* These operations are disabled (prohibited) & unimplemented */
private:
  _TDL_SpawnStatementTreeNode (
			 const _TDL_SpawnStatementTreeNode & theObjectToCopy );
  _TDL_SpawnStatementTreeNode & operator = (
			 const _TDL_SpawnStatementTreeNode & theObjectToCopy );

};

#endif /* _TDL_SpawnStatementTreeNode_H */
