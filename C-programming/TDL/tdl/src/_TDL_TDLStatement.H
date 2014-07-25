/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

#ifndef _TDL_TDLStatement_H
#define _TDL_TDLStatement_H

#define _TDL_INTERNAL_
#include "tdl.H"
#include "_TDL_NamesList.H"
#include "_TDL_Snode.H"
#include "_TDL_Slist.H"
#include "_TDL_Dnode.H"
#include "_TDL_Dlist.H"
#include "_TDL_TreeNode.H"
#include "_TDL_TreeNodeBranch.H"
#include "_TDL_IterationIndex.H"

class _TDL_HandleManager;
class _TDL_SpawnStatementData;

/*
 * _TDL_TDLStatment is an abstract base class.  Its subclasses represent
 * the SPAWN and WITH statements in TDL.
 */

class _TDL_SpawnStatement;
class _TDL_WithStatement;


class _TDL_TDLStatement :
#ifndef DISABLE_ISTREAMBASE
		        public virtual iostreamBase,
#endif /* ! DISABLE_ISTREAMBASE */
		        public _TDL_NamesList,
		        public _TDL_Snode
{
protected:
  _TDL_TreeNode * topmostTreeNode;
  _TDL_Slist      dataObjectSlist;
  _TDL_Slist      objectsToDelete;

	/* These are used for doing incremental cached-based lookups */
  _TDL_TreeNode * cachedTreeNode;

	/* In certain obscure cases relating to distributed invocation,
	 * SPAWN statements must examine WITH statements for constraints
	 * before they are inserted into the dynamic task-tree hierarchy.
	 * Thus, we set up a default WITH parent tree to search through.
	 * We will assume the defaultWithParent does NOT need to be copied
	 * or deleted, since it is most likely a C-char-string-constant.
	 */
  const char * defaultWithParent;

	/* And, of course, the defaultWithParent string requires that we
	 * have some mechanism for accessing our _TDL_HandleManager object.
	 */
  _TDL_HandleManager * ourHandleManager;



public:
  _TDL_TDLStatement ( u_int4       theInitialCapacity
				    = _TDL_ArrayList::DEFAULT_INITIAL_CAPACITY,
		      const char * theFirstName
				    = (const char *)NULL,
		      _TDL_HandleManager * theHandleManger
				    = (_TDL_HandleManager *) NULL )
    :  _TDL_NamesList    ( theInitialCapacity, theFirstName    ),
       _TDL_Snode        (                                     ),
       topmostTreeNode   ( (_TDL_TreeNode *) NULL              ),
       dataObjectSlist   ( TRUE /* Delete contained objects */ ),
       objectsToDelete   ( TRUE /* Delete contained objects */ ),
       cachedTreeNode    ( (_TDL_TreeNode *) NULL              ),
       defaultWithParent ( (const char *) NULL                 ),
       ourHandleManager  ( theHandleManger                     )
  {}

  virtual ~_TDL_TDLStatement();
  
  const _TDL_Slist & getDataObjectSlist() const { return dataObjectSlist; }

  _TDL_TreeNode    * getTopmostTreeNode() const { return topmostTreeNode; }
  void               setTopmostTreeNode( _TDL_TreeNode * theTopmostTreeNode )
				      { topmostTreeNode = theTopmostTreeNode; }

  const char * getDefaultWithParent() const { return defaultWithParent; }
  void         setDefaultWithParent( const char * theDefaultWithParent )
			    { defaultWithParent = theDefaultWithParent; }
  
  _TDL_TDLStatement * getDefaultWithParent_AsTdlStatement() const;

	/* When extracting the default-with-parent, what we really want is
	 * the _TDL_TreeNode inside the WITH-TDLStatement's iteration tree that
	 * corresponds to the same set of iteration indexes as a particular
	 * _TDL_SpawnStatementData instance located inside our
	 * (this _TDL_TDLStatement object's) iteration tree.
	 *
	 * We do *NOT* want to create new nodes...  If the WITH-side tree
	 * doesn't extend far enough yet, we want to only go down as far as we
	 * can go along the iteration indexes determined by the
	 * _TDL_SpawnStatementData instance.
	 *
	 * This also implies that we can use the _TDL_SpawnStatementData's
	 * iteration indexes on all the WITH statements going all the way up
	 * the default-with-tree, including on statements that will never
	 * have as many iteration indexes as it has.
	 */
  const _TDL_TreeNode * getDefaultWithParent_AsTreeNode (
		 const _TDL_TDLStatement       * theWithTdlStatement,
		 const _TDL_SpawnStatementData * theSpawnStatementData ) const;



  _TDL_HandleManager * getOurHandleManager() const { return ourHandleManager; }
  void         setOurHandleManager( _TDL_HandleManager * theOurHandleManager )
				    { ourHandleManager = theOurHandleManager; }




	/* Subclass interface */
  virtual status_t createObjects (
		_TDL_Snode    * & theStatementData,
		_TDL_TreeNode * & theTreeNode       ) TCM_PURE_VIRTUAL_METHOD ;

  virtual status_t deleteObjects (
		_TDL_Snode    *   theStatementData,
		_TDL_TreeNode *   theTreeNode       ) TCM_PURE_VIRTUAL_METHOD ;


	/* Note: Non-Const methods will create the node if necessary */
	/*           Const methods will merely return NULL!          */
  _TDL_TreeNode * getTreeNode( const _TDL_Dlist & theIterationIndexes,
			       BOOLEAN            theLastNodeIsABranch = FALSE)
			  { return getTreeNode ( theIterationIndexes, TRUE,
						 theLastNodeIsABranch );    }

  _TDL_TreeNode * getTreeNodeConst (
			       const _TDL_Dlist & theIterationIndexes,
			       BOOLEAN            theLastNodeIsABranch = FALSE)
			  { return ((_TDL_TDLStatement *)this)
			       -> getTreeNode ( theIterationIndexes, FALSE,
						theLastNodeIsABranch );     }

  _TDL_TreeNode * getTreeNode( const _TDL_Dlist & theIterationIndexes,
			       BOOLEAN            theCanCreateNewNodes,
			       BOOLEAN            theLastNodeIsABranch =FALSE);

	/* Note: Non-Const methods will create the node if necessary */
	/*           Const methods will merely return NULL!          */
  _TDL_Snode * getStatementData      ( const _TDL_Dlist & theIterationIndexes);
  _TDL_Snode * getStatementDataConst ( const _TDL_Dlist & theIterationIndexes);

	/* Returns TRUE if the TreeNode DOES exist. */
  BOOLEAN hasTreeNode ( const _TDL_Dlist & theIterationIndexes )
	   { return (   getTreeNodeConst ( theIterationIndexes )
		     != (_TDL_TreeNode *) NULL ) ? TRUE : FALSE; }
  


	/********************************************/
	/* Incremental cached-based lookup methods. */
	/********************************************/

  const _TDL_TreeNode * getCachedTreeNode()   const  { return cachedTreeNode; }
  _TDL_TreeNode       * getCachedTreeNodeNonConst()  { return cachedTreeNode; }
  void                  setCachedTreeNode( _TDL_TreeNode * theCachedTreeNode )
				        { cachedTreeNode = theCachedTreeNode; }

  _TDL_TreeNode       * getCachedTopmostTreeNodeBranch (
				  BOOLEAN theShouldBeATreeNodeBranch = FALSE );

  BOOLEAN hasCachedTreeNode()  const
	       { return ( getCachedTreeNode() != (const _TDL_TreeNode *) NULL )
		   ? TRUE : FALSE; }

  _TDL_TDLStatement & resetCachedLookup()
		 { setCachedTreeNode ( getTopmostTreeNode() ); return *this; }

	/* Note: Non-Const methods will create the node if necessary */
	/*           Const methods will merely return NULL!          */

  _TDL_TDLStatement & cacheDescendIntoBranch      ( int4 theChildIndex )
	    { cacheDescendInto ( theChildIndex, TRUE,  TRUE  ); return *this; }

  _TDL_TDLStatement & cacheDescendIntoBranchConst ( int4 theChildIndex )
	    { cacheDescendInto ( theChildIndex, TRUE,  FALSE ); return *this; }


  _TDL_TreeNode     * cacheDescendIntoNode        ( int4 theChildIndex )
	    { cacheDescendInto ( theChildIndex, FALSE, TRUE  );
	      return getCachedTreeNodeNonConst(); }

  _TDL_TreeNode     * cacheDescendIntoNodeConst   ( int4 theChildIndex )
	    { cacheDescendInto ( theChildIndex, FALSE, FALSE );
	      return getCachedTreeNodeNonConst(); }


  void                cacheDescendInto ( int4    theChildIndex,
					 BOOLEAN theIsDescendingIntoBranch,
					 BOOLEAN theCanCreateNewNodes );


    /* Aliases for end-user accessibility/convenience with TDL_REF.
     * I always swore I would never do this.  But the convenience tempts me.
     *  To use:  TDL_REF(foo)[1][2](3) is equivalent to foo[1][2][3].
     */
  _TDL_TDLStatement & operator[] ( int4 theChildIndex )
			   { return cacheDescendIntoBranch ( theChildIndex ); }
  _TDL_TreeNode     * operator() ( int4 theChildIndex
					  = _TDL_TreeNode::NO_ARRAY_INDEX )
			   { return cacheDescendIntoNode   ( theChildIndex ); }

	/********************************************/
	/********************************************/


  	/* Note: theName will *NOT* be deleted when this object is destroyed!*/
  _TDL_TDLStatement * addName ( const char * theName )
		         { _TDL_NamesList::addName ( theName );  return this; }

	/* I keep forgetting this is inherited. */
  const char * getName() const  { return _TDL_NamesList::getName(); }

  virtual _TDL_TDLStatement * setTcmTaskTreeNodeName ( STRING theNodeName );

  operator const TCM_Task_Tree_Ref & ();




	/* destroy(), destroyIfUnused(), and doDestroy() operate analogously
	 * to, but still somewhat differently from, their counterparts in
	 * _TDL_TreeNode.
	 *
	 * Specifically, these versions of destruction act upon the set of
	 * Data objects (_TDL_SpawnStatementData or _TDL_WithStatementData
	 * instances) specified by a set of iteration indexes.
	 *
	 * They act to destroy all of the _TDL_SpawnStatementData objects
	 * specified by those iteration indexes.  Or all of the
	 * _TDL_SpawnStatementData objects contained in _TDL_WithStatementData
	 * objects that are specified by those iteration indexes.
	 */
  status_t destroy ( const _TDL_Dlist & theIterationIndexes,
		     BOOLEAN            theCanStillBeReferenced = FALSE )
		       { BOOLEAN  foundSpawnStatementData = FALSE;
			 return doDestroy ( theIterationIndexes,
					    theCanStillBeReferenced, FALSE,
					    foundSpawnStatementData, 0     ); }

  status_t destroyIfUnused( const _TDL_Dlist & theIterationIndexes,
			    BOOLEAN            theCanStillBeReferenced = FALSE)
		       { BOOLEAN  foundSpawnStatementData = FALSE;
		         return doDestroy ( theIterationIndexes,
					    theCanStillBeReferenced, TRUE,
					    foundSpawnStatementData, 0     ); }

  virtual status_t doDestroy ( const _TDL_Dlist & theIterationIndexes,
			       BOOLEAN            theCanStillBeReferenced,
			       BOOLEAN            theDestroyIfUnused,
			       BOOLEAN &          theFoundSpawnStatementData,
			       int4               theDepth                   );


	/* Lets allow safe-downcasts... (Basic RTTI) */
  virtual operator const _TDL_SpawnStatement * () const;
  virtual operator const _TDL_WithStatement  * () const;

	/* These versions of safe-downcasting might be clearer... */
  const _TDL_SpawnStatement * getSpawnStatementConst() const
			      { return (const _TDL_SpawnStatement *) (*this); }
  const _TDL_WithStatement  * getWithStatementConst()  const
			      { return (const _TDL_WithStatement  *) (*this); }

	/* And non-const versions...  (These default to just casting away *
         * const'ness after calling the const versions.)                  */
  virtual operator _TDL_SpawnStatement * ();
  virtual operator _TDL_WithStatement  * ();

	/* These versions of safe-downcasting might be clearer... */
  _TDL_SpawnStatement * getSpawnStatement()
				    { return (_TDL_SpawnStatement *) (*this); }
  _TDL_WithStatement  * getWithStatement()
				    { return (_TDL_WithStatement  *) (*this); }


	/* Convenience tests... */
  BOOLEAN isSpawnStatement() const 
    { return    getSpawnStatementConst()
	     != (const _TDL_SpawnStatement *) NULL ? TRUE : FALSE; }

  BOOLEAN isWithStatement()  const 
    { return    getWithStatementConst()
	     != (const _TDL_WithStatement  *) NULL ? TRUE : FALSE; }


	/* iostreamBase interface */
  virtual ostream & printObject ( ostream    & theOstream,
				  const char * theIndentString = "") const;


protected:
	/* Private data access method. */
  _TDL_Slist & getDataObjectSlistNonConst()      { return dataObjectSlist; }
  _TDL_Slist & getObjectsToDeleteSlistNonConst() { return objectsToDelete; }

  status_t addDataObject ( _TDL_Snode * theDataObject )
		 { return getDataObjectSlistNonConst()
			    . appendNode ( theDataObject ); }

  status_t addObjectToDelete ( _TDL_Snode * theObjectToDelete )
		 { return getObjectsToDeleteSlistNonConst()
			    . appendNode ( theObjectToDelete ); }


	/* These operations are disabled (prohibited) & unimplemented */
private:
  _TDL_TDLStatement              ( const _TDL_TDLStatement * theObjectToCopy );
  _TDL_TDLStatement & operator = ( const _TDL_TDLStatement * theObjectToCopy );
};



#endif /* _TDL_TDLStatement_H */

