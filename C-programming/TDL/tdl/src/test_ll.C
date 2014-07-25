/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

#include <tdl.H>

#define perr(X)    cerr << # X " = " << (X) << endl;
#define ptst(X)    cerr << # X " = " << (((X) != ((void*)0)) ? "success" : "***FAILURE***") << endl;

class sn : public _TDL_Snode
{
public:
  char * id;

  sn ( char * theId ) : id ( theId ) {}

	/* Abstract iostreamBase-interface method */
  virtual ostream & printObject ( ostream    & theOstream,
				  const char * theIndentString = "") const
  {
    theOstream << theIndentString << id;
    return theOstream;
  }
};

void pslist ( _TDL_Slist & theSlist, ostream & theOstream )
{
  _TDL_Snode * snode;

  for ( snode  = theSlist . getFirstNode();
	snode != (_TDL_Snode *) NULL;
	snode  = snode -> getNextNode() )
  {
    snode -> operator << ( theOstream );
    if ( snode -> getNextNode() != (_TDL_Snode *) NULL )
      theOstream << " , ";
    else
      theOstream << endl;
  }
}


class dn : public _TDL_Dnode
{
public:
  char * id;

  dn ( char * theId ) : id ( theId ) {}

	/* Abstract iostreamBase-interface method */
  virtual ostream & printObject ( ostream    & theOstream,
				  const char * theIndentString = "") const
  {
    theOstream << theIndentString << id;
    return theOstream;
  }
};


void pdlist ( _TDL_Dlist & theDlist, ostream & theOstream )
{
  _TDL_Dnode * dnode;

  for ( dnode  = theDlist . getFirstNode();
	dnode != (_TDL_Dnode *) NULL;
	dnode  = dnode -> getNextNode() )
  {
    dnode -> operator << ( theOstream );
    if ( dnode -> getNextNode() != (_TDL_Dnode *) NULL )
      theOstream << " , ";
    else
      theOstream << endl;
  }
}


int
main()
{
	/* If we don't TCM_Initialize(), we get a core dump when we exit. */
  TCM_Initialize();

  _TDL_Slist  slist;
  sn * a = new sn ( "a" );
  sn * b = new sn ( "b" );
  sn * c = new sn ( "c" );
  sn * d = new sn ( "d" );

  _TDL_Dlist  dlist;
  dn * A = new dn ( "A" );
  dn * B = new dn ( "B" );
  dn * C = new dn ( "C" );
  dn * D = new dn ( "D" );


  perr ( slist . appendNode ( a ) );
  pslist ( slist, cerr );
  perr ( slist . count() );
  perr ( slist . contains ( a ) );
  perr ( slist . contains ( b ) );
  ptst ( slist . removeNode ( a ) );
  pslist ( slist, cerr );
  cerr << endl;

  perr ( slist . appendNode ( a ) );
  perr ( slist . appendNode ( b ) );
  pslist ( slist, cerr );
  perr ( slist . count() );
  perr ( slist . contains ( a ) );
  perr ( slist . contains ( b ) );
  perr ( slist . contains ( c ) );
  ptst ( slist . removeNode ( a ) );
  pslist ( slist, cerr );
  ptst ( slist . removeNode ( b ) );
  pslist ( slist, cerr );
  cerr << endl;

  perr ( slist . appendNode ( a ) );
  perr ( slist . appendNode ( b ) );
  pslist ( slist, cerr );
  ptst ( slist . removeNode ( b ) );
  pslist ( slist, cerr );
  ptst ( slist . removeNode ( a ) );
  pslist ( slist, cerr );
  cerr << endl;
  cerr << endl;

  perr ( slist . appendNode ( a ) );
  perr ( slist . appendNode ( b ) );
  perr ( slist . appendNode ( c ) );
  pslist ( slist, cerr );
  perr ( slist . count() );
  perr ( slist . contains ( a ) );
  perr ( slist . contains ( b ) );
  perr ( slist . contains ( c ) );
  perr ( slist . contains ( d ) );
  ptst ( slist . removeNode ( a ) );
  pslist ( slist, cerr );
  ptst ( slist . removeNode ( b ) );
  pslist ( slist, cerr );
  ptst ( slist . removeNode ( c ) );
  pslist ( slist, cerr );
  cerr << endl;

  perr ( slist . appendNode ( a ) );
  perr ( slist . appendNode ( b ) );
  perr ( slist . appendNode ( c ) );
  pslist ( slist, cerr );
  ptst ( slist . removeNode ( c ) );
  pslist ( slist, cerr );
  ptst ( slist . removeNode ( b ) );
  pslist ( slist, cerr );
  ptst ( slist . removeNode ( a ) );
  pslist ( slist, cerr );
  cerr << endl;

  perr ( slist . appendNode ( a ) );
  perr ( slist . appendNode ( b ) );
  perr ( slist . appendNode ( c ) );
  pslist ( slist, cerr );
  ptst ( slist . removeNode ( a ) );
  pslist ( slist, cerr );
  ptst ( slist . removeNode ( c ) );
  pslist ( slist, cerr );
  ptst ( slist . removeNode ( b ) );
  pslist ( slist, cerr );
  cerr << endl;

  perr ( slist . appendNode ( a ) );
  perr ( slist . appendNode ( b ) );
  perr ( slist . appendNode ( c ) );
  pslist ( slist, cerr );
  ptst ( slist . removeNode ( c ) );
  pslist ( slist, cerr );
  ptst ( slist . removeNode ( a ) );
  pslist ( slist, cerr );
  ptst ( slist . removeNode ( b ) );
  pslist ( slist, cerr );
  cerr << endl;
  cerr << endl;

  perr ( slist . appendNode ( a ) );
  perr ( slist . appendNode ( b ) );
  perr ( slist . appendNode ( c ) );
  pslist ( slist, cerr );
  ptst ( slist . removeNode ( b ) );
  pslist ( slist, cerr );
  ptst ( slist . removeNode ( a ) );
  pslist ( slist, cerr );
  ptst ( slist . removeNode ( c ) );
  pslist ( slist, cerr );
  cerr << endl;

  perr ( slist . appendNode ( a ) );
  perr ( slist . appendNode ( b ) );
  perr ( slist . appendNode ( c ) );
  pslist ( slist, cerr );
  ptst ( slist . removeNode ( b ) );
  pslist ( slist, cerr );
  ptst ( slist . removeNode ( c ) );
  pslist ( slist, cerr );
  ptst ( slist . removeNode ( a ) );
  pslist ( slist, cerr );
  cerr << endl;
  cerr << endl;

  cerr << "==============================================="
       << endl << endl << endl;

  perr ( dlist . appendNode ( A ) );
  pdlist ( dlist, cerr );
  perr ( dlist . count() );
  perr ( dlist . contains ( A ) );
  perr ( dlist . contains ( B ) );
  ptst ( dlist . removeNode ( A, _TDL_Dlist::FORWARD ) );
  pdlist ( dlist, cerr );
  cerr << endl;

  perr ( dlist . appendNode ( A ) );
  perr ( dlist . appendNode ( B ) );
  pdlist ( dlist, cerr );
  perr ( dlist . count() );
  perr ( dlist . contains ( A ) );
  perr ( dlist . contains ( B ) );
  perr ( dlist . contains ( C ) );
  ptst ( dlist . removeNode ( A, _TDL_Dlist::FORWARD ) );
  pdlist ( dlist, cerr );
  ptst ( dlist . removeNode ( B, _TDL_Dlist::FORWARD ) );
  pdlist ( dlist, cerr );
  cerr << endl;

  perr ( dlist . appendNode ( A ) );
  perr ( dlist . appendNode ( B ) );
  pdlist ( dlist, cerr );
  ptst ( dlist . removeNode ( B, _TDL_Dlist::FORWARD ) );
  pdlist ( dlist, cerr );
  ptst ( dlist . removeNode ( A, _TDL_Dlist::FORWARD ) );
  pdlist ( dlist, cerr );
  cerr << endl;
  cerr << endl;

  perr ( dlist . appendNode ( A ) );
  perr ( dlist . appendNode ( B ) );
  perr ( dlist . appendNode ( C ) );
  pdlist ( dlist, cerr );
  perr ( dlist . count() );
  perr ( dlist . contains ( A ) );
  perr ( dlist . contains ( B ) );
  perr ( dlist . contains ( C ) );
  perr ( dlist . contains ( D ) );
  ptst ( dlist . removeNode ( A, _TDL_Dlist::FORWARD ) );
  pdlist ( dlist, cerr );
  ptst ( dlist . removeNode ( B, _TDL_Dlist::FORWARD ) );
  pdlist ( dlist, cerr );
  ptst ( dlist . removeNode ( C, _TDL_Dlist::FORWARD ) );
  pdlist ( dlist, cerr );
  cerr << endl;

  perr ( dlist . appendNode ( A ) );
  perr ( dlist . appendNode ( B ) );
  perr ( dlist . appendNode ( C ) );
  pdlist ( dlist, cerr );
  ptst ( dlist . removeNode ( C, _TDL_Dlist::FORWARD ) );
  pdlist ( dlist, cerr );
  ptst ( dlist . removeNode ( B, _TDL_Dlist::FORWARD ) );
  pdlist ( dlist, cerr );
  ptst ( dlist . removeNode ( A, _TDL_Dlist::FORWARD ) );
  pdlist ( dlist, cerr );
  cerr << endl;

  perr ( dlist . appendNode ( A ) );
  perr ( dlist . appendNode ( B ) );
  perr ( dlist . appendNode ( C ) );
  pdlist ( dlist, cerr );
  ptst ( dlist . removeNode ( A, _TDL_Dlist::FORWARD ) );
  pdlist ( dlist, cerr );
  ptst ( dlist . removeNode ( C, _TDL_Dlist::FORWARD ) );
  pdlist ( dlist, cerr );
  ptst ( dlist . removeNode ( B, _TDL_Dlist::FORWARD ) );
  pdlist ( dlist, cerr );
  cerr << endl;

  perr ( dlist . appendNode ( A ) );
  perr ( dlist . appendNode ( B ) );
  perr ( dlist . appendNode ( C ) );
  pdlist ( dlist, cerr );
  ptst ( dlist . removeNode ( C, _TDL_Dlist::FORWARD ) );
  pdlist ( dlist, cerr );
  ptst ( dlist . removeNode ( A, _TDL_Dlist::FORWARD ) );
  pdlist ( dlist, cerr );
  ptst ( dlist . removeNode ( B, _TDL_Dlist::FORWARD ) );
  pdlist ( dlist, cerr );
  cerr << endl;
  cerr << endl;

  perr ( dlist . appendNode ( A ) );
  perr ( dlist . appendNode ( B ) );
  perr ( dlist . appendNode ( C ) );
  pdlist ( dlist, cerr );
  ptst ( dlist . removeNode ( B, _TDL_Dlist::FORWARD ) );
  pdlist ( dlist, cerr );
  ptst ( dlist . removeNode ( A, _TDL_Dlist::FORWARD ) );
  pdlist ( dlist, cerr );
  ptst ( dlist . removeNode ( C, _TDL_Dlist::FORWARD ) );
  pdlist ( dlist, cerr );
  cerr << endl;

  perr ( dlist . appendNode ( A ) );
  perr ( dlist . appendNode ( B ) );
  perr ( dlist . appendNode ( C ) );
  pdlist ( dlist, cerr );
  ptst ( dlist . removeNode ( B, _TDL_Dlist::FORWARD ) );
  pdlist ( dlist, cerr );
  ptst ( dlist . removeNode ( C, _TDL_Dlist::FORWARD ) );
  pdlist ( dlist, cerr );
  ptst ( dlist . removeNode ( A, _TDL_Dlist::FORWARD ) );
  pdlist ( dlist, cerr );
  cerr << endl;
  cerr << endl;


  cerr << "==============================================="
       << endl << endl << endl;

  perr ( dlist . appendNode ( A ) );
  pdlist ( dlist, cerr );
  perr ( dlist . count() );
  perr ( dlist . contains ( A ) );
  perr ( dlist . contains ( B ) );
  ptst ( dlist . removeNode ( A, _TDL_Dlist::BACKWARD ) );
  pdlist ( dlist, cerr );
  cerr << endl;

  perr ( dlist . appendNode ( A ) );
  perr ( dlist . appendNode ( B ) );
  pdlist ( dlist, cerr );
  perr ( dlist . count() );
  perr ( dlist . contains ( A ) );
  perr ( dlist . contains ( B ) );
  perr ( dlist . contains ( C ) );
  ptst ( dlist . removeNode ( A, _TDL_Dlist::BACKWARD ) );
  pdlist ( dlist, cerr );
  ptst ( dlist . removeNode ( B, _TDL_Dlist::BACKWARD ) );
  pdlist ( dlist, cerr );
  cerr << endl;

  perr ( dlist . appendNode ( A ) );
  perr ( dlist . appendNode ( B ) );
  pdlist ( dlist, cerr );
  ptst ( dlist . removeNode ( B, _TDL_Dlist::BACKWARD ) );
  pdlist ( dlist, cerr );
  ptst ( dlist . removeNode ( A, _TDL_Dlist::BACKWARD ) );
  pdlist ( dlist, cerr );
  cerr << endl;
  cerr << endl;

  perr ( dlist . appendNode ( A ) );
  perr ( dlist . appendNode ( B ) );
  perr ( dlist . appendNode ( C ) );
  pdlist ( dlist, cerr );
  perr ( dlist . count() );
  perr ( dlist . contains ( A ) );
  perr ( dlist . contains ( B ) );
  perr ( dlist . contains ( C ) );
  perr ( dlist . contains ( D ) );
  ptst ( dlist . removeNode ( A, _TDL_Dlist::BACKWARD ) );
  pdlist ( dlist, cerr );
  ptst ( dlist . removeNode ( B, _TDL_Dlist::BACKWARD ) );
  pdlist ( dlist, cerr );
  ptst ( dlist . removeNode ( C, _TDL_Dlist::BACKWARD ) );
  pdlist ( dlist, cerr );
  cerr << endl;

  perr ( dlist . appendNode ( A ) );
  perr ( dlist . appendNode ( B ) );
  perr ( dlist . appendNode ( C ) );
  pdlist ( dlist, cerr );
  ptst ( dlist . removeNode ( C, _TDL_Dlist::BACKWARD ) );
  pdlist ( dlist, cerr );
  ptst ( dlist . removeNode ( B, _TDL_Dlist::BACKWARD ) );
  pdlist ( dlist, cerr );
  ptst ( dlist . removeNode ( A, _TDL_Dlist::BACKWARD ) );
  pdlist ( dlist, cerr );
  cerr << endl;

  perr ( dlist . appendNode ( A ) );
  perr ( dlist . appendNode ( B ) );
  perr ( dlist . appendNode ( C ) );
  pdlist ( dlist, cerr );
  ptst ( dlist . removeNode ( A, _TDL_Dlist::BACKWARD ) );
  pdlist ( dlist, cerr );
  ptst ( dlist . removeNode ( C, _TDL_Dlist::BACKWARD ) );
  pdlist ( dlist, cerr );
  ptst ( dlist . removeNode ( B, _TDL_Dlist::BACKWARD ) );
  pdlist ( dlist, cerr );
  cerr << endl;

  perr ( dlist . appendNode ( A ) );
  perr ( dlist . appendNode ( B ) );
  perr ( dlist . appendNode ( C ) );
  pdlist ( dlist, cerr );
  ptst ( dlist . removeNode ( C, _TDL_Dlist::BACKWARD ) );
  pdlist ( dlist, cerr );
  ptst ( dlist . removeNode ( A, _TDL_Dlist::BACKWARD ) );
  pdlist ( dlist, cerr );
  ptst ( dlist . removeNode ( B, _TDL_Dlist::BACKWARD ) );
  pdlist ( dlist, cerr );
  cerr << endl;
  cerr << endl;

  perr ( dlist . appendNode ( A ) );
  perr ( dlist . appendNode ( B ) );
  perr ( dlist . appendNode ( C ) );
  pdlist ( dlist, cerr );
  ptst ( dlist . removeNode ( B, _TDL_Dlist::BACKWARD ) );
  pdlist ( dlist, cerr );
  ptst ( dlist . removeNode ( A, _TDL_Dlist::BACKWARD ) );
  pdlist ( dlist, cerr );
  ptst ( dlist . removeNode ( C, _TDL_Dlist::BACKWARD ) );
  pdlist ( dlist, cerr );
  cerr << endl;

  perr ( dlist . appendNode ( A ) );
  perr ( dlist . appendNode ( B ) );
  perr ( dlist . appendNode ( C ) );
  pdlist ( dlist, cerr );
  ptst ( dlist . removeNode ( B, _TDL_Dlist::BACKWARD ) );
  pdlist ( dlist, cerr );
  ptst ( dlist . removeNode ( C, _TDL_Dlist::BACKWARD ) );
  pdlist ( dlist, cerr );
  ptst ( dlist . removeNode ( A, _TDL_Dlist::BACKWARD ) );
  pdlist ( dlist, cerr );
  cerr << endl;
  cerr << endl;


  cerr << "-----------------------------------------------" << endl;

  pslist ( slist, cerr );
  perr   ( slist . prependNode ( a ) );
  pslist ( slist, cerr );
  perr   ( slist . prependNode ( b ) );
  pslist ( slist, cerr );
  perr   ( slist . prependNode ( c ) );
  pslist ( slist, cerr );
  ptst   ( slist . removeNode  ( b ) );
  pslist ( slist, cerr );
  ptst   ( slist . removeNode  ( a ) );
  pslist ( slist, cerr );
  ptst   ( slist . removeNode  ( c ) );
  pslist ( slist, cerr );
  cerr << endl << endl;

  pslist ( slist, cerr );
  perr   ( slist . prependNode ( a ) );
  pslist ( slist, cerr );
  perr   ( slist . appendNode  ( b ) );
  pslist ( slist, cerr );
  perr   ( slist . prependNode ( c ) );
  pslist ( slist, cerr );
  ptst   ( slist . removeNode  ( a ) );
  pslist ( slist, cerr );
  ptst   ( slist . removeNode  ( b ) );
  pslist ( slist, cerr );
  ptst   ( slist . removeNode  ( c ) );
  pslist ( slist, cerr );
  cerr << endl << endl;

  pslist ( slist, cerr );
  perr   ( slist . appendNode  ( a ) );
  pslist ( slist, cerr );
  perr   ( slist . prependNode ( b ) );
  pslist ( slist, cerr );
  perr   ( slist . appendNode  ( c ) );
  pslist ( slist, cerr );
  ptst   ( slist . removeNode  ( a ) );
  pslist ( slist, cerr );
  ptst   ( slist . removeNode  ( b ) );
  pslist ( slist, cerr );
  ptst   ( slist . removeNode  ( c ) );
  pslist ( slist, cerr );
  cerr << endl << endl;


  pdlist ( dlist, cerr );
  perr   ( dlist . prependNode ( A ) );
  pdlist ( dlist, cerr );
  perr   ( dlist . prependNode ( B ) );
  pdlist ( dlist, cerr );
  perr   ( dlist . prependNode ( C ) );
  pdlist ( dlist, cerr );
  ptst   ( dlist . removeNode  ( B ) );
  pdlist ( dlist, cerr );
  ptst   ( dlist . removeNode  ( A ) );
  pdlist ( dlist, cerr );
  ptst   ( dlist . removeNode  ( C ) );
  pdlist ( dlist, cerr );
  cerr << endl << endl;

  pdlist ( dlist, cerr );
  perr   ( dlist . prependNode ( A ) );
  pdlist ( dlist, cerr );
  perr   ( dlist . appendNode  ( B ) );
  pdlist ( dlist, cerr );
  perr   ( dlist . prependNode ( C ) );
  pdlist ( dlist, cerr );
  ptst   ( dlist . removeNode  ( A ) );
  pdlist ( dlist, cerr );
  ptst   ( dlist . removeNode  ( B ) );
  pdlist ( dlist, cerr );
  ptst   ( dlist . removeNode  ( C ) );
  pdlist ( dlist, cerr );
  cerr << endl << endl;

  pdlist ( dlist, cerr );
  perr   ( dlist . appendNode  ( A ) );
  pdlist ( dlist, cerr );
  perr   ( dlist . prependNode ( B ) );
  pdlist ( dlist, cerr );
  perr   ( dlist . appendNode  ( C ) );
  pdlist ( dlist, cerr );
  ptst   ( dlist . removeNode  ( A ) );
  pdlist ( dlist, cerr );
  ptst   ( dlist . removeNode  ( B ) );
  pdlist ( dlist, cerr );
  ptst   ( dlist . removeNode  ( C ) );
  pdlist ( dlist, cerr );
  cerr << endl << endl;


}
