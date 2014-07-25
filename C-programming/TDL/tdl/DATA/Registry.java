
/** This class establishes a one-to-one or one-to-N mapping between names
 *  and objects.  If the Registry is created to enable one-to-N mappings,
 *  two or more objects may share the same name.  Objects may *NOT* be
 *  registered under more than one name in any event...
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 *
 */

import java.util.Enumeration;

public class Registry extends Object
{
	/* Instance variables */
  protected DataHashtable  nameToObjectHashtable;
  protected DataHashtable  objectToNameHashtable;
  protected boolean        allowNamesToMapToMultipleObjects;

	/* Instance methods */
  public Registry ( boolean theAllowNamesToMapToMultipleObjects )
  {
    nameToObjectHashtable = new DataHashtable();
    objectToNameHashtable = new DataHashtable();
    allowNamesToMapToMultipleObjects = theAllowNamesToMapToMultipleObjects;
  }


  protected DataHashtable getNameToObjectHashtable ( )
				   { return nameToObjectHashtable; }

  protected DataHashtable getObjectToNameHashtable ( )
				   { return objectToNameHashtable; }

  protected boolean       getAllowNamesToMapToMultipleObjects ( )
				   { return allowNamesToMapToMultipleObjects; }


  public boolean register ( String theName, Object theObject )
  {
	/* Idiocy check. */
    if ( theObject instanceof DataVector )
    {
      System.err.println ( "[Registry:register]  Error:  Object may not be "
			   + "an instance of the DataVector class." );
      return false;
    }

	/* Is this name/object pair already registered together? */
    if ( hasNameObjectPair ( theName, theObject ) )
    {
      System.err.println ( "[Registry:register]  Warning:  Object/Name already"
			   + " registered together.  (\"" + theName + "\")" );
      return true;  /* Already done before we start... */
    }

	/* If this name is already in use... */
    if (   ( getAllowNamesToMapToMultipleObjects() == false )
	&& ( getIsNameRegistered ( theName )                ) )
    {
      System.err.println ( "[Registry:register]  Warning:  Name already "
			   + "registered.  (\"" + theName + "\")" );
      return false;
    }

	/* If this object is already in use... */
    if ( getIsObjectRegistered ( theObject ) )
    {
      System.err.println ( "[Registry:register]  Warning:  Object already "
			   + "registered under name \""
			   + getNameForObject ( theObject ) + "\"." );
      return false;
    }


    if ( getIsNameRegistered ( theName ) == false )
    {
      getNameToObjectHashtable() . put ( theName, theObject );
    }
    else
    {
      DataVector  currentVector = getObjectsForName ( theName );
      currentVector . addElement ( theObject );
      getNameToObjectHashtable() . put ( theName, currentVector );
    }

    getObjectToNameHashtable() . put ( theObject, theName );
    return true;
  }


  public boolean unregister ( Object theObject )
  {
	/* If this Object is not already in use */
    if ( getIsObjectRegistered ( theObject ) == false )
    {
      System.err.println ( "[Registry:unregister]  Warning:  "
			   + "Object is not registered." );
      return false;
    }

    String  theName = getNameForObject ( theObject );

    if ( theName == null )
    {
      System.err.println ( "[Registry:unregister]  Warning:  "
			   + "Object's name is not registered." );
    }
    else
    {
      switch ( getNumberOfObjectsForName ( theName ) )
      {
	case 0:
	  System.err.println ( "[Registry:unregister]  Warning:  "
			     + "No Object's registered under Object's Name." );
	  break;

	case 1:
	  if ( getFirstObjectForName ( theName ) != theObject )
	    System.err.println ( "[Registry:unregister]  Warning:  "
				 + "Object's Name's Object is not Object." );
	  else
	    getNameToObjectHashtable() . remove ( theName );
	  break;

	default:
	  if ( getObjectsForName ( theName ) . removeElement ( theObject )
	       == false )
	  {
	    System.err.println ( "[Registry:unregister]  Warning:  "
				 + "Object not found under Object's Name." );
	  }
	  break;
      }
    }

    getObjectToNameHashtable() . remove ( theObject );
    return true;
  }


  public DataVector getObjectsForName ( String theName )
  {
    Object  aObject = getNameToObjectHashtable() . get ( theName );

    if ( aObject == null )
      return null;

    else if ( aObject instanceof DataVector )
    {
      if (  ((DataVector) aObject) . count() <= 0 )
	return null;
      else
	return (DataVector) aObject;
    }

    else
    {
      DataVector  returnVector = new DataVector(1);
      returnVector . addElement ( aObject );
      return returnVector;
    }
  }

  public Object getFirstObjectForName ( String theName )
  {
    Object  aObject = getNameToObjectHashtable() . get ( theName );

    if ( aObject == null )
      return null;

    else if ( aObject instanceof DataVector )
    {
      if (  ((DataVector) aObject) . count() <= 0 )
	return null;
      else
	return ((DataVector) aObject) . elementAt ( 0 );
    }

    else
      return aObject;
  }

  public int getNumberOfObjectsForName ( String theName )
  {
    Object  aObject = getNameToObjectHashtable() . get ( theName );

    if ( aObject == null )
      return 0;

    else if ( aObject instanceof DataVector )
      return ((DataVector) aObject) . count();

    else
      return 1;
  }


  public String  getNameForObject ( Object theObject )
  {
    return (String) getObjectToNameHashtable() . get ( theObject );
  }


  public boolean hasNameObjectPair ( String theName, Object theObject )
  {
    switch ( getNumberOfObjectsForName ( theName ) )
    {
      case 0:
	return false;

      case 1:
	if ( getFirstObjectForName ( theName ) != theObject )
	  return false;
	break;

      default:
	if ( getObjectsForName ( theName ) . contains ( theObject ) == false )
	  return false;
	break;
    }

    if ( getNameForObject ( theObject ) != theName )
      return false;

    return true;
  }



  public boolean getIsNameRegistered ( String theName )
  {
    return getFirstObjectForName ( theName ) != null;
  }

  public boolean getIsObjectRegistered ( Object theObject )
  {
    return getNameForObject ( theObject ) != null;
  }

  public Enumeration getObjects()
  {
    return getObjectToNameHashtable() . keys();
  }

  public Enumeration getNames()
  {
    return getNameToObjectHashtable() . keys();
  }

  public String toString()
  {
    StringBuffer  stringBuffer = new StringBuffer();
    Enumeration   names = getNames();

    stringBuffer . append ( "Registry " );
    stringBuffer . append ( Integer.toHexString ( hashCode() ) );
    stringBuffer . append ( ":\n" );
    while ( names . hasMoreElements() )
    {
      stringBuffer . append ( "     \"" );
      stringBuffer . append ( (String) (names.nextElement()) );
      stringBuffer . append ( "\"\n" );
    }
    stringBuffer . append ( "\n" );
    return stringBuffer . toString();
  }
}
  
