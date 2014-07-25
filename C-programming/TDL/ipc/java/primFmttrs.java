/*****************************************************************************
 *       $Id: primFmttrs.java,v 1.6 2013/07/23 21:11:38 reids Exp $
 * $Revision: 1.6 $
 *     $Date: 2013/07/23 21:11:38 $
 *   $Author: reids $
 *    $State: Exp $
 *   $Locker:  $
 *
 * PROJECT:	NM-DS1
 *
 * FILE:	primFmttrs.java
 *
 * DESCRIPTION: JAVA class for marshalling and unmarshalling IPC byte arrays
 *              into JAVA objects..
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see ipc/LICENSE.TXT)
 *
 * $Log: primFmttrs.java,v $
 * Revision 1.6  2013/07/23 21:11:38  reids
 * Updated for using SWIG
 *
 * Revision 1.5  2011/08/16 16:04:05  reids
 * Updated Java version to support 64-bit machines
 *
 * Revision 1.4  2009/09/04 19:11:20  reids
 * IPC Java is now in its own package
 *
 * Revision 1.3  2009/02/07 18:54:45  reids
 * Updates for use on 64 bit machines
 *
 * Revision 1.2  2009/01/12 15:54:55  reids
 * Added BSD Open Source license info
 *
 * Revision 1.1  2002/01/02 17:40:17  reids
 * Initial (and more, or less, complete) release of Java version of IPC.
 *
 *
 *****************************************************************************/

package ipc.java;

import java.lang.reflect.*;

public class primFmttrs {

  public static final int INT_FMT       = 1;
  public static final int BOOLEAN_FMT   = 2;
  public static final int FLOAT_FMT     = 3;
  public static final int DOUBLE_FMT    = 4;
  public static final int BYTE_FMT      = 5;
  public static final int STR_FMT       = 7;
  public static final int UBYTE_FMT     = 9;
  public static final int CHAR_FMT      = 16;
  public static final int SHORT_FMT     = 17;
  public static final int LONG_FMT      = 18;
  public static final int USHORT_FMT    = 28;
  public static final int UINT_FMT      = 29;
  public static final int ULONG_FMT     = 30;
  public static final int ENUM_FMT      = 31;
  public static final int MAXFORMATTERS = 32;

  public static final int CHAR_SIZE   = 1;
  public static final int BYTE_SIZE   = 1;
  public static final int SHORT_SIZE  = 2;
  public static final int INT_SIZE    = 4;
  public static final int LONG_SIZE   = 4;
  public static final int FLOAT_SIZE  = 4;
  public static final int DOUBLE_SIZE = 8;
  public static final int PTR_SIZE    = 4;
  public static final int ENUM_SIZE   = INT_SIZE;

  private static Field getNthField (Object object, int n, Class theClass) 
    throws Exception {
    Field field = object.getClass().getFields()[n];
    if (theClass != null && field.getType() != theClass) {
      throw new Exception("Data structure does not match format"+
			  " -- Should be "+theClass+"; is "+field.getType());
    } else {
      return field;
    }
  }

  private static void setCharField (Object object, int n, char theChar)
    throws Exception {
    Field f = getNthField(object, n, char.class);
    f.setAccessible(true);
    f.setChar(object, theChar); 
  }

  private static char getCharField (Object object, int n) throws Exception {
    Field f = getNthField(object, n, char.class);
    return f.getChar(object); 
  }

  private static void setBooleanField (Object object, int n,
				       boolean theBoolean) throws Exception {
    Field f = getNthField(object, n, boolean.class);
    f.setAccessible(true);
    f.setBoolean(object, theBoolean); 
  }

  private static boolean getBooleanField (Object object,
					  int n) throws Exception {
    Field f = getNthField(object, n, boolean.class);
    return f.getBoolean(object); 
  }

  private static void setByteField (Object object, int n, byte theByte) 
    throws Exception {
    Field f = getNthField(object, n, byte.class);
    f.setAccessible(true);
    f.setByte(object, theByte); 
  }

  private static byte getByteField (Object object, int n) throws Exception {
    Field f = getNthField(object, n, byte.class);
    return f.getByte(object); 
  }

  private static void setIntField (Object object, int n, int theInt)
    throws Exception {
    Field f = getNthField(object, n, int.class);
    f.setAccessible(true);
    f.setInt(object, theInt); 
  }

  public static int getIntField (Object object, int n) throws Exception {
    Field f = getNthField(object, n, int.class);
    return f.getInt(object); 
  }

  private static void setShortField (Object object, int n, short theShort)
    throws Exception {
    Field f = getNthField(object, n, short.class);
    f.setAccessible(true);
    f.setShort(object, theShort); 
  }

  private static short getShortField (Object object, int n) throws Exception {
    Field f = getNthField(object, n, short.class);
    return f.getShort(object); 
  }

  private static void setLongField (Object object, int n, long theLong)
    throws Exception {
    Field f = getNthField(object, n, long.class);
    f.setAccessible(true);
    f.setLong(object, theLong); 
  }

  private static long getLongField (Object object, int n) throws Exception {
    Field f = getNthField(object, n, long.class);
    return f.getLong(object); 
  }

  private static void setFloatField (Object object, int n, float theFloat)
    throws Exception {
    Field f = getNthField(object, n, float.class);
    f.setAccessible(true);
    f.setFloat(object, theFloat); 
  }

  private static float getFloatField (Object object, int n) throws Exception {
    Field f = getNthField(object, n, float.class);
    return f.getFloat(object); 
  }

  private static void setDoubleField (Object object, int n, double theDouble)
    throws Exception {
    Field f = getNthField(object, n, double.class);
    f.setAccessible(true);
    f.setDouble(object, theDouble); 
  }

  private static double getDoubleField (Object object, int n) throws Exception{
    Field f = getNthField(object, n, double.class);
    return f.getDouble(object); 
  }

  private static void setStringField (Object object, int n, String theString)
    throws Exception {
    Field f = getNthField(object, n, String.class);
    f.setAccessible(true);
    f.set(object, theString); 
  }

  private static String getStringField (Object object, int n) throws Exception{
    Field f = getNthField(object, n, String.class);
    return (String)f.get(object); 
  }

  public static void setObjectField (Object object, int n, Object theObject)
    throws Exception {
    Field f = getNthField(object, n, null);
    if (Object.class.isAssignableFrom(f.getType())) {
	f.setAccessible(true);
      f.set(object, theObject); 
    } else {
      throw new Exception("Data structure does not match format"+
			  " -- Should be Object; is "+f.getType());
    }
  }

  public static Object getObjectField (Object object, int n) throws Exception{
    Field f = getNthField(object, n, null);
    if (Object.class.isAssignableFrom(f.getType()))
      return f.get(object); 
    else
      throw new Exception("Data structure does not match format"+
			  " -- Should be Object; is "+f.getType());
  }

  public static Object getPtrField (Object object, int n) throws Exception{
    Field f = getNthField(object, n, null);
    return f.get(object); 
  }

  private static class TransFormat {
    TransFormat () { typeSize = 0; }
    TransFormat (int theTypeSize) { typeSize = theTypeSize; }
    void Encode (Object dataStruct,
		 int dstart, BUFFER_TYPE buffer) throws Exception {}
    void Decode (Object dataStruct,
		 int dstart, BUFFER_TYPE buffer) throws Exception {}
    int ELength (Object dataStruct, int dstart) throws Exception {
	return typeSize;
    }
    int ALength () { return 1; }
    boolean SimpleType () { return true; }
    void EncodeElement (Object array, int index, 
			BUFFER_TYPE buffer) throws Exception {}
    void DecodeElement (Object array, int index,
			BUFFER_TYPE buffer) throws Exception {}

    private int typeSize;
  }

  private static class STR_Trans extends TransFormat {
    void Encode (Object dataStruct,
		 int dstart, BUFFER_TYPE buffer) throws Exception {
      IPC.formatPutString(buffer, (dataStruct == null ? ""
				   : getStringField(dataStruct, dstart)));
    }
    void Decode (Object dataStruct,
		 int dstart, BUFFER_TYPE buffer) throws Exception {
      setStringField(dataStruct, dstart, IPC.formatGetString(buffer));
    }
    int ELength (Object dataStruct, int dstart) throws Exception { 
	/* One int for the size, plus the number of characters 
	   (or 1 if null or the empty string) */
      int strlen = (dataStruct == null ? 0
		    : getStringField(dataStruct, dstart).length());
      if (strlen == 0) strlen = 1;
      return INT_SIZE + strlen;
    }
    boolean SimpleType () { return false; }
    void EncodeElement(int type, Object array, int index, BUFFER_TYPE buffer) {
      IPC.formatPutString(buffer, ((String[])array)[index]);
    }
    void DecodeElement(int type, Object array, int index, BUFFER_TYPE buffer) {
      ((String[])array)[index] = IPC.formatGetString(buffer);
    }
  }

  private static class BYTE_Trans extends TransFormat {
    BYTE_Trans() { super(BYTE_SIZE); }
    void Encode (Object dataStruct,
		 int dstart, BUFFER_TYPE buffer) throws Exception {
      IPC.formatPutByte(buffer, getByteField(dataStruct, dstart)); 
    }
    void Decode (Object dataStruct,
		 int dstart, BUFFER_TYPE buffer) throws Exception {
      setByteField(dataStruct, dstart, (byte)IPC.formatGetByte(buffer));
    }
    void EncodeElement(int type, Object array, int index, BUFFER_TYPE buffer) {
      IPC.formatPutByte(buffer, ((byte[])array)[index]);
    }
    void DecodeElement(int type, Object array, int index, BUFFER_TYPE buffer) {
      ((byte[])array)[index] = (byte)IPC.formatGetByte(buffer);
    }
  }

  private static class UBYTE_Trans extends TransFormat {
    UBYTE_Trans() { super(BYTE_SIZE); }
    void Encode (Object dataStruct,
		 int dstart, BUFFER_TYPE buffer) throws Exception {
      IPC.formatPutUByte(buffer, getByteField(dataStruct, dstart)); 
    }
    void Decode (Object dataStruct,
		 int dstart, BUFFER_TYPE buffer) throws Exception {
      setByteField(dataStruct, dstart, (byte)IPC.formatGetUByte(buffer));
    }
    void EncodeElement(int type, Object array, int index, BUFFER_TYPE buffer) {
      IPC.formatPutUByte(buffer, ((byte[])array)[index]);
    }
    void DecodeElement(int type, Object array, int index, BUFFER_TYPE buffer) {
      ((byte[])array)[index] = (byte)IPC.formatGetUByte(buffer);
    }
  }

  private static class SHORT_Trans extends TransFormat {
    SHORT_Trans() { super(SHORT_SIZE); }
    void Encode (Object dataStruct,
		 int dstart, BUFFER_TYPE buffer) throws Exception {
      IPC.formatPutShort(buffer, getShortField(dataStruct, dstart)); 
    }
    void Decode (Object dataStruct,
		 int dstart, BUFFER_TYPE buffer) throws Exception {
      setShortField(dataStruct, dstart, (short)IPC.formatGetShort(buffer));
    }
    void EncodeElement(int type, Object array, int index, BUFFER_TYPE buffer) {
      IPC.formatPutShort(buffer, ((short[])array)[index]);
    }
    void DecodeElement(int type, Object array, int index, BUFFER_TYPE buffer) {
      ((short[])array)[index] = (short)IPC.formatGetShort(buffer);
    }
  }

  private static class INT_Trans extends TransFormat {
    INT_Trans() { super(INT_SIZE); }
    void Encode (Object dataStruct,
		 int dstart, BUFFER_TYPE buffer) throws Exception {
      IPC.formatPutInt(buffer, getIntField(dataStruct, dstart)); 
    }
    void Decode (Object dataStruct,
		 int dstart, BUFFER_TYPE buffer) throws Exception {
      setIntField(dataStruct, dstart, IPC.formatGetInt(buffer));
    }
    void EncodeElement(Object array, int index, BUFFER_TYPE buffer) {
      IPC.formatPutInt(buffer, ((int[])array)[index]);
    }
    void DecodeElement(Object array, int index, BUFFER_TYPE buffer) {
      ((int[])array)[index] = IPC.formatGetInt(buffer);
    }
  }

  private static class CHAR_Trans extends TransFormat {
    CHAR_Trans() { super(CHAR_SIZE); }
    void Encode (Object dataStruct,
		 int dstart, BUFFER_TYPE buffer) throws Exception {
      IPC.formatPutChar(buffer, getCharField(dataStruct, dstart)); 
    }
    void Decode (Object dataStruct,
		 int dstart, BUFFER_TYPE buffer) throws Exception {
      setCharField(dataStruct, dstart, IPC.formatGetChar(buffer));
    }
    void EncodeElement(Object array, int index, BUFFER_TYPE buffer) {
      IPC.formatPutChar(buffer, ((char[])array)[index]);
    }
    void DecodeElement(Object array, int index, BUFFER_TYPE buffer) {
      ((char[])array)[index] = IPC.formatGetChar(buffer);
    }
  }

  private static class FLOAT_Trans extends TransFormat {
    FLOAT_Trans() { super(FLOAT_SIZE); }
    void Encode (Object dataStruct,
		 int dstart, BUFFER_TYPE buffer) throws Exception {
      IPC.formatPutFloat(buffer, getFloatField(dataStruct, dstart)); 
    }
    void Decode (Object dataStruct,
		 int dstart, BUFFER_TYPE buffer) throws Exception {
      setFloatField(dataStruct, dstart, (float)IPC.formatGetFloat(buffer));
    }
    void EncodeElement(Object array, int index, BUFFER_TYPE buffer) {
      IPC.formatPutFloat(buffer, ((float[])array)[index]);
    }
    void DecodeElement(Object array, int index, BUFFER_TYPE buffer) {
      ((float[])array)[index] = (float)IPC.formatGetFloat(buffer);
    }
  }

  private static class DOUBLE_Trans extends TransFormat {
    DOUBLE_Trans() { super(DOUBLE_SIZE); }
    void Encode (Object dataStruct,
		 int dstart, BUFFER_TYPE buffer) throws Exception {
      IPC.formatPutDouble(buffer, getDoubleField(dataStruct, dstart)); 
    }
    void Decode (Object dataStruct,
		 int dstart, BUFFER_TYPE buffer) throws Exception {
      setDoubleField(dataStruct, dstart, IPC.formatGetDouble(buffer));
    }
    void EncodeElement(Object array, int index, BUFFER_TYPE buffer) {
      IPC.formatPutDouble(buffer, ((double[])array)[index]);
    }
    void DecodeElement(Object array, int index, BUFFER_TYPE buffer) {
      ((double[])array)[index] = IPC.formatGetDouble(buffer);
    }
  }

  private static class BOOLEAN_Trans extends TransFormat {
    BOOLEAN_Trans() { super(INT_SIZE); }
    void Encode (Object dataStruct,
		 int dstart, BUFFER_TYPE buffer) throws Exception {
      IPC.formatPutInt(buffer, (getBooleanField(dataStruct, dstart) ? 1 : 0));
    }
    void Decode (Object dataStruct,
		 int dstart, BUFFER_TYPE buffer) throws Exception {
      setBooleanField(dataStruct, dstart, (IPC.formatGetInt(buffer) != 0));
    }
    boolean SimpleType () { return false; }
    void EncodeElement(Object array, int index, BUFFER_TYPE buffer) {
      IPC.formatPutInt(buffer, (((boolean[])array)[index] ? 1 : 0));
    }
    void DecodeElement(Object array, int index, BUFFER_TYPE buffer) {
      ((boolean[])array)[index] = (IPC.formatGetInt(buffer) != 0);
    }
  }

  private static class USHORT_Trans extends TransFormat {
    USHORT_Trans() { super(SHORT_SIZE); }
    void Encode (Object dataStruct,
		 int dstart, BUFFER_TYPE buffer) throws Exception {
      IPC.formatPutUShort(buffer, getShortField(dataStruct, dstart)); 
    }
    void Decode (Object dataStruct,
		 int dstart, BUFFER_TYPE buffer) throws Exception {
      setShortField(dataStruct, dstart, (short)IPC.formatGetUShort(buffer));
    }
    void EncodeElement(Object array, int index, BUFFER_TYPE buffer) {
      IPC.formatPutUShort(buffer, ((short[])array)[index]);
    }
    void DecodeElement(Object array, int index, BUFFER_TYPE buffer) {
      ((short[])array)[index] = (short)IPC.formatGetUShort(buffer);
    }
  }

  private static class UINT_Trans extends TransFormat {
    UINT_Trans() { super(INT_SIZE); }
    void Encode (Object dataStruct,
		 int dstart, BUFFER_TYPE buffer) throws Exception {
      IPC.formatPutUInt(buffer, getIntField(dataStruct, dstart)); 
    }
    void Decode (Object dataStruct,
		 int dstart, BUFFER_TYPE buffer) throws Exception {
      setIntField(dataStruct, dstart, (int)IPC.formatGetUInt(buffer));
    }
    void EncodeElement(Object array, int index, BUFFER_TYPE buffer) {
      IPC.formatPutUInt(buffer, ((int[])array)[index]);
    }
    void DecodeElement(Object array, int index, BUFFER_TYPE buffer) {
      ((int[])array)[index] = (int)IPC.formatGetUInt(buffer);
    }
  }

  private static class LONG_Trans extends TransFormat {
    LONG_Trans() { super(LONG_SIZE); }
    void Encode (Object dataStruct,
		 int dstart, BUFFER_TYPE buffer) throws Exception {
      long theLong = getLongField(dataStruct, dstart);
      if ((theLong > 0 && theLong > Integer.MAX_VALUE) ||
	  (theLong < 0 && theLong < Integer.MIN_VALUE)) {
	throw new Exception("Will lose precision in transferring long: "
			    + theLong);
      }
      //IPC.formatPutLong(buffer, theLong);
      IPC.formatPutInt(buffer, (int)theLong);
    }
    void Decode (Object dataStruct,
		 int dstart, BUFFER_TYPE buffer) throws Exception {
      //setLongField(dataStruct, dstart, IPC.formatGetLong(buffer));
      setLongField(dataStruct, dstart, IPC.formatGetInt(buffer));
    }
    void EncodeElement(Object array, int index, BUFFER_TYPE buffer)
      throws Exception {
      long theLong = ((long[])array)[index];
      if ((theLong > 0 && theLong > Integer.MAX_VALUE) ||
	  (theLong < 0 && theLong < Integer.MIN_VALUE)) {
	throw new Exception("Will lose precision in transferring long: "
			    + theLong);
      }      
      //IPC.formatPutLong(buffer, theLong);
      IPC.formatPutInt(buffer, (int)theLong);
    }
    void DecodeElement(Object array, int index, BUFFER_TYPE buffer) {
      //((long[])array)[index] = IPC.formatGetLong(buffer);
      ((long[])array)[index] = IPC.formatGetInt(buffer);
    }
  }

  private static class ENUM_Trans extends TransFormat {
    ENUM_Trans() { super(INT_SIZE); }
    void Encode (Object dataStruct,
		 int dstart, BUFFER_TYPE buffer) throws Exception {
      Field f = getNthField(dataStruct, dstart, null);
      if (Enum.class.isAssignableFrom(f.getType())) {
	IPC.formatPutInt(buffer, ((Enum)f.get(dataStruct)).ordinal());
      } else {
	throw new Exception("Data structure does not match format"+
			    " -- Should be enum; is "+f.getType());
      }
    }
    void Decode (Object dataStruct,
		 int dstart, BUFFER_TYPE buffer) throws Exception {
      Field f = getNthField(dataStruct, dstart, null);
      if (Enum.class.isAssignableFrom(f.getType())) {
	// Probably a cleaner way to do this, but I couldn't figure it out
	Method m = f.getType().getMethod("values", (Class[])null);
	int index = IPC.formatGetInt(buffer);
	Enum val = ((Enum [])m.invoke(f.get(dataStruct)))[index];
	f.setAccessible(true);
	f.set(dataStruct, val);
      } else {
	throw new Exception("Data structure does not match format"+
			    " -- Should be enum; is "+f.getType());
      }
    }
    void EncodeElement(Object array, int index, 
		       BUFFER_TYPE buffer) throws Exception {
      Object object = ((Object[])array)[index];
      Class oclass = object.getClass();
      if (Enum.class.isAssignableFrom(oclass)) {
	IPC.formatPutInt(buffer, ((Enum)object).ordinal());
      } else {
	throw new Exception("Data structure does not match format"+
			    " -- Should be enum; is "+oclass);
      }
    }
    void DecodeElement(Object array, int index, 
		       BUFFER_TYPE buffer) throws Exception {
      Class oclass = array.getClass().getComponentType();
      if (Enum.class.isAssignableFrom(oclass)) {
	// Probably a cleaner way to do this, but I couldn't figure it out
	Method m = oclass.getMethod("values", (Class[])null);
	int enumIndex = IPC.formatGetInt(buffer);
	Enum val = ((Enum [])m.invoke(((Enum[])array)[index]))[enumIndex];
	((Enum[])array)[index] = val;
      } else {
	throw new Exception("Data structure does not match format"+
			    " -- Should be enum; is "+oclass);
      }
    }
  }

  private static TransFormat strTrans = new STR_Trans();
  private static TransFormat byteTrans = new BYTE_Trans();
  private static TransFormat ubyteTrans = new UBYTE_Trans();
  private static TransFormat shortTrans = new SHORT_Trans();
  private static TransFormat intTrans = new INT_Trans();
  private static TransFormat charTrans = new CHAR_Trans();
  private static TransFormat floatTrans = new FLOAT_Trans();
  private static TransFormat doubleTrans = new DOUBLE_Trans();
  private static TransFormat booleanTrans = new BOOLEAN_Trans();
  private static TransFormat ushortTrans = new USHORT_Trans();
  private static TransFormat uintTrans = new UINT_Trans();
  private static TransFormat longTrans = new LONG_Trans();
  private static TransFormat enumTrans = new ENUM_Trans();

  private static TransFormat pickTrans (int type) throws Exception {
    switch (type) {
    case STR_FMT: return strTrans;
    case BYTE_FMT: return byteTrans;
    case UBYTE_FMT: return ubyteTrans;
    case SHORT_FMT: return shortTrans;
    case INT_FMT: return intTrans;
    case CHAR_FMT: return charTrans;
    case FLOAT_FMT: return floatTrans;
    case DOUBLE_FMT: return doubleTrans;
    case BOOLEAN_FMT: return booleanTrans;
    case USHORT_FMT: return ushortTrans;
    case UINT_FMT: return uintTrans;
    case LONG_FMT: 
    case ULONG_FMT: return longTrans;
    case ENUM_FMT: return enumTrans;
    default: throw new Exception("Unhandled format "+ type);
    }
  }

  public static void Encode(int type, Object dataStruct, int dstart,
			    BUFFER_TYPE buffer) throws Exception {
    pickTrans(type).Encode(dataStruct, dstart, buffer);
  }

  public static void Decode(int type, Object dataStruct, int dstart, 
			    BUFFER_TYPE buffer) throws Exception {
    pickTrans(type).Decode(dataStruct, dstart, buffer);
  }

  public static int ELength(int type, Object dataStruct,
			    int dstart) throws Exception{
    return pickTrans(type).ELength(dataStruct, dstart);
  }

  public static int ALength(int type) throws Exception { 
    return pickTrans(type).ALength();
  }

  public static boolean SimpleType(int type) throws Exception {
   return pickTrans(type).SimpleType();
  }

  public static void EncodeElement(int type, Object array, int index,
				   BUFFER_TYPE buffer) throws Exception {
    pickTrans(type).EncodeElement(array, index, buffer);
  }

  public static void DecodeElement(int type, Object array, int index,
				   BUFFER_TYPE buffer) throws Exception {
    pickTrans(type).DecodeElement(array, index, buffer);
  }
}
