#*****************************************************************************
#        $Id: primFmttrs.py,v 1.4 2013/07/24 03:20:23 reids Exp $
# $Revision: 1.4 $
#     $Date: 2013/07/24 03:20:23 $
#   $Author: reids $
#    $State: Exp $
#   $Locker:  $
#
# PROJECT:	NM-DS1
#
# FILE:	primFmttrs.py
#
# DESCRIPTION: Classes for marshalling and unmarshalling IPC byte arrays
#              into Python objects..
#
# $Log: primFmttrs.py,v $
# Revision 1.4  2013/07/24 03:20:23  reids
# Updated size of LONG format
#
# Revision 1.3  2013/07/23 21:12:52  reids
# Made consistent with other language ports
#
# Revision 1.2  2012/02/27 16:55:46  reids
# Fixed some problems with python and significantly improved transfer of arrays to/from python
#
# Revision 1.1  2011/08/16 16:00:37  reids
# Adding Python interface to IPC
#
#
#****************************************************************************/

import _IPC # C functions
import sys
from IPC import Raise

INT_FMT       = 1
BOOLEAN_FMT   = 2
FLOAT_FMT     = 3
DOUBLE_FMT    = 4
BYTE_FMT      = 5
STR_FMT       = 7
UBYTE_FMT     = 9
CHAR_FMT      = 16
SHORT_FMT     = 17
LONG_FMT      = 18
USHORT_FMT    = 28
UINT_FMT      = 29
ULONG_FMT     = 30
ENUM_FMT      = 31
MAXFORMATTERS = 32

CHAR_SIZE   = 1
BYTE_SIZE   = 1
SHORT_SIZE  = 2
INT_SIZE    = 4
LONG_SIZE   = 8
FLOAT_SIZE  = 4
DOUBLE_SIZE = 8
PTR_SIZE    = 4
ENUM_SIZE   = INT_SIZE

def getNthFieldName (object, n) :
  try :
    fieldName = object._fields[n]
    if (isinstance(fieldName, (tuple, list))) :
      return fieldName[0]
    else :
      return fieldName
  except (AttributeError, IndexError) :
    return "_f%d" % n

def findClass (className, parent=None) :
  split = className.split('.')
  slen = len(split)
  try :
    if (slen > 1) :
      module = sys.modules[split[slen-2]]
    elif (not parent is None) :
      module = sys.modules[parent.__module__]
    else :
      module = sys.modules['__main__']
    return module.__dict__[split[slen-1]]
  except KeyError :
    Raise('%s not a valid class name' % className)

def getNthFieldClass (object, n) :
  try :
    fieldName = object._fields[n]
    if (isinstance(fieldName, (tuple, list))) :
      fieldName = fieldName[1]
      if (isinstance(fieldName, (tuple, list))) :
        fieldName = fieldName[0]
      if (isinstance(fieldName, str)) :
        fieldName = findClass(fieldName, object)
      return fieldName
    else :
      return None
  except (AttributeError, IndexError) :
    return None

def getField (object, n, theClass=None) :
  fieldName = getNthFieldName(object, n)
  try :
     field = object.__dict__[fieldName]
     if (theClass is None or isinstance(field, theClass)) :
       return field
     else :
       Raise("getField: %r not of class %s" % (field, theClass.__name__))
  except KeyError:
    return None

def setField (object, n, theValue) :
  fieldName = getNthFieldName(object, n);
  object.__dict__[fieldName] = theValue

def setCharField (object, n, theChar) :
  if (isinstance(theChar, str) and len(theChar) == 1) :
    return setField(object, n, theChar)
  else :
    Raise("%s is not a single character" % theChar)

def getCharField (object, n) :
  theChar = getField(object, n, str)
  if (not theChar is None and len(theChar) == 1) :
    return theChar
  else:
    Raise("%s (field %s of %s) is not a single character" % \
          (theChar, getNthFieldName(object, n), object))

def setBooleanField (object, n, theBoolean) :
  if (theBoolean in (0, 1)) :
    if (theBoolean == 1) : return setField(object, n, True)
    else : return setField(object, n, False)
  else :
    Raise("%s is not Boolean" % theBoolean)

def getBooleanField (object, n) :
  theBoolean = getField(object, n, bool)
  if (isinstance(theBoolean, bool)) :
    if (theBoolean == True) : return 1
    else : return 0
  else:
    Raise("%s (field %s of %s) is not Boolean" % \
          (theBoolean, getNthFieldName(object, n), object))

def setByteField (object, n, theByte) :
  if (isinstance(theByte, int) and abs(theByte) <= 0XFF) :
    return setField(object, n, theByte)
  else :
    Raise("%s is not a byte" % theByte)

def getByteField (object, n) :
  theByte = getField(object, n, int)
  if (not theByte is None and abs(theByte) <= 0XFF) :
    return theByte
  else:
    Raise("%s (field %s of %s) is not a byte" % \
          (theByte, getNthFieldName(object, n), object))

def setIntField (object, n, theInt) :
  if (isinstance(theInt, int) and abs(theInt) <= 0XFFFFFFFF) :
    return setField(object, n, theInt)
  else :
    Raise("%s is not a int" % theInt)

def getIntField (object, n) :
  theInt = getField(object, n, int)
  if (not theInt is None and abs(theInt) <= 0XFFFFFF) :
    return theInt
  else:
    Raise("%s (field %s of %s) is not an int" % \
          (theInt, getNthFieldName(object, n), object))

def setShortField (object, n, theShort) :
  if (isinstance(theShort, int) and abs(theShort) <= 0XFFFF) :
    return setField(object, n, theShort)
  else :
    Raise("%s is not a short" % theShort)

def getShortField (object, n) :
  theShort = getField(object, n, int)
  if (not theShort is None and abs(theShort) <= 0XFFFF) :
    return theShort
  else:
    Raise("%s (field %s of %s) is not a short" % \
          (theShort, getNthFieldName(object, n), object))

def setLongField (object, n, theLong) :
  if (isinstance(theLong, int)) :
    return setField(object, n, theLong)
  else :
    Raise("%s is not a long" % theLong)

def getLongField (object, n) :
  theLong = getField(object, n, int)
  if (not theLong is None) :
    return theLong
  else:
    Raise("%s (field %s of %s) is not a long" % \
          (theLong, getNthFieldName(object, n), object))

def setFloatField (object, n, theFloat) :
  if (isinstance(theFloat, float)) :
    return setField(object, n, theFloat)
  else :
    Raise("%s is not a float" % theFloat)

def getFloatField (object, n) :
  theFloat = getField(object, n, float)
  if (not theFloat is None) :
    return theFloat
  else:
    Raise("%s (field %s of %s) is not a float" % \
          (theFloat, getNthFieldName(object, n), object))

def setDoubleField (object, n, theDouble) :
  if (isinstance(theDouble, float)) :
    return setField(object, n, theDouble)
  else :
    Raise("%s is not a double" % theDouble)

def getDoubleField (object, n) :
  theDouble = getField(object, n, float)
  if (not theDouble is None) :
    return theDouble
  else:
    Raise("%s (field %s of %s) is not a double" % \
          (theDouble, getNthFieldName(object, n), object))

def setStringField (object, n, theString) :
  if (isinstance(theString, str)) :
    return setField(object, n, theString)
  else :
    Raise("%s is not a string" % theString)

def getStringField (object, n) :
  theString = getField(object, n, str)
  if (not theString is None) :
    return theString
  else:
    Raise("%s (field %s of %s) is not a string" % \
          (theString, getNthFieldName(object, n), object))

def setObjectField (object, n, theObject) :
  return setField(object, n, theObject)

def getObjectField (object, n) :
  return getField(object, n)

class TransFormat :
  def TransFormat (self, theTypeSize=0) : self.typeSize = theTypeSize
  def Encode (self, dataStruct, dstart, buffer) : pass
  def Decode (self, dataStruct, dstart, buffer) : pass
  def ELength (self, dataStruct, dstart) : return self.typeSize
  def ALength (self) : return 1
  def SimpleType (self) : return True
  def PrimType (self) : return self.primType
  def EncodeElement (self, array, index, buffer) : pass
  def DecodeElement (self, array, index, buffer) : pass
  def EncodeArray (self, array, len, buffer) : pass
  def DecodeArray (self, array, len, buffer) : pass

class STR_Trans(TransFormat) :
  def __init__(self) : self.primType = str

  def Encode (self, dataStruct, dstart, buffer) :
    if (dataStruct == None) : str = ""
    else : str = getStringField(dataStruct, dstart)
    _IPC.formatPutString(buffer, str)

  def Decode (self, dataStruct, dstart, buffer) :
    setStringField(dataStruct, dstart, _IPC.formatGetString(buffer))

  def ELength (self, dataStruct, dstart) :
    # One int for the size, plus the number of characters (or 1 if empty string)
    if (isinstance(dataStruct, str)) : strlen = len(dataStruct)
    elif (dataStruct == None) : strlen = 0
    else : strlen = len(getStringField(dataStruct, dstart))
    if (strlen == 0) : strlen = 1
    return INT_SIZE + strlen;

  def SimpleType (self) : return False

  def EncodeElement(self, array, index, buffer) :
    _IPC.formatPutString(buffer, array[index])

  def DecodeElement(self, array, index, buffer) :
    array[index] = _IPC.formatGetString(buffer)

class BYTE_Trans(TransFormat) :
  def __init__(self) : self.typeSize = BYTE_SIZE; self.primType = int

  def Encode (self, dataStruct, dstart, buffer) :
    _IPC.formatPutByte(buffer, getByteField(dataStruct, dstart))

  def Decode (self, dataStruct, dstart, buffer) :
    setByteField(dataStruct, dstart, _IPC.formatGetByte(buffer))

  def EncodeElement(self, array, index, buffer) : 
    _IPC.formatPutByte(buffer, array[index])

  def DecodeElement(self, array, index, buffer) :
    array[index] = _IPC.formatGetByte(buffer)

  def EncodeArray(self, array, len, buffer) :
    _IPC.encodeByteArray(array, len, buffer)

  def DecodeArray(self, array, len, buffer) :
    _IPC.decodeByteArray(array, len, buffer)

class UBYTE_Trans(TransFormat) :
  def __init__(self) : self.typeSize = BYTE_SIZE; self.primType = int

  def Encode (self, dataStruct, dstart, buffer) :
    _IPC.formatPutUByte(buffer, getByteField(dataStruct, dstart))

  def Decode (self, dataStruct, dstart, buffer) :
    setByteField(dataStruct, dstart, _IPC.formatGetUByte(buffer))

  def EncodeElement(self, array, index, buffer) : 
    _IPC.formatPutUByte(buffer, array[index])

  def DecodeElement(self, array, index, buffer) :
    array[index] = _IPC.formatGetUByte(buffer)

  def EncodeArray(self, array, len, buffer) :
    _IPC.encodeUByteArray(array, len, buffer)

  def DecodeArray(self, array, len, buffer) :
    _IPC.decodeUByteArray(array, len, buffer)

class SHORT_Trans(TransFormat) :
  def __init__(self) : self.typeSize = SHORT_SIZE; self.primType = int

  def Encode (self, dataStruct, dstart, buffer) :
    _IPC.formatPutShort(buffer, getShortField(dataStruct, dstart))

  def Decode (self, dataStruct, dstart, buffer) :
    setShortField(dataStruct, dstart, _IPC.formatGetShort(buffer))

  def EncodeElement(self, array, index, buffer) : 
    _IPC.formatPutShort(buffer, array[index])

  def DecodeElement(self, array, index, buffer) :
    array[index] = _IPC.formatGetShort(buffer)

  def EncodeArray(self, array, len, buffer) :
    _IPC.encodeShortArray(array, len, buffer)

  def DecodeArray(self, array, len, buffer) :
    _IPC.decodeShortArray(array, len, buffer)

class INT_Trans(TransFormat) :
  def __init__(self) : self.typeSize = INT_SIZE; self.primType = int

  def Encode (self, dataStruct, dstart, buffer) :
    _IPC.formatPutInt(buffer, getIntField(dataStruct, dstart))

  def Decode (self, dataStruct, dstart, buffer) :
    setIntField(dataStruct, dstart, _IPC.formatGetInt(buffer))

  def EncodeElement(self, array, index, buffer) : 
    _IPC.formatPutInt(buffer, array[index])

  def DecodeElement(self, array, index, buffer) :
    array[index] = _IPC.formatGetInt(buffer)

  def EncodeArray(self, array, len, buffer) :
    _IPC.encodeIntArray(array, len, buffer)

  def DecodeArray(self, array, len, buffer) :
    _IPC.decodeIntArray(array, len, buffer)

class CHAR_Trans(TransFormat) :
  def __init__(self) : self.typeSize = CHAR_SIZE; self.primType = str

  def Encode (self, dataStruct, dstart, buffer) :
    _IPC.formatPutChar(buffer, getCharField(dataStruct, dstart))

  def Decode (self, dataStruct, dstart, buffer) :
    setCharField(dataStruct, dstart, _IPC.formatGetChar(buffer))

  def EncodeElement(self, array, index, buffer) : 
    _IPC.formatPutChar(buffer, array[index])

  def DecodeElement(self, array, index, buffer) :
    array[index] = _IPC.formatGetChar(buffer)

  def EncodeArray(self, array, len, buffer) :
    _IPC.encodeCharArray(array, len, buffer)

  def DecodeArray(self, array, len, buffer) :
    _IPC.decodeCharArray(array, len, buffer)

class FLOAT_Trans(TransFormat) :
  def __init__(self) : self.typeSize = FLOAT_SIZE; self.primType = float

  def Encode (self, dataStruct, dstart, buffer) :
    _IPC.formatPutFloat(buffer, getFloatField(dataStruct, dstart))

  def Decode (self, dataStruct, dstart, buffer) :
    setFloatField(dataStruct, dstart, _IPC.formatGetFloat(buffer))

  def EncodeElement(self, array, index, buffer) : 
    _IPC.formatPutFloat(buffer, array[index])

  def DecodeElement(self, array, index, buffer) :
    array[index] = _IPC.formatGetFloat(buffer)

  def EncodeArray(self, array, len, buffer) :
    _IPC.encodeFloatArray(array, len, buffer)

  def DecodeArray(self, array, len, buffer) :
    _IPC.decodeFloatArray(array, len, buffer)

class DOUBLE_Trans(TransFormat) :
  def __init__(self) : self.typeSize = DOUBLE_SIZE; self.primType = float

  def Encode (self, dataStruct, dstart, buffer) :
    _IPC.formatPutDouble(buffer, getDoubleField(dataStruct, dstart))

  def Decode (self, dataStruct, dstart, buffer) :
    setDoubleField(dataStruct, dstart, _IPC.formatGetDouble(buffer))

  def EncodeElement(self, array, index, buffer) : 
    _IPC.formatPutDouble(buffer, array[index])

  def DecodeElement(self, array, index, buffer) :
    array[index] = _IPC.formatGetDouble(buffer)

  def EncodeArray(self, array, len, buffer) :
    _IPC.encodeDoubleArray(array, len, buffer)

  def DecodeArray(self, array, len, buffer) :
    _IPC.decodeDoubleArray(array, len, buffer)

class BOOLEAN_Trans(TransFormat) :
  def __init__(self) : self.typeSize = INT_SIZE; self.primType = int

  def Encode (self, dataStruct, dstart, buffer) :
    _IPC.formatPutInt(buffer, getBooleanField(dataStruct, dstart))

  def Decode (self, dataStruct, dstart, buffer) :
    setBooleanField(dataStruct, dstart, _IPC.formatGetInt(buffer))

  def SimpleType (self) : return False

  def EncodeElement(self, array, index, buffer) : 
    _IPC.formatPutBoolean(buffer, array[index])

  def DecodeElement(self, array, index, buffer) :
    array[index] = _IPC.formatGetBoolean(buffer)

  def EncodeArray(self, array, len, buffer) :
    _IPC.encodeBooleanArray(array, len, buffer)

  def DecodeArray(self, array, len, buffer) :
    _IPC.decodeBooleanArray(array, len, buffer)

class USHORT_Trans(TransFormat) :
  def __init__(self) : self.typeSize = SHORT_SIZE; self.primType = int

  def Encode (self, dataStruct, dstart, buffer) :
    _IPC.formatPutUShort(buffer, getShortField(dataStruct, dstart))

  def Decode (self, dataStruct, dstart, buffer) :
    setShortField(dataStruct, dstart, _IPC.formatGetUShort(buffer))

  def EncodeElement(self, array, index, buffer) : 
    _IPC.formatPutUShort(buffer, array[index])

  def DecodeElement(self, array, index, buffer) :
    array[index] = _IPC.formatGetUShort(buffer)

  def EncodeArray(self, array, len, buffer) :
    _IPC.encodeUShortArray(array, len, buffer)

  def DecodeArray(self, array, len, buffer) :
    _IPC.decodeUShortArray(array, len, buffer)

class UINT_Trans(TransFormat) :
  def __init__(self) : self.typeSize = INT_SIZE; self.primType = int

  def Encode (self, dataStruct, dstart, buffer) :
    _IPC.formatPutUInt(buffer, getIntField(dataStruct, dstart))

  def Decode (self, dataStruct, dstart, buffer) :
    setIntField(dataStruct, dstart, _IPC.formatGetUInt(buffer))

  def EncodeElement(self, array, index, buffer) : 
    _IPC.formatPutUInt(buffer, array[index])

  def DecodeElement(self, array, index, buffer) :
    array[index] = _IPC.formatGetUInt(buffer)

  def EncodeArray(self, array, len, buffer) :
    _IPC.encodeUIntArray(array, len, buffer)

  def DecodeArray(self, array, len, buffer) :
    _IPC.decodeUIntArray(array, len, buffer)

class LONG_Trans(TransFormat) :
  def __init__(self) : self.typeSize = LONG_SIZE; self.primType = long

  def Encode (self, dataStruct, dstart, buffer) :
    theLong = getLongField(dataStruct, dstart)
    if (-0X7FFFFFFF <= theLong <= 0X7FFFFFFF) :
      _IPC.formatPutInt(buffer, theLong)
    # 8 byte longs not implemented, yet
    #elif (0 <= theLong < 0XFFFFFFFFFFFFFFFF) :
    #  _IPC.formatPutLong(buffer, theLong)
    else :
      Raise("Will lose precision in transferring long: %d" % theLong)

  def Decode (self, dataStruct, dstart, buffer) :
#    setLongField(dataStruct, dstart, _IPC.formatGetLong(buffer))
    setLongField(dataStruct, dstart, _IPC.formatGetInt(buffer))

  def EncodeElement(self, array, index, buffer) : 
    theLong = array[index]
    if (-0X7FFFFFFF <= theLong <= 0X7FFFFFFF) :
      _IPC.formatPutInt(buffer, theLong)
    # 8 byte longs not implemented, yet
    #elif (0 <= theLong < 0XFFFFFFFFFFFFFFFF) :
    #  _IPC.formatPutLong(buffer, theLong)
    else :
      Raise("Will lose precision in transferring long: %d" % theLong)

  def DecodeElement(self, array, index, buffer) :
#    array[index] = _IPC.formatGetLong(buffer)
    array[index] = _IPC.formatGetInt(buffer)

  def EncodeArray(self, array, len, buffer) :
    _IPC.encodeLongArray(array, len, buffer)

  def DecodeArray(self, array, len, buffer) :
    _IPC.decodeLongArray(array, len, buffer)

class ENUM_Trans (TransFormat) :
  def __init__(self) : self.typeSize = ENUM_SIZE; self.primType = int

  def Encode (self, dataStruct, dstart, buffer) :
    _IPC.formatPutInt(buffer, getIntField(dataStruct, dstart))

  def Decode (self, dataStruct, dstart, buffer) :
    setIntField(dataStruct, dstart, _IPC.formatGetInt(buffer))

  def EncodeElement(self, array, index, buffer) : 
    _IPC.formatPutInt(buffer, array[index])

  def DecodeElement(self, array, index, buffer) :
    array[index] = _IPC.formatGetInt(buffer)

  def EncodeArray(self, array, len, buffer) :
    _IPC.encodeIntArray(array, len, buffer)

  def DecodeArray(self, array, len, buffer) :
    _IPC.decodeIntArray(array, len, buffer)

TransFormatArray = [None]*MAXFORMATTERS
TransFormatArray[STR_FMT]     = STR_Trans()
TransFormatArray[BYTE_FMT]    = BYTE_Trans()
TransFormatArray[UBYTE_FMT]   = UBYTE_Trans()
TransFormatArray[SHORT_FMT]   = SHORT_Trans()
TransFormatArray[INT_FMT]     = INT_Trans()
TransFormatArray[CHAR_FMT]    = CHAR_Trans()
TransFormatArray[FLOAT_FMT]   = FLOAT_Trans()
TransFormatArray[DOUBLE_FMT]  = DOUBLE_Trans()
TransFormatArray[BOOLEAN_FMT] = BOOLEAN_Trans()
TransFormatArray[USHORT_FMT]  = USHORT_Trans()
TransFormatArray[UINT_FMT]    = UINT_Trans()
TransFormatArray[LONG_FMT]    = LONG_Trans()
TransFormatArray[ENUM_FMT]    = ENUM_Trans()

def pickTrans (type) :
  if (0 <= type < MAXFORMATTERS) :
    fn = TransFormatArray[type];
    if (not fn is None) :
      return fn
  Raise("pickTrans: Unhandled format %s" % type)


def Encode(type, dataStruct, dstart, buffer) :
  pickTrans(type).Encode(dataStruct, dstart, buffer)

def Decode(type, dataStruct, dstart, buffer) :
  pickTrans(type).Decode(dataStruct, dstart, buffer)

def ELength(type, dataStruct, dstart) :
  return pickTrans(type).ELength(dataStruct, dstart)

def ALength(type) :
  return pickTrans(type).ALength()

def SimpleType(type) :
  return pickTrans(type).SimpleType()

def EncodeElement(type, array, index, buffer) :
  pickTrans(type).EncodeElement(array, index, buffer)

def DecodeElement(type, array, index, buffer) :
  pickTrans(type).DecodeElement(array, index, buffer)

def PrimType(type) :
  return pickTrans(type).PrimType()

def EncodeArray(type, array, len, buffer) :
  pickTrans(type).EncodeArray(array, len, buffer)

def DecodeArray(type, array, len, buffer) :
  pickTrans(type).DecodeArray(array, len, buffer)
