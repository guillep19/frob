#############################################################################
#       $Id: formatters.py,v 1.3 2013/07/23 21:12:52 reids Exp $
# $Revision: 1.3 $
#     $Date: 2013/07/23 21:12:52 $
#   $Author: reids $
#    $State: Exp $
#   $Locker:  $
#
# PROJECT:	IPC
#
# FILE:	formatters.py
#
# DESCRIPTION: Functions for marshalling and unmarshalling IPC byte arrays
#              into Python objects..
#
# Copyright (c) 2008, Carnegie Mellon University
#     This software is distributed under the terms of the 
#     Simplified BSD License (see ipc/LICENSE.TXT)
#
# $Log: formatters.py,v $
# Revision 1.3  2013/07/23 21:12:52  reids
# Made consistent with other language ports
#
# Revision 1.2  2012/02/27 16:55:46  reids
# Fixed some problems with python and significantly improved transfer of arrays to/from python
#
# Revision 1.1  2011/08/16 16:00:36  reids
# Adding Python interface to IPC
#
#############################################################################

import primFmttrs
import _IPC
from _IPC import *
from types import InstanceType
import IPC # to get Raise, for some reason from IPC import Raise doesn't work

PrimitiveFMT  = 0
LengthFMT     = 1
StructFMT     = 2
PointerFMT    = 3
FixedArrayFMT = 4
VarArrayFMT   = 5
BadFormatFMT  = 6
NamedFMT      = 7
EnumFMT       = 8

# Generic data structure for IPC data (if class is not specified explicitly)
class IPCdata :
  def __repr__(self) :
    _repr = "<%s" % self.__class__.__name__
    sepr = ":"
    if (hasattr(self, "_fields")) :
      for f in self._fields :
        if (isinstance(f, (tuple, list))) : f = f[0]
        try:
          _repr = _repr + "%s %s: %s" % (sepr, f, repr(getattr(self, f)))
          sepr = ";"
        except: pass
    else :
      n = 0
      try :
        while True :
          _repr = _repr + "%s %s" % (sepr, repr(getattr(self, "_f%d" % n)))
          n = n + 1; sepr = ';'
      except : pass
    return _repr + ">"

def isSimpleType (format) :
  return (formatType(format) == PrimitiveFMT and
          primFmttrs.SimpleType(formatPrimitiveProc(format)));

def fixedArrayNumElements (formatArray) :
  n = formatFormatArrayMax(formatArray)
  numElements = 1
  for i in range(2, n) :
    numElements = numElements * formatFormatArrayItemInt(formatArray, i);
  return numElements;

def varArrayDimSize (dim, formatArray, dataStruct) :
  sizePlace = formatFormatArrayItemInt(formatArray, dim);
  size = primFmttrs.getIntField(dataStruct, sizePlace-1);
  return size;

def varArrayNumElements (formatArray, dataStruct) :
  n = formatFormatArrayMax(formatArray)
  numElements = 1
  for i in range(2, n) :
    numElements = numElements * varArrayDimSize(i, formatArray, dataStruct);
  return numElements;

# dataStruct is non-null if this is a variable length array
def arrayBufferSize (array, dim, max, formatArray, arrayFormat, dataStruct) :
  bufSize = 0
  if (dataStruct is None) :
    len = formatFormatArrayItemInt(formatArray, dim)
  else :
    len = varArrayDimSize(dim, formatArray, dataStruct)
  for i in range(len) :
    if (dim == max) :
      bufSize = bufSize + bufferSize1(arrayFormat, array[i], 0, 0, True)
    else :
      bufSize = bufSize + arrayBufferSize(array[i], dim+1, max, formatArray,
                                          arrayFormat, dataStruct)
  return bufSize;

# dataStruct is the parent structure
def arrayTransferToBuffer (array, buffer, dim, max, isSimple, 
                           formatArray, arrayFormat, dataStruct, isVarArray) :
  if (isVarArray) :
    len = varArrayDimSize(dim, formatArray, dataStruct)
  else :
    len = formatFormatArrayItemInt(formatArray, dim)

  if (isSimple and dim == max) :
    primFmttrs.EncodeArray(formatPrimitiveProc(arrayFormat), array, len, buffer)
    return
  if (isSimple) : primProc = primFmttrs.pickTrans(formatPrimitiveProc(arrayFormat))
  i = 0
  while (i < len) :
    if (dim != max) :
      arrayTransferToBuffer(array[i], buffer, dim+1, max, isSimple,
                            formatArray, arrayFormat, dataStruct, isVarArray)
    elif (isSimple) :
      primProc.EncodeElement(array, i, buffer)
    elif (formatType(arrayFormat) == EnumFMT) :
      primFmttrs.EncodeElement(primFmttrs.ENUM_FMT, array, i, buffer);
    else :
      transferToBuffer(arrayFormat, array[i], 0, buffer, 0, True)
    i = i + 1

# dataStruct is the parent structure
def arrayTransferToDataStructure (array, buffer, dim, max, len, isSimple,
                                  formatArray, arrayFormat,
                                  dataStruct, dStart, isVarArray, oclass) :
  nextLen=0
  if (dim != max) :
    if (isVarArray) :
      nextLen = varArrayDimSize(dim+1, formatArray, dataStruct)
    else :
      nextLen = formatFormatArrayItemInt(formatArray, dim+1)

  if (isSimple and dim == max) :
    primFmttrs.DecodeArray(formatPrimitiveProc(arrayFormat), array, len, buffer)
    return
  if (isSimple) : primProc = primFmttrs.pickTrans(formatPrimitiveProc(arrayFormat))
  i = 0
  while (i < len) :
    if (dim != max) :
      array[i] = validateArrayObject(array[i], nextLen, array, -1)
      arrayTransferToDataStructure(array[i], buffer, dim+1, max, nextLen,
                                   isSimple, formatArray, arrayFormat,
                                   dataStruct, dStart, isVarArray, oclass)
    elif (isSimple) :
      primProc.DecodeElement(array, i, buffer)
    elif (formatType(arrayFormat) == EnumFMT) :
	primFmttrs.DecodeElement(primFmttrs.ENUM_FMT, array, i, buffer);
    else :
      array[i] = validateObject(array[i], dataStruct, dStart, oclass)
      transferToDataStructure(arrayFormat, array[i], 0, buffer, 0, True)
    i = i + 1

def validateObject (object, parentObject, index, oclass=None) :
  if (object is None) :
    if (oclass is None) :
      oclass = primFmttrs.getNthFieldClass(parentObject, index)
      if (oclass is None) : oclass = IPCdata
    if (isinstance(oclass, (tuple, list))) : oclass = oclass[0]
    object = oclass()

  return object

def invalidStructFormat(oclass, dclass) :
  if (oclass != None) : oclass = oclass.__name__;
  if (dclass != None) : dclass = dclass.__name__;
  return "ERROR: Data structure does not match format -- Should be %s; is %s"%\
         (dclass, oclass)

def invalidArrayFormat(object, index) :
  return "ERROR: Data structure %s does not match format -- field %s needs to be an array" % \
         (object, primFmttrs.getNthFieldName(object, index))

def validateArrayObject (arrayObject, dim, object, index) :
  if (not (arrayObject is None or isinstance(arrayObject, (list, tuple)))) :
    IPC.Raise(invalidArrayFormat(object, index))
  elif (arrayObject is None or len(arrayObject) != dim) :
    arrayObject = [None]*dim;
  return arrayObject;

def feasibleToDecodeVarArray (size, formatArray, dStart) :
  max = _IPC.formatFormatArrayMax(formatArray)
  if (max > 3) : # Number of dimensions is max-2
    for i in range(2,max) :
      if (_IPC.formatFormatArrayItemInt(formatArray, i) > dStart) :
        return False
  return True

def bufferSize1 (format, dataStruct, dStart, parentFormat, isTopLevelStruct) :
  bufSize = 0
  ftype = _IPC.formatType(format)
  
  if (ftype == LengthFMT) :
    IPC.Raise("Python version of IPC can only use explicit formats")
  elif (ftype == PrimitiveFMT) :
    primType = _IPC.formatPrimitiveProc(format)
    bufSize = bufSize + primFmttrs.ELength(primType, dataStruct, dStart)
  elif (ftype == PointerFMT) :
    bufSize = bufSize + 1
    if (not primFmttrs.getObjectField(dataStruct, dStart) is None) :
      bufSize = bufSize + \
                bufferSize1(_IPC.formatChoosePtrFormat(format, parentFormat),
                            dataStruct, dStart, 0, isTopLevelStruct)
  elif (ftype == StructFMT) :
    formatArray = _IPC.formatFormatArray(format)
    structStart = 0
    n = _IPC.formatFormatArrayMax(formatArray);
    if (isTopLevelStruct) :
      struct = dataStruct
    else :
      struct = primFmttrs.getObjectField(dataStruct, dStart)
    for i in range(1, n) :
      bufSize = bufSize + \
                bufferSize1(_IPC.formatFormatArrayItemPtr(formatArray, i),
                            struct, structStart, format, False)
      structStart = structStart + 1
  elif (ftype == FixedArrayFMT) :
    formatArray = _IPC.formatFormatArray(format)
    nextFormat = _IPC.formatFormatArrayItemPtr(formatArray, 1)
    if (isTopLevelStruct) :
      arrayObject = dataStruct
    else :
      arrayObject = primFmttrs.getObjectField(dataStruct, dStart)
    if (not isinstance(arrayObject, (tuple, list))) :
      IPC.Raise(invalidArrayFormat(dataStruct, dStart))
    elif (isSimpleType(nextFormat)) :
      bufSize = bufSize + (bufferSize1(nextFormat, arrayObject, 0, 0, False) *
                           fixedArrayNumElements(formatArray))
    else :
      bufSize = bufSize + \
                arrayBufferSize(arrayObject, 2, 
                                _IPC.formatFormatArrayMax(formatArray)-1,
                                formatArray, nextFormat, None)
  elif (ftype == VarArrayFMT) :
    formatArray = _IPC.formatFormatArray(format)
    nextFormat = _IPC.formatFormatArrayItemPtr(formatArray, 1)
    if (isTopLevelStruct) :
      arrayObject = dataStruct
    else :
      arrayObject = primFmttrs.getObjectField(dataStruct, dStart)
    # For the size of the array
    bufSize = bufSize + primFmttrs.ELength(primFmttrs.INT_FMT, None, 0)
    if (not isinstance(arrayObject, (tuple, list))) :
      IPC.Raise(invalidArrayFormat(dataStruct, dStart))
    elif (isSimpleType(nextFormat)) :
      bufSize = bufSize + (bufferSize1(nextFormat, arrayObject, 0, 0, False) *
                           varArrayNumElements(formatArray, dataStruct))
    else :
      bufSize = bufSize + \
                arrayBufferSize(arrayObject, 2, 
                                _IPC.formatFormatArrayMax(formatArray)-1,
                                formatArray, nextFormat, dataStruct)
  elif (ftype == NamedFMT) :
    bufSize = bufSize + \
              bufferSize1(_IPC.findNamedFormat(format), dataStruct, dStart,
                          parentFormat, isTopLevelStruct);
  elif (ftype == EnumFMT) :
    bufSize = bufSize + primFmttrs.ELength(primFmttrs.INT_FMT, None, 0);
  else :
    IPC.Raise("Unhandled format: %s" % ftype)

  return bufSize;

def bufferSize (formatter, object, isTopLevelStruct) :
  if (not validFormatter(formatter)) : return 0
  else : return bufferSize1(formatter, object, 0, 0, isTopLevelStruct)

def transferToBuffer (format, dataStruct, dStart, buffer, parentFormat, 
                      isTopLevelStruct) :
  ftype = _IPC.formatType(format)

  if (ftype == LengthFMT) :
    IPC.Raise("Python version of IPC can only use explicit formats")
  elif (ftype == PrimitiveFMT) :
    if (isTopLevelStruct) :
      object = IPCdata()
      object._f0 = dataStruct
      dataStruct = object
    primFmttrs.Encode(_IPC.formatPrimitiveProc(format), dataStruct, dStart,
 		      buffer)
  elif (ftype == PointerFMT) :
    object = primFmttrs.getObjectField(dataStruct, dStart)
    # 'Z' means data, 0 means NIL
    if (object is None) : theChar = '\0'
    else : theChar = 'Z'
    _IPC.formatPutChar(buffer, theChar);
    if (not object is None) :
      transferToBuffer(_IPC.formatChoosePtrFormat(format, parentFormat),
 		       dataStruct, dStart, buffer, 0, isTopLevelStruct)
  elif (ftype == StructFMT) :
    formatArray = _IPC.formatFormatArray(format)
    structStart = 0
    n = _IPC.formatFormatArrayMax(formatArray)
    if (isTopLevelStruct) : struct = dataStruct
    else : struct = primFmttrs.getObjectField(dataStruct, dStart)
    for i in range(1, n) :
      transferToBuffer(_IPC.formatFormatArrayItemPtr(formatArray, i),
 		       struct, structStart, buffer, format, False)
      structStart = structStart + 1
  elif (ftype == FixedArrayFMT) :
    formatArray = _IPC.formatFormatArray(format)
    nextFormat = _IPC.formatFormatArrayItemPtr(formatArray, 1)
    if (isTopLevelStruct) : arrayObject = dataStruct
    else : arrayObject = primFmttrs.getObjectField(dataStruct, dStart)
    if (not isinstance(arrayObject, (tuple, list))) :
      IPC.Raise(invalidArrayFormat(dataStruct, dStart))
    else :
      arrayTransferToBuffer(arrayObject, buffer, 2, 
 			    _IPC.formatFormatArrayMax(formatArray)-1,
 			    isSimpleType(nextFormat),
 			    formatArray, nextFormat, dataStruct, False);
  elif (ftype == VarArrayFMT) :
    formatArray = _IPC.formatFormatArray(format)
    nextFormat = _IPC.formatFormatArrayItemPtr(formatArray, 1)
    if (isTopLevelStruct) : arrayObject = dataStruct
    else : arrayObject = primFmttrs.getObjectField(dataStruct, dStart)
    # For the size of the array 
    _IPC.formatPutInt(buffer, varArrayNumElements(formatArray,
 							dataStruct))
    if (not isinstance(arrayObject, (tuple, list))) :
      IPC.Raise(invalidArrayFormat(dataStruct, dStart))
    else :
      arrayTransferToBuffer(arrayObject, buffer, 2, 
 			    _IPC.formatFormatArrayMax(formatArray)-1,
			    isSimpleType(nextFormat),
 			    formatArray, nextFormat, dataStruct, True)
  elif (ftype == NamedFMT) :
    transferToBuffer(_IPC.findNamedFormat(format), dataStruct, dStart, buffer,
 		     parentFormat, isTopLevelStruct)
  elif (ftype == EnumFMT) :
    primFmttrs.Encode(primFmttrs.INT_FMT, dataStruct, dStart, buffer);
  else :
    IPC.Raise("Unhandled format: %s" % ftype)

def transferToDataStructure (format, dataStruct, dStart, buffer, parentFormat,
                             isTopLevelStruct, oclass=None) :
  ftype = _IPC.formatType(format)

  if (ftype == LengthFMT) :
    IPC.Raise("Python version of IPC can only use explicit formats")
  elif (ftype == PrimitiveFMT) :
    try: dataStruct.__dict__ # Is this a settable object
    except AttributeError:
      IPC.Raise(invalidStructFormat(type(dataStruct),
                                    primFmttrs.PrimType(_IPC.formatPrimitiveProc(format))))
    primFmttrs.Decode(_IPC.formatPrimitiveProc(format), dataStruct, dStart,
                      buffer)
  elif (ftype == PointerFMT) :
    theChar = _IPC.formatGetChar(buffer)
    if (theChar == '\0') :
      primFmttrs.setObjectField(dataStruct, dStart, None)
    else :
      transferToDataStructure(_IPC.formatChoosePtrFormat(format, parentFormat),
 		              dataStruct, dStart, buffer, 0, isTopLevelStruct,
                              oclass)
  elif (ftype == StructFMT) :
    formatArray = _IPC.formatFormatArray(format)
    structStart = 0
    n = _IPC.formatFormatArrayMax(formatArray)
    if (isTopLevelStruct) :
      struct = dataStruct
    else :
      struct1 = primFmttrs.getObjectField(dataStruct, dStart)
      struct = validateObject(struct1, dataStruct, dStart)
      if (not struct == struct1) :
        primFmttrs.setObjectField(dataStruct, dStart, struct)
    for i in range(1, n) :
      transferToDataStructure(_IPC.formatFormatArrayItemPtr(formatArray, i),
                              struct, structStart, buffer, format, False)
      structStart = structStart + 1
  elif (ftype == FixedArrayFMT) :
    formatArray = _IPC.formatFormatArray(format)
    nextFormat = _IPC.formatFormatArrayItemPtr(formatArray, 1)
    size = _IPC.formatFormatArrayItemInt(formatArray, 2)
    if (isTopLevelStruct and isinstance(dataStruct, (tuple, list))) :
      arrayObject = dataStruct;
    else :
      arrayObject = primFmttrs.getObjectField(dataStruct, dStart)
      arrayObject = validateArrayObject(arrayObject, size, dataStruct, dStart)
      primFmttrs.setObjectField(dataStruct, dStart, arrayObject)

    arrayTransferToDataStructure(arrayObject, buffer, 2, 
                                 _IPC.formatFormatArrayMax(formatArray)-1,
                                 size, isSimpleType(nextFormat), formatArray,
                                 nextFormat, dataStruct, dStart, False, oclass);
  elif (ftype == VarArrayFMT) :
    formatArray = _IPC.formatFormatArray(format)
    nextFormat = _IPC.formatFormatArrayItemPtr(formatArray, 1)
    # The total size of the array is the stored first
    size = _IPC.formatGetInt(buffer)
    numDims = _IPC.formatFormatArrayMax(formatArray) - 2

    if (numDims > 1) : size = varArrayDimSize(2, formatArray, dataStruct)

    if (not feasibleToDecodeVarArray(size, formatArray, dStart)) :
      IPC.Raise("Python version of IPC cannot decode " + \
                "multi-dimensional variable length arrays unless " + \
                "the size variables appear BEFORE the array " + \
                "in the enclosing structure")
    elif (isTopLevelStruct) :
      arrayObject = dataStruct
    else :
      arrayObject = primFmttrs.getObjectField(dataStruct, dStart)
      arrayObject = validateArrayObject(arrayObject, size, dataStruct, dStart)
      primFmttrs.setObjectField(dataStruct, dStart, arrayObject)

    arrayTransferToDataStructure(arrayObject, buffer, 2, numDims+1, size,
                                 isSimpleType(nextFormat), formatArray,
                                 nextFormat, dataStruct, dStart, True, oclass)
  elif (ftype == NamedFMT) :
    transferToDataStructure(_IPC.findNamedFormat(format), dataStruct, dStart,
                            buffer, parentFormat, isTopLevelStruct)
  elif (ftype == EnumFMT) :
    primFmttrs.Decode(primFmttrs.INT_FMT, dataStruct, dStart, buffer)
  else :
    IPC.Raise("Unhandled format: %s" % ftype)

def encodeData (formatter, object, buffer) :
  transferToBuffer(formatter, object, 0, buffer, 0, True)

def decodeData (formatter, buffer, object, oclass) :
  transferToDataStructure(formatter, object, 0, buffer, 0, True, oclass)

# Marshalls the object into a byte array.
# Fills in the IPC_VARCONTENT structure with the length and byteArray.
#   "formatter" is a C pointer. Returns any error conditions.
def marshall (formatter, object, varcontent) :
  if (not checkMarshallStatus(formatter)) :
    return _IPC.IPC_Error
  else :
    varcontent.length = bufferSize(formatter, object, True)
    varcontent.content = None
    if (varcontent.length > 0) :
      varcontent.content = createByteArray(varcontent.length)
      buffer = createBuffer(varcontent.content)
      encodeData(formatter, object, buffer)
      if (bufferLength(buffer) != varcontent.length) :
        IPC.Raise("Mismatch between buffer size (%d) and encoded data (%d)" % \
                  (varcontent.length, bufferLength(buffer)))
      freeBuffer(buffer)
    return _IPC.IPC_OK

# Creates an object according to the formatter and byteArray (both C pointers).
# Returns a tuple of the object created any error conditions.
# Uses the object if passed (and if the formatter is a struct or array)
def unmarshall (formatter, byteArray, object=None, oclass=None) :
  if (not checkMarshallStatus(formatter)) :
    return _IPC.IPC_Error
  elif (validFormatter(formatter)) :
    buffer = createBuffer(byteArray)
    needEnclosingObject = not _IPC.formatType(formatter) in \
                              (StructFMT, FixedArrayFMT, VarArrayFMT)
    if (needEnclosingObject or object is None) :
      needEnclosingObject = (_IPC.formatType(formatter) != StructFMT)
      if (oclass is None or needEnclosingObject) : theObject = IPCdata()
      else : theObject = oclass()
    else :
      if (not oclass is None and not isinstance(object, oclass)) :
        IPC.Raise("unmarshall: Object %s and class %s do not match" % \
                  (object, oclass))
      theObject = object
    decodeData(formatter, buffer, theObject, oclass)
    if (needEnclosingObject) : theObject = theObject._f0
    _IPC.freeBuffer(buffer)
    return (theObject, _IPC.IPC_OK)
  else :
    return (None, _IPC.IPC_OK)

def createFixedArray(arrayClass, formatter) :
  size = _IPC.formatFormatArrayItemInt(_IPC.formatFormatArray(formatter), 2);
  return [None] * size
