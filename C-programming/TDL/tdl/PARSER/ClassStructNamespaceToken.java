/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

public class ClassStructNamespaceToken extends Token
{
  public static final int CLASS     = TDLParser.CLASS;
  public static final int STRUCT    = TDLParser.STRUCT;
  public static final int NAMESPACE = TDLParser.NAMESPACE;

  public Token idToken;
  public int   type;
}

