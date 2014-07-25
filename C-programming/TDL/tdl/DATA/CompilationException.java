
/*
 * Note:  This class is intended as the general-case exception
 * to be raised when compilation of TDL code into C++ code fails.
 *
 * Note:  Since this is a subclass of RuntimeException, 
 * it can be thrown anywhere and does not *NEED* to be declared
 * (with a throws clause) or be caught!
 *
 * However, it is always nice to declare it in a throws clause
 * for stylistic reasons.  (Ie: Future support & maintenance.)
 * And, if it is not caught, the program terminates, reporting
 * this exception & a method-trace of exactly where and when
 * it occurred.  (Which is probably undesirable for compilation
 * errors.)
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 *
 */

public class CompilationException extends RuntimeException
{
  public CompilationException ( String theExceptionString )
  {
    super ( theExceptionString );
  }
}

