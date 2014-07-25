/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

public interface DataValidateCode
{
  public final static int  BASE_REFERENCE = 0;

    /*
     *    These methods write any warnings and/or errors to "theDestination".
     * A count of how many warnings/errors were written is kept in
     * "theReturnValue" object.
     *
     *    The "int theReference" is reserved for future expansion.
     *
     *    These methods can throw a CompilationException in the event of
     * a catastrophic failure.  A catastrophic failure is defined to be a
     * failure that prevents further error/warning reporting from being
     * accomplished.  Non-catastrophic failures are reported as errors.
     */
	/* Validates code that is outside of any Task */
  public void validateExternalCode( int                         theReference,
				    DataValidateCodeReturnValue theReturnValue)
    throws CompilationException;

	/* Validates code that is inside of a Task */
  public void validateInternalCode( int                         theReference,
				    DataValidateCodeReturnValue theReturnValue)
    throws CompilationException;
}

