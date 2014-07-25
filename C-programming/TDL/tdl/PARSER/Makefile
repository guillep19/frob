#
# Copyright (c) 2008, Carnegie Mellon University
#     This software is distributed under the terms of the 
#     Simplified BSD License (see tdl/LICENSE.TXT)
#

ifndef BASE_TDL_DIR
  BASE_TDL_DIR = ..
  export BASE_TDL_DIR
endif

     JJ_FILES = TDLParser.jj

    SRC_FILES = ClassStructNamespaceToken.java

 GENERATED_FILES = CharStream.java            \
                   ParseError.java            \
                   TDLParserConstants.java    \
                   TDLParserTokenManager.java \
                   Token.java                 \
                   ParseException.java        \
                   TokenMgrError.java         \

     RM_FILES = $(GENERATED_FILES) $(GENERATED_FILES:.java=.class) \
                JJTDLParserCalls.class 'TDLParser$$JJCalls.class' \
		'TDLParser$$1.class' 'TDLParser$$LookaheadSuccess.class'


include $(BASE_TDL_DIR)/Makefile.generic

EXTRA_JAVACC_FUNCTION = echo ' ' ; echo '  "javacc TDLParser" may print multiple warning messages about:' ; echo ' ' ; echo '    "...String with IGNORE_CASE is partially superceded by string at line..."' ; echo ' ' ; echo '  These messgaes are expected, unavoidable, and should be ignored.' ; echo '  They are a byproduct of various tokens that are used in both' ; echo '  C++ (as case-sensitive tokens) and TDL (as case-INsensitive tokens).' ; echo '  The TDLParser uses all the variants appropriately depending upon the context.' ; echo ' '

JAVACC_FILTER = | $(GREP) -v '^Warning: Line 78[45678], Column 5: String with IGNORE_CASE is partially superceded by string at line 77[01237].$$'

