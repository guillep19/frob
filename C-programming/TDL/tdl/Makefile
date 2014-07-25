TDL_SKIP_CLEAN_ON_MAKE = 1
# Copyright (c) 2008, Carnegie Mellon University
#     This software is distributed under the terms of the 
#     Simplified BSD License (see tdl/LICENSE.TXT)

                JAVA_SUBDIRS = PARSER DATA
                 CXX_SUBDIRS = src
                     SUBDIRS = $(JAVA_SUBDIRS) $(CXX_SUBDIRS)
        EXTRA_SRC_MAKE_FILES = Makefile.generic

               TDLC_JAR_FILE = tdl_x.jar
                     TMP_DIR = tmp
                    RM_FILES = $(TDLC_JAR_FILE)

### Enable conditonal-cleaning for this directory.
    DIRECTORY_NEEDS_CLEANING = TRUE

.PHONY: default cmu-install

default: all

# Added by Trey for CMU's FIRE project.
cmu-install: all jar
	@[ -d include ] || mkdir include
	cp -f src/*.H include
	mv -f tdl_x.jar java/00_tdl.jar
	gmake -C .. MODULES=tdl install


ifdef BASE_TDL_DIR
  include $(BASE_TDL_DIR)/Makefile.generic
else
  ifdef BASE_ROOT_DIR
    include $(BASE_ROOT_DIR)/$(shell if [ ! -d $(BASE_ROOT_DIR)/tdl -a -d  $(BASE_ROOT_DIR)/tcaV8/tdl ]; then echo "tcaV8/tdl"; else echo "tdl"; fi)/Makefile.generic
  else
    include ./Makefile.generic
  endif
endif


####################################################################
##### Make CLASSPATH include all our local directories...     ######
###### Note:  This value will be passed to recursive makes... ######
####################################################################
  empty:=
  space:= $(empty) $(empty)
  TDL_CLASSPATH := .$(subst $(space),$(empty),$(foreach DIR,$(JAVA_SUBDIRS),:$(BASE_TDL_DIR)/$(DIR)))
ifdef CLASSPATH
  export CLASSPATH := $(TDL_CLASSPATH):$(CLASSPATH)
else
  export CLASSPATH := $(TDL_CLASSPATH)
endif


FORCE:

PARSER DATA GRAPH GUI: FORCE
	cd $@ ; $(MAKE) all ; cd .. ;

wctotal:
	$(WC) $(SRC_MAKEFILES) $(JJ_FILES) $(SRC_FILES)    \
	      `$(foreach DIR, $(SUBDIRS),              \
	         cd $(DIR) ;                           \
	         $(MAKE) echoSrcFiles                  \
			| grep -v 'Entering directory' \
			| grep -v 'Leaving directory'  \
			| sed 's|^|$(DIR)/|g'          \
			| sed 's| | $(DIR)/|g' ;       \
		 cd .. ; )`  < $(DEV_NULL)

testclasspath:
	@echo $(CLASSPATH)

tdlc:
	$(JAVA) TDLC $(TDLC_ARGS)





#############################################################################
#### This is a really quick, down, and dirty way to make a TDL jar file. ####
#############################################################################

jar:
	@if [ -a $(TMP_DIR) ]; then echo "Error:  '$(TMP_DIR)' already exists."; exit -1; fi
	$(RM) $(TDLC_JAR_FILE)
	$(MKDIR) $(TMP_DIR)
	$(CP) DATA/*.class PARSER/*.class $(TMP_DIR)
	cd $(TMP_DIR) ; jar -cvf ../$(TDLC_JAR_FILE) . ; cd ..
	jar -tvf $(TDLC_JAR_FILE)
	$(RM) -rf $(TMP_DIR)

