
default: all

#
# These can be globally enabled/disabled here.
# (Note: This TEST/Makefile is independent of the global Makefile.generic!)
#
# Copyright (c) 2008, Carnegie Mellon University
#     This software is distributed under the terms of the 
#     Simplified BSD License (see tdl/LICENSE.TXT)
#

### DISTRIBUTED must be set in the enviornment if it is to be used.
export DISTRIBUTED

### IF THREADED is already set in the environment, it will override the value
### that is dynamically determined for it from the libtcm.a file down below .


#
# This can be set to simplify things...
#
ifndef BASE_ROOT_DIR
    # Guess a random value here instead of using $(error),
    # since BASE_ROOT_DIR may never get used down below.
  BASE_ROOT_DIR = $(HOME)/work
  export BASE_ROOT_DIR
endif

#
# Or these can be set to simplify things...
#
ifndef BASE_TCM_DIR
  BASE_TCM_DIR = $(BASE_ROOT_DIR)/$(shell if [ ! -d $(BASE_ROOT_DIR)/tcm -a -d  $(BASE_ROOT_DIR)/tcaV8/tcm ]; then echo "tcaV8/tcm"; else echo "tcm"; fi)
  export BASE_TCM_DIR
endif

ifndef BASE_TDL_DIR
  BASE_TDL_DIR = $(BASE_ROOT_DIR)/$(shell if [ ! -d $(BASE_ROOT_DIR)/tdl -a -d  $(BASE_ROOT_DIR)/tcaV8/tdl ]; then echo "tcaV8/tdl"; else echo "tdl"; fi)
  export BASE_TDL_DIR
endif

ifndef BASE_IPC_DIR
  BASE_IPC_DIR = $(BASE_ROOT_DIR)/ipc
  export BASE_IPC_DIR
endif

ifndef BASE_THREADS_DIR
  BASE_THREADS_DIR = $(BASE_ROOT_DIR)/$(shell if [ ! -d $(BASE_ROOT_DIR)/threads -a -d $(BASE_ROOT_DIR)/THREADS ]; then echo "THREADS"; else echo "threads"; fi)
  export BASE_THREADS_DIR
endif

#
# Provide some really basic error checking
#
ifeq ($(shell if [ ! -d $(BASE_TCM_DIR) ]; then echo "1"; else echo "0"; fi),1)
  $(warning "Error:  Unable to locate TCM directory [$(BASE_TCM_DIR)].")
  $(warning "Error:  Please setenv BASE_TCM_DIR or BASE_ROOT_DIR appropriately.")
  $(error )
endif
ifeq ($(shell if [ ! -d $(BASE_TDL_DIR) ]; then echo "1"; else echo "0"; fi),1)
  $(warning "Error:  Unable to locate TDL directory [$(BASE_TDL_DIR)].")
  $(warning "Error:  Please setenv BASE_TDL_DIR or BASE_ROOT_DIR appropriately.")
  $(error )
endif
#
# Don't test BASE_IPC_DIR or BASE_THREADS_DIR, as they may not get used...
#



#
# Determine the OS/System/Release library subdirectory.
#
ifndef BASE_OS_SYSTEM
  BASE_OS_SYSTEM = $(shell uname -s)
  export BASE_OS_SYSTEM
endif

ifndef BASE_OS_RELEASE
  BASE_OS_RELEASE = $(shell uname -r|tr '-' '.'|awk -F. '{print $$1 "." $$2}')
  export BASE_OS_RELEASE
endif

ifndef BASE_OS_DIR
  BASE_OS_DIR     = $(BASE_OS_SYSTEM)-$(BASE_OS_RELEASE)
  export BASE_OS_DIR
endif

#
# If TDL_USE_SINGLE_DIR is defined, we will (by default) place all object
# in a single directory, and all the library files in a single directory.
#
# If it is NOT defined, we will (by default) place all object/library
# files in their own OS-specific subdirectories.
#
# Naturally, anything and everything can be overriden with the right setenv's.
#
ifdef TDL_USE_SINGLE_DIR

  ifndef TDL_OS_DIR
    TDL_OS_DIR = 
    export TDL_OS_DIR
  endif

  ifndef TDL_OBJECT_DIR
    TDL_OBJECT_DIR = obj
      # Export TDL_OBJECT_DIR:  There is no GLOBAL object directory.
      # Each subdirectory gets its own private object directory.
    export TDL_OBJECT_DIR
  endif

else

  ifndef TDL_OS_DIR
    TDL_OS_DIR = $(BASE_OS_DIR)
    export TDL_OS_DIR
  endif

  ifndef TDL_OBJECT_DIR
    TDL_OBJECT_DIR = obj/$(TDL_OS_DIR)
      # Export TDL_OBJECT_DIR:  There is no GLOBAL object directory.
      # Each subdirectory gets its own private object directory.
    export TDL_OBJECT_DIR
  endif

endif


#
# Users should either setenv the above BASE directories appropriately,
# or they should set these directly.
#  (Note: setenv'ed values will override these values...)
#
ifndef TCM_INCLUDE_DIR
  TCM_INCLUDE_DIR = $(BASE_TCM_DIR)/include
  export TCM_INCLUDE_DIR
endif

ifndef TCM_LIBRARY_DIR
  TCM_LIBRARY_DIR = $(BASE_TCM_DIR)/lib
  export TCM_LIBRARY_DIR
endif

ifndef TDL_INCLUDE_DIR
  TDL_INCLUDE_DIR = $(BASE_TDL_DIR)/src
  #Do NOT Export:  Needs to be reset if/when BASE_TDL_DIR changes.
endif

ifndef TDL_LIBRARY_DIR
  TDL_LIBRARY_DIR = $(BASE_TDL_DIR)/lib/$(TDL_OS_DIR)
  #Do NOT Export:  Needs to be reset if/when BASE_TDL_DIR changes.
endif

ifndef IPC_INCLUDE_DIR
  IPC_INCLUDE_DIR = $(BASE_IPC_DIR)/include
  export IPC_INCLUDE_DIR
endif

ifndef IPC_LIBRARY_DIR
  IPC_LIBRARY_DIR = $(BASE_IPC_DIR)/lib/$(BASE_OS_DIR)
  export IPC_LIBRARY_DIR
endif

  # This is actually used here.
  # (And 'make echo-CENTRAL_PATH' could prove useful.)
  # It is extracted from the environment by tdldef.C's runCentral() at runtime.
ifndef CENTRAL_PATH
  CENTRAL_PATH = $(BASE_IPC_DIR)/bin/$(BASE_OS_DIR)/central
  export CENTRAL_PATH
endif

ifndef CXX_THREADS_INCLUDE_DIR
  CXX_THREADS_INCLUDE_DIR = $(BASE_THREADS_DIR)
  export CXX_THREADS_INCLUDE_DIR
endif

ifndef CXX_THREADS_LIBRARY_DIR
  ifdef THREADS_USE_SINGLE_DIR
    CXX_THREADS_LIBRARY_DIR = $(BASE_THREADS_DIR)/lib/
  else
    CXX_THREADS_LIBRARY_DIR = $(BASE_THREADS_DIR)/lib/$(BASE_OS_DIR)
  endif
  export CXX_THREADS_LIBRARY_DIR
endif



#
# A few common defintions.
# Moved to here to support the THREADED code.
#
              JAVA = java
              DIFF = diff
               SED = sed
              GREP = grep
               AWK = awk
              SORT = sort
                RM = rm -f
                WC = wc
             TOUCH = touch
          DEV_NULL = /dev/null
           STRINGS = strings

        TDLC_FLAGS = -2Sy
         DIFF_ARGS =

ifndef HOSTNAME
          HOSTNAME = $(shell hostname)
endif

               CXX = g++
            CFLAGS =
         CXX_FLAGS =
     CXX_STD_FLAGS = $(DISTRIBUTED_CXX_FLAGS) $(THREADED_CXX_FLAGS) \
                     -Wall $(CFLAGS)

     CXX_LIBRARIES = -ltdl -ltcm \
                      $(DISTRIBUTED_LIBRARIES) $(THREADED_LIBRARIES)

     CXX_DIR_FLAGS = -I. -I$(TDL_INCLUDE_DIR) -L$(TDL_LIBRARY_DIR) \
                     -I$(TCM_INCLUDE_DIR) -L$(TCM_LIBRARY_DIR)     \
                     $(CXX_DISTRIBUTED_INCLUDE_DIRS)               \
                     $(CXX_DISTRIBUTED_LIBRARY_DIRS)               \
                     $(CXX_THREADED_INCLUDE_DIRS)                  \
                     $(CXX_THREADED_LIBRARY_DIRS) 
     TDL_CXX_FLAGS = $(CXX_STD_FLAGS) $(CXX_FLAGS) $(DEBUG_CXX_FLAGS) \
                     $(CXX_LIBRARIES) $(CXX_DIR_FLAGS)


#
# IF THREADED is already set in the environment, that value OVERRIDES this!
# Otherwise, dynamically determine the value of THREADED from libtcm.a.
#
ifndef THREADED
  ifneq "0" "$(strip $(shell $(STRINGS) $(TCM_LIBRARY_DIR)/libtcm.a | $(GREP) TCM_SetIsThreadedTask | $(WC) -l ) )"
     THREADED = 1
     export THREADED
  endif
endif



#
# If we don't have TDLC on the current CLASSPATH, add in the
# default (probable) entries to the beginning of the CLASSPATH.
#
ifneq "0" "$(strip $(shell $(JAVA) TDLC -V 2>&1 >> $(DEV_NULL); echo $$?))"

  export CLASSPATH := .:$(BASE_TDL_DIR)/PARSER:$(BASE_TDL_DIR)/DATA:$(BASE_TDL_DIR)/tdl_x.jar:$(CLASSPATH)

  # If we *STILL* don't have it, user needs to recompile...
  # Note:  CLASSPATH cannot be setenv'ed for $shell.
  # Note:  \"$(CLASSPATH)\" instead of $(CLASSPATH) will break it.
  # Note:  $warning/$error requires '\;'.  $shell requires ';'.
  # Note:  space after error in $error is required.
  ifneq '0' '$(strip $(shell export CLASSPATH=$(CLASSPATH); $(JAVA) TDLC -V 2>&1 >> $(DEV_NULL); echo $$?))'

    $(warning "Error:  TDLC was not found in CLASSPATH or Parent dir (..)")
    $(warning "        Perhaps you could recompile it?  (cd ..\; make)")

    ifndef TDL_DO_NOT_ABORT_ON_TDLC_NOT_FOUND_ERROR
      $(error )
    endif

  endif
endif




CXX_DISTRIBUTED_INCLUDE_DIRS = 
CXX_DISTRIBUTED_LIBRARY_DIRS = 

ifdef DISTRIBUTED
        DISTRIBUTED_LIBRARIES = -lipcInterface -lipc
#CXX_DISTRIBUTED_INCLUDE_DIRS = -I$(IPC_INCLUDE_DIR)
 CXX_DISTRIBUTED_LIBRARY_DIRS = -L$(IPC_LIBRARY_DIR)
endif

CXX_THREADED_INCLUDE_DIRS =
CXX_THREADED_LIBRARY_DIRS =

ifdef THREADED
           THREADED_LIBRARIES = -lcxxThreads -lpthread
    CXX_THREADED_INCLUDE_DIRS = -I$(CXX_THREADS_INCLUDE_DIR)
    CXX_THREADED_LIBRARY_DIRS = -L$(CXX_THREADS_LIBRARY_DIR)
endif




      TDL_PROGRAMS = reid_1.tdl      \
                reid_2_test.tdl      \
                reid_3_test.tdl      \
                   test_2_4.tdl      \
                   test_2_5.tdl      \
                   test_2_6.tdl      \
                   test_2_6a.tdl     \
                   test_2_7a.tdl     \
                   test_2_8.tdl      \
                   test_3_1.tdl      \
                   test_3_2.tdl      \
                   test_3_3.tdl      \
                   test_3_4.tdl      \
                   test_3_4a.tdl     \
                   test_3_4b.tdl     \
                   test_3_5.tdl      \
                   test_3_6.tdl      \
                   test_3_6a.tdl     \
                   test_3_7.tdl      \
                   test_3_7a.tdl     \
                   test_3_7b.tdl     \
                   test_3_8.tdl      \
                   test_4_1.tdl      \
                   test_4_1a.tdl     \
                   test_4_2.tdl      \
                   test_5_1.tdl      \
                   test_5_2.tdl      \
                   test_5_3.tdl      \
                   test_5_4.tdl      \
                   test_5_5.tdl      \
                   test_5_6.tdl      \
                   test_5_7.tdl      \
                   test_5_8.tdl      \
                   test_6_1.tdl      \
                   test_6_1a.tdl     \
                   test_6_2.tdl      \
                   test_6_3.tdl      \
                   test_6_4.tdl      \
                   test_6_5.tdl      \
                   test_6_6.tdl      \
                   test_7_1.tdl      \
                   test_7_2.tdl      \
                   test_7_3.tdl      \
                   test_7_4.tdl      \
                   test-1.3.x.12.tdl \
                   test-1.3.x.13.tdl \
                   test-1.3.x.14.tdl \
                   test-1.3.5.15.tdl \
                   test-1.3.5.16.tdl \
                   test-1.3.5.17.tdl \
                   test-1.3.5.18.tdl \
                   test-1.3.5.19.tdl \
                   test-1.3.5.20.tdl \
                   test-1.3.5.21.tdl \
                   test-1.3.5.22.tdl \
                   test-1.3.5.26.tdl 


#                   test_2_7.tdl      \
#                   BOMB_1.tdl   -- tests error & warning reporting



### Note: -[1234].tdl, -5c.tdl, -5s.tdl, -6c.tdl, and -6s.tdl match
### special rules that are used for testing the various TDLC option flags.
 DISTRIBUTED_TDL_PROGRAMS = test-1.3.x.1.tdl          \
                            test-1.3.x.2.tdl          \
                            test-1.3.x.3-1.tdl        \
                            test-1.3.x.3-2.tdl        \
                            test-1.3.x.3-3.tdl        \
                            test-1.3.x.3-4.tdl        \
                            test-1.3.x.4-client-5.tdl \
                            test-1.3.x.4-client-6.tdl \
                            test-1.3.x.5.tdl          \
                            test-1.3.x.6.tdl          \
                            test-1.3.x.7.tdl          \
                            test-1.3.x.8.tdl          \
                            test-1.3.x.9.tdl          \
                            test-1.3.x.11.tdl         \
                            test-1.3.6.26.tdl         \
                            test-1.3.5.27.tdl         

### For distributed testing, sometimes we need to make a second executable.
 DISTRIBUTED_TDL_SPLIT_PROGRAMS = test-1.3.x.4-server-5.tdl \
                                  test-1.3.x.4-server-6.tdl 


    THREADED_TDL_PROGRAMS = test-1.3.x.10.tdl            \
                            test-1.3.5.23.THREADED.tdl   \
                            test-1.3.5.24.THREADED.tdl   \
                            test-1.3.5.25.THREADED.tdl   


    TDL_FILEBASES_DIFF          := $(TDL_PROGRAMS:.tdl=)

ifdef THREADED
    TDL_PROGRAMS += $(THREADED_TDL_PROGRAMS)
    TDL_FILEBASES_DIFF_THREADED := $(THREADED_TDL_PROGRAMS:.tdl=)
endif

ifdef DISTRIBUTED
    TDL_PROGRAMS += $(DISTRIBUTED_TDL_PROGRAMS)
    TDL_FILEBASES_DIFF_DISTRIBUTED := $(DISTRIBUTED_TDL_PROGRAMS:.tdl=)

    TDL_SORTED = $(DISTRIBUTED_TDL_PROGRAMS:.tdl=.out.sed.sorted)         \
                 $(DISTRIBUTED_TDL_PROGRAMS:.tdl=.master_out.sed.sorted)
endif


    TDL_FILEBASES  := $(TDL_PROGRAMS:.tdl=)
    TDL_OUTPUT     := $(TDL_PROGRAMS:.tdl=.out)
    TDL_SED_OUTPUT := $(TDL_PROGRAMS:.tdl=.out.sed)
    TDL_CODE        =
    TDL_HEADERS     =


ifdef DISTRIBUTED
       ### For the split-programs, we create *ONLY* the .H/.C/executable
     TDL_PROGRAMS += $(DISTRIBUTED_TDL_SPLIT_PROGRAMS)

       ### And, for the split-programs, we will need export header/code files.
     TDL_CODE     += $(DISTRIBUTED_TDL_SPLIT_PROGRAMS:.tdl=-distributed_only.C)
     TDL_HEADERS  += $(DISTRIBUTED_TDL_SPLIT_PROGRAMS:.tdl=-distributed_only.H)
endif


    TDL_CODE      += $(TDL_PROGRAMS:.tdl=.C)
    TDL_HEADERS   += $(TDL_PROGRAMS:.tdl=.H)
    TDL_BINARIES   = $(TDL_PROGRAMS:.tdl=.exe)


       ### Force "make clean" to get these files too,
       ### even if the user didn't do "make clean DISTRIBUTED=1".
       ### (Also, some of these files are not otherwise listed.)
    TDL_DISTRIBUTED_CLEAN_FILES =                                             \
                   $(DISTRIBUTED_TDL_PROGRAMS:.tdl=.out)                      \
                   $(DISTRIBUTED_TDL_PROGRAMS:.tdl=.out.sed)                  \
                   $(DISTRIBUTED_TDL_PROGRAMS:.tdl=.C)                        \
                   $(DISTRIBUTED_TDL_PROGRAMS:.tdl=.H)                        \
                   $(DISTRIBUTED_TDL_PROGRAMS:.tdl=.exe)                      \
                   $(DISTRIBUTED_TDL_SPLIT_PROGRAMS:.tdl=.C)                  \
                   $(DISTRIBUTED_TDL_SPLIT_PROGRAMS:.tdl=.H)                  \
                   $(DISTRIBUTED_TDL_SPLIT_PROGRAMS:.tdl=.exe)                \
                   $(DISTRIBUTED_TDL_SPLIT_PROGRAMS:.tdl=-distributed_only.H) \
                   $(DISTRIBUTED_TDL_SPLIT_PROGRAMS:.tdl=-distributed_only.C) \
                   $(TDL_SORTED)


ifndef TDL_DISABLE_TESTING_LABELS
  ifdef THREADED
    TDL_THREADED_LABEL = echo -e '\nThreading:'
  endif

  ifdef DISTRIBUTED
    TDL_DISTRIBUTED_LABEL = echo -e '\nDistributed:'
  endif
endif


.SECONDARY: $(TDL_CODE) $(TDL_HEADERS) $(TDL_BINARIES)
.PRECIOUS:  $(TDL_OUTPUT) $(TDL_SED_OUTPUT)


####################################################################
##### Test for specific TDLC flags/options (-1,-2,-3,-4,-5,-6) #####
####################################################################

%-1.C: %.tdl
	$(JAVA) TDLC -1Syo $*-1 $<

%-2.C: %.tdl
	$(JAVA) TDLC -2Syo $*-2 $<

%-3.C: %.tdl
	$(JAVA) TDLC -3Syo $*-3 $<

%-4.C: %.tdl
	$(JAVA) TDLC -4Cyo $*-4.C $<
	$(TOUCH) $*-4.H


   ### TDLC -5 results in a .H file that needs to be included.
%-client-5.C: %-client.tdl %-server-5-distributed_only.H
	$(JAVA) TDLC -2Syo $*-client-5 $<

%-server-5.C: %-server.tdl
	$(JAVA) TDLC -2Syo $*-server-5 $<

%-server-5-distributed_only.H: %-server.tdl
	$(JAVA) TDLC -5Hyo $*-server-5-distributed_only.H $<

   ### TDLC -5C is a complete no-op -- E.g. An empty file...
%-server-5-distributed_only.C: %-server-5-distributed_only.H
	@

%-client-5.exe: CXX_FLAGS += -DTDLC_5

%-client-5.out: %-client-5.exe %-server-5.exe
	./$< > $@ 2>&1



   ### TDLC -6 results in a .H and .C file that need to be included/linked.
%-client-6.C: %-client.tdl                  \
              %-server-6-distributed_only.C \
              %-server-6-distributed_only.H 
	$(JAVA) TDLC -2Syo $*-client-6 $<

%-server-6.C: %-server.tdl
	$(JAVA) TDLC -2Syo $*-server-6 $<

%-server-6-distributed_only.C: %-server.tdl
	$(JAVA) TDLC -6Syo $*-server-6-distributed_only $<

%-server-6-distributed_only.H: %-server-6-distributed_only.C
	@

%-client-6.exe: CXX_FLAGS += -DTDLC_6 \
                `echo $*-server-6-distributed_only.C | $(SED) 's|-client-6||g'`

%-client-6.out: %-client-6.exe %-server-6.exe
	./$< > $@ 2>&1 

####################################################################
##### End tests for specific TDLC flags/options. ###################
####################################################################





%.C: %.tdl
	$(JAVA) TDLC $(TDLC_FLAGS) $<

# Do a no-op here...  To prevent running 'java TDLC ...' twice...
#  (Note:  Needs the tab with an empty line afterwards...)
%.H: %.C
	@

# If the libraries exist, and they are newer than our *.exe file,
# we need to recompile.  (This is used for testing validation purposes.)
# If the libraries DON'T EXIST, we need to print a warning.
%.exe: %.C %.H $(wildcard $(TDL_LIBRARY_DIR)/libtdl.a) \
               $(wildcard $(TCM_LIBRARY_DIR)/libtcm.a)
	@if [ ! -f $(TDL_LIBRARY_DIR)/libtdl.a ]; then echo " "; echo "Error:  $(TDL_LIBRARY_DIR)/libtdl.a is missing...."; echo " "; exit -1; fi
	@if [ ! -f $(TCM_LIBRARY_DIR)/libtcm.a ]; then echo " "; echo "Error:  $(TCM_LIBRARY_DIR)/libtcm.a is missing...."; echo " "; exit -1; fi
	$(CXX) $< -o $@ $(TDL_CXX_FLAGS)


# If the libraries don't exist, the %.exe rule may never be invoked...
check_libraries_exist:
	@if [ ! -f $(TDL_LIBRARY_DIR)/libtdl.a ]; then echo " "; echo "Warning:  $(TDL_LIBRARY_DIR)/libtdl.a is missing...."; echo " "; fi
	@if [ ! -f $(TCM_LIBRARY_DIR)/libtcm.a ]; then echo " "; echo "Warning:  $(TCM_LIBRARY_DIR)/libtcm.a is missing...."; echo " "; fi


%.out: %.exe
	./$< > $@ 2>&1 

%.out.sed: %.out
	$(SED) -e 's| 0x0 | (nil) |g' < $< | $(SED) -e 's| 0x0$$| (nil)|g' | $(SED) -e 's|0x[0-9a-fA-F]\{0,8\}|0x........|g' | $(SED) -e 's|$(HOSTNAME)|localhost.localdomain|g' | $(SED) -e 's|Task Control Management [0-9]\+.[0-9]\+.[0-9]\+ ([a-zA-Z]\+-[0-9]\+-[0-9]\+)|Task Control Management x.y.z (MON-DAY-YEAR)|g' > $<.sed
	@ echo " "

%.sorted: %
	@echo " "                               > $@ 2>&1
ifdef TDL_CHECK_CENTRAL
	@$(GREP) '^central:' $< 2>&1 | $(SORT) >> $@ 2>&1
endif
	@$(GREP) '^ Client:' $<                >> $@ 2>&1
	@$(GREP) '^ Server:' $<                >> $@ 2>&1
	@$(GREP) -v -e '^$$' -e '^central:' -e '^ Client:' -e '^ Server:' $< >> $@ 2>&1



all: qb

out: check_libraries_exist $(TDL_OUTPUT)

sed: check_libraries_exist $(TDL_SED_OUTPUT)

test: check_libraries_exist $(TDL_OUTPUT)
	$(foreach PROGRAM, $(TDL_FILEBASES), echo "$(PROGRAM):" ; diff $(DIFF_ARGS) $(PROGRAM).master_out $(PROGRAM).out; echo " ";)

rawdiff sedtest: check_libraries_exist $(TDL_SED_OUTPUT)
	( $(foreach PROGRAM, $(TDL_FILEBASES), echo "$(PROGRAM):" ; diff $(DIFF_ARGS) $(PROGRAM).master_out.sed $(PROGRAM).out.sed; echo " ";) )


diff: check_libraries_exist cleansorted $(TDL_SED_OUTPUT) $(TDL_SORTED)
	@( $(foreach PROGRAM, $(TDL_FILEBASES_DIFF), echo "$(PROGRAM):" ; diff $(DIFF_ARGS) $(PROGRAM).master_out.sed $(PROGRAM).out.sed; echo " ";) )
	@$(TDL_THREADED_LABEL)
	@( $(foreach PROGRAM, $(TDL_FILEBASES_DIFF_THREADED), echo "$(PROGRAM):" ; diff $(DIFF_ARGS) $(PROGRAM).master_out.sed $(PROGRAM).out.sed; echo " ";) echo " " )
	@$(TDL_DISTRIBUTED_LABEL)
	@( $(foreach PROGRAM, $(TDL_FILEBASES_DIFF_DISTRIBUTED), echo "$(PROGRAM):" ; diff $(DIFF_ARGS) $(PROGRAM).master_out.sed.sorted $(PROGRAM).out.sed.sorted; echo " ";) echo " " )


sd sdiff simplediff: check_libraries_exist cleansorted \
                     $(TDL_SED_OUTPUT) $(TDL_SORTED)
	@( $(foreach PROGRAM, $(TDL_FILEBASES_DIFF), [ "`diff $(PROGRAM).master_out.sed $(PROGRAM).out.sed | wc -l | tr -d ' '`" = "0" ] || ( echo "$(PROGRAM):" ; diff $(DIFF_ARGS) $(PROGRAM).master_out.sed $(PROGRAM).out.sed ; echo " ") ;) )
	@$(TDL_THREADED_LABEL)
	@( $(foreach PROGRAM, $(TDL_FILEBASES_DIFF_THREADED), [ "`diff $(PROGRAM).master_out.sed $(PROGRAM).out.sed | wc -l | tr -d ' '`" = "0" ] || ( echo "$(PROGRAM):" ; diff $(DIFF_ARGS) $(PROGRAM).master_out.sed $(PROGRAM).out.sed ; echo " ") ;) echo " " )
	@$(TDL_DISTRIBUTED_LABEL)
	@( $(foreach PROGRAM, $(TDL_FILEBASES_DIFF_DISTRIBUTED), [ "`diff $(PROGRAM).master_out.sed.sorted $(PROGRAM).out.sed.sorted | wc -l | tr -d ' '`" = "0" ] || (echo "$(PROGRAM):" ; diff $(DIFF_ARGS) $(PROGRAM).master_out.sed.sorted $(PROGRAM).out.sed.sorted; echo " ");) echo " " )


qd quickdiff diffIgnoreNoise:
	$(MAKE) diff | $(GREP) -v -e '^[<>] central:  ' -e '^[0-9,acd]*$$' -e '^---$$' -e '^[<>] \( Client:  Task\| Server:  Task\|Task\) Control Management 2\.[67]\.[0-9]* ([a-zA-Z]*-[0-9]*-[0-9]*)$$'

sqd simplequickdiff simplediffIgnoreNoise:
	$(MAKE) sdiff | $(GREP) -v -e '^[<>] central:  ' -e '^[0-9,acd]*$$' -e '^---$$' -e '^[<>] \( Client:  Task\| Server:  Task\|Task\) Control Management 2\.[67]\.[0-9]* ([a-zA-Z]*-[0-9]*-[0-9]*)$$'


### Lets space the quicktest out nicely, lining everything up...
SPACE_TEXT_OUT = $(AWK) 'BEGIN {printf "%s",substr("                      ",0,22-length("$1"))}'

qb quicktest_backend: check_libraries_exist cleansorted \
                      $(TDL_SED_OUTPUT) $(TDL_SORTED)
	@ ( $(foreach PROGRAM, $(TDL_FILEBASES_DIFF), echo -n "$(PROGRAM):" ; $(call SPACE_TEXT_OUT,$(PROGRAM)); diff $(DIFF_ARGS) $(PROGRAM).master_out.sed $(PROGRAM).out.sed | wc | tr -d '\012'; [ "`diff $(PROGRAM).master_out.sed $(PROGRAM).out.sed | wc -l | tr -d ' '`" = "0" ] || echo -n "          ********************" ; echo " "; ) )
	@$(TDL_THREADED_LABEL)
	@ ( $(foreach PROGRAM, $(TDL_FILEBASES_DIFF_THREADED), echo -n "$(PROGRAM):" ; $(call SPACE_TEXT_OUT,$(PROGRAM)); diff $(DIFF_ARGS) $(PROGRAM).master_out.sed $(PROGRAM).out.sed | wc | tr -d '\012'; [ "`diff $(PROGRAM).master_out.sed $(PROGRAM).out.sed | wc -l | tr -d ' '`" = "0" ] || echo -n "          ********************" ; echo " "; ) echo " " )
	@$(TDL_DISTRIBUTED_LABEL)
	@ ( $(foreach PROGRAM, $(TDL_FILEBASES_DIFF_DISTRIBUTED), echo -n "$(PROGRAM):" ; $(call SPACE_TEXT_OUT,$(PROGRAM)); diff $(DIFF_ARGS) $(PROGRAM).master_out.sed.sorted $(PROGRAM).out.sed.sorted | wc | tr -d '\012'; [ "`diff $(PROGRAM).master_out.sed.sorted $(PROGRAM).out.sed.sorted | wc -l | tr -d ' '`" = "0" ] || echo -n "          ********************" ; echo " "; ) echo " " )

qt quicktest: quicktest_backend bombtest


quiet:
	@echo " "
	@echo " "
	@$(MAKE) 2>&1 | $(GREP) -v -e '^g++' -e '^\./' -e '^sed' -e '^[ ]*$$' -e '\.tdl: In method `' -e ': warning: unused variable `' -e ':  Warning:  Use of "PARENT" keyword is deprecated.  Assuming you meant to use "THIS"' -e ':  Warning:  Use of "SELF" keyword is deprecated\.  Assuming you meant to use "CHILD"\.' -e ':  Warning:  Using TCM_LastChild ( TCM_GetCurrentTaskTreeRefForThisThread() ) for "PREVIOUS"' -e ':  Warning:  Using TCM_GetCurrentTaskTreeRefForThisThread() for "THIS"' -e ':  Warning:  Are you certain you want to Terminate on THIS?' -e '^0 Errors and [0-9]\+ Warning[s]\? found\.' -e '\.tdl: In function `' -e ': warning: label `[a-zA-Z_]\+'"' defined but not used" -e ':  Warning:  Using an Iteration-Set of References for Constraint Reference\.' -e ':  Warning:  Using an Iteration-Set of References for Constraint Statement Reference\.'



bombtest:
	@echo ""
	@echo "BombTest:"
	$(JAVA) TDLC -2H BOMB_1.tdl 2>&1 | diff BOMB_1.out -
	@echo "BombTest complete."


clean:
	$(RM) $(TDL_CODE)   $(TDL_HEADERS) $(TDL_BINARIES) \
              $(TDL_OUTPUT) $(TDL_SED_OUTPUT)              \
              $(TDL_DISTRIBUTED_CLEAN_FILES)

reallyclean: clean
	$(RM) $(wildcard #*)  $(wildcard *~)

cleansorted:
ifdef DISTRIBUTED
	@$(RM) $(TDL_SORTED)
endif

wc:
	$(WC) $(TDL_HEADERS)  $(TDL_CODE) < $(DEV_NULL)
	$(WC) $(TDL_PROGRAMS)             < $(DEV_NULL)

regenerate_masters: $(TDL_OUTPUT) $(TDL_SED_OUTPUT)
	$(foreach PROGRAM, $(TDL_FILEBASES), echo "$(PROGRAM)" ; mv $(PROGRAM).master_out $(PROGRAM).master_out.old ; mv $(PROGRAM).master_out.sed $(PROGRAM).master_out.sed.old ; cp $(PROGRAM).out $(PROGRAM).master_out ; cp $(PROGRAM).out.sed $(PROGRAM).master_out.sed ; )

testclasspath:
	@echo $(CLASSPATH)

testTDLC:
	$(JAVA) TDLC -V

echo-%:
	@echo $* == "$($*)"

echoraw-%:
	@echo "$($*)"
