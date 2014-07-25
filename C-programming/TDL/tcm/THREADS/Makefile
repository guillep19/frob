
###
### Copyright (c) 2001, by David B. Apfelbaum.
### All rights reserved.
###
### Standard BSD Open-Source License:
###
### Redistribution and use in source and binary forms,
### with or without modification, are permitted provided
### that the following conditions are met:
###
###  + Redistributions of source code must retain the
###    above copyright notice, this list of conditions
###    and the following disclaimer. 
###
###  + Redistributions in binary form must reproduce the
###    above copyright notice, this list of conditions
###    and the following disclaimer in the documentation
###    and/or other materials provided with the
###    distribution. 
###
###  + Neither the name of David B. Apfelbaum nor the
###    names of any contributors may be used to endorse
###    or promote products derived from this software
###    without specific prior written permission. 
###
### THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS
### AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED
### WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
### IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
### A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT
### SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR ANY
### DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
### CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
### PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
### USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
### HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
### WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
### (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY
### WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
### OF THE POSSIBILITY OF SUCH DAMAGE.
###


ifndef BASE_THREADS_DIR
  BASE_THREADS_DIR = .
endif



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
# If THREADS_USE_SINGLE_DIR is defined, we will (by default) place all object
# in a single directory, and all the library files in a single directory.
#
# If it is NOT defined, we will (by default) place all object/library
# files in their own OS-specific subdirectories.
#
# Naturally, anything and everything can be overriden with the right setenv's.
#
ifdef THREADS_USE_SINGLE_DIR

  ifndef THREADS_OS_DIR
    THREADS_OS_DIR = 
  endif

  ifndef THREADS_OBJECT_DIR
    THREADS_OBJECT_DIR = obj
  endif

else

  ifndef THREADS_OS_DIR
    THREADS_OS_DIR     = $(BASE_OS_DIR)
  endif

  ifndef THREADS_OBJECT_DIR
    THREADS_OBJECT_DIR = obj/$(THREADS_OS_DIR)
  endif

endif

ifndef THREADS_LIBRARY_DIR
  THREADS_LIBRARY_DIR = $(BASE_THREADS_DIR)/lib/$(THREADS_OS_DIR)
endif


       RM = rm -f
    RMDIR = rm -ri
    MKDIR = mkdir -p
       AR = ar
 AR_FLAGS = rv
   RANLIB = ranlib

OBJ_ONLY = Mutex MutexedObject Semaphore Thread
SKIP_OUTPUT = test_mutextiming

OBJS = $(patsubst %,$(THREADS_OBJECT_DIR)/%.o,$(OBJ_ONLY))

POSSIBLE_BINS = $(patsubst %.C,%,$(wildcard *.C))

BINS = $(filter-out $(OBJ_ONLY),$(POSSIBLE_BINS))

OUTPUT_BINS = $(filter-out $(SKIP_OUTPUT), $(BINS))

OUTPUT = $(patsubst %,%.out,$(OUTPUT_BINS))
SEDOUT = $(patsubst %.out,%.out.sed,$(OUTPUT))

FLAGS = -g -O2 -Wall -W -Wswitch -Wuninitialized -Wparentheses -fno-implicit-templates -I.

LIBS = -lpthread

DIFF_ARGS =

LIBRARY_FILE = $(THREADS_LIBRARY_DIR)/libcxxThreads.a

CXX = g++


.PRECIOUS: $(BINS) $(OUTPUT) $(SEDOUT)


default: lib

all: $(OBJS) $(BINS) lib

clean:
	$(RM) *.o $(OBJS) $(BINS) $(OUTPUT) $(SEDOUT) $(LIBRARY_FILE)

reallyclean: clean
	$(RM) *~
	-$(RMDIR) $(THREADS_OBJECT_DIR)
	-$(RMDIR) $(THREADS_LIBRARY_DIR)

lib $(LIBRARY_FILE): $(OBJS)
	@if [ ! -d $(THREADS_LIBRARY_DIR) ]; then $(MKDIR) $(THREADS_LIBRARY_DIR); fi
	$(RM)                 $(LIBRARY_FILE)
	$(AR)     $(AR_FLAGS) $(LIBRARY_FILE) $(OBJS)
	$(RANLIB)             $(LIBRARY_FILE)

$(OBJ_ONLY):
	@echo -n ""

$(THREADS_OBJECT_DIR)/%.o : %.C %.H
	@if [ ! -d $(THREADS_OBJECT_DIR) ]; then $(MKDIR) $(THREADS_OBJECT_DIR); fi
	$(CXX) -c $< -o $@ $(FLAGS)
	@echo " "

% : %.C $(OBJS)
	$(CXX) $< -o $@ $(FLAGS) $(OBJS) $(LIBS)

### Special case.
test_mutextiming: test_mutextiming.C Mutex.H Thread_Common.H
	$(CXX) $< -o $@ $(FLAGS) $(LIBS)

%.out : %
	./$< > $@ 2>&1

%.out.sed : %.out
	sed -e 's|0x[0-9a-fA-F]\{0,8\}|0x........|g'  < $<   > $<.sed
	@echo " "

diff: $(SEDOUT)
	@ ( $(foreach PROGRAM, $(OUTPUT_BINS), echo "$(PROGRAM):" ; diff $(DIFF_ARGS) $(PROGRAM).master_out.sed $(PROGRAM).out.sed; echo " ";) )
	@echo " "
	@echo "Some differences with test_thread1 are to be expected, "
	@echo "as it utilizes/demonstrates a race condtion."
	@echo " "
	@echo "A single-line difference in test_semaphore3 in which the"
	@echo "sem_getvalue is 1 instead of 2 is also normal. It is the"
	@echo "product of a deliberately attempted race condition not quite"
	@echo "managing to print the value in time before it gets decremented."
	@echo " "

test qt quicktest: $(SEDOUT)
	@echo " "
	@echo " "
	@ ( $(foreach PROGRAM, $(OUTPUT_BINS), echo -n "$(PROGRAM):     	"; diff $(DIFF_ARGS) $(PROGRAM).master_out.sed $(PROGRAM).out.sed | wc | tr -d '\012'; [ "`diff $(PROGRAM).master_out.sed $(PROGRAM).out.sed | wc -l | tr -d ' '`" = "0" ] || echo -n "          ********************" ; echo " "; ) )
	@echo " "
	@echo "Some differences with test_thread1 are to be expected, "
	@echo "as it utilizes/demonstrates a race condtion."
	@echo " "
	@echo "A single-line difference in test_semaphore3 in which the"
	@echo "sem_getvalue is 1 instead of 2 is also normal. It is the"
	@echo "product of a deliberately attempted race condition not quite"
	@echo "managing to print the value in time before it gets decremented."
	@echo " "


regenerate_masters:
	$(foreach PROGRAM, $(OUTPUT_BINS), cp $(PROGRAM).out $(PROGRAM).master_out ; cp $(PROGRAM).out.sed $(PROGRAM).master_out.sed ; )


debug:
	@echo $(OBJS)
	@echo $(BINS)
	@echo $(OUTPUT)
	@echo $(SEDOUT)

