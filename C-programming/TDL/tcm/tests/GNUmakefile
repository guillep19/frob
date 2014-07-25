#############################################################################
#       $Id: GNUmakefile,v 1.11 2009/01/15 17:00:06 reids Exp $
# $Revision: 1.11 $
#     $Date: 2009/01/15 17:00:06 $
#   $Author: reids $
#    $State: Exp $
#   $Locker:  $
#
# PROJECT:	Task Control Management
#
# FILE:		GNUmakefile
#		Based on Erebus prototypical Makefile
#		Makefile.proto,v 1.1 1992/06/16 22:50:21 jwest Exp 
#
# DESCRIPTION: Makes the tcm libraries.
#
# EXPORTS:
#	The following targets are available as part of Makefile.std:
#		all bins libs privs objs
#		clean install
#		lint depend
#
# Copyright (c) 2008, Carnegie Mellon University
#     This software is distributed under the terms of the 
#     Simplified BSD License (see tcm/LICENSE.TXT)
#
# HISTORY:
#
# $Log: GNUmakefile,v $
# Revision 1.11  2009/01/15 17:00:06  reids
# Released under simplified BSD open source license
#
# Revision 1.10  2003/04/17 23:41:23  da0g
# Threads library directory can be OS-specific.
#
# Revision 1.9  2003/04/17 21:13:06  da0g
# Added CENTRAL_BIN_DIR to DEFINES, derived from CENTRAL_PATH or IPC_DIR.
# Added TCM_TESTS_DIR to DEFINES.
# Added distrTest distrTestA distrTestB treeTest to DELETES for "make clean".
# Remove automatic overriding of IPC_DIR from etc/GNUmakefile.defs.
# Added utilization of THREADS_DIR from etc/GNUmakefile.defs.
# Added echo- and echoraw- make targets.  (For debugging.)
#
# Revision 1.8  2002/03/26 05:20:15  reids
# Updates to regressions tests
#
# Revision 1.7  2001/11/20 19:19:59  reids
# Better ways to make distributed and non-distributed binaries
#
# Revision 1.6  2000/02/03 21:29:10  reids
# Generalized by moving "HOME_DIR" into the makefile.
#
# Revision 1.5  1999/08/05 21:28:41  reids
# Removed all dependence on TDL and IPC.
# Enabled distributed test files to use either TCA or IPC as the
# 	communication package.
#
# Revision 1.4  1999/08/04 16:50:36  reids
# Changes for TCM 2.0 -- for the .log files, just changes to the header line;
#   The .dlog files are produced by the distributed TCM (2 processes)
#
# Revision 1.3  98/03/06  12:43:22  reids
# Modifications to support Solaris.
# 
# Revision 1.2  97/12/29  16:20:59  reids
# Version that has the basic functionality needed to support TDL.
# 
# Revision 1.1  97/12/04  17:52:08  reids
# TCM Test Files
# 
# Revision 1.1  97/11/21  14:06:18  reids
# First release of TCM -- seems to be a stable version
# 
#
#############################################################################

#############################################################################
# Module Configuration Macros
#############################################################################

PROJECT_DIR	= 

INSTALL_DIR	= ..
ifndef BASE_DIR
BASE_DIR	:= $(shell cd $(INSTALL_DIR);/bin/pwd)
endif
MODULE_DIR	= src
PROJECT		= tcm
MODULE		= tcm
SUBDIRS		=
DEPEND_FILE	= GNUmakefile.$(THIS_MACHINE)
DEPEND_PREFIX	= $(ODIR)/
VERSION_FILE	= ../tcm.h
VERSION_PREFIX	= TCM

#############################################################################
# Where are the standard files?
#############################################################################

ifndef FAST_COMPILE
PWD 	:= $(shell /bin/pwd)
endif

ifndef STD_FILE
STD_FILE := $(shell ls $(BASE_DIR)/etc/GNUmakefile.std | grep -v "not found")
ifeq ($(STD_FILE),)
STD_FILE := $(PROJECT_DIR)/etc/GNUmakefile.std
endif
DEFS_FILE := $(shell ls $(BASE_DIR)/etc/GNUmakefile.defs | grep -v "not found")
ifeq ($(DEFS_FILE),)
DEFS_FILE := $(PROJECT_DIR)/etc/GNUmakefile.defs
endif
endif

##########
# Include standard definitions
##########

include $(DEFS_FILE)

##########
# Override defaults here.
##########

ifdef USE_TCA
DEFINES += -DUSE_TCA
endif

DEFINES += -DHOME_DIR=\"$(HOME)\" 

# Move these to here too.
ifdef CENTRAL_PATH
  DEFINES += -DCENTRAL_BIN_DIR=\"$(shell echo "$(CENTRAL_PATH)"|sed 's|/central||g')\"
else
  DEFINES += -DCENTRAL_BIN_DIR=\"$(IPC_DIR)/bin/$(THIS_OS)-$(SUFFIX)\"
endif

DEFINES += -DTCM_TESTS_DIR=\"$(BASE_DIR)/tests\"

# "make clean" should remove these too.
DELETES += distrTest distrTestA distrTestB treeTest

##########
# File Definition Macros.  Deletion of generated files assumed
#    to be o.k.  Source files will never be deleted.
##########

# Generated files - installed
PUBLIC_BINS_SunOS	= 
PUBLIC_BINS		= $(PUBLIC_BINS_$(THIS_OS))
#Uncomment the next line if you want to build the lisp library.
PUBLIC_LIBS_LISP	= 
PUBLIC_LIBS_Solaris	= 
PUBLIC_LIBS_SunOS	= $(PUBLIC_LIBS_LISP)
PUBLIC_LIBS_linux	= 
PUBLIC_LIBS_RISC	= $(PUBLIC_LIBS_LISP)
PUBLIC_LIBS		= $(PUBLIC_LIBS_$(THIS_OS)) \
			$(PUBLIC_LIBS_$(THIS_MACHINE))
PUBLIC_LINTS		= 

MODULE_LIBS		= 
MODULE_LINTS		= 

# Source files - installed
PUBLIC_INCS		= 
PUBLIC_MANS		= 
PUBLIC_DOCS		= 

MODULE_INCS		= 
MODULE_DOCS		= 

INCS			= $(MODULE_INCS) $(PUBLIC_INCS)

# This should be defined from the DEFS_FILE
ifndef IPC_DIR
  IPC_DIR  = $(HOME)/code/ipc
endif

INC_PATH += -I../include -I../src -I$(IPC_DIR)/include

# Source files - not installed

SRCS =  handleTest.cc treeTest.cc distrTestA.cc distrTestB.cc distrTest.cc

#IMPLICIT_TEMPLATES = 1
ifdef IMPLICIT_TEMPLATES
CPPFLAGS +=  -fno-implicit-templates
endif

PRIVATE_INCS = 

MISC	= GNUmakefile

# Generated files - not installed

_OBJS = $(SRCS:.cc=.o)
OBJS = $(patsubst %,$(ODIR)/%,$(_OBJS))

DISTR_LIBS = -lipcInterface -lipc

ifdef THREADED
ifndef THREADS_DIR
  THREADS_DIR = ../THREADS
endif
ifndef THREADS_LIBRARY_DIR
  THREADS_LIBRARY_DIR = $(THREADS_DIR)/lib
endif
DISTR_LIBS += -L$(THREADS_LIBRARY_DIR) -lcxxThreads -lpthread
LIBRARIES += -L$(THREADS_LIBRARY_DIR) -lcxxThreads -lpthread
endif

##########
# Default Target
##########

all:: srcs libs bins privs

# The vx works compiler constants are in etc/GNUmakefile.defs.
vx:  $(VODIR)/.force $(LDIR)/.force $(BDIR)/.force

##########
# User Defined Targets
##########

# Define rules for all files listed in BINS, LIBS, and LINTS in this section.
# Example executable, object, library, and combined objects are shown below:
#
#  $(BDIR)/sample: file1.o file2.o $(LIBDEP)
#	$(LINK.c) -o $@ file1.o file2.o $(LIBRARIES)
#
#  $(ODIR)/alternateName.o: file1.c
#	$(COMPILE.c) -o $@ file1.c
#
#  $(LDIR)/libsample.a: file1.o file2.o
#	-$(RM) $@
#	$(AR) $(ARFLAGS) $@ file1.o file2.o
#	$(RANLIB) $@
#
#  $(ODIR)/combined.o: $(ODIR)/part1.o $(ODIR)/part2.o
#	$(LD) -r -o $@ $(ODIR)/part1.o $(ODIR)/part2.o; chmod a-x $@

#
# Targets for building a release
#

#
# the lisp version of the code is special.
#

$(LODIR)/%.o: %.cc
	$(COMPILE.c) -I$(LISP_INC_DIR) $(CFLAGS_LISP) -o $@ $<

#
# So is the version for the shared library.
#

$(SODIR)/%.o: %.cc
	$(COMPILE.c) -fpic -o $@ $<

$(VODIR)/%.o: %.cc
	$(COMPILE.vx) -o $@ $<

$(ODIR)/handleTest.o : handle.h handleTest.cc

handleTest: $(ODIR)/handleTest.o
	$(LINK.cc) -o $@ $(ODIR)/handleTest.o

$(ODIR)/distrTestA.o : distrTestA.cc ../lib/libtcm.a distrTests.h
	$(COMPILE.cc) -DDISTRIBUTED -o $@ $<

distrTestA: $(ODIR)/distrTestA.o $(PUBLIC_LIB_DIR)/libtcm.a
	$(LINK.cc) -o $@ $(ODIR)/distrTestA.o -L$(PUBLIC_LIB_DIR) \
		$(LIBRARIES) $(DISTR_LIBS)

$(ODIR)/distrTestB.o : distrTestB.cc ../lib/libtcm.a distrTests.h
	$(COMPILE.cc) -DDISTRIBUTED -o $@ $<

distrTestB: $(ODIR)/distrTestB.o $(PUBLIC_LIB_DIR)/libtcm.a
	$(LINK.cc) -o $@ $(ODIR)/distrTestB.o -L$(PUBLIC_LIB_DIR) \
		$(LIBRARIES) $(DISTR_LIBS)

distrTest: $(ODIR)/distrTest.o $(PUBLIC_LIB_DIR)/libtcm.a distrTests.h \
	   distrTestA distrTestB
	$(LINK.cc) -o $@ $(ODIR)/distrTest.o -L$(PUBLIC_LIB_DIR) $(LIBRARIES)

distr: distrTestA distrTestB distrTest

treeTest: $(ODIR)/treeTest.o $(PUBLIC_LIB_DIR)/libtcm.a
	$(LINK.cc) -o $@ $(ODIR)/treeTest.o -L$(PUBLIC_LIB_DIR) $(LIBRARIES)

exceptions.c : exceptions.tdl
	java TDLC -Hyo exceptions.h exceptions.tdl 
	java TDLC -Cyo exceptions.c exceptions.tdl 

$(ODIR)/exceptions.o : exceptions.c
	$(COMPILE.cc) -DDISTRIBUTED=DISTRIBUTED -I/usr/reids/code/tdl/src -o $@ $<

exceptions: $(ODIR)/exceptions.o $(PUBLIC_LIB_DIR)/libtcm.a
	$(LINK.cc) -o $@ $(ODIR)/exceptions.o -L$(PUBLIC_LIB_DIR) \
	-L/usr/reids/code/tdl/lib/Linux-2.2 -ltdl -lipcInterface -lipc $(LIBRARIES)

da0g.bug2.c : da0g.bug2.tdl
	java TDLC -Hyo da0g.bug2.h da0g.bug2.tdl 
	java TDLC -Cyo da0g.bug2.c da0g.bug2.tdl 

$(ODIR)/da0g.bug2.o : da0g.bug2.c
	$(COMPILE.cc) -DDISTRIBUTED=DISTRIBUTED -I/usr/reids/code/tdl/src -o $@ $<

da0g.bug2: $(ODIR)/da0g.bug2.o $(PUBLIC_LIB_DIR)/libtcm.a
	$(LINK.cc) -o $@ $(ODIR)/da0g.bug2.o -L$(PUBLIC_LIB_DIR) \
	-L/usr/reids/code/tdl/lib/Linux-2.2 -ltdl -lipcInterface -lipc $(LIBRARIES)

##########
# Include standard target definitions and RCS rules
##########

include $(STD_FILE)


# Standard Makefile-testing macro:
echo-%:
	@echo $* == "\"$($*)\""

echoraw-%:
	@echo "$($*)"
