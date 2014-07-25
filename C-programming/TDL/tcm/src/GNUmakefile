#############################################################################
#       $Id: GNUmakefile,v 1.16 2009/01/15 17:00:04 reids Exp $
# $Revision: 1.16 $
#     $Date: 2009/01/15 17:00:04 $
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
# Revision 1.16  2009/01/15 17:00:04  reids
# Released under simplified BSD open source license
#
# Revision 1.15  2008/07/16 06:15:09  reids
# Updates for newer (pickier) compilers
#
# Revision 1.14  2002/12/23 02:23:19  da0g
# Added tcmThread.cc
#
# Revision 1.13  2002/01/18 14:20:18  reids
# Upgraded tracing options
#
# Revision 1.12  2001/11/16 18:57:52  trey
# commented out #ifndef statement that was causing problems during recursive gmake
#
# Revision 1.11  2001/10/23 22:52:57  da0g
# Added Threading support.  Cached lastchild.
#
# Revision 1.10  2001/07/24 12:49:02  reids
# Enable monitor client data to be sent with all the monitor activation
#   functions (ActivateAt, ActivateIn, ActivateInAfter).  Fixed bugs in
#   the way monitor data was being set.
# Separate templates for virtual nodes from regular templates (compiled in
#   only when DISTRIBUTED option is used -- makes code cleaner).
#
# Revision 1.9  1999/10/21 21:32:31  reids
# Changed list.cc and list.h to tcmList.cc and tcmList.h to avoid conflicts
#   with other occurrences of those names in other packages.
#
# Revision 1.8  1999/08/04 14:00:16  reids
# Distributed version of TCM.  Provides the ability for one process to
#  add tasks, constraints, exception handlers, etc. with another process.
#  The distributed version is created by compiling with -DDISTRIBUTED flag,
#   which can be gotten by "gmaking" with DISTRIBUTED=1.
#
# Revision 1.7  1999/06/06 13:48:07  reids
# Changes to TCM 1.3.0:
#   New list data structures.
#   New, generalized logging interface.
#   Fixed firstChild to work if there are no children.
#   Fixed code to deallocate a node.
#   In certain circmumstances, don't send termination signal when task
#     itself terminating.
#
# Revision 1.6  98/08/05  10:57:25  reids
# Made a compiler option for printing out debugging information.
#   Created macros for TCM_TerminateAtAfter and TCM_DelayUntilAfter, for
#   backwards compatibility.
# 
# Revision 1.5  98/03/18  14:46:09  reids
# Change to support Solaris.
# 
# Revision 1.4  98/01/30  14:50:46  reids
# Updated to compile under gcc 2.7.2 and under Linux.
# Also, made STRING "const char *" and changed API to take const arguments,
#   where applicable.
# 
# Revision 1.3  97/12/29  17:06:09  reids
# Version that has the basic functionality needed to support TDL.
# 
# Revision 1.2  97/12/04  17:49:59  reids
# Another fairly stable version (except that monitors do not quite work)
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
ifeq ($(ARCH),sparcSol2)
DEPEND_FILE	= GNUmakefile.$(ARCH)
else
DEPEND_FILE	= GNUmakefile.$(THIS_MACHINE)
endif
DEPEND_PREFIX	= $(ODIR)/
VERSION_FILE	= tcm.h
VERSION_PREFIX	= TCM

#############################################################################
# Where are the standard files?
#############################################################################

ifndef FAST_COMPILE
PWD 	:= $(shell /bin/pwd)
endif

#ifndef STD_FILE
STD_FILE := $(shell ls $(BASE_DIR)/etc/GNUmakefile.std | grep -v "not found")
ifeq ($(STD_FILE),)
STD_FILE := $(PROJECT_DIR)/etc/GNUmakefile.std
endif
DEFS_FILE := $(shell ls $(BASE_DIR)/etc/GNUmakefile.defs | grep -v "not found")
ifeq ($(DEFS_FILE),)
DEFS_FILE := $(PROJECT_DIR)/etc/GNUmakefile.defs
endif
#endif

##########
# Include standard definitions
##########

include $(DEFS_FILE)

##########
# Override defaults here.
##########

#DEFINES_DEBUG = -DTRACE

DEFINES += $(DEFINES_$(DEBUG))

ifdef TRACE
DEFINES += -D$(TRACE)
endif

ifdef DISTRIBUTED
DEFINES  += -DDISTRIBUTED
INC_PATH += -I$(IPC_DIR)/include
endif

ifdef THREADED
DEFINES  += -DTHREADED
INC_PATH += -I$(THREADS_DIR)
endif

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
PUBLIC_LIBS		= libtcm.a $(PUBLIC_LIBS_$(THIS_OS)) \
			$(PUBLIC_LIBS_$(THIS_MACHINE))
PUBLIC_LINTS		= 

MODULE_LIBS		= 
MODULE_LINTS		= 

# Source files - installed
PUBLIC_INCS		= tcmBasics.h tcmHandle.h tcm.h
PUBLIC_MANS		= 
PUBLIC_DOCS		= 

MODULE_INCS		= 
MODULE_DOCS		= 

INCS			= $(MODULE_INCS) $(PUBLIC_INCS)

# Source files - not installed

SRCS =  tcmBasics.cc tcmLogging.cc tcmList.cc event.cc action.cc taskTree.cc \
	agenda.cc tcmGlobal.cc tplConstr.cc exception.cc monitor.cc \
	external.cc tcm.cc tcmLogInterface.cc tcmHandle.cc tcmThread.cc
ifdef DISTRIBUTED
SRCS += virtual.cc distributed.cc
endif
ifdef THREADED
SRCS += ThreadManager.cc
endif

IMPLICIT_TEMPLATES = 1
ifdef IMPLICIT_TEMPLATES
CPPFLAGS +=  -fno-implicit-templates
SRCS += templates.cc
ifdef DISTRIBUTED
SRCS += vtemplates.cc
endif

ifdef USE_RTI_COMPILER
$(ODIR)/templates.o: templates.cc
	$(CXX) $(CPPFLAGS) -o $@ -c templates.cc
else
$(ODIR)/templates.o: templates.cc
	$(CCC) $(CCFLAGS) -o $@ -c templates.cc
$(ODIR)/tcmThread.o: tcmThread.cc
	$(CCC) $(CCFLAGS) -o $@ -c tcmThread.cc
$(ODIR)/vtemplates.o: vtemplates.cc
	$(CCC) $(CCFLAGS) -o $@ -c vtemplates.cc
endif

endif

PRIVATE_INCS = tcmPriv.h tcmLogging.h event.h action.h \
	taskTree.h agenda.h tcmGlobal.h tplConstr.h exception.h monitor.h \
	tcmList.h queue.h tcmLogInterface.h virtual.h distributed.h \
	ThreadManager.h tcmThread.h

MISC	= GNUmakefile

# Generated files - not installed

_OBJS  = $(SRCS:.cc=.o)
OBJS = $(patsubst %,$(ODIR)/%,$(_OBJS))

##########
# Default Target
##########

all:: srcs libs bins privs

# The vx works compiler constants are in etc/GNUmakefile.defs.
vx:  $(VODIR)/.force $(LDIR)/.force $(BDIR)/.force \
	$(PUBLIC_LIB_DIR)/libtcm_vx.a 

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

$(LDIR)/libtcm.a: $(OBJS)
	$(RM) $@
	$(AR) $(ARFLAGS) $@ $(OBJS)
	$(RANLIB) $@

$(PUBLIC_LIB_DIR)/libtcm.a: $(OBJS)

$(LDIR)/libtcm_vx.a: $(OBJS)
	rm -f $(LDIR)libtcm_vx.a
	$(VX_AR) rvu $(LDIR)/libtcm_vx.a $(OBJS)
	$(VX_RANLIB) $(LDIR)/libtcm_vx.a

$(PUBLIC_LIB_DIR)/libtcm_vx.a: $(LDIR)/libtcm_vx.a

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

##########
# Include standard target definitions and RCS rules
##########

include $(STD_FILE)
