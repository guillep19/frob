#############################################################################
#       $Id: GNUmakefile,v 1.5 2009/01/15 17:00:02 reids Exp $
# $Revision: 1.5 $
#     $Date: 2009/01/15 17:00:02 $
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
# Revision 1.5  2009/01/15 17:00:02  reids
# Released under simplified BSD open source license
#
# Revision 1.4  2002/08/08 23:25:54  reids
# Don't make libtcaInterface by default any more
#
# Revision 1.3  2002/06/19 13:40:31  reids
# Fixed dependency
#
# Revision 1.2  2001/11/17 02:42:42  trey
# fixed so we look for GNUmakefile.{defs,std} in the right place on a recursive make
#
# Revision 1.1  1999/08/05 21:30:24  reids
# Abstract interface to higher-level communication packages.
# Currently supports IPC and TCA -- hopefully can extend to support
#   other packages, as well (TCX, RTC, NDDS).
#
#
#############################################################################

#############################################################################
# Module Configuration Macros
#############################################################################

INSTALL_DIR	= ..
ifndef BASE_DIR
BASE_DIR	:= $(shell cd $(INSTALL_DIR);/bin/pwd)
endif
MODULE_DIR	= commInterface
PROJECT		= tcm
MODULE		= tcm
SUBDIRS		=
ifeq ($(ARCH),sparcSol2)
DEPEND_FILE	= GNUmakefile.$(ARCH)
else
DEPEND_FILE	= GNUmakefile.$(THIS_MACHINE)
endif
DEPEND_PREFIX	= $(ODIR)/
VERSION_FILE	= tcmPriv.h
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

##########
# File Definition Macros.  Deletion of generated files assumed
#    to be o.k.  Source files will never be deleted.
##########

# Generated files - installed
PUBLIC_BINS_SunOS	= 
PUBLIC_BINS		= 
#Uncomment the next line if you want to build the lisp library.
PUBLIC_LIBS_LISP	= 
PUBLIC_LIBS_Solaris	= 
PUBLIC_LIBS_SunOS	= 
PUBLIC_LIBS_linux	= 
PUBLIC_LIBS_RISC	= 
PUBLIC_LIBS		= libipcInterface.a # libtcaInterface.a
PUBLIC_LINTS		= 

MODULE_LIBS		= 
MODULE_LINTS		= 

# Source files - installed
PUBLIC_INCS		= commInterface.h ipcInterface.h tcaInterface.h
PUBLIC_MANS		= 
PUBLIC_DOCS		= 

MODULE_INCS		= 
MODULE_DOCS		= 

INCS			= $(MODULE_INCS) $(PUBLIC_INCS)

# Source files - not installed

IPC_SRCS = commInterface.cc ipcInterface.cc
TCA_SRCS = commInterface.cc tcaInterface.cc
SRCS     = $(IPC_SRCS) $(TCA_SRCS)

PRIVATE_INCS = 

MISC	= GNUmakefile

# Generated files - not installed

_IPC_OBJS = $(IPC_SRCS:.cc=.o)
IPC_OBJS = $(patsubst %,$(ODIR)/%,$(_IPC_OBJS))
_TCA_OBJS = $(TCA_SRCS:.cc=.o)
TCA_OBJS = $(patsubst %,$(ODIR)/%,$(_TCA_OBJS))

OBJS = $(IPC_OBJS) $(TCA_OBJS)

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

$(LDIR)/libipcInterface.a: $(IPC_OBJS)
	$(RM) $@
	$(AR) $(ARFLAGS) $@ $(IPC_OBJS)
	$(RANLIB) $@

$(PUBLIC_LIB_DIR)/libipcInterface.a: $(IPC_OBJS)

$(LDIR)/libtcaInterface.a: $(TCA_OBJS)
	$(RM) $@
	$(AR) $(ARFLAGS) $@ $(TCA_OBJS)
	$(RANLIB) $@

$(PUBLIC_LIB_DIR)/libtcaInterface.a: $(TCA_OBJS)

install.ipc : $(PUBLIC_LIB_DIR)/libipcInterface.a

install.tca : $(PUBLIC_LIB_DIR)/libtcaInterface.a

#install :: install.ipc install.tca

#
# Targets for building a release
#

##########
# Include standard target definitions and RCS rules
##########

include $(STD_FILE)
