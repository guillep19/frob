#############################################################################
#       $Id: GNUmakefile,v 1.4 2009/01/15 17:00:03 reids Exp $
# $Revision: 1.4 $
#     $Date: 2009/01/15 17:00:03 $
#   $Author: reids $
#    $State: Exp $
#   $Locker:  $
#
# PROJECT:	TCM
#
# FILE:		GNUmakefile
#		Based on Erebus prototypical Makefile
#		Makefile.proto,v 1.1 1992/06/16 22:50:21 jwest Exp 
#
# DESCRIPTION:
# Makes the tcm libraries and central server.
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
# Revision 1.4  2009/01/15 17:00:03  reids
# Released under simplified BSD open source license
#
# Revision 1.3  2001/11/16 18:57:31  trey
# commented out #ifndef statement that was causing problems during recursive gmake
#
# Revision 1.2  1999/06/06 08:18:22  reids
# Changed occurrences of "TCA" to "TCM"
#
# Revision 1.1  97/11/21  14:06:01  reids
# First release of TCM -- seems to be a stable version
# 
# Revision 1.19  1996/02/11  21:34:31  rich
# Updated GNUmakefiles for faster complilation.  Use FAST_COMPILE=1 for
# routine recompiles.
#
# Revision 1.18  1995/10/07  19:05:12  rich
# Pre-alpha release of tca-8.2.
# Added PROJECT_DIR..
#
# Revision 1.17  1995/07/08  17:50:18  rich
# Linux Changes.  Also added GNUmakefile.defs.
#
# Revision 1.16  1995/07/06  21:14:16  rich
# Solaris and Linux changes.
#
# Revision 1.15  1995/06/14  17:50:10  rich
# Added updateVersion.
#
# Revision 1.14  1995/06/14  03:18:35  rich
# Added DBMALLOC_DIR.
#
# Revision 1.13  1995/04/09  20:27:27  rich
# Added /usr/local/include and /usr/local/lib to the paths for compiling
# for sunOS machines. (Support for new vendor OS).
#
# Revision 1.12  1995/04/07  05:01:53  rich
# Fixed GNUmakefiles to find the release directory.
#
# Revision 1.11  1995/04/04  19:40:56  rich
# Added sgi support.
# Fixed rule to compile module libs.
# Added some support for C++ programs.
#
# Revision 1.10  1995/03/30  15:31:56  rich
# DBMALLOC works.  To use "gmake -k -w DBMALLOC=DBMALLOC install"
#
# Revision 1.9  1995/03/18  15:10:37  rich
# Fixed updateVersion script so it can be run from any directory.
#
# Revision 1.8  1995/03/16  18:04:51  rich
# Merged in changes to the 7.9 branch.
# Changed the VERSION_ to TCA_VERSION_
#
# Revision 1.7  1995/01/31  00:55:09  rich
# Fixed CFLAGS so -gstabs comes before -g for i386_mach.
#
# Revision 1.6  1995/01/18  22:56:03  rich
# Now look for cvs in /usr/local before /usr/misc.
#
# Revision 1.5  1994/11/02  21:24:52  rich
# Now works for linux machines (i486).
# Got afs to work on alpha (and hopefully other vendor OS's)
#
# Revision 1.4  1994/05/31  03:21:33  rich
# Removed CFLAGS_sun4.
# fixLinks now works with /usr/bin/csh.
#
# Revision 1.3  1994/05/11  01:54:45  rich
# Moved all the version information to the src directory.
#
# Revision 1.2  1994/05/06  05:15:05  rich
# Removed duplicate log entries.
# Fixed TAGS file generation.
#
# Revision 1.1  1994/05/06  04:45:06  rich
# Added a GNUmakefile and other utilities to this directory.
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
MODULE_DIR	= etc
PROJECT		= tcm
MODULE		= tcm
SUBDIRS		=

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
PUBLIC_BINS		= 
PUBLIC_LIBS		= 
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

# Generated files - not installed
PRIVS	= 

OBJS	= 

# Source files - not installed

SRCS		= 

PRIVATE_INCS	= 

MISC		= GNUmakefile GNUmakefile.std tcvs\
		GNUmakefile.defs

##########
# Default Target
##########

all:: srcs libs bins privs

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

##########
# Include standard target definitions and RCS rules
##########

include $(STD_FILE)
