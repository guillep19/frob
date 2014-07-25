#############################################################################
#       $Id: GNUmakefile,v 1.7 2009/01/15 17:00:02 reids Exp $
# $Revision: 1.7 $
#     $Date: 2009/01/15 17:00:02 $
#   $Author: reids $
#    $State: Exp $
#   $Locker:  $
#
# PROJECT:	TCA
#
# FILE:		GNUmakefile
#		Based on Erebus prototypical Makefile
#		Makefile.proto,v 1.1 1992/06/16 22:50:21 jwest Exp 
#
# DESCRIPTION:
# Makes the tca libraries and central server.
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
# Revision 1.7  2009/01/15 17:00:02  reids
# Released under simplified BSD open source license
#
# Revision 1.6  2002/06/19 13:41:41  reids
# Changed PROJECT_DIR to not refer to AFS space
#
# Revision 1.5  2001/11/17 02:41:47  trey
# made symlink update for cmu-install more specific
#
# Revision 1.4  2001/11/16 21:34:35  trey
# bug fix in cmu-install commands
#
# Revision 1.3  2001/11/16 21:24:18  trey
# added cmu-install target for use with the FIRE project
#
# Revision 1.2  2001/11/16 18:56:54  trey
# removed spurious reference to README
#
# Revision 1.1  1997/11/21 14:04:43  reids
# First release of TCM -- seems to be a stable version
#
# Revision 1.26  1996/01/08  18:20:42  rich
# Remove old README file.
#
# Revision 1.25  1995/12/17  20:18:58  rich
# Changed Makefile to Makefile.generic.
#
# Revision 1.24  1995/10/10  00:41:42  rich
# Fixed typo.
#
# Revision 1.23  1995/10/07  18:59:01  rich
# Pre-alpha release of tca-8.2.
# Added PROJECT_DIR.
#
# Revision 1.22  1995/07/08  17:49:37  rich
# Linux Changes.  Also added GNUmakefile.defs.
#
# Revision 1.21  1995/07/06  21:13:25  rich
# Solaris and Linux changes.
#
# Revision 1.20  1995/06/14  03:18:05  rich
# Added DBMALLOC_DIR.
#
# Revision 1.19  1995/04/17  16:32:39  rich
# Adding lisp as a subdirectory so it gets included in the tar file.
#
# Revision 1.18  1995/04/09  20:26:58  rich
# Added /usr/local/include and /usr/local/lib to the paths for compiling
# for sunOS machines. (Support for new vendor OS).
#
# Revision 1.17  1995/04/07  05:01:14  rich
# Fixed GNUmakefiles to find the release directory.
#
# Revision 1.16  1995/04/04  19:39:48  rich
# Added sgi support.
#
# Revision 1.15  1995/03/30  15:31:23  rich
# DBMALLOC works.  To use "gmake -k -w DBMALLOC=DBMALLOC install"
#
# Revision 1.14  1995/03/18  15:09:43  rich
# Fixed updateVersion script so it can be run from any directory.
#
# Revision 1.13  1995/03/16  18:04:02  rich
# Merged in changes to the 7.9 branch.
# Changed the VERSION_ to TCA_VERSION_
#
# Revision 1.12  1995/01/30  16:13:16  rich
# Change order of CFLAGS components so -gstabs comes after -g for i386 mach.
#
# Revision 1.11  1995/01/25  21:42:31  rich
# Changing version to 8.0 to start developement.
#
# Revision 1.10  1995/01/24  23:57:36  rich
# Release of tca 7.9.  Mostly speed improvements.
# The cvs binaries may now be located in /usr/local.
#
# Revision 1.9  1994/11/03  06:46:09  rich
# Removed duplicate log entries.
#
# Revision 1.8  1994/11/02  22:50:39  rich
# Minor version number should have been incremented.
#
# Revision 1.7  1994/11/02  21:22:43  rich
# Now works for linux machines (i486).
# Made libc.h and tcaMatrix.h module includes.
# Got afs tp work on alpha (and hopefully other vendor OS's)
# Added generic Makefiles.
#
# Revision 1.6  1994/05/31  04:04:05  rich
# Removed -fpic.
#
# Revision 1.5  1994/05/31  03:20:28  rich
# Added new suddirs tools and tutorial
#
# Revision 1.4  1994/05/11  01:32:02  rich
# Put the version information only in the src directory GNUmakefile.
#
# Revision 1.3  1994/05/06  07:39:33  rich
# Create both tar.gz and tar.Z ftp files.
#
# Revision 1.2  1994/05/06  05:14:29  rich
# Removed duplicate log entries.
#
# Revision 1.1  1994/05/06  04:44:00  rich
# Updated the README.
# Recursion works in the GNUmakefile.
# fixLinks and makeTAGS were moved to the etc directory.
#
# Revision 1.3  1994/05/05  00:54:17  rich
# Get dependency correct.
#
# Revision 1.2  1994/05/05  00:49:32  rich
# Removed duplicate includes.
#
# Revision 1.1  1994/05/05  00:45:45  rich
# Added a gmake makefile GNUmakefile so that the system can be easily
# compiled on different machines.
# Can now create the targets: tarfile and ftp for creating versions for export.
#
#############################################################################

.KEEP_STATE:

#############################################################################
# Module configuration.
#############################################################################

INSTALL_DIR	= .
BASE_DIR	:= $(shell cd $(INSTALL_DIR);/bin/pwd)
MODULE_DIR	= 
PROJECT		= tca
PROJECT_DIR	= .
MODULE		= 
SUBDIRS		= src utils etc test doc tutorial tools lisp commInterface

#############################################################################
# Standard Makefile Includes
#############################################################################

PWD 	:= $(shell /bin/pwd)
STD_FILE := $(shell ls $(INSTALL_DIR)/etc/GNUmakefile.std $(PROJECT_DIR)/etc/GNUmakefile.std 2>/dev/null | grep -v "not found" | head -1)
DEFS_FILE := $(shell ls $(INSTALL_DIR)/etc/GNUmakefile.defs $(PROJECT_DIR)/etc/GNUmakefile.defs 2>/dev/null | grep -v "not found" | head -1)

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
PUBLIC_LIBS_sun4	= 
PUBLIC_LIBS_pmax	= 
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

SRCS	= 

PRIVATE_INCS	= 

MISC		=  GNUmakefile # README

# All source files
SRCS_ALL	= $(INCS) $(PRIVATE_INCS) $(SRCS) $(MANS) $(DOCS) $(MISC)

# Files to be removed by 'make clean' and 'make realclean'
DELETES		= core $(DEPEND_FILE) \
	  	  $(PUBLIC_BINS) $(PUBLIC_LIBS) $(PUBLIC_LINTS) \
		  $(MODULE_LIBS) $(MODULE_LINTS) $(PRIVS) $(OBJS) make.state \
		  $(wildcard .nse_dep*) $(wildcard *.CKP) $(wildcard \#*\#) \
		  $(wildcard *~) $(wildcard .*~) $(wildcard *.CKP)\
	          $(wildcard *.BAK) $(wildcard .*.BAK)

# Files never to be removed, even if Make encounters an error
.PRECIOUS: $(SRCS_ALL) $(SUBDIRS)

##########
# Default Target
##########

all:: srcs libs bins privs

# I added this in order to complete the installation so that everything
# is in the right place for the FIRE project makefile system.
# Copying extra headers into include/ is necessary because the few headers
# installed by default #include some of the others. -Trey 16 Nov 2001
.PHONY: cmu-install
cmu-install: install
	-mkdir lib/$(HOST_OS)
	mv -f lib/lib* lib/$(HOST_OS)/	
	cp -f src/*.h include
	gmake -C .. MODULES=tcm install

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

link_tar::
	echo "Making link_tar for: man";
	mkdir $(FTP_DIR)/$(PROJECT)
	ln -s  $(PWD)/man $(FTP_DIR)/$(PROJECT)/man;

##########
# Include standard target definitions and RCS rules
##########

include $(STD_FILE)
