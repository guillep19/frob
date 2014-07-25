#############################################################################
#       $Id: GNUmakefile,v 1.10 2013/07/31 02:03:21 reids Exp $
# $Revision: 1.10 $
#     $Date: 2013/07/31 02:03:21 $
#   $Author: reids $
#    $State: Exp $
#   $Locker:  $
#
# PROJECT:	NM-DS1
#
# FILE:		GNUmakefile
#		Based on TCA GNUmakefile (Richard Goodwin)
#
# DESCRIPTION: Makes the JAVA version of IPC.
#
# EXPORTS:
#	The following targets are available as part of GNUmakefile.std:
#		all bins libs privs objs
#		clean install
#		lint depend
#
# Copyright (c) 2008, Carnegie Mellon University
#     This software is distributed under the terms of the 
#     Simplified BSD License (see ipc/LICENSE.TXT)
#
# HISTORY:
#
# $Log: GNUmakefile,v $
# Revision 1.10  2013/07/31 02:03:21  reids
# Fixed the path to shared library
#
# Revision 1.9  2013/07/23 21:11:38  reids
# Updated for using SWIG
#
# Revision 1.8  2009/09/04 19:11:20  reids
# IPC Java is now in its own package
#
# Revision 1.7  2009/02/07 18:54:45  reids
# Updates for use on 64 bit machines
#
# Revision 1.6  2009/01/12 15:54:55  reids
# Added BSD Open Source license info
#
# Revision 1.5  2003/02/13 20:37:44  reids
# Updated to work under Solaris
#
# Revision 1.4  2002/06/25 21:20:30  reids
# Changed order of building to enable build from scratch
#
# Revision 1.3  2002/01/04 16:03:14  reids
# How to use Java version of IPC
#
# Revision 1.2  2002/01/02 21:38:03  reids
# Changed makefile so it java directory can be "installed" from top level,
#   and so that tar file ("gmake ftp") includes java stuff.
#
# Revision 1.1  2002/01/02 17:40:15  reids
# Initial (and more, or less, complete) release of Java version of IPC.
#
#
#############################################################################

.KEEP_STATE:

#############################################################################
# Module configuration.
#############################################################################

INSTALL_DIR	= ..
BASE_DIR	:= $(shell cd $(INSTALL_DIR);/bin/pwd)
MODULE_DIR	= java
PROJECT		= ipc
MODULE		= ipc
SUBDIRS		=
DEPEND_FILE	= $(PWD)/GNUmakefile.$(THIS_MACHINE)
DEPEND_PREFIX	= $(ODIR)/ $(SODIR)/
VERSION_FILE	= ../src/ipcPriv.h
VERSION_PREFIX	= IPC

#############################################################################
# Standard Makefile Includes
#############################################################################

PWD 	:= $(shell /bin/pwd)
STD_FILE := $(shell ls $(INSTALL_DIR)/etc/GNUmakefile.std 2>/dev/null | grep -v "not found" | head -1)
DEFS_FILE := $(shell ls $(INSTALL_DIR)/etc/GNUmakefile.defs 2>/dev/null | grep -v "not found" | head -1)

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

JAVALIB			= libipcjava.so

PUBLIC_LIBS_RISC	= 
PUBLIC_LIBS	     	= $(JAVALIB)

PUBLIC_LINTS		= 

MODULE_LIBS		= 
MODULE_LINTS		= 

# Source files - installed
PUBLIC_INCS		= 
PUBLIC_MANS		= 
PUBLIC_DOCS		= 

MODULE_INCS		= 
MODULE_DOCS		= 

_SWIG_GEN		= IPCConstants IPC_ERROR_TYPE IPCJNI IPC_RETURN_TYPE \
			  IPC_VARCONTENT_TYPE IPC_VERBOSITY_TYPE \
			  BUFFER_TYPE FORMAT_TYPE MSG_INSTANCE IPC_CONTEXT\
			  SWIGTYPE_p_fd_set SWIGTYPE_p_void \
			  FORMATTER_CONTAINER_TYPE TIMER_REF_CONTAINER_TYPE \
			  SWIGTYPE_p_f_p_struct__X_IPC_REF_p_void_p_void__void
SWIG_GEN_SRCS		= $(patsubst %,%.java,$(_SWIG_GEN))
SWIG_GEN_CLASSES	= $(patsubst %,%.class,$(_SWIG_GEN))

install_libs		:: CLASSES

CFLAGS			+= -fno-strict-aliasing

CFLAGS			+= -fPIC -O2
ifndef JAVA_INC_DIR
JAVA_INC_DIR		= /usr/java/jdk1.3.1/include
endif
INC_PATH		+= -I$(JAVA_INC_DIR) -I../include

# Needed for jni_md.h
ifeq ($(THIS_OS),Linux)
INC_PATH		+= -I$(JAVA_INC_DIR)/linux
else
ifeq ($(THIS_MACHINE),sun4u)
INC_PATH		+= -I/usr/java/include -I/usr/java/include/solaris
else
ifeq ($(THIS_OS),Solaris)
INC_PATH		+= -I/usr/local/include
endif
endif
endif

BUILD_DIR		= build/ipc/java

INC_PATH		+= -I./$(BUILD_DIR)
INC_PATH		+= -I../src

INCS			= $(MODULE_INCS) $(PUBLIC_INCS)

# Generated files - not installed
PRIVS	= 

_OBJS = IPC_wrap.o

OBJS = $(patsubst %,$(ODIR)/%,$(_OBJS))

# Source files - not installed

JAVA_SRCS	= IPC.java formatters.java primFmttrs.java ipcJava.java
SRCS            = IPC.i ipcJava.h ipcJava.c $(JAVA_SRCS)

PRIVATE_INCS	= 

MISC	= GNUmakefile README

# All source files
SRCS_ALL	= $(INCS) $(PRIVATE_INCS) $(SRCS) $(MANS) $(DOCS) $(MISC)

LOCAL_LIBS	= $(patsubst %,$(LDIR)/%,$(PUBLIC_LIBS))

LOCAL_BINS	= 

# Files to be removed by 'make clean' and 'make realclean'
DELETES		= core $(DEPEND_FILE) \
	  	  $(LOCAL_BINS) $(LOCAL_LIBS) $(PUBLIC_LINTS) \
		  $(MODULE_LIBS) $(MODULE_LINTS) $(PRIVS) $(OBJS) make.state \
		  $(wildcard .nse_dep*) $(wildcard *.CKP) \
		  $(wildcard *~) $(wildcard .*~) $(wildcard *.CKP)\
	          $(wildcard *.BAK) $(wildcard .*.BAK) *.class $(GEN_HEADERS) \
		  IPC.java IPC_wrap.c IPC_wrap.h $(SWIG_GEN_SRCS) \
		  $(BUILD_DIR)/*

# Files never to be removed, even if Make encounters an error
.PRECIOUS: $(SRCS_ALL) $(SUBDIRS)

##########
# Default Target
##########

all:: srcs libs bins privs

# The vx works compiler constants are in etc/GNUmakefile.defs.
#vx:  $(VODIR)/.force $(LDIR)/.force \
#	$(PUBLIC_LIB_DIR)/ipcModule.o $(PUBLIC_LIB_DIR)/centralModule.o

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

IPC_wrap.c : IPC.i ../src/ipcFFI.h ../src/ipcFFI.c ipcJava.h ipcJava.c
	swig -java $(INC_PATH) -outcurrentdir -o $@ -package "ipc.java" $<

CLASSES : IPC_wrap.c $(JAVA_SRCS) $(SWIG_GEN_SRCS)
	$(MKDIR) build
	javac -classpath build -d build $(JAVA_SRCS) $(SWIG_GEN_SRCS)

IPC_wrap.h : IPC_wrap.c
	javah -classpath build -force -jni -o IPC_wrap.h ipc.java.IPCJNI

$(ODIR)/IPC_wrap.o : IPC_wrap.h

ifeq ($(THIS_OS),wnt)
$(LDIR)/$(JAVALIB): $(OBJS)
	$(RM) $@
	$(LD_SHARED) -o $@ $(OBJS) $(LIB_PATH) -lipc
else
$(LDIR)/$(JAVALIB): $(OBJS) $(PUBLIC_LIB_DIR)/libipc.a
	$(RM) $@
	cd ../src; $(MAKE) MAKE_SHARED_LIBS=1 install; cd ../java
	$(LD_SHARED) -o $@ $(OBJS) \
	  $(PWD)/$(PUBLIC_LIB_DIR)/libipc.so.$(IPC_VERSION_MAJOR).$(IPC_VERSION_MINOR)
endif

$(PUBLIC_LIB_DIR)/$(JAVALIB): $(OBJS)

# this dependency is used to update the version information.
# $(ODIR)/ipc.o: $(SRCS_ALL)

# $(LODIR)/ipc.o: $(SRCS_ALL)

#
# Targets for building a release
#

##########
# Include standard target definitions and RCS rules
##########

include $(STD_FILE)
