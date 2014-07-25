#############################################################################
#       $Id: GNUmakefile,v 2.6 2013/08/09 14:01:25 reids Exp $
# $Revision: 2.6 $
#     $Date: 2013/08/09 14:01:25 $
#   $Author: reids $
#    $State: Exp $
#   $Locker:  $
#
# PROJECT:	NM-DS1
#
# FILE:		GNUmakefile
#		Based on TCA GNUmakefile (Richard Goodwin)
#
# DESCRIPTION: Makes the LISP version of IPC.
#
# EXPORTS:
#	The following targets are available as part of GNUmakefile.std:
#		all bins libs privs objs
#		clean install
#		lint depend
#
#
# HISTORY:
#
# $Log: GNUmakefile,v $
# Revision 2.6  2013/08/09 14:01:25  reids
# Updates for running Lisp version under Darwin
#
# Revision 2.5  2013/07/31 02:03:21  reids
# Fixed the path to shared library
#
# Revision 2.4  2013/07/24 01:43:08  reids
# A few bugs fixed for the 3.10.0 version
#
# Revision 2.3  2013/07/23 21:11:49  reids
# Updated for using SWIG
#
#
#############################################################################

.KEEP_STATE:

#############################################################################
# Module configuration.
#############################################################################

INSTALL_DIR	= ..
BASE_DIR	:= $(shell cd $(INSTALL_DIR);/bin/pwd)
MODULE_DIR	= lisp
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

LISPLIB_PREFIX		= libipc_lisp
ifeq ($(THIS_OS),wnt)
LISPLIB_POSTFIX		= dll
else
LISPLIB_POSTFIX		= so
endif

# Generated files - installed
PUBLIC_BINS_SunOS	= 
PUBLIC_BINS		= 

LISPLIB			= $(LISPLIB_PREFIX).$(LISPLIB_POSTFIX)

PUBLIC_LIBS_RISC	= 
PUBLIC_LIBS	     	= $(LISPLIB)

PUBLIC_LINTS		= 

MODULE_LIBS		= 
MODULE_LINTS		= 

# Source files - installed
PUBLIC_INCS		= 
PUBLIC_MANS		= 
PUBLIC_DOCS		= 

MODULE_INCS		= 
MODULE_DOCS		= 

# To eliminate warnings (from SWIG-generated code) about type-punning
CFLAGS			+= -fno-strict-aliasing

#install_libs		:: formatters.class primFmttrs.class IPC.class
CFLAGS			+= -fpic -O2 \
			    -Wno-strict-prototypes -Wno-missing-prototypes \
			    -Wno-missing-declarations
# Uncomment next two lines if using 32 bit Lisp
#CFLAGS		+= -m32
#LD_SHARED	+= -melf_i386

ifndef LISP_INC_DIR
LISP_INC_DIR		= /usr/include/lisp.h
endif
INC_PATH		+= -I$(LISP_INC_DIR)

ifeq ($(THIS_OS),Solaris)
INC_PATH		+= -I/usr/local/include
endif
INC_PATH		+= -I../src

INCS			= $(MODULE_INCS) $(PUBLIC_INCS)

# Generated files - not installed
PRIVS	= 

_OBJS = IPC_wrap.o

OBJS = $(patsubst %,$(ODIR)/%,$(_OBJS))

# Source files - not installed

SRCS            = IPC.i formatters.lisp primFmttrs.lisp ipcLisp.lisp \
		  ipcLisp.h ipcLisp.c

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
		  $(wildcard .nse_dep*) $(wildcard *.CKP) $(wildcard \#*\#) \
		  $(wildcard *~) $(wildcard .*~) $(wildcard *.CKP)\
	          $(wildcard *.BAK) $(wildcard .*.BAK) *.class $(GEN_HEADERS) \
		  IPC.lisp IPC_wrap.c *.fasl

# Files never to be removed, even if Make encounters an error
.PRECIOUS: $(SRCS_ALL) $(SUBDIRS)

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

INC_PATH += -I/usr/local/acl/acl90-smp/misc

IPC_wrap.c : IPC.i ../src/ipcFFI.h ../src/ipcFFI.c \
	   ipcLisp.h ipcLisp.c ipcLisp.lisp
	swig -allegrocl $(INC_PATH) -outcurrentdir \
		-DLIB_DIR='"$(PWD)/$(PUBLIC_LIB_DIR)"' \
		-DLISP_DIR='"$(PWD)"' -o $@ $<
	mv -f IPC.cl IPC.lisp

ifeq ($(THIS_OS),wnt)
$(LDIR)/$(LISPLIB): $(OBJS)
	$(RM) $@
	$(LD_SHARED) -o $@ $(OBJS) $(LIB_PATH) -lipc
else
ifeq ($(THIS_OS),Darwin)
$(LDIR)/$(LISPLIB): $(OBJS)
	$(RM) $@
	cd ../src; $(MAKE) MAKE_SHARED_LIBS=1 USE_LISP=1 install; cd ../lisp
	$(LD_SHARED) -flat_namespace -undefined warning -o $@ $(OBJS) \
	  $(PWD)/$(PUBLIC_LIB_DIR)/libipc.so.$(IPC_VERSION_MAJOR).$(IPC_VERSION_MINOR)
else
$(LDIR)/$(LISPLIB): $(OBJS)
	$(RM) $@
	cd ../src; $(MAKE) MAKE_SHARED_LIBS=1 USE_LISP=1 install; cd ../lisp
	$(LD_SHARED) -o $@ $(OBJS) \
	  $(PWD)/$(PUBLIC_LIB_DIR)/libipc.so.$(IPC_VERSION_MAJOR).$(IPC_VERSION_MINOR)
endif
endif

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
