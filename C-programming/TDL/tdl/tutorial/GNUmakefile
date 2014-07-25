#############################################################################
#       $Id: GNUmakefile,v 1.24 97/05/29 09:22:45 reids Exp $
# $Revision: 1.24 $
#     $Date: 97/05/29 09:22:45 $
#   $Author: reids $
#    $State: Exp $
#   $Locker:  $
#
# PROJECT:	TDL
#
# FILE:		GNUmakefile
#		Based on TCA Makefiles
#
# DESCRIPTION:
#
# EXPORTS:
#	The following targets are available as part of Makefile.std:
#		all bins libs privs objs
#		clean install
#		lint depend
#
# HISTORY:
#
# $Log:	GNUmakefile,v $
#############################################################################

#############################################################################
# Module Configuration Macros
#############################################################################

INSTALL_DIR	= ..
ifndef BASE_DIR
BASE_DIR	:= $(shell cd $(INSTALL_DIR);pwd)
endif
MODULE_DIR	= tutorial
PROJECT		= tdl
MODULE		= tutorial
SUBDIRS		= simulator
DEPEND_FILE	= GNUmakefile.$(THIS_MACHINE)
DEPEND_PREFIX	= $(ODIR)/ 

#############################################################################
# Where are the standard files?
#############################################################################

ifndef FAST_COMPILE
PWD 	:= $(shell pwd)
endif

STD_FILE := $(shell ls GNUmakefile.std | grep -v "not found")
DEFS_FILE := $(shell ls GNUmakefile.defs | grep -v "not found")

##########
# Include standard definitions
##########

include $(DEFS_FILE)

##########
# Override defaults here.
##########

INC_PATH	= -I. -I$(PUBLIC_INC_DIR) -I$(MODULE_INC_DIR) -Isimulator \
		-I$(IPC_DIR)/include -I$(TCM_DIR)/src -I$(TDL_DIR)/src \
		$(INC_PATH_$(DBMALLOC))

LIB_PATH += -Lsimulator -L$(IPC_DIR)/lib/$(ARCH) \
	    -L$(TCM_DIR)/lib -L$(TDL_DIR)/src

LIBRARIES	= -ltdl -ltcm -lipc $(LIBS_$(THIS_OS)) -lm $(LIBS_$(DBMALLOC))

GRAPHICS_LIBRARIES = -lezx -lXt -lXaw -lXmu -lXext -lX11

##########
# File Definition Macros.  Deletion of generated files assumed
#    to be o.k.  Source files will never be deleted.
##########

# Generated files - not installed
PRIVS	= artist taskTrees taskTreesConcPlan monitors exceptions 

BB_OBJS = $(ODIR)/billBoard.o $(ODIR)/artist.o

THE_OBJS      = taskTrees.o taskTreesConcPlan.o monitors.o exceptions.o

OBJS = $(patsubst %,$(ODIR)/%,$(THE_OBJS)) $(BB_OBJS)


# Source files - not installed

SRCS            = taskTrees.tdl taskTreesConcPlan.tdl \
		monitors.tdl exceptions.tdl \
		billBoard.cc artist.tdl

PRIVATE_INCS	= billBoard.h simCommands.h

MISC		= GNUmakefile README sim.param sim.param.exception

##########
# Default Target
##########

# Put this here to force libezx.a to be made first.
all::
	cd simulator; $(MAKE) all
all:: srcs libs bins privs

##########
# User Defined Targets
##########

TDL_JAR_DIR = $(TDL_DIR)
CLASSPATH := .:$(TDL_JAR_DIR)/tdl_x.jar:$(TDL_JAR_DIR)/ifc111.jar:$(CLASSPATH)
export CLASSPATH

JAVA = java
JAVA_FLAGS = 
TDLC_HEADER_FLAGS = -2Hy -o
TDLC_SOURCE_FLAGS = -2Cy -o

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

artist.h : artist.tdl
	$(JAVA) $(JAVA_FLAGS) TDLC $(TDLC_HEADER_FLAGS) $@ $<

artist.cc : artist.tdl artist.h
	$(JAVA) $(JAVA_FLAGS) TDLC $(TDLC_SOURCE_FLAGS) $@ $<

taskTrees.h : taskTrees.tdl
	$(JAVA) $(JAVA_FLAGS) TDLC $(TDLC_HEADER_FLAGS) $@ $<

taskTrees.cc : taskTrees.tdl taskTrees.h
	$(JAVA) $(JAVA_FLAGS) TDLC $(TDLC_SOURCE_FLAGS) $@ $<

taskTreesConcPlan.h : taskTreesConcPlan.tdl
	$(JAVA) $(JAVA_FLAGS) TDLC $(TDLC_HEADER_FLAGS) $@ $<

taskTreesConcPlan.cc : taskTreesConcPlan.tdl taskTreesConcPlan.h
	$(JAVA) $(JAVA_FLAGS) TDLC $(TDLC_SOURCE_FLAGS) $@ $<

monitors.h : monitors.tdl
	$(JAVA) $(JAVA_FLAGS) TDLC $(TDLC_HEADER_FLAGS) $@ $<

monitors.cc : monitors.tdl monitors.h
	$(JAVA) $(JAVA_FLAGS) TDLC $(TDLC_SOURCE_FLAGS) $@ $<

exceptions.h : exceptions.tdl
	$(JAVA) $(JAVA_FLAGS) TDLC $(TDLC_HEADER_FLAGS) $@ $<

exceptions.cc : exceptions.tdl exceptions.h
	$(JAVA) $(JAVA_FLAGS) TDLC $(TDLC_SOURCE_FLAGS) $@ $<

artist: $(BB_OBJS) $(LIBDEP)
	$(LINK.cc) -o $@ $(BB_OBJS) $(GRAPHICS_LIBRARIES) $(LIBRARIES)

taskTrees: $(ODIR)/taskTrees.o $(LIBDEP)
	$(LINK.cc) -o $@ $(ODIR)/taskTrees.o $(LIBRARIES)

taskTreesConcPlan: $(ODIR)/taskTreesConcPlan.o $(LIBDEP)
	$(LINK.cc) -o $@ $(ODIR)/taskTreesConcPlan.o $(LIBRARIES)

monitors: $(ODIR)/monitors.o $(LIBDEP)
	$(LINK.cc) -o $@ $(ODIR)/monitors.o $(LIBRARIES)

exceptions: $(ODIR)/exceptions.o $(LIBDEP)
	$(LINK.cc) -o $@ $(ODIR)/exceptions.o $(LIBRARIES)

##########
# Include standard target definitions and RCS rules
##########

clean::
	-$(RM) artist.h artist.cc taskTrees.h taskTrees.cc \
	       taskTreesConcPlan.h taskTreesConcPlan.cc \
	       monitors.h monitors.cc exceptions.h exceptions.cc

include $(STD_FILE)
