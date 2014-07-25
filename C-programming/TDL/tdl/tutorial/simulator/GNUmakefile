#############################################################################
#       $Id: GNUmakefile,v 1.21 97/06/17 12:48:06 reids Exp $
# $Revision: 1.21 $
#     $Date: 97/06/17 12:48:06 $
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

INSTALL_DIR	= ../..
ifndef BASE_DIR
BASE_DIR	:= $(shell cd $(INSTALL_DIR);pwd)
endif
MODULE_DIR	= tutorial/simulator
PROJECT		= tdl
MODULE		= simulator
SUBDIRS		=
DEPEND_FILE	= GNUmakefile.$(THIS_MACHINE)
DEPEND_PREFIX	= $(ODIR)/ 

#############################################################################
# Where are the standard files?
#############################################################################

ifndef FAST_COMPILE
PWD 	:= $(shell pwd)
endif

STD_FILE := $(shell ls ../GNUmakefile.std | grep -v "not found")
DEFS_FILE := $(shell ls ../GNUmakefile.defs | grep -v "not found")

##########
# Include standard definitions
##########

include $(DEFS_FILE)

##########
# Override defaults here.
##########

PRIVS = libezx.a simulator

LIBRARIES	= -lipc -lezx -lXt -lXaw -lXmu -lXext -lX11 \
		 $(LIBS_$(THIS_OS)) -lm $(LIBS_$(DBMALLOC))

##########
# File Definition Macros.  Deletion of generated files assumed
#    to be o.k.  Source files will never be deleted.
##########

_SIM_OBJS = action.o draw_env.o environment.o files.o sensors.o \
	   sim-xwindows.o sim.o simInterface.o updates.o 
SIM_OBJS  = $(patsubst %,$(ODIR)/%,$(_SIM_OBJS))

_EZX_OBJS  = ezx.o cursorx.o
EZX_OBJS   = $(patsubst %,$(ODIR)/%,$(_EZX_OBJS))

OBJS = $(SIM_OBJS) $(EZX_OBJS)

# Source files - not installed

SRCS            = action.c draw_env.c environment.c files.c sensors.c \
		sim-xwindows.c sim.c simInterface.c updates.c ezx.c cursorx.c

PRIVATE_INCS	= action.h common.h draw_env.h environment.h files.h \
		sensors.h sim-xwindows.h sim.h simInterface.h simMessages.h \
		updates.h ezx.h cursorx.h

MISC		= GNUmakefile README

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

libezx.a: $(EZX_OBJS)
	$(RM) $@
	$(AR) $(ARFLAGS) $@ $(EZX_OBJS)
	$(RANLIB) $@

simulator: $(OBJS) $(LIBDEP) libezx.a
	$(LINK.c) -o $@ $(OBJS) $(LIBRARIES)

##########
# Include standard target definitions and RCS rules
##########

include $(STD_FILE)
