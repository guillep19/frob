#!/bin/tcsh -f

# Copyright (c) 2008, Carnegie Mellon University
#     This software is distributed under the terms of the 
#     Simplified BSD License (see tcm/LICENSE.TXT)

# This is a shell script to run a TCM test

# Usage: testnum, which-test, message, log-file

if ($1 < 0 || $1 == $2) then
echo $3
./distrTest $2 > & ! test
sed  -e 's|0X[0-9A-F]\?[0-9A-F]\?[0-9A-F]\?[0-9A-F]\?[0-9A-F]\?[0-9A-F]\?[0-9A-F]\?[0-9A-F]\?[0-9A-F]\?|0X.......|g'  < test | grep -v "Task Control Management" | sort >! test.sed
# \v == 0x0B == 013 == The ever so seldom used 'Vertical Tab'.
# (Used to filter/cope-with the "... connected." message split.)
cat $4 | sed 's|^Attempting to connect to IPC central server on localhost\.\.\.|Attempting to connect to IPC central server on localhost... connected.|g' | tr '\013' '\012' | grep -v '^ connected.$' | sort | diff test.sed -
endif
