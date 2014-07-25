#!/bin/tcsh -f

# Copyright (c) 2008, Carnegie Mellon University
#     This software is distributed under the terms of the 
#     Simplified BSD License (see tcm/LICENSE.TXT)

# This is a shell script to update a TCM test file

# Usage: testnum, which-test, message, log-file

if ($1 < 0 || $1 == $2) then
echo $3
./treeTest $2 > & ! test
sed  -e 's|0X[0-9A-F]\?[0-9A-F]\?[0-9A-F]\?[0-9A-F]\?[0-9A-F]\?[0-9A-F]\?[0-9A-F]\?[0-9A-F]\?[0-9A-F]\?|0X.......|g'  < test >! test.sed
/bin/mv test.sed $4
endif
