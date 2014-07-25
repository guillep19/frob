#!/bin/tcsh -f

# Copyright (c) 2008, Carnegie Mellon University
#     This software is distributed under the terms of the 
#     Simplified BSD License (see tcm/LICENSE.TXT)

# This is a shell script to do regression testing for TCM

setenv TCM_DIR ../
setenv TEST_DIR $TCM_DIR/tests

# Usage: regression.tests.sh --> does TCM regression tests
# Usage: regression.tests.sh update --> updates regression test files
# Usage: regression.tests.sh distr --> does TCM distributed regression tests
# Usage: regression.tests.sh update distr --> updates distributed test files

# Usage: regression.tests.sh distr-sorted --> does TCM distributed regression
#                                             tests with sort enabled.
#  (distr-sorted option is not compatible with update.)

set testnum = -1

set ACTION = diff
set PROGRAM = treeTest
set SUFFIX = log

if ($1 == "update") then
    if ($2 == "distr") then
        echo "Updating TCM distributed regression test files"
        set ACTION = $TEST_DIR/tcm.update.distr.sh
        set PROGRAM = distrTest
        set SUFFIX = dlog
        if ($3 != "") set testnum = $3
    else
        echo "Updating TCM regression test files"
        set ACTION = $TEST_DIR/tcm.update.sh
        if ($2 != "") set testnum = $2
    endif
else
    if ($1 == "distr") then
        if ($2 == "update") then
            echo "Updating TCM distributed regression test files"
            set ACTION = $TEST_DIR/tcm.update.distr.sh
            set PROGRAM = distrTest
            set SUFFIX = dlog
            if ($3 != "") set testnum = $3
        else
            echo "Performing TCM distributed regression tests"
            set ACTION = $TEST_DIR/tcm.test.distr.sh
            set PROGRAM = distrTest
            set SUFFIX = dlog
            if ($2 != "") set testnum = $2
        endif
    else if ($1 == "distr-sorted") then
        echo "Performing TCM >>>SORTED<<< distributed regression tests"
        set ACTION = $TEST_DIR/tcm.test.distr_sorted.sh
        set PROGRAM = distrTest
        set SUFFIX = dlog
        if ($2 != "") set testnum = $2
    else
        echo "Performing TCM regression tests"
        set ACTION = $TEST_DIR/tcm.test.sh
        if ($1 != "") set testnum = $1
    endif
endif

cd $TEST_DIR

make DEBUG=NONE $PROGRAM
if ( $status ) exit $status

$ACTION $testnum 0 "FIRST TEST IS THE NOMINAL SCENARIO (0)" treeTest.$SUFFIX
$ACTION $testnum 1 "NOMINAL WITH UNUSED TERMINATION (1)" completionTest.$SUFFIX

$ACTION $testnum 11 "TIMING TEST WITH DELAY COMMANDS (11)" timingTest1.$SUFFIX
$ACTION $testnum 12 "TIMING TEST USING 'DELAY UNTIL' (12)" timingTest2.$SUFFIX
$ACTION $testnum 13 "TIMING TEST OF 'DELAY UNTIL AFTER' (13)" timingTest3.$SUFFIX

$ACTION $testnum 21 "SUSPEND TEST1 (21)" suspendTest1.$SUFFIX
$ACTION $testnum 22 "SUSPEND TEST2 (22)" suspendTest2.$SUFFIX
$ACTION $testnum 23 "SUSPEND TEST3 (23)" suspendTest3.$SUFFIX
$ACTION $testnum 24 "SUSPEND TEST4 (24)" suspendTest4.$SUFFIX

$ACTION $testnum 31 "EXTERNAL TEST1 (31)" externalTest1.$SUFFIX

$ACTION $testnum 41 "SIGNAL TEST1 (41)" signalTest1.$SUFFIX
$ACTION $testnum 42 "SIGNAL TEST OF 'INVOKE WHEN' (42)" signalTest2.$SUFFIX

$ACTION $testnum 51 "EXCEPTION TEST1 (51)" exceptionTest1.$SUFFIX
$ACTION $testnum 52 "EXCEPTION TEST2 (52)" exceptionTest2.$SUFFIX
$ACTION $testnum 53 "EXCEPTION TEST3 (53)" exceptionTest3.$SUFFIX

$ACTION $testnum 61 "MONITOR TEST1 (61)" monitorTest1.$SUFFIX
$ACTION $testnum 62 "MONITOR TEST2 (62)" monitorTest2.$SUFFIX
$ACTION $testnum 63 "MONITOR TEST3 (63)" monitorTest3.$SUFFIX
$ACTION $testnum 64 "MONITOR TEST4 (64)" monitorTest4.$SUFFIX
$ACTION $testnum 65 "MONITOR TEST5 (65)" monitorTest5.$SUFFIX
$ACTION $testnum 66 "MONITOR TEST6 OF 'INVOKE AFTER' (66)" monitorTest6.$SUFFIX
$ACTION $testnum 67 "MONITOR TEST7 (67)" monitorTest7.$SUFFIX

$ACTION $testnum 102 "TERMINATE TEST2 (102)" killTest2.$SUFFIX
$ACTION $testnum 103 "TERMINATE TEST3 (103)" killTest3.$SUFFIX
$ACTION $testnum 104 "TERMINATE TEST4 (104)" killTest4.$SUFFIX
$ACTION $testnum 105 "TERMINATE TEST5 (105)" killTest5.$SUFFIX
$ACTION $testnum 106 "TERMINATE TEST6 (106)" killTest6.$SUFFIX
$ACTION $testnum 107 "TERMINATE TEST7 (107)" killTest7.$SUFFIX
$ACTION $testnum 108 "TERMINATE TEST8 (108)" killTest8.$SUFFIX
$ACTION $testnum 109 "TERMINATE TEST9 (109)" killTest9.$SUFFIX
$ACTION $testnum 110 "TERMINATE TEST10 (110)" killTest10.$SUFFIX
$ACTION $testnum 111 "TERMINATE TEST11 (111)" killTest11.$SUFFIX
$ACTION $testnum 112 "TERMINATE TEST12 (112)" killTest12.$SUFFIX
$ACTION $testnum 113 "TERMINATE TEST13 (113)" killTest13.$SUFFIX
$ACTION $testnum 114 "TERMINATE TEST14 (114)" killTest14.$SUFFIX
$ACTION $testnum 115 "TERMINATE TEST15 (115)" killTest15.$SUFFIX
$ACTION $testnum 116 "TERMINATE TEST16 (116)" killTest16.$SUFFIX
$ACTION $testnum 117 "TERMINATE TEST17 (117)" killTest17.$SUFFIX

$ACTION $testnum 201 "TERMINATE AT TEST (201)" terminateAtTest.$SUFFIX
$ACTION $testnum 202 "TERMINATE IN TEST (202)" terminateInTest.$SUFFIX
$ACTION $testnum 203 "TERMINATE AT AFTER TEST1 (203)" terminateAtAfterTest1.$SUFFIX
$ACTION $testnum 204 "TERMINATE AT AFTER TEST1a (204)" terminateAtAfterTest1a.$SUFFIX
$ACTION $testnum 205 "TERMINATE AT AFTER TEST2 (205)" terminateAtAfterTest2.$SUFFIX
$ACTION $testnum 206 "TERMINATE AT AFTER TEST2a (206)" terminateAtAfterTest2a.$SUFFIX
$ACTION $testnum 207 "ON TERMINATE TEST1 (207)" onTermination1.$SUFFIX
$ACTION $testnum 208 "ON TERMINATE TEST2 (208)" onTermination2.$SUFFIX
$ACTION $testnum 209 "ON TERMINATE TEST3 (209)" onTermination3.$SUFFIX

$ACTION $testnum 210 "TERMINATE AT TEST2 (210)" terminateAtTest2.$SUFFIX
$ACTION $testnum 211 "TERMINATE AT AFTER TEST3 (211)" terminateAtAfterTest3.$SUFFIX

$ACTION $testnum 301 "DEALLOCATE TEST1 (301)" deallocate1.$SUFFIX
$ACTION $testnum 302 "DEALLOCATE TEST2 (302)" deallocate2.$SUFFIX
