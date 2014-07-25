TDL_SKIP_CLEAN_ON_MAKE = 1
#
# Note:  This makefile (via its use of Makefile.generic) DEPENDS on
# TCM_INCLUDE_DIR, TCM_LIBRARY_DIR, etc being set appropriately.
# They can be set as environment variables, or as arguments to make...
#
# Copyright (c) 2008, Carnegie Mellon University
#     This software is distributed under the terms of the 
#     Simplified BSD License (see tdl/LICENSE.TXT)
#

ifndef BASE_TDL_DIR
  BASE_TDL_DIR = ..
  export BASE_TDL_DIR
endif

### Enable conditonal-cleaning for this directory.
    DIRECTORY_NEEDS_CLEANING = TRUE


    CXX_INCLUDE_FILES = tdl.H                           \
                        tdldef.H                        \
                        stdtypes.H                      \
                        TDL_StringBuffer.H              \
                        _TDL_InterfaceToTCM.H           \
                        _TDL_Snode.H                    \
                        _TDL_Slist.H                    \
                        _TDL_Dnode.H                    \
                        _TDL_Dlist.H                    \
                        _TDL_ArrayList.H                \
                        _TDL_NamesList.H                \
                        _TDL_ConstraintsList.H          \
                        _TDL_TreeNode.H                 \
                        _TDL_TreeNodeBranch.H           \
                        _TDL_IterationIndex.H           \
                        _TDL_TDLStatement.H             \
                        _TDL_SpawnStatement.H           \
                        _TDL_SpawnStatementData.H       \
                        _TDL_SpawnStatementTreeNode.H   \
                        _TDL_Exceptions.H               \
                        _TDL_Constraints.H              \
                        _TDL_WithStatement.H            \
                        _TDL_WithStatementData.H        \
                        _TDL_WithStatementTreeNode.H    \
                        _TDL_functions_2.H              \
                        _TDL_ActionOrVoid.H             \
                        _TDL_AllocationFunction.H       \
                        _TDL_DistributedRegistryEntry.H \
                        _TDL_HandleManager.H            \
                        _TDL_DelayedAllocation.H        

    CXX_SOURCE_FILES  = tdldef.C                        \
                        tdldef-distributed.C            \
                        TDL_StringBuffer.C              \
                        _TDL_BaseAction.C               \
                        _TDL_Snode.C                    \
                        _TDL_Slist.C                    \
                        _TDL_Dnode.C                    \
                        _TDL_Dlist.C                    \
                        _TDL_ArrayList.C                \
                        _TDL_NamesList.C                \
                        _TDL_ConstraintsList.C          \
                        _TDL_TreeNode.C                 \
                        _TDL_TreeNodeBranch.C           \
                        _TDL_IterationIndex.C           \
                        _TDL_TDLStatement.C             \
                        _TDL_SpawnStatement.C           \
                        _TDL_SpawnStatementData.C       \
                        _TDL_SpawnStatementTreeNode.C   \
                        _TDL_Exceptions.C               \
                        _TDL_Constraints.C              \
                        _TDL_WithStatement.C            \
                        _TDL_WithStatementData.C        \
                        _TDL_WithStatementTreeNode.C    \
                        _TDL_ActionOrVoid-distributed.C \
                        _TDL_AllocationFunction.C       \
                        _TDL_DistributedRegistryEntry.C \
                        _TDL_HandleManager.C           


   CXX_TARGET_LIBRARY = $(TDL_LIBRARY_DIR)/libtdl.a

#
# Test Programs...
#
   CXX_EXECUTABLES    = test1.C test_ll.C test_time.C


ifdef debug_no_insert
    DEBUG_CXX_FLAGS = -D_TDL_DISABLE_NODE_INSERTION_FOR_TESTING
endif

include $(BASE_TDL_DIR)/Makefile.generic

