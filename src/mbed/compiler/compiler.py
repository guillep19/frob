#!/usr/bin/env python

import ast, _ast
import argparse

parser = argparse.ArgumentParser(description='Compiles a frob program to binary bytecode.')
parser.add_argument('-c', type=str, help='input filename', required=True)
args = parser.parse_args()
print 'filename: ', args.c


f = open(args.c, 'r')
source = f.read()

abs_tree = ast.parse(source)

class FuncLister(ast.NodeVisitor):
  def visit_Name(self, node):
    print(node.name)
    self.generic_visit(node)


def list_functions(tree):
  FuncLister().visit(abs_tree)

# Transformation: Replace constant value in constant name.
class ConstantReplacer(ast.NodeVisitor):
  _constants = {}

  def visit_Module(self, node):
    """Find eglible variables to be inlined and store
    the Name->value mapping in self._constants for later use"""
    assigns = [x for x in node.body if
               type(x) == _ast.Assign]
    for assign in assigns:
        if type(assign.value) in (_ast.Num, _ast.Str):
            for name in assign.targets:
                if name.id.isupper():
                    self._constants[name.id] = assign.value
    return self.generic_visit(node)

  def visit_Name(self, node):
    """If node.id is in self._constants, replace the
    loading of the node with the actual value"""
    return self._constants.get(node.id, node)


def replace_constants(tree):
    replacer = ConstantReplacer()
    newtree = replacer.visit(tree)
    return newtree

import pdb; pdb.set_trace()
