#!/usr/bin/env python

import ast
import argparse

parser = argparse.ArgumentParser(description='Compiles a frob program to binary bytecode.')
parser.add_argument('-c', type=str, help='input filename', required=True)
args = parser.parse_args()
print 'filename: ', args.c


f = open(args.c, 'r')
source = f.read()

abs_tree = ast.parse(source)

class FuncLister(ast.NodeVisitor):
  def visit_FunctionDef(self, node):
    print(node.name)
    self.generic_visit(node)

FuncLister().visit(abs_tree)

import pdb; pdb.set_trace()
