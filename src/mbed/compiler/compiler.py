#!/usr/bin/env python

import ast
import argparse

parser = argparse.ArgumentParser(description='Compiles a frob program to binary bytecode.')
parser.add_argument('-c', type=str, help='input filename', required=True)
args = parser.parse_args()

f = open(args.c, 'r')
source = f.read()
tree = ast.parse(source)

variable_to_index = {}
variable_count = 0


# Transformation: Replace constant value in constant name.
class ConstantReplacer(ast.NodeVisitor):
  _constants = {}

  def visit_Module(self, node):
    """Find eglible variables to be inlined and store
    the Name->value mapping in self._constants for later use"""
    assigns = [x for x in node.body if
               type(x) == ast.Assign]
    for assign in assigns:
        if type(assign.value) in (ast.Num, ast.Str):
            for name in assign.targets:
                if name.id.isupper():
                    print name.id, "Es constante!!! y vale ", assign.value.n
                    self._constants[name.id] = assign.value
                else:
                    print name.id, "No es constante, guardar en hash de variables"
                    variable_to_index[name.id] = variable_count
                    variable_count = variable_count + 1
    return self.generic_visit(node)

  def visit_Name(self, node):
    """If node.id is in self._constants, replace the
    loading of the node with the actual value"""
    print "visit_Name:  ", node.id, " value: ", self._constants.get(node.id)
    return self._constants.get(node.id, node)


def replace_constants(tree):
    replacer = ConstantReplacer()
    newtree = replacer.visit(tree)
    return newtree


def generate_bytecode(node, function_parameters=[], inside_function=False):
  if type(node) == ast.Module:
    print "Module"
    bytecode = []
    for child in node.body:
      child_bytecode = generate_bytecode(child)
      bytecode.extend(child_bytecode)
  elif type(node) == ast.Assign:
    print "Assign"
    opcode = 'local_store' if inside_function else 'store'
    bytecode = generate_bytecode(node.value,
                                 function_parameters,
                                 inside_function)
    bytecode.append(
      {'opcode': opcode, 'arg': node.targets[0].id}
    )
  elif type(node) == ast.FunctionDef:
    print "Function"
    bytecode = []
    paramname_to_index = dict((p.id, i) for i, p in enumerate(node.args.args))
    print "paramname_to_index:", paramname_to_index
    is_function = not node.name.startswith("task_")
    print "function ", node.name, " isfunction=", is_function
    for child in node.body:
      child_bytecode = generate_bytecode(child,
                                         function_parameters=paramname_to_index,
                                         inside_function=is_function)
      bytecode.extend(child_bytecode)
    if len(bytecode) > 0:
      print "Ok, setting function label"
      bytecode[0]['label'] = node.name
    else:
      print "Aca daba index out of range!! wtf!"
    #FunctionDef(
    #body=[Return(value=Compare(left=Name(id='distancia'),
    #                           ops=[Lt()],
    #                           comparators=[Name(id='DISTANCIA_CASA')]))])
  elif type(node) == ast.Return:
    print "Return"
    bytecode = generate_bytecode(node.value,
                                 function_parameters,
                                 inside_function)
    bytecode.append({'opcode':'return'})
  elif type(node) == ast.Compare:
    print "Compare"
    left_bytecode = generate_bytecode(node.left,
                                      function_parameters,
                                      inside_function)
    oper_bytecode = generate_bytecode(node.ops[0])
    right_bytecode = generate_bytecode(node.comparators[0],
                                       function_parameters,
                                       inside_function)
    bytecode = []
    bytecode.extend(left_bytecode)
    bytecode.extend(right_bytecode)
    bytecode.extend(oper_bytecode)
  elif type(node) == ast.Lt:
    print "Lt"
    bytecode = [{'opcode': 'lt'}]
  elif type(node) == ast.Num:
    bytecode = [{'opcode': 'push', 'arg': node.n}]
  elif type(node) == ast.Name:
    print "Name"
    if node.id in function_parameters:
      print "Es parametro de la funcion"
      bytecode = [
        {'opcode':'load_param',
         'arg': function_parameters[node.id]}]
    else:
      bytecode = [{'opcode': 'load', 'arg': node.id}]
      # TODO: verificar scope (FALTA VARIABLES LOCALES)
  else:
    print "No reconoci nada"
    bytecode = []
  return bytecode

import json

def print_bytecode(bytecode):
  print '\n'.join([json.dumps(x) for x in bytecode])

import pdb; pdb.set_trace()
print "ayuda:"
print "generate_bytecode(tree)"
print "print_bytecode(bytecode)"
