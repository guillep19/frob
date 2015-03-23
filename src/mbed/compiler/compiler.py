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

uid_gen = iter(xrange(1024))
restrictions = []

def generate_bytecode(node,
                      function_parameters=[],
                      inside_function=False):
  if type(node) == ast.Module:
    bytecode = []
    functions_bytecode = []
    for child in node.body:
      child_bytecode = generate_bytecode(child)
      if type(child) == ast.FunctionDef:
        functions_bytecode.extend(child_bytecode)
      else:
        bytecode.extend(child_bytecode)
    bytecode.append({'opcode': 'halt'})
    # El codigo de las funciones lo agrego al final, para que quede pronto
    # para ejecutar.
    bytecode.extend(functions_bytecode)
  elif type(node) == ast.Assign:
    opcode = 'local_store' if inside_function else 'store'
    bytecode = generate_bytecode(node.value,
                                 function_parameters,
                                 inside_function)
    bytecode.append(
      {'opcode': opcode, 'arg': node.targets[0].id}
    )
  elif type(node) == ast.FunctionDef:
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
      bytecode[0]['label'] = node.name
    else:
      print "ERROR out of index!"
    if not is_function:
      bytecode.append({'label': 'end_%s' % node.name, 'opcode': 'halt'})
    #FunctionDef(
    #body=[Return(value=Compare(left=Name(id='distancia'),
    #                           ops=[Lt()],
    #                           comparators=[Name(id='DISTANCIA_CASA')]))])
  elif type(node) == ast.While:
    uid = uid_gen.next()
    l_start = 'start_while_%s' % uid
    l_end = 'end_while_%s' % uid
    bytecode = generate_bytecode(node.test)
    bytecode[0]['label'] = l_start
    bytecode.append({'opcode': 'jump_false', 'arg': l_end})
    for child in node.body:
      child_bytecode = generate_bytecode(child,
                                         function_parameters,
                                         inside_function)
      bytecode.extend(child_bytecode)
    bytecode.append({'opcode': 'jump', 'arg': l_start})
    bytecode.append({'label': l_end, 'opcode': 'nop'})
  elif type(node) == ast.With:
    event = node.context_expr
    event_name = event.func.id
    task_name = event.args[0].id
    body_bytecode = []
    for child in node.body:
      child_bytecode = generate_bytecode(child,
                                         function_parameters,
                                         inside_function)
      body_bytecode.extend(child_bytecode)
    restrictions.append({'task': task_name,
                         'event': event_name,
                         'bytecode': body_bytecode})
    # No retorno nada, ya que lo inserto luego
    bytecode = []
  elif type(node) == ast.If:
    uid = uid_gen.next()
    l_else = 'else_%s' % uid
    l_end = 'end_if_%s' % uid
    bytecode = generate_bytecode(node.test)
    bytecode.append({'opcode': 'jump_false', 'arg': l_else})
    for child in node.body:
      child_bytecode = generate_bytecode(child,
                                         function_parameters,
                                         inside_function)
      bytecode.extend(child_bytecode)
    if node.orelse:
      bytecode.append({'opcode': 'jump', 'arg': l_end})
      else_code = []
      for child in node.orelse:
        child_bytecode = generate_bytecode(child,
                                           function_parameters,
                                           inside_function)
        else_code.extend(child_bytecode)
      else_code[0]['label'] = l_else
      bytecode.extend(else_code)
    bytecode.append({'label': l_end, 'opcode': 'nop'})
  elif type(node) == ast.Return:
    bytecode = generate_bytecode(node.value,
                                 function_parameters,
                                 inside_function)
    bytecode.append({'opcode':'return'})
  elif type(node) == ast.Compare:
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
    bytecode = [{'opcode': 'lt'}]
  elif type(node) == ast.Gt:
    bytecode = [{'opcode': 'gt'}]
  elif type(node) == ast.Num:
    bytecode = [{'opcode': 'push', 'arg': node.n}]
  elif type(node) == ast.Name:
    if node.id in function_parameters:
      bytecode = [
        {'opcode':'load_param',
         'arg': function_parameters[node.id]}]
    elif node.id == 'False':
      bytecode = [{'opcode': 'push', 'arg': 0}]
    elif node.id == 'True':
      bytecode = [{'opcode': 'push', 'arg': 1}]
    else:
      bytecode = [{'opcode': 'load', 'arg': node.id}]
      # TODO: verificar scope (FALTA VARIABLES LOCALES probar con ejemplo fib)
  elif type(node) == ast.BinOp:
    bytecode = generate_bytecode(node.left)
    bytecode.extend(generate_bytecode(node.right))
    bytecode.extend(generate_bytecode(node.op))
  elif type(node) == ast.Add:
    bytecode = [{'opcode': 'add'}]
  elif type(node) == ast.UnaryOp:
    bytecode = generate_bytecode(node.operand)
    bytecode.extend(generate_bytecode(node.op))
  elif type(node) == ast.Not:
    bytecode = [{'opcode': 'not'}]
  elif type(node) == ast.Expr:
    bytecode = generate_bytecode(node.value)
    #Expr(value=Call(func=Name(id='stop'), args=[Name(id='task_seguir_linea')]))
  elif type(node) == ast.Call:
    function_name = node.func.id
    if function_name == 'stop' or function_name == 'start':
      bytecode = []
      for arg in node.args:
        bytecode.append({'opcode': function_name, 'arg': arg.id})
    elif function_name == 'read':
      bytecode = []
      for arg in node.args:
        bytecode.append({'opcode': function_name, 'arg': arg.id})
    elif function_name == 'write':
      bytecode = generate_bytecode(node.args[1])
      bytecode.append({'opcode': function_name, 'arg': node.args[0].id})
    else:
      bytecode = []
      for arg in node.args:
        arg_bytecode = generate_bytecode(arg)
        bytecode.extend(arg_bytecode)
      bytecode.append({'opcode': 'call', 'arg': function_name})
  else:
    print 'ERROR: No reconoci el nodo:', node
    bytecode = []
  return bytecode

import json

def print_bytecode(bytecode):
  lineno = 0
  for inst in bytecode:
    line = '%d:' % lineno
    if inst.get('label') is not None:
      line += '[%s]' % inst.get('label')
    line += ' %s' % inst.get('opcode')
    if inst.get('arg') is not None:
      line += ' %s' % inst.get('arg')
    print line
    lineno += 1


#print "////////////////////////generate_bytecode////////////////////////"
bytecode = generate_bytecode(tree)
print "////////////////////////print_bytecode////////////////////////"
print_bytecode(bytecode)

from itertools import takewhile, dropwhile

def span(condition, xs):
  # Returns two lists, one is takewhile(condition, list) and the other is the rest.
  return ([x for x in takewhile(condition, xs)],
          [x for x in dropwhile(condition, xs)])

def insert_restrictions(restrictions, bytecode):
  for restriction in restrictions:
    if restriction['event'] == 'on_finish':
      print "Sustituyendo en on_finish de tarea %s" % restriction['task']
      label = 'end_%s' % restriction['task']
      condition = (lambda inst: inst.get('label') != label)
      (xs_1, xs_2) = span(condition, bytecode)
      bytecode = xs_1
      bytecode.extend(restriction['bytecode'])
      bytecode.extend(xs_2)
  return bytecode
import pdb; pdb.set_trace()
