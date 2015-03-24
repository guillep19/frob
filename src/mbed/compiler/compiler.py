#!/usr/bin/env python

import ast
import argparse
from itertools import takewhile, dropwhile

parser = argparse.ArgumentParser(description='Compiles a frob program to binary bytecode.')
parser.add_argument('-c', type=str, help='input filename', required=True)
args = parser.parse_args()

f = open(args.c, 'r')
source = f.read()
tree = ast.parse(source)

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
    bytecode = [{'label': node.name, 'opcode': 'nop'}]
    paramname_to_index = dict((p.id, i) for i, p in enumerate(node.args.args))
    is_function = not node.name.startswith("task_")
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
  elif type(node) == ast.While:
    uid = uid_gen.next()
    l_start = 'start_while_%s' % uid
    l_end = 'end_while_%s' % uid
    bytecode = [{'label': l_start, 'opcode': 'nop'}]
    bytecode.extend(generate_bytecode(node.test))
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
    if node.orelse:
      bytecode.append({'opcode': 'jump_false', 'arg': l_else})
    else:
      bytecode.append({'opcode': 'jump_false', 'arg': l_end})
    for child in node.body:
      child_bytecode = generate_bytecode(child,
                                         function_parameters,
                                         inside_function)
      bytecode.extend(child_bytecode)
    if node.orelse:
      bytecode.append({'opcode': 'jump', 'arg': l_end})
      else_code = [{'label': l_else, 'opcode': 'nop'}]
      for child in node.orelse:
        child_bytecode = generate_bytecode(child,
                                           function_parameters,
                                           inside_function)
        else_code.extend(child_bytecode)
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


def print_bytecode(bytecode):
  lineno = 0
  for inst in bytecode:
    line = '%d:' % lineno
    if inst.get('label') is not None:
      line += '[%s]' % inst.get('label')
    if inst.get('opcode') is not None:
      line += ' %s' % inst.get('opcode')
      if inst.get('arg') is not None:
        line += ' %s' % inst.get('arg')
    else:
      line += ' {}'.format(inst.get('value'))
    print line
    lineno += 1

def generate_c_code(bytecode):
  # Used to generate vmcode.c file.
  lineno = 0
  lines = []
  print "extern CODE code = {"
  for inst in bytecode:
    line = '  /*{0:0>3}:*/ '.format(lineno)
    if inst.get('opcode') is not None:
      line += ' {} << 8'.format(inst.get('opcode'))
      if inst.get('arg') is not None:
        line += ' | {}'.format(inst.get('arg'))
    else:
      line += ' {}'.format(inst.get('value'))
    lines.append(line)
    lineno += 1
  content = ',\n'.join(lines)
  print content
  print "};"



def span(condition, xs):
  # Returns two lists, one is takewhile(condition, list) and the other is the rest.
  return ([x for x in takewhile(condition, xs)],
          [x for x in dropwhile(condition, xs)])

def insert_restrictions(restrictions, bytecode):
  for restriction in restrictions:
    if restriction['event'] == 'on_finish':
      #print "Sustituyendo en on_finish de tarea %s" % restriction['task']
      label = 'end_%s' % restriction['task']
      condition = (lambda inst: inst.get('label') != label)
      (xs_1, xs_2) = span(condition, bytecode)
      bytecode = xs_1
      bytecode.extend(restriction['bytecode'])
      bytecode.extend(xs_2)
  return bytecode

def optimize_and_calculate_labels(bytecode):
  # Eliminates the nop instructions used to simplify bytecode generation.
  # Generates a map 'labels' where 'labels[label]'
  # stores the index in the final code.
  # Important: After this transformation, the instructions can not be
  # changed.
  # Returns: (bytecode, labels)
  globals_uid_gen = iter(xrange(1024))
  optimized = []
  labels = {}
  global_index = {}
  lineno = 0
  for inst in bytecode:
    length = 1
    if inst.get('label') is not None:
      labels[inst.get('label')] = lineno
    # TODO: Split code with length > 1 (like push or jump)
    opcode = inst.get('opcode')
    if opcode == 'push':
        length = 2
        optimized.append({'opcode': 'push', 'label': inst.get('label')})
        optimized.append({'value': int(inst.get('arg'))})
    elif opcode == 'jump' or opcode == 'jump_false':
        length = 2
        optimized.append({'opcode': opcode, 'label': inst.get('label')})
        optimized.append({'value': inst.get('arg')})
    elif opcode == 'call' or opcode == 'start' or opcode == 'stop':
        length = 2
        optimized.append({'opcode': opcode, 'label': inst.get('label')})
        optimized.append({'value': inst.get('arg')})
    elif opcode == 'load' or opcode == 'store':
        var_name = inst.get('arg')
        if global_index.get(var_name) is None:
            global_index[var_name] = globals_uid_gen.next() #TODO: Podria hacerlo mientras recorro el arbol
        optimized.append(inst)
    elif opcode == 'nop':
        length = 0
    else:
        # Dont modify the line
        optimized.append(inst)
    lineno += length
  return (optimized, labels, global_index)

def replace_labels(labels, global_index, bytecode):
  # Replace labels for relative or absolute values
  final_bytecode = []
  for inst in bytecode:
    opcode = inst.get('opcode')
    if opcode == 'load' or opcode == 'store':
      inst['arg'] = global_index.get(inst.get('arg'))
    elif opcode == 'read' or opcode == 'write': #(TODO"agrupar con load y store?)
      inst['arg'] = global_index.get(inst.get('arg'))
    elif opcode is None and inst.get('value') is not None:
      # Reemplazo la etiqueta por el numero de linea.
      value = inst.get('value')
      if type(value) == str and labels.get(value) is not None:
        inst['value'] = labels.get(value)
    else:
      pass
      #print "Dejo intacta opcode=", opcode
    final_bytecode.append(inst)
  return final_bytecode

# Generate bytecode and restrictions from tree
bytecode = generate_bytecode(tree)
# 
bytecode_wr = insert_restrictions(restrictions, bytecode)
(bytecode_opt, labels, global_index) = optimize_and_calculate_labels(bytecode_wr)
#print labels
#print global_index
bytecode_final = replace_labels(labels, global_index, bytecode_opt)
#print_bytecode(bytecode_final)


generate_c_code(bytecode_final)
