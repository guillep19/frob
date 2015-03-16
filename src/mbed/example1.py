#!/usr/bin/env python

import ast

source="""
button = 0
led = 1
i = 0
while i <= 5:
  i = i + 1
  value = read(button)
  if value == 1:
    write(led, 1)
  else:
    write(led, 0)
"""

abs_tree = ast.parse(source)
import pdb; pdb.set_trace()
