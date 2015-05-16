




fold(rises_last_update, (0, 0))

fold f v signal = fold f f(v, signal)


(fold f v src)




def f(inputs):
  x = procesar(inputs)
  return x
  
dst = (lift f src)


NodeLift {
  waiting: {0: src},
  memory: 0,
  function: f,
  output: 
}
