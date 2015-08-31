extern CODE code = {
  /*000:*/  t_push << 8,
  /*001:*/  0,
  /*002:*/  t_store << 8 | 0, //INPUT_CLOCK
  /*003:*/  t_push << 8,
  /*004:*/  1,
  /*005:*/  t_store << 8 | 1, //INPUT_DISTANCE
  /*006:*/  t_push << 8,
  /*007:*/  2,
  /*008:*/  t_store << 8 | 2, //INPUT_COLOR_LEFT
  /*009:*/  t_push << 8,
  /*010:*/  3,
  /*011:*/  t_store << 8 | 3, //INPUT_COLOR_RIGHT
  /*012:*/  t_read << 8 | 0, //signal 0
  /*013:*/  0, //input 0
  /*014:*/  t_read << 8 | 1, //signal 1
  /*015:*/  1, //input 1
  /*016:*/  t_read << 8 | 2, //signal 2
  /*017:*/  2, //input 2
  /*018:*/  t_read << 8 | 3, //signal 3
  /*019:*/  3, //input 3
  /*020:*/  t_halt << 8
};
