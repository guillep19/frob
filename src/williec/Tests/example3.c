extern CODE code = {
  /*000:*/  t_push << 8,
  /*001:*/  0,
  /*002:*/  t_store << 8 | 0, //INPUT_CLOCK
  /*003:*/  t_push << 8,
  /*004:*/  1,
  /*005:*/  t_store << 8 | 1, //OUTPUT_ENGINE_L
  /*006:*/  t_push << 8,
  /*007:*/  2,
  /*008:*/  t_store << 8 | 2, //OUTPUT_ENGINE_R
  /*009:*/  t_read << 0, //signal 0
  /*010:*/  0, //input 0
  /*011:*/  t_write << 8 | 0, //signal 0
  /*012:*/  1, //OUTPUT_ENGINE_L
  /*013:*/  t_write << 8 | 0, //signal 0
  /*014:*/  2, //OUTPUT_ENGINE_R
  /*020:*/  t_halt << 8
};

