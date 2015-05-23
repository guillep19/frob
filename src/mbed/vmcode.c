extern CODE code = {
  /*000:*/  t_push << 8,
  /*001:*/  1,
  /*002:*/  t_store << 8 | 0, //sensor_d
  /*003:*/  t_push << 8,
  /*004:*/  1,
  /*005:*/  t_store << 8 | 1, //led
  /*006:*/  t_read << 8 | 0, //ev.d 0
  /*007:*/  0, //sensor_d
  /*008:*/  t_lift << 8 | 1, //ev.d 1
  /*009:*/  0, //ev.s 0
  /*010:*/  14, //hay_casa(s)
  /*011:*/  t_write << 8 | 1, //led
  /*012:*/  1, //ev 1
  /*013:*/  t_halt << 8,
  /*hay_casa:*/
  /*014:*/  t_load_param << 8 | 0,
  /*015:*/  t_push << 8,
  /*016:*/  100,
  /*017:*/  t_cmp_lt << 8,
  /*018:*/  t_ret
};
