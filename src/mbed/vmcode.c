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
  /*010:*/  25, //ev.d = hay_casa(ev.s)
  /*011:*/  t_write << 8 | 1, //ev 1
  /*012:*/  1, //led on board
  /*013:*/  t_push << 8,
  /*014:*/  1,
  /*015:*/  t_store << 8 | 2, //fold initial value
  /*015:*/  t_folds << 8 | 2, //ev 2
  /*016:*/  1, //ev 1
  /*017:*/  2, //fold initial value (a variable)
  /*018:*/  30, //ev2 = suma(acum, ev1)
  /*019:*/  t_lift << 8 | 3, //ev 3 (define 3)
  /*020:*/  2, //ev 2 (source)
  /*021:*/  34, //ev.3 = mayor_a_5(ev.2)
  /*022:*/  t_write << 8 | 3, //ev3
  /*023:*/  2, //led externo
  /*024:*/  t_halt << 8,
  /*hay_casa:*/
  /*025:*/  t_load_param << 8 | 0,
  /*026:*/  t_push << 8,
  /*027:*/  100,
  /*028:*/  t_cmp_lt << 8,
  /*029:*/  t_ret << 8,
  /*suma:*/
  /*030:*/  t_load_param << 8 | 0,
  /*031:*/  t_load_param << 8 | 1,
  /*032:*/  t_add << 8,
  /*033:*/  t_ret << 8,
  /*mayor_a_5:*/
  /*034:*/  t_load_param << 8 | 0,
  /*035:*/  t_push << 8,
  /*036:*/  5,
  /*037:*/  t_cmp_gt << 8,
  /*038:*/  t_ret << 8
};
