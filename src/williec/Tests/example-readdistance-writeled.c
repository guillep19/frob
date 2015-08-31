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
  /*010:*/  26, //ev.d = hay_casa(ev.s)
  /*011:*/  t_write << 8 | 1, //ev 1
  /*012:*/  1, //led on board
  /*013:*/  t_push << 8,
  /*014:*/  1,
  /*015:*/  t_store << 8 | 2, //fold initial value
  /*016:*/  t_folds << 8 | 2, //ev 2
  /*017:*/  1, //ev 1
  /*018:*/  2, //fold initial value (a variable)
  /*019:*/  31, //ev2 = suma(acum, ev1)
  /*020:*/  t_lift << 8 | 3, //ev 3 (define 3)
  /*021:*/  2, //ev 2 (source)
  /*022:*/  35, //ev.3 = mayor_a_5(ev.2)
  /*023:*/  t_write << 8 | 3, //ev3
  /*024:*/  2, //led externo
  /*025:*/  t_halt << 8,
  /*hay_casa:*/
  /*026:*/  t_load_param << 8 | 0,
  /*027:*/  t_push << 8,
  /*028:*/  100,
  /*029:*/  t_cmp_lt << 8,
  /*030:*/  t_ret << 8,
  /*suma:*/
  /*031:*/  t_load_param << 8 | 0,
  /*032:*/  t_load_param << 8 | 1,
  /*033:*/  t_add << 8,
  /*034:*/  t_ret << 8,
  /*mayor_a_5:*/
  /*035:*/  t_load_param << 8 | 0,
  /*036:*/  t_push << 8,
  /*037:*/  5,
  /*038:*/  t_cmp_gt << 8,
  /*039:*/  t_ret << 8
};
