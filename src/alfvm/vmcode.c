
#include "vmcode.h"

CODE code = {
  /*000:*/  t_push << 8,
  /*001:*/  2,
  /*002:*/  t_store << 8 | 0, //INPUT_COLOR_LEFT
  /*003:*/  t_push << 8,
  /*004:*/  3,
  /*005:*/  t_store << 8 | 1, //INPUT_COLOR_RIGHT
  /*006:*/  t_push << 8,
  /*007:*/  1,
  /*008:*/  t_store << 8 | 2, //OUTPUT_ENGINE_LEFT
  /*009:*/  t_push << 8,
  /*010:*/  2,
  /*011:*/  t_store << 8 | 3, //OUTPUT_ENGINE_RIGHT
  /*012:*/  t_push << 8,
  /*013:*/  100,
  /*014:*/  t_store << 8 | 4, //MIN_GREY
  /*015:*/  t_push << 8,
  /*016:*/  185,
  /*017:*/  t_store << 8 | 5, //MAX_SPEED
  /*018:*/  t_push << 8,
  /*019:*/  0,
  /*020:*/  t_store << 8 | 6, //MIN_SPEED
  /*021:*/  t_read << 8 | 0, //signal 0 (color_l)
  /*022:*/  0, //input 0 (COLOR_LEFT)
  /*023:*/  t_read << 8 | 1, //signal 1 (color_r)
  /*024:*/  1, //input 1 (COLOR_RIGHT)
  /*025:*/  t_lift << 8 | 2, // speed_left
  /*026:*/  0, // color_l
  /*027:*/  36, //color2speed
  /*028:*/  t_lift << 8 | 3, // speed_right
  /*029:*/  1, // color_r
  /*030:*/  36, //color2speed
  /*031:*/  t_write << 8 | 2, //speed_left
  /*032:*/  2, //OUTPUT_ENGINE_LEFT
  /*033:*/  t_write << 8 | 3, //speed_right
  /*034:*/  1, //OUTPUT_ENGINE_RIGHT
  /*035:*/  t_halt << 8,
  /*color2speed:*/
  /*036:*/  t_load_param << 8 | 0, //grey
  /*037:*/  t_load << 8 | 4, //MIN_GREY
  /*038:*/  t_cmp_gt << 8, //grey > MIN_GREY
  /*039:*/  t_jump_false << 8,
  /*040:*/  43, //color2speed.else
  /*041:*/  t_load << 8 | 5, //MAX_SPEED
  /*042:*/  t_ret << 8,
  /*color2speed.else*/
  /*043:*/  t_load << 8 | 6, //MIN_SPEED
  /*044:*/  t_ret << 8
};

