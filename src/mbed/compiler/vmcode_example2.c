extern CODE code = {
  /*000:*/  push << 8,
  /*001:*/  1,
  /*002:*/  store << 8 | 0,
  /*003:*/  push << 8,
  /*004:*/  2,
  /*005:*/  store << 8 | 1,
  /*006:*/  push << 8,
  /*007:*/  3,
  /*008:*/  store << 8 | 2,
  /*009:*/  push << 8,
  /*010:*/  1,
  /*011:*/  store << 8 | 3,
  /*012:*/  push << 8,
  /*013:*/  2,
  /*014:*/  store << 8 | 4,
  /*015:*/  push << 8,
  /*016:*/  100,
  /*017:*/  store << 8 | 5,
  /*018:*/  push << 8,
  /*019:*/  50,
  /*020:*/  store << 8 | 6,
  /*021:*/  start << 8,
  /*022:*/  30,
  /*023:*/  start << 8,
  /*024:*/  74,
  /*025:*/  halt << 8,
  /*026:*/  load_param << 8 | 0,
  /*027:*/  load << 8 | 5,
  /*028:*/  cmp_lt << 8,
  /*029:*/  ret << 8,
  /*030:*/  push << 8,
  /*031:*/  0,
  /*032:*/  store << 8 | 7,
  /*033:*/  push << 8,
  /*034:*/  0,
  /*035:*/  store << 8 | 8,
  /*036:*/  load << 8 | 7,
  /*037:*/  push << 8,
  /*038:*/  2,
  /*039:*/  cmp_lt << 8,
  /*040:*/  jump_false << 8,
  /*041:*/  65,
  /*042:*/  read << 8 | 0,
  /*043:*/  store << 8 | 9,
  /*044:*/  load << 8 | 9,
  /*045:*/  call << 8,
  /*046:*/  26,
  /*047:*/  op_not << 8,
  /*048:*/  jump_false << 8,
  /*049:*/  55,
  /*050:*/  push << 8,
  /*051:*/  0,
  /*052:*/  store << 8 | 8,
  /*053:*/  jump << 8,
  /*054:*/  63,
  /*055:*/  push << 8,
  /*056:*/  1,
  /*057:*/  store << 8 | 8,
  /*058:*/  load << 8 | 7,
  /*059:*/  push << 8,
  /*060:*/  1,
  /*061:*/  add << 8,
  /*062:*/  store << 8 | 7,
  /*063:*/  jump << 8,
  /*064:*/  36,
  /*065:*/  stop << 8,
  /*066:*/  74,
  /*067:*/  start << 8,
  /*068:*/  111,
  /*069:*/  halt << 8,
  /*070:*/  load_param << 8 | 0,
  /*071:*/  load << 8 | 6,
  /*072:*/  cmp_lt << 8,
  /*073:*/  ret << 8,
  /*074:*/  push << 8,
  /*075:*/  1,
  /*076:*/  jump_false << 8,
  /*077:*/  110,
  /*078:*/  read << 8 | 1,
  /*079:*/  store << 8 | 10,
  /*080:*/  read << 8 | 2,
  /*081:*/  store << 8 | 11,
  /*082:*/  push << 8,
  /*083:*/  1,
  /*084:*/  store << 8 | 12,
  /*085:*/  push << 8,
  /*086:*/  1,
  /*087:*/  store << 8 | 13,
  /*088:*/  load << 8 | 10,
  /*089:*/  call << 8,
  /*090:*/  70,
  /*091:*/  jump_false << 8,
  /*092:*/  96,
  /*093:*/  push << 8,
  /*094:*/  0,
  /*095:*/  store << 8 | 13,
  /*096:*/  load << 8 | 11,
  /*097:*/  call << 8,
  /*098:*/  70,
  /*099:*/  jump_false << 8,
  /*100:*/  104,
  /*101:*/  push << 8,
  /*102:*/  0,
  /*103:*/  store << 8 | 12,
  /*104:*/  load << 8 | 12,
  /*105:*/  write << 8 | 3,
  /*106:*/  load << 8 | 13,
  /*107:*/  write << 8 | 4,
  /*108:*/  jump << 8,
  /*109:*/  74,
  /*110:*/  halt << 8,
  /*111:*/  push << 8,
  /*112:*/  0,
  /*113:*/  write << 8 | 3,
  /*114:*/  push << 8,
  /*115:*/  0,
  /*116:*/  write << 8 | 4,
  /*117:*/  halt << 8
};
