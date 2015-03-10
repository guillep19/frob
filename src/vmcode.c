
extern CODE code = {
  push << 8 | 10,
  push << 8 | 20,
  add << 8, //30
  push << 8 | 5,
  sub << 8, //25
  push << 8 | 3,
  jump << 8, //goto ip+3
  push << 8 | 30,
  sub << 8, //no lo hace
  push << 8 | 0xff,
  75,
  add << 8, //25 + 75
  push << 8 | 1,
  sub << 8,
  push << 8 | 16,
  halt << 8
}; //12 bytes

