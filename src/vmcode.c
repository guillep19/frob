
extern CODE code = {
  push | 10,
  push | 20,
  add, //30
  push | 5,
  sub, //25
  push | 3,
  jump, //goto ip+3
  push | 30,
  sub, //no lo hace
  push | 75,
  add, //25 + 75
  sub | 4,
  push | 16,
  halt
}; //12 bytes

