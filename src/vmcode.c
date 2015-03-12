/* example program:
   ----------------
   Turns a led on, when a button is on.
   ------------------------------------
    //configuration values:
    button = 0
    led = 1
    //program:
    i = 0
    while i <= 5:
      i = i + 1
      value = read(button)
      if value == 1:
        write(led, 1)
      else:
        write(led, 0)
*/

extern CODE code = {
  //button->0, led->1, i->2, value->3
  push << 8 | 0,
  store << 8 | 0, //button = 0
  push << 8 | 1,
  store << 8 | 1, //led = 1
  push << 8 | 0,
  store << 8 | 2, //i = 0
  //while:
  push << 8 | 19,
  push << 8 | 5, //push 5
  load << 8 | 2, //push i
  jump_gt << 8, //i> 5 goto 'end'
    load << 8 | 0,
    push << 8 | 1,
    add << 8,
    store << 8 | 1, //i = i + 1
    read << 8 | 0, // read globals[0] (button)
    store << 8 | 3, //value = read(button)
    push << 8 | 5, //d(jump_neq, else)
    load << 8 | 3,
    push << 8 | 1,
    jump_neq << 8, //goto 'else'
      push << 8 | 1,
      write << 8 | 1, //write(led, 1)
      push << 8 | 4,
      jump << 8, //goto endif
      //else:
      push << 8 | 0,
      write << 8 | 1, //write(led, 0)
    //'endif'
    push << 8 | (0x00ff & -22),
    jump << 8, //goto while
  //'end'
  halt << 8
  //58 bytes 
}; 

/*
//12 bytes
  push << 8 | 10,
  push << 8 | 20,
  store << 8,
  load << 8,
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
*/

