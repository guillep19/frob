
var arduino_family = new Models.Family({
  name: 'Arduino',
  kits: new Collections.Kit()
});

var k_arduino = new Models.Kit({
  family: 'Arduino',
  type: TYPES.board,
  name: 'Uno'
});

