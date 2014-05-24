processor: {
  "model": "string", (
  "vendor": "string",
  "
}

processor_models: {
  "model": "string" (primary_key),
  "architecture": "avr" | "arm"
}
var Architectures = {
  avr: 'avr',
  kit: 'kit'
}
var TYPES = {
  board: 'board',
  kit: 'kit'
}

var f_arduino = new Models.family({
  name: 'Arduino',
  kits: new Collections.Kit({});
});
var f_mbed = new Models.family({
  name: 'Mbed',
  kits: new Collections.Kit({});
});
var f_fischer = new Models.family({
  name: 'Fischer',
  kits: new Collections.Kit({})
});

var families = new Collections.family([f_arduino, f_mbed]);


