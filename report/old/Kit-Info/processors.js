
(function(app) {
  var Models = app.Models;

  var arm_cortex_m3 = new Models.Processor({
    architecture: ARCHITECTURES.arm,
    bits: 32
  });

  var arm_cortex_a8 = new Models.Processor({
    architecture: ARCHITECTURES.arm,
    specific_architecture: "ARMv7",
    bits: 32,
    cores: 1,
    frequency_min: "600MHz",
    frequency_max: "1GHz",
    caches: [
      {level: 1, type: 'instruction', size: '16-32k'},
      {level: 1, type: 'data', size: '16-32k'},
      {level: 2, type: 'integrated'}
    ],
    bus: {width: '64-or 128-bit', name: 'AMBA3 Bus Interface'},
    extras: [
      'NEON Data Engine',
      'Floating Point Unit'
    ]
  });

  app.processors_hash = {
    'arm_cortex_m3': arm_cortex_m3,
    'arm_cortex_a8': arm_cortex_a8
  }
  app.processors = new Collections.Processor([
                     arm_cortex_m3,
                     arm_cortex_a8
                   ]);
})(this);

