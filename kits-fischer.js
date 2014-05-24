var k_fischer_1 = new Models.kit({
  family: f_fischer,
  type: TYPES.kit,
  name: 'ROBO TX Automation Robots',
  description: 'Four reality-based and fully functional industrial robots:' +
               ' High-rack storage, 3-axis robot and 2 other grappler' +
               ' robots. Stable fischertechnik aluminum channels used in' +
               ' all models. The instructional activity booklet provides' +
               ' support in the form of background information, projects' +
               ' and programming tips.' +
               ' For programmers and designers from age 10. ' +
               '  Including instructional activity booklet on CD' +
               '  Incl. assembly instruction booklet' +
               ' incl. 2x »Encoder Motors«, 2x »XS Motors«, 6x Sensors' +
               ' Additionally required: ROBO TX Controller,' +
               ' Software ROBO Pro, Power Set or Accu Set'
});
var k_fischer_2 = new Models.kit({
  family: f_fischer,
  type: TYPES.kit,
  name: 'ROBO TX ElectroPneumatic',
  description: 'The subjects of electro-pneumatics and vacuum technology are' +
               ' demonstrated clearly with the aid of fascinating models such' +
               ' as the pneumatic motor, color sorting robot for colored' +
               ' parts,  ball obstacle course and pinball machine' +
               'The new, powerful and compact compressor guarantees a' +
               ' reliable supply of compressed air to the models.' +
               ' The electro-magnetic valves included allow remote' +
               ' control of the models with a PC.' +
               'Incl. related instructional material on CD' +
               'Incl. compressor, Mini Motor, 2x solenoid valves, optical' +
               ' color sensor, vacuum suction device, 3x cylinders with' +
               ' spring, 2x photo-transistors,' +
               ' 2x lens tip lamps, 11 flex-rails' +
               'Additionally required is:' +
               '  ROBO TX Controller' +
               '  Software ROBO Pro' +
               '  Power Set or Accu Set' 
});
var k_fischer_3 = new Models.kit({
  family: f_fischer,
  type: TYPES.kit,
  name: 'ROBO TX Explorer',
  description: 'Explore unknown areas, measure distances, follow trails,' +
               ' show driving directions by means of blinking signals,' +
               ' recognize colors, measure temperatures, avoid obstacles' +
               ' without touching them, recognize day and night, turn' +
               ' headlights on and off automatically and trigger an alarm' +
               ' etc. The ROBO TX Explorer sensors can do all of this and' +
               ' lots more:' +
               ' The NTC resistor, the photoresistor, the ultrasonic' +
               ' distance sensor, the infrared color sensor and the specially' +
               ' developed trail sensor. Thanks to two encoder motors, the' +
               ' tracks can be controlled precisely and steered' +
               ' synchronically.' +
               ' With the rescue robot, which is contained as a model, the' +
               ' construction set provides the ideal basis for' +
               ' participation in the RoboCup.' +
               ' Includes the didactic activity booklet. ' +
               ' Includes two encoder motors, three indicator lights, buzzer,' +
               ' NTC resistor, photoresistor, ultrasonic distance sensor,' +
               ' optical color sensor and an IR trail sensor' +
               ' Additionally required is ROBO TX Controller, Software ROBO Pro, Accu Set'
});
var k_fischer_4 = new Models.kit({
  family: f_fischer,
  type: TYPES.kit,
  name: 'ROBOTICS LT Beginner Set',
  description: ''
});
var k_fischer_5 = new Models.kit({
  family: f_fischer,
  type: TYPES.kit,
  name: 'ROBOTICS TXT Discovery Set',
  description: ''
});

var k_fischer_board_1 = new Models.kit({
  family: f_fischer,
  type: TYPES.board,
  name: 'ROBO TX Controller',
  description: '',
  price: '5000 UY/$'
});

var k_fischer_board_2 = new Models.kit({
  family: f_fischer,
  type: TYPES.board,
  name: 'ROBOTICS TXT Controller',
  description: 'The compact ROBOTICS TXT Controller (90 x 90 x 25mm) can be' +
               ' controlled easily with the color 2.4" touch display. The' +
               ' combined Bluetooth/WiFi RF module provides the perfect,' +
               ' wireless interface for numerous applications. The numerous' +
               ' interfaces also include a USB host port for USB sticks and' +
               ' other components such as the fischertechnik USB camera.' +
               ' The integrated Micro SD card slot allows expansion of the' +
               ' memory capacity. Controllers can be coupled.'
  processors: [arm_cortex_a8, arm_cortex_m3]
  
  • Dual Processor: ARM Cortex A8 (32bit/500mHz) + Cortex M3
  
  • Memory capacity: 128 MB DDR3 RAM, 64 MB Flash
  
  • Memory expansion Micro SD card slot
  
  • Display: Color 2.4" touch display (320x240 pixels)
  
  • 8 Universal inputs: Digital/analog 0-9VDC, analog 0-5 kΩ
  
  • 4 high speed numerical inputs: Digital, frequency up to 1kHz
  
  • 4 Motor outputs 9V/250mA (max: 800 mA): speed infinitely controllable, short-circuit proof, alternative 8 single outputs for components such as lights, etc.
  
  • Combined Bluetooth/WiFi RF module: BT 2.1 EDR+ 4.0, WLAN 802.11 b/g/n
  
  • Infrared receiver diode: for fischertechnik Control Set transmitter
  
  • USB 2.0 Client: Mini USB port for connection to PC
  
  • USB Host interface: USB A port for fischertechnik USB camera, USB sticks, etc.
  
  • Camera interface: over USB Host, Linux camera driver integrated into operating system
  
  • 10-pin male connector for additional inputs and outputs as well as I2C interface
  
  • Integrated loud speaker
  
  • Integrated real time clock with exchangeable buffer battery: for capturing measured values within a defined period of time
  
  • Linux-based open source operating system
  
  • Programming possible with ROBO Pro, C-Compiler, PC-Library, and many others.
  
  • Link to smartphones/tablet PC's via Bluetooth or WLAN, allowing them to be used as terminals for the controller. Programming using ROBO-Pro software.
  
  • Dimensions: 90 x 90 x 25 mm
  
  • Power supply: 9V DC 3.45 mm socket, or 2.5 mm fischertechnik sockets (for Accu Set)'
});
var k_fischer_soft_1 = new Models.kit({
  family: f_fischer,
  type: TYPES.software,
  name: 'ROBO Pro Software',
  description: 'New graphic programmer\'s application'
               'Simple entry for beginners through programing of flow charts' +
               ' consisting of various software building blocks. The' +
               ' exchange of the data between the software building blocks' +
               ' and the subprograms can be done through variables and' +
               ' graphical connections as well.' +
               ' This allows the program functions to be shown in an' +
               ' understandable manner. There are no problems with the' +
               ' preparation of teach-in programs or data exchange with' +
               ' other Windows® software.' +
               ' Requirements:' +
               ' - Windows XP, Vista, 7 or 8' +
               ' - Minimum Pentium II 500MHz; 256MB RAM 50 MB HD free;' +
               ' min.1024x768 High Color 16 bit;' +
               ' 1 USB-port for ROBO TX Controller or ROBOTICS TXT Controller'
});


