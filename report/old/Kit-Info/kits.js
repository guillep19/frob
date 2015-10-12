
(function(app) {
  var Models = app.Models;

  var f_arduino = new Models.Family({
    name: 'Arduino',
    kits: new Collections.Kit()
  });
  var f_fischer = new Models.Family({
    name: 'Fischer',
    kits: new Collections.Kit()
  });
  var f_mbed = new Models.Family({
    name: 'Mbed',
    kits: new Collections.Kit()
  });
  var f_robotis = new Models.Family({
    name: 'Robotis',
    kits: new Collections.Kit()
  });

  app.families = new Collections.Family([
                   f_arduino, 
                   f_mbed, 
                   f_fischer, 
                   f_robotis
                 ]);
})(this);
