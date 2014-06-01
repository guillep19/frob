

(function(app) {
  var Models = app.Models;

  var f_arduino = new Models.family({
    name: 'Arduino',
    kits: new Collections.kit();
  });
  var f_mbed = new Models.family({
    name: 'Mbed',
    kits: new Collections.kit();
  });
  var f_fischer = new Models.family({
    name: 'Fischer',
    kits: new Collections.kit()
  });
  var f_robotis = new Models.family({
    name: 'Robotis',
    kits: new Collections.kit();
  });

  app.families = new Collections.family([
                   f_arduino, 
                   f_mbed, 
                   f_fischer, 
                   f_robotis
                 ]);
})(this);
