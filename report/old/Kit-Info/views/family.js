
var Views.FamilyListItem = Backbone.View.extend({
  initialize: function() {

  },
  render: function() {
    this.$el.html(
  }

var Views.FamilyList = Backbone.View.extend({
  initialize: function() {
    this.model.on('change', this.render()); 
  },
  render: function() {
    var html = <li> + 
    this.$el.html();
    window.families.each(function(model) {
      var v = 
    });
  }
});
