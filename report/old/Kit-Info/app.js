
$(function(app) {
  //define view 
  var App = Backbone.View.extend({
    el: 'body',
    render: function() {
      var html = "<h1>Families</h1><ul>";
      var createElem = function(name) {
        return "<li>" + name + "</li>";
      };
      window.families.each(function(model) {
        html += createElem(model.get('name'));
      });
      html += "</ul>";
      this.$el.html(html);
      return this;
    }
  });
  //create view instance
  var view = new App();
  view.render();
});
