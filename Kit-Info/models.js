
var Models = Models || {},
    Collections = Collections || {};

/* Models */
Models.Family = Backbone.Model.extend({
  defaults: {
    name: undefined,
    kits: undefined //Collections.Kit
  },
  url: function() {
    return '/family' + this.get('name');
  }
});

Models.Kit = Backbone.Model.extend({
  defaults: {
    'name': '', 
    'family': undefined, // Models.Family
    'type': undefined, // (TYPES) Board or Platform
    'processors': [], //First processor is main processor
    'others': []
  }
});

Models.Processor = Backbone.Model.extend({
  defaults: {
    architecture: undefined
  }
});

/* Collections */
Collections.Family = Backbone.Collection.extend({
  url: '/family',
  model: Models.Family
});

Collections.Kit = Backbone.Collection.extend({
  model: Models.Kit,
  url: '/kit'
});

Collections.Processor = Backbone.Collection.extend({
  model: Models.Processor,
  url: '/processor'
});

