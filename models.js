
var Models = Models || {};

var Models.Family = Backbone.Model.extend({
  defaults: {
    name: undefined,
    kits: undefined //Collections.Kit
  },
  url: '/family' + this.get('name')
});

var Collections.Family = Backbone.Collection.extend({
  url: '/family'
});

var Models.Kit = Backbone.Model.extend({
  defaults: {
    'name': '', 
    'family': undefined, // Models.Family
    'type': undefined, // (TYPES) Board or Platform
    'processors': [], //First processor is main processor
    'others': []
  }
});

var Collections.Kit = Backbone.Collection.extend({
  model: Models.Kit
});


var Models.Processor = Backbone.Model.extend({
  defaults: {

  }
});

kit: {
  "name": "string" (primary key)
  "type": "platform" | "board"
}

platform: {
  "name": "string" (foreign key (kit))
  ...
}

board: {
  "name": "string", (foreign key (kit))
  "processor": "string", (foreign key (processor))
  "other_processors": (can be null)
}

processor: {
  "model": "string", (
  "vendor": "string",
  "
}

processor_models: {
  "model": "string" (primary_key),
  "architecture": "avr" | "arm"
}

Family 

Kit = new Models.kit({
  family: 'Arduino',
  name: ''


