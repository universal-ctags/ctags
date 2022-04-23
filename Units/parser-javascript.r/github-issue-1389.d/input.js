var Namespace = Ember.Namespace = Ember.Object.extend=({
  isNamespace: true,

  init: function() {
    Ember.Namespace.NAMESPACES.push(this);
    Ember.Namespace.PROCESSED = false;
  }
});

// https://github.com/universal-ctags/ctags/issues/1389
