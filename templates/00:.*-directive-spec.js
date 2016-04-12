describe('$1', function(){
  'use strict';
  var $controller,
      ctrlName = '${1:ctrl.namespace.SomethingCtrl}';

  beforeEach(function () {
    module('${2:path.to.my.module}');

    inject(function ($injector) {
      $controller = $injector.get('$controller');
    });
  });

  describe('When I want to do something', function() {
    describe('And some condition is true', function() {
      it('should perform some action and fail', function(){
        var ctrl = $controller(ctrlName, {/* DI obj */}, {/*Bindings obj*/});
        $0
        expect(ctrl.something).toBeDefined();
      });
    });
  });
});
