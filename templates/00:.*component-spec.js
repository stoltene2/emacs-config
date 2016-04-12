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

  describe('When I test the lifecycle events of the component', function() {
    describe('And I want to test the intialization', function() {
      it('should initialize the component', function(){
        var ctrl = $controller(ctrlName, {/* DI obj */}, {/*Bindings obj*/});
        ctrl.$onInit();
        $0
        expect(ctrl.something).toBeDefined();
      });
    });

    describe('And I want to test the destruction', function() {
      it('should destroy attached handlers', function(){
        var ctrl = $controller(ctrlName, {/* DI obj */}, {/*Bindings obj*/});
        ctrl.$onInit();
        /* spyOn something on the controller */
        ctrl.$onDestroy();
        expect(ctrl.somethingOnCtrol).toHaveBeenCalled();
      });
    });

    describe('And I want to test the binding updates', function() {
      it('should update values when the bindings change', function() {
        var ctrl = $controller(ctrlName, {/* DI obj */}, {/*Bindings obj*/});
        ctrl.$onInit();
        ctrl.$onChanges({/* obj of changes */});
        expect(ctrl.somethingOnCtrlToChange).toHaveBeenCalled();
      });
    });
  });
});
