describe('$1', function() {
  'use strict';

  var ${1:$(es/find-class-from-module-string yas-text)} = '${1:service.namespace.ServiceName}';

  beforeEach(function () {
    module('${2:path.to.my.module}');

    inject(function ($injector) {
      ${1:$(es/find-class-from-module-string yas-text)} = $injector.get('$1');
    });
  });

  describe('When I want to do something', function() {
    describe('And some condition is true', function() {
      it('should perform some action and fail', function(){
        var svc = new ${1:$(es/find-class-from-module-string yas-text)}();
        $0
        expect(svc.something).toBeDefined();
      });
    });
  });
});
