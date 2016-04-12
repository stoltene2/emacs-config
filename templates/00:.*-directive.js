

angular.module('${1:`js-proj-prefix-module`}').controller('$1.${2:$(s-upper-camel-case yas-text)}Ctrl', [
  function() {
    'use strict';
    var vm = this;
    $0
  }
]);

angular.module('$1').directive('$2', [
  function() {
    'use strict';
    return {
      bindings: { },
      controller: '$1.${2:$(s-upper-camel-case yas-text)}Ctrl',
      controllerAs: '${2:$(s-lower-camel-case yas-text)}Ctrl',
      templateUrl: '`(file-name-sans-extension (buffer-file-name))`-template.html',
      bindToController: true
    };
  }
]);
