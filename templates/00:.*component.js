angular.module('${1:`js-proj-prefix-module`}').controller('$1.${2:$(s-upper-camel-case yas-text)}Ctrl', [
  function() {
    'use strict';
    var vm = this;

    vm.$onInit = function() {
      /* Handle any initialization logic here */
    };

    vm.$onDestroy = function() {
      /* Remove any sort of handlers */
    };

    vm.$onChanges = function (changesObj) {
      /* update changes based on bindings change */
    };
  }
]);

angular.module('$1').component('$2', {
  bindings: { },
  controller: '$1.${2:$(s-upper-camel-case yas-text)}Ctrl',
  controllerAs: '${2:$(s-lower-camel-case yas-text)}Ctrl',
  templateUrl: '$0`(file-name-sans-extension (buffer-file-name))`-template.html'
});
