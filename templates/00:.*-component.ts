
export class $1Controller {

  static $inject = [ 'path.to.injected.module' ];

  // Public variables
  constructor(private InjectedService: any) { }

  $onInit() {
    // Component intialization
  }

  $onChanges() {

  }

  templateBoundFunction() {

  }

  onSelect() {

  }

  private functionToDoSomething(thing: any): void {

  }
}

export class ${1:$(s-upper-camel-case yas-text)} implements angular.IComponentOptions {
  static $name = '$2';
  controller = ${1:$(s-upper-camel-case yas-text)}Controller;
  bindings = {
    someBinding: '<',
    onSelect: '&'
  };
  templateUrl = '${3:`default-directory`}${1:$(s-snake-case yas-text)}.html';
};
