angular.module('numbers', [])
  .directive('chart', function() {
    return {
      restrict: 'A',
      link: function(scope, element, attrs) {
              attrs.$observe('chart', function(rawValue) {
                var data = JSON.parse(rawValue); //[4, 8, 15, 16, 23, 42];

                var x = d3.scale.linear()
                    .domain([0, d3.max(data)])
                    .range([0, 420]);

                var p = d3.select(element[0]).selectAll("div").data(data);
                p.enter().append("div")
                    .style("width", function(d) { return x(d) + "px"; })
                    .text(function(d) { return d; });
                p.transition()
                    .style("width", function(d) { return x(d) + "px"; })
                    .text(function(d) { return d; });
                p.exit().remove();
              });
            }
    };
  })
  .directive('tabs', function() {
    return {
      restrict: 'E',
      transclude: true,
      scope: {},
      controller: function($scope, $element) {
        var panes = $scope.panes = [];

        $scope.select = function(pane) {
        angular.forEach(panes, function(pane) { pane.selected = false; });
        pane.selected = true;
      };

      this.addPane = function(pane) {
        if (panes.length == 0) $scope.select(pane);
        panes.push(pane);
        };
      },
      template:
        '<div class="tabbable">' +
        '<ul class="nav nav-tabs">' +
        '<li ng-repeat="pane in panes" ng-class="{active:pane.selected}">'+
        '<a href="" ng-click="select(pane)">{{pane.title}}</a>' +
        '</li>' +
        '</ul>' +
        '<div class="tab-content" ng-transclude></div>' +
        '</div>',
      replace: true
    };
  })
  .directive('pane', function() {
    return {
      require: '^tabs',
      restrict: 'E',
      transclude: true,
      scope: { title: '@' },
      link: function(scope, element, attrs, tabsCtrl) {
        tabsCtrl.addPane(scope);
      },
      template: '<div class="tab-pane" ng-class="{active: selected}" ng-transclude></div>',
      replace: true
    };
  })