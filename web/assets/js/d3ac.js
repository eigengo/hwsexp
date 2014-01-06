angular.module('dthree', [])
  .directive('chart', function() {
    return {
      restrict: 'A',
      link: function(scope, element, attrs) {
              attrs.$observe('chart', function(rawValue) {
                var data = JSON.parse(rawValue);

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

