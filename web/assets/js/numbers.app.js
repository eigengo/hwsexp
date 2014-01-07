angular.module('numbers.app', ['d3.directives', 'numbers.directives'])
  .controller('NumbersCtrl', ['$scope', function($scope) {
    function createWebSocket(path) {
      var host = window.location.hostname;
      if (host == '') host = 'localhost';
      var uri = 'ws://' + host + ':9160' + path;

      var Socket = "MozWebSocket" in window ? MozWebSocket : WebSocket;
      return new Socket(uri);
    }

    $scope.numbers = {};
    $scope.expression = "evendistr 25 [0..100] forever every 1000ms";

    var socket = createWebSocket('/'); 
    socket.onopen = function() {
       //socket.send("even 0-100 every 1s");
    };
    socket.onmessage = function(e) {
       $scope.$apply(function() {
         $scope.numbers = e.data;
       });
    };

    $scope.executeExpression = function() {
      socket.send($scope.expression);
    };
  }]);
