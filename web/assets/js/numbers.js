function NumbersCtrl($scope) {
  function createWebSocket(path) {
    var host = window.location.hostname;
    if (host == '') host = 'localhost';
    var uri = 'ws://' + host + ':9160' + path;

    var Socket = "MozWebSocket" in window ? MozWebSocket : WebSocket;
    return new Socket(uri);
  }

  $scope.numbers = {};

  var socket = createWebSocket('/'); 
  // new SockJS('ws://localhost:9160', undefined, {protocols_whitelist: ['websocket']});
  socket.onopen = function() {
     socket.send("even 0-100 every 1s");
  };
  socket.onmessage = function(e) {
     $scope.$apply(function() {
       $scope.numbers = e.data;
     });
  };

}
