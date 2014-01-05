function createWebSocket(path) {
    var host = window.location.hostname;
    if(host == '') host = 'localhost';
    var uri = 'ws://' + host + ':9160' + path;

    var Socket = "MozWebSocket" in window ? MozWebSocket : WebSocket;
    return new Socket(uri);
}

var users = [];

function onMessage(event) {
    console.log(event.data);
}

$(document).ready(function () {
    $('#join-form').submit(function () {
        $('#warnings').html('');
        var ws = createWebSocket('/');

        ws.onopen = function() { ws.send("even 0-100 every 2s"); };
        ws.onmessage = onMessage; 

        return false;
    });
});