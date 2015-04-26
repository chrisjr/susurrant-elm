/*
Assumes an elm application "main" with ports oscIn and oscOut
*/

(function (elmApp) {
    var socket = io.connect('http://127.0.0.1', { port: 8081, rememberTransport: false});
    socket.on('connect', function() {
        socket.emit('config',
            {
                server: {
                    port: 3333,
                    host: '127.0.0.1'
                },
                client: {
                    port: 3334,
                    host: '127.0.0.1'
                }
            }
        );
        elmApp.ports.oscConnection.send(true);
    });

    socket.on('message', function(obj) {
        console.log(obj);
        // elmApp.ports.oscIn.send(obj);
    });
    socket.on('disconnect', function() {
        elmApp.ports.oscConnection.send(false);
    });
    function flatten(xs) {
        if (xs.length == 0) return xs;
        return xs.reduce(function(a, b) {
            return a.concat(b);
        });
    }

    function oscSend(obj) {
        var address = obj[0],
            args = flatten(flatten(obj[1])),
            msg = {"address": address, "args": args};
        console.log(msg);
        socket.emit('message', msg);
    }

    elmApp.ports.oscOut.subscribe(oscSend)
})(main);
