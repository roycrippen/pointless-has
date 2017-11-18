function createChatSocket() {
    if (window.location.host == '') {
        /* Running on localhost */
        return new WebSocket('ws://localhost:9160/');
    } else {
        /* Running in "production" */
        return new WebSocket('wss://jaspervdj.be/websockets/example/chat/');
    }
}


function onMessage(event) {
    var p = $(document.createElement('p')).text(event.data);

    $('#messages').append("<br /> message: <br />").append(p);
    $('#messages').animate({
        scrollTop: $('#messages')[0].scrollHeight
    });

}

$(document).ready(function () {
    // $('#join-form').submit(function () {
    $('#warnings').html('');

    var ws = new WebSocket('ws://localhost:9160/');

    ws.onopen = function () {
        ws.send("pointless_connection");
    };

    ws.onmessage = function (event) {
        console.log(event.data)
        if (event.data = "pointless") {

            $('#chat-section').show();

            ws.onmessage = onMessage;
            $('#join-form').submit(function () {
                var topic = $('#user').val();
                // var text = $('#text').val();
                ws.send(topic);
                $('#user').val('');
                return false;
            });
        } else {
            $('#warnings').append(event.data);
            ws.close();
        }
    };

    $('#join').append('Connecting...');

    return false;
    // });
});