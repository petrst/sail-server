var ws;
function connect(service,onmessage, onopen, onerror, onclose){
	if ("MozWebSocket" in window) {
		WebSocket = MozWebSocket;
	}
	if ("WebSocket" in window) {
		ws = new WebSocket("ws://localhost:8080/websocket/"+service);

        if ( onopen==undefined ){
            ws.onopen = function() {
                console.log("websocket connected to /"+service);
            };
        } else {
            ws.onopen = onopen;
        }

		ws.onmessage = myonmessage; 
		ws.onclose = function() {
			console.log("websocket closed");
		};
	} else {
		// browser does not support websockets
		console.log("sorry, your browser does not support websockets.");
	}
}
