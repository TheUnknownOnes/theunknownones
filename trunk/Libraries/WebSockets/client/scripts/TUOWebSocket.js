/* 
 * TUOWebSocket Client Script
 */

TUOWebSocketDebug = {
     debug : true /* set to false to disable logging */
    ,console : window.console /* here the loggings will appear */
    ,logMessage : function(message){
        if (this.debug && this.console) {this.console.log(message)}
    }
    ,logError : function(message){
        if (this.debug && this.console) {this.console.error(message)}
    }
}

TUOWebSocket = function(aHost,
                        aPort,
                        aAppName,
                        aResponseObjectName){
     this.internalWebSocket = null;
     this.host = aHost;
     this.port = aPort;
     this.appName = aAppName;
     this.responseObjectName = aResponseObjectName;
     this.guid = null;

     this.onOpen = null;
     this.onMessage = null;
     this.onError = null;
     this.onClose = null;
}

TUOWebSocket.prototype = {

     readyState: function() {
         if (this.internalWebSocket != null) {
            return this.internalWebSocket.readyState;
         }
         else {
             return WebSocket.CLOSED;
         }
     }
    ,readyStateText: function(){
         switch(this.readyState()){
             case WebSocket.CONNECTING :return 'connecting';break
             case WebSocket.OPEN :return 'open';break
             case WebSocket.CLOSING :return 'closing';break
             case WebSocket.CLOSED :return 'closed';break
         }
     }
    ,logout: function(){
        if (this.internalWebSocket) {this.internalWebSocket.close();}
    }
    ,uri: function(){
        var luri = 'ws://'+this.host+':'+this.port+'/'+this.appName+'?vguid=this.wrapper.guid&cresponse='+this.responseObjectName+'.send';
        if (this.guid) {
            luri = luri + '&guid=' + this.guid
        }
        return luri;
    }

    ,send: function(data){
         TUOWebSocketDebug.logMessage('sending data '+data);
         if (this.readyState() != WebSocket.OPEN) {
             this.logError("WebSocket not ready for sending data ready state "+this.readyState()+' '+this.readyStateText());
         }
         else {
             this.internalWebSocket.send(data);
         }
     }
    ,login: function(){
         if (this.readyState() == WebSocket.CLOSED) {
            TUOWebSocketDebug.logMessage('connecting to '+this.uri());
            this.internalWebSocket = new WebSocket(this.uri());
            this.internalWebSocket.wrapper = this;
            this.internalWebSocket.onopen = function() {
                 TUOWebSocketDebug.logMessage('connection established');
                 if (this.wrapper.onOpen) {
                     this.wrapper.onOpen()
                 }
            }
            this.internalWebSocket.onmessage = function(evt) {
                 TUOWebSocketDebug.logMessage('message received '+evt.data);

                 if (this.wrapper.onMessage) {
                     this.wrapper.onMessage(data)
                 }
                /* try to find GUID in question */
                var guidmatch = evt.data.match("^(\{{1}([0-9a-fA-F]){8}-([0-9a-fA-F]){4}-([0-9a-fA-F]){4}-([0-9a-fA-F]){4}-([0-9a-fA-F]){12}\}{1})");
                if (!guidmatch) {
                    /* no GUID ... just do it */
                    eval(evt.data);
                }
                else {
                    prefix="O";
                    /* we found a GUID ... now it's time to get an answer */
                    try {
                        result=eval(evt.data.substr(38, evt.data.length));
                    }
                    catch(error) {
                        result=error.toString();
                        prefix="E";
                    }

                   this.wrapper.send(prefix+guidmatch[0]+result);
                }
            }
            this.internalWebSocket.onerror = function(evt) {
                 this.logError('error ocurred '+evt.data);
                 if (this.wrapper.onError) {
                     this.wrapper.onError(evt.data)
                 }
                 else {
                     alert(data)
                 }
            }
            this.internalWebSocket.onclose = function() {
                TUOWebSocketDebug.logMessage('connection closed');
                if (this.wrapper.onClose) {
                     this.wrapper.onClose()
                }
                this.wrapper.internalWebSocket=null;
            }
        }
        else {
            TUOWebSocketDebug.logError('already in connection state '+this.readyState()+' '+this.readyStateText());
        }
    }
}

/*
 * don't forget the IP, Port and Name of your ServerApp
 * Parameters:
 * Host                 Hostname or IP of Server
 * Port                 Serverport
 * AppName              Name of Application
 * ResponseObjectName   Name of object variable that holds the TUOWebSocket
 *                      object we need this for server communications. This must
 *                      be the name of the variable the object is actually
 *                      assigned to.
 *                      Please refer for sourcecode of server for it's usage.
 */
appTUOWebSocket_Marco = new TUOWebSocket('10.1.8.5',2680,'myGroovyApp','appTUOWebSocket_Marco');
appTUOWebSocket_Benny = new TUOWebSocket('10.1.8.2',2680,'myGroovyApp','appTUOWebSocket_Benny');