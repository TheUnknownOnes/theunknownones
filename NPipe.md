### What is NPipe? ###

TNPipeServer and TNPipeClient are two components for Delphi [6|7|2005] enabling you to easily set up interprocess communication.
This is achieved by utilizing named pipes. Since this feature is only available in Windows NT, 2000 and upwards it will only work on those systems.
### The components ###

**TNPipeServer** is the server component. It allows you to set up a named pipe server which can be accessed by various TNPipeClients (though not simultaniously). You can specify how this server can be accessed. Either local only or from the whole network. (For a network server you need to have enabled file and printer sharing on the host)

Some of the events

  * **OnStartWork** is raised after the server has been told how large the incoming data will be but before the data actually is transmitted.
  * **OnProgress** is raised after a chunk of data (32kb) has been received. Using this and the above event only makes sense if you want to show a progress bar and with a larger amount of data.
  * **OnIncomingData** is raised after all data from the client has been read. Then you have the chance to send a reply to the client.



**TNPipeClient** enables you to connect to a running TNPipeServer. A "." in the PipeServer property tells the client to connect to a server running on the local machine, whereas the IP or hostname connect you to a server over the network. (To send over the network you need to have the Microsoft Network Client installed on the client)

Some of the events

  * **OnStartWork** is raised after the client has told the server how large the outgoing data will be and a second time after the server has sent the size of its reply. The workmode parameter will tell you whether you are about to send or to receive.
  * **OnProgress** is raised after a chunk of data has been either sent or received.
  * **OnServerReply** is raised after all reply data from the server has been read.

To send data you simply use the function Send() or SendBuffer() from the client.

Server and Client naturally have to share the same pipe name.