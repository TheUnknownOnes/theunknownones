##############################
#                            #
#        VNCWrapper          #
#   by TheUnknownOnes.net    #
##############################

Install
-------
To install copy the 2 resource-files from the units-directory
and the units of your choice into the search-path of delphi
or the search-path of your project.
Now, copy the package, you need for your Delphi-version out of
the bpl-directory into a folder of your choice and intall it via 
the menu "components -> install package".

Requirements
------------
In order to use the VNCWrapper you need Delphi [6,7,2005,2006] and
a operating system supported by RealVNC Free Edition. Your application
needs a folder ("BinaryPath") with write and execute rights.

The options
-----------

TVNCClientWrapper
    BinaryPath : String
      Path where the binaries are extracted and executed
    ZlibLevel : TZLibLevel
      Zlib compression level
    Listen : Boolean
      Accept incoming connections from VNC servers.
    AutoReconnect : Boolean
      Offer to reconnect to the remote server if the connectionis dropped because an error occurs.
    MenuKey : TMenuKey
      The key which brings up the popup menu
    Monitor : String
      The monitor to open the VNC Viewer window on, if available.
    AcceptBell : Boolean
      Produce a system beep when requested to by the server.
    Emulate3 : Boolean
      Emulate middle mouse button when left and right buttons are used simulatenously.
    PointerEventDelay : Word
      The interval to delay between sending one pointer event and the next.
    Protocol33 : Boolean
      Only use protocol version 3.3
    DisableWinKeys : Boolean
      Pass special Windows keys directly to the server.
    ServerCutText : Boolean
      Accept clipboard changes from the server.
    ClientCutText : Boolean
      Send clipboard changes to the server.
    SendKeyEvents : Boolean
      Send key presses (and releases) to the server.
    SendPointerEvents : Boolean
      Send pointer (mouse) events to the server.
    Shared : Boolean
      Allow existing connections to the server to continue.(Default is to disconnect all other clients)
    AutoSelect: Boolean 
      Auto select pixel format and encoding
    PreferredEncoding : TEncoding
      Preferred graphical encoding to use - overridden by AutoSelect if set. (ZRLE, Hextile or Raw)
    ullScreen : Boolean
      Use the whole display to show the remote desktop.(Press MenuKey to access the viewer menu)
    LowColourLevel : TColorLevel
      Colour level to use on slow connections. 
        0 = Very Low (8 colours)
        1 = Low (64 colours)
        2 = Medium (256 colours)
    
TVNCServerWrapper
    BinaryPath : String
      Path where the binaries are extracted and executed
    MaxCutText : LongWord
      Maximum permitted length of an incoming clipboard update
    PollConsoleWindows : Boolean
      Server should poll console windows for updates
    ZlibLevel : TZLibLevel
      Zlib compression level
    UseCaptureBlt : Boolean
      Use a slower capture method that ensures that alpha blended windows appear correctly
    DeadKeyAware : Boolean
      Whether to assume the viewer has already interpreted dead key sequences into latin-1 characters
    QueryConnect : Boolean
      Prompt the local user to accept or reject incoming connections.
    SendCutText : Boolean
      Send clipboard changes to clients.
    AcceptCutText : Boolean
      Accept clipboard updates from clients.
    AcceptPointerEvents : Boolean
      Accept pointer press and release events from clients.
    AcceptKeyEvents : Boolean
      Accept key press and release events from clients.
    DisconnectClients : Boolean
      Disconnect existing clients if an incoming connection is non-shared. 
      If combined with NeverShared then new connections will be refused while there is a client active
    NeverShared : Boolean
      Never treat incoming connections as shared, regardless of the client-specified setting
    AlwaysShared : Boolean
      Always treat incoming connections as shared, regardless of the client-specified setting
    Protocol33 : Boolean
      Always use protocol version 3.3 for backwards compatibility with badly-behaved clients
    CompareFB : Boolean
      Perform pixel comparison on framebuffer to reduce unnecessary updates
    ClientWaitTimeMillis : Word
      The number of milliseconds to wait for a client which is no longer responding
    IdleTimeout : Word
      The number of seconds after which an idle VNC connection will be dropped (zero means no timeout)
    RemapKeys : TStrings
      Comma-separated list of incoming keysyms to remap.  
      Mappings are expressed as two hex values, prefixed by 0x, and separated by ->
    BlacklistTimeout : Word
      The initial timeout applied when a host is first black-listed.  
      The host cannot re-attempt a connection until the timeout expires.
    BlacklistThreshold : Byte
      The number of unauthenticated connection attempts allowed from any individual host before that host is black-listed
    Password : String
      The password which clients must supply to access the server
    PasswordFile : String
      Password file for VNC authentication
    ReverseSecurityType : TReverseSecurityType
      Specify encryption scheme to use for reverse connections (None)
    SecurityType : TSecurityType
      Specify which security scheme to use for incoming connections (None, VncAuth)
    DisableEffects : Boolean
      Disable desktop user interface effects when the server is in use.
    RemovePattern : Boolean
      Remove the desktop background pattern when the server is in use.
    RemoveWallpaper : Boolean
      Remove the desktop wallpaper when the server is in use.
    DisplayDevice : String
      Display device name of the monitor to be remoted, or empty to export the whole desktop.
    DisconnectAction : TDisconnectAction
      Action to perform when all clients have disconnected. (None, Lock, Logoff)
    DisableLocalInputs : Boolean
      Disable local keyboard and pointer input while the server is in use
    UpdateMethod : TUpdateMethod
      How to discover desktop updates; 
        0 - Polling
        1 - Application hooking
        2 - Driver hooking.
    QueryOnlyIfLoggedOn : Boolean
      Only prompt for a local user to accept incoming connections if there is a user logged on
    LocalHost : Boolean
      Only accept connections from via the local loop-back network interface
    Hosts : TStrings
      ilter describing which hosts are allowed access to this server
    PortNumber : Word
      TCP/IP port on which the server will accept connections
    HTTPPortNumber : Word
      TCP/IP port on which the server will serve the Java applet VNC Viewer
    DisableClose : Boolean
      Disable the Close entry in the VNC Server tray menu.
    QueryConnectTimeout : Byte
      Number of seconds to show the Accept Connection dialog before rejecting the connection