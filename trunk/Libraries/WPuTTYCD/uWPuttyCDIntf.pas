unit uWPuttyCDIntf;

interface

uses
  Classes,
  Windows,
  Messages;

const
  DLLName = 'WPuTTYCD.dll';

type
  WPCD_HINST = Pointer;
  WP_INST = WPCD_HINST;

const
  TIMING_TIMER_ID = 1968;

  WM_ASYNC_NETEVENT = WM_APP + 151;
  WM_AGENT_CALLBACK = WM_APP + 152;

  WM_WPCD_NOTIFY = WM_APP + 150;

    {
    *                         Notify Modes
    *
    * WPCD_NOTIFY_OFF
    *    All Notifications are disabled
    * WPCD_NOTIFY_CONSTRAINED
    *    Notifications are enabled for both message and notify function,
    *    but with constrains on notify functions.
    *    In this mode if you miss use notify_func(); you may put
    *    your program in a deadlock condition.
    * WPCD_NOTIFY_RELAXED
    *    Notifications are enabled for both message and notify function,
    *    without constrains.
    *    This mode breaks the concurrency rules.
    *
    * To disable function notify, set notify_func() = NULL; default is disabled.
    * To disable message notify, set msg_notify =  0; default is WM_WPCD_NOTIFY
    *
    * Is developed _only_ for the C++ wrapper class and the OCX, so don’t use it.
    *
    }
  WPCD_NOTIFY_OFF = 0;
  WPCD_NOTIFY_CONSTRAINED = 1;
  WPCD_NOTIFY_RELAXED = 2;

    {
    * DWORD Mask to enable/disable each of the Notification Events
    * sent in notification message WPARAM as listed below.
    *
    *   1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    *  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
    *  |F|E|D|C|B|A|9|8|7|6|5|4|3|2|1|0|F|E|D|C|B|A|9|8|7|6|5|4|3|2|1|0|
    *  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
    *
    * For example
    *    If bit 00 is set this means WPCN_SESCONNECT is enabled
    *    If bit 08 is reset this means WPCN_RETRY is disabled
    *    If bit 0D is set this means WPCN_SESCLOSED is enabled
    }
  WPCN_ENABLEALL = $FFFFFFFF;  //* def all events enabled.	*/
  WPCN_NMWPARAM_MIN = $0001;    //* min value for NM WPARAM.*/
  WPCN_NMWPARAM_MAX = $0020;    //* max value for NM WPARAM.*/

  //* Macros for events, bit mask handling */
  function WPCD_ISVALIDEVENT(id_event : DWORD) : Boolean;
  function _WPCD_ISEVENTNOTIFY(nm_mask, id_event : DWORD) : Boolean;
  procedure _WPCD_SETEVENTNOTIFY(var nm_mask : DWORD; id_event : DWORD);
  procedure WPCD_RESETEVENTNOTIFY(var nm_mask : DWORD; id_event : DWORD);

const
  //* Notification message WPARAM */
  WPCN_SESCONNECT = $0001;    //* session connected.	*/
  WPCN_SESDATAREADY = $0002;    //* session data ready.	*/
  WPCN_SESDISCONNECT = $0003;    //* session disconnected.	*/
  WPCN_CRITICAL = $0004;    //* critical condition.	*/
  WPCN_SETCHANGE = $0005;    //* settings changed.	*/
  WPCN_BUSY = $0006;    //* ServEnt busy status.    */
  WPCN_FATAL = $0007;    //* fatal condition.        */
  WPCN_RETRY = $0008;    //* retry strt/end in LPARAM*/
  WPCN_CLEARSELECT = $0009;    //* clear selection.	*/
  WPCN_PENDINGCLOSE = $000A;    //* session waiting 4 close */
  WPCN_REMOTEEXIT = $000B;    //* session remotely closed.*/
  WPCN_SESABORTED = $000C;    //* session forced to close */
  WPCN_SESCLOSED = $000D;    //* session closed.         */
  WPCN_SESDESCLOSED = $000E;    //* session closed by user. */
  WPCN_SRVNTLERR = $000F;    //* ServEnt thread last err.*/

  //* Comm notification message Hints in LPARAM */
  WPCN_FALSE = $00000000;      //* Notify FALSE/NO.  */
  WPCN_TRUE = not WPCN_FALSE;	           //* Notify TRUE/YES.  */

    {* WPCN_SESDATAREADY notification message, Hints in LPARAM
     * WPCN_HOSTNAME
     *    Real Host Name is now ready.
     * WPCN_SPECIALSMENU
     *    A New Specials Menu is now ready.
     * WPCN_HOSTDATA
     *    New Host Data is now ready.
     }
  WPCN_HOSTNAME = $00000001;
  WPCN_SPECIALSMENU = $00000002;
  WPCN_HOSTDATA = $00000003;

    {
    * WPCN_FATAL and WPCN_CONNFATAL notification message, Hints in LPARAM
    *
    * WPCN_FALSEFATAL
    *    Fake Fatal Error caused by ServEnt thread, Only needs attention.
    *    Still we can continue with the connection.
    * WPCN_SRVNTFATAL
    *    Fatal Error caused by ServEnt thread.
    * WPCN_SRVNTFATALSTP
    *    Severe Fatal Error caused by ServEnt thread.
    *    ServEnt thread will stop/exit connection.
    * WPCN_CONNFATAL
    *    Connection Fatal Error caused by ServEnt thread.
    * WPCN_CONNFATALSTP
    *    Severe Connection Fatal Error caused by ServEnt thread.
    *    ServEnt thread will stop/exit connection.
    * WPCN_CALERFATAL
    *    Fatal Error caused by Caller thread.
    * WPCN_CALERFATALSTP
    *    Severe Fatal Error caused by Or occurs to Caller thread.
    *    Caller thread must stop/exit connection.
    }
  WPCN_FALSEFATAL = WPCN_FALSE;
  WPCN_SRVNTFATAL = $00000001;
  WPCN_SRVNTFATALSTP = $00000002;
  WPCN_CONNFATAL = $00000003;
  WPCN_CONNFATALSTP = $00000004;
  WPCN_CALERFATAL = $00000005;
  WPCN_CALERFATALSTP = $00000006;

    { WPCN_BUSY notification message LPARAM
    * Hint from backend to frontend about time-consuming operations.
    * Initial state is assumed to be BUSY_NOT.
    * WPCN_BUSY_NOT
    *    Not busy, all user interaction OK
    * WPCN_BUSY_WAITING
    *    Waiting for something; local event loops still running so some
    *    local interaction (e.g. menus) OK, but network stuff is suspended
    * WPCN_BUSY_CPU
    *    Locally busy (e.g. crypto); user interaction suspended
    }
  WPCN_BUSY_NOT = $00000000;
  WPCN_BUSY_WAITING = $00000001;
  WPCN_BUSY_CPU = $00000002;

    {
    * W-PuTTY-CD Return Codes:
    * Zero means no error, all error code are negative,
    * return code greater than zero hold relevant data,
    * Querying function all IsFunctions() if returns zero this means Is _Not_
    * Function with BOOL return type,  return (BOOL)0 == (FALSE) if _cannot_
    *    Except one function WPCD_SetMode() which returns
    *    old mode value or the default value of passed mode
    * Function with HWND return type,  return (HWND)0 == (NULL) if _cannot_
    * Function with DWORD return type, return (DWORD)0 if _cannot_
    * Function with LRESULT return type, return (LRESULT)0 if _cannot_
    * Function with any pointer return type, return NULL if _cannot_
    *
    * Last Error:
    *  1.All IsFunctions() do not affect Last Error
    *  2.Functions which do not require connection handle
    *    frontend_less_functions() and the WPCD_DupInstance() function
    *    set the last_error and you can call WPCD_GetLastError() and
    *    WPCD_SetLastError() to get and set this static global variable.
    *    These functions always reset last_error with each call.
    *  3.Functions which do require connection handle frontend_functions()
    *    set caller_last_error and you can call WPCD_GetCallerLastError(frontend)
    *    WPCD_SetCallerLastError(frontend) to get and set this connection variable.
    *    These functions always reset caller_last_error with each call.
    *  4.Functions called by servat thread which runs the PuTTYs code
    *    set servent_last_error and you can call WPCD_GetServentLastError(frontend)
    *    WPCD_SetServentLastError(frontend) to get and set this connection variable
    *    These functions do not reset servent_last_error.
    *
    }
  WPCD_NOERROR = 0;     //* no error.		*/
  //* Successful WPCD_Init() not yet performed  */
  WPCD_NOTINITIALISED = -1;    //* not initialized.        */
  WPCD_INVALID_HANDLE = -2;    //* invalid handle.		*/
  WPCD_INVALID_PARAMETER = -3;    //* invalid parameter.	*/
  WPCD_IN_PROGRESS = -4;    //*operation now in progress*/
  WPCD_EINPROGRESS = WPCD_IN_PROGRESS;
  WPCD_ALREADY_CONNECTED = -5;    //* already connected.	*/
  WPCD_EISCONN = WPCD_ALREADY_CONNECTED;
  WPCD_NOT_CONNECTED = -6;    //* not connected.		*/
  WPCD_ENOTCONN = WPCD_NOT_CONNECTED;
  WPCD_CONNECTION_CLOSED = -7;    //* connection closed.	*/
  WPCD_ESHUTDOWN = WPCD_CONNECTION_CLOSED;
  WPCD_REMOTE_EXIT = -8;    //* remote host close conn.	*/
  WPCD_ECONNRESET = WPCD_REMOTE_EXIT;  //* peer reset. */
  WPCD_CONNECT_ERROR = -9;    //* error while connecting.	*/
  WPCD_NOCONN_EXCEEDED = -10;   //* No connection exceeded. */
  WPCD_NETWORK_DOWN = -11;   //* network is down.        */
  WPCD_ENETDOWN = WPCD_NETWORK_DOWN;
  WPCD_BUSY = -12;   //* busy, try later.	*/
  WPCD_WOULD_BLOCK = -13;   //* function would block.   */
  WPCD_EWOULDBLOCK = WPCD_WOULD_BLOCK;
  WPCD_TIMEOUT = -14;   //* busy, try later.	*/
  WPCD_ETIMEDOUT = WPCD_TIMEOUT;
  WPCD_USER_CANCELED = -15;   //* user cancelled.		*/
  WPCD_INTERNAL_ERROR = -16;   //* internal error.		*/
  WPCD_MEM_ERROR = -17;   //* memory error.		*/
  WPCD_CRC_ERROR = -18;   //* crc error.		*/
  WPCD_FATAL_ERROR = -19;   //* fatal error.		*/
  WPCD_SYSTEM_ERROR = -20;   //* system error.		*/
  WPCD_UNKNOWN_ERROR = -21;   //* unkown reason for error.*/

  //* Session connection state */
  WPCD_SESSION_CLOSED = 0;     //* session in close state. */
  WPCD_SESSION_PRE_CONNECT = 1;     //* preparing for connection*/
  WPCD_SESSION_WAITING_CONNECT = 2;     //* waiting for connect.	*/
  WPCD_SESSION_TRYING_CONNECT = 3;     //* trying to connect.	*/
  WPCD_SESSION_CONNECTED = 4;     //* connected.		*/
  WPCD_SESSION_WAITING_CLOSE = 5;     //* waiting for close.      */
  WPCD_SESSION_CONN_CLOSED = WPCD_SESSION_CLOSED;

    {
    * < _Simon_ _wrote_ _this _ >
    * Some global flags denoting the type of application.
    *
    * FLAG_VERBOSE is set when the user requests verbose details.
    *
    * FLAG_STDERR is set in command-line applications (which have a
    * functioning stderr that it makes sense to write to) and not in
    * GUI applications (which don't).
    *
    * FLAG_INTERACTIVE is set when a full interactive shell session is
    * being run, _either_ because no remote command has been provided
    * _or_ because the application is GUI and can't run non-
    * interactively.
    *
    * These flags describe the type of _application_ - they wouldn't
    * vary between individual sessions - and so it's OK to have this
    * variable be GLOBAL.
    *
    * Note that additional flags may be defined in platform-specific
    * headers. It's probably best if those ones start from $1000, to
    * avoid collision.
    }

  FLAG_VERBOSE = $0001;
  FLAG_STDERR = $0002;
  FLAG_INTERACTIVE = $0004;

  //* Events and Synchronizations. */
   {
   * Events, Masks, Messages and IDs; used by Get/Set_Event() functions
   * LRESULT WPCD_API WPCD_GetDefaultEvent(int idEvent);
   * LRESULT WPCD_API WPCD_SetDefaultEvent(int idEvent, LPARAM lValue);
   * LRESULT WPCD_API WPCD_GetEvent(WPCD_HINST hFrontEnd, int idEvent);
   * LRESULT WPCD_API WPCD_SetEvent(WPCD_HINST hFrontEnd, int idEvent, LPARAM lValue);
   }
  EVENT_NOTIFY_MASK = 1;           //* notify events enab/disab mask.*/
  EVENT_NOTIFY_FUNC = 2;           //* notification callback function*/
  EVENT_NOTIFY_MSG = 3;           //* notification message to parent*/
  EVENT_ASYNC_NET = 4;           //* net event Msg to/from parent. */
  EVENT_AGNTCALLBK = 5;           //* agent callback, tnow not used.*/
  EVENT_TIMER_ID = 6;           //* Timing Timer ID.              */
    {
    * Synchronizations, Sleep times & Control. used by Get/Set_Sync() functions
    * DWORD   WPCD_API WPCD_GetSync(WPCD_HINST hFrontEnd, int idSync);
    * DWORD   WPCD_API WPCD_SetSync(WPCD_HINST hFrontEnd, int idSync, DWORD dwValue);
    }
  SYNC_CALLER_SLEEP = 1;           //* caller thread sleep time in ms*/
  SYNC_SERVET_SLEEP = 2;           //* Servent thread sleep time ms  */
  SYNC_SERVET_WAIT = 3;           //* Servent thread wait time in ms*/
  SYNC_RETRY_COUNT = 4;           //* caller thread retry count     */

   {
   * Modes used by Set/Get_Mode() functions
   * int  WPCD_GetMode(WPCD_HINST hFrontEnd, int mode);
   * int  WPCD_SetMode(WPCD_HINST hFrontEnd, int mode, int value);
   }
  MODE_AUTO_CONNECT = 1;           //* Auto Connect mode.            */
  MODE_NOTIFY = 2;           //* Notification mode.            */
  MODE_BS_DEL = 3;           //* Backspace=DEL in negotiations */
  MODE_TN_KBD = 5;           //* Telnet keyboard.              */
  MODE_TN_NL = 6;           //* Telnet new line               */

  MODE_OFF = FALSE;       //* Mode disabled.                */
  MODE_ON = not MODE_OFF;   //* Mode enabled.                 */


type
  //* Current Connection Protocol, Trying to keep on PuTTY style           */
  ConnectionProtocol = (
                         //* Protocol undefined */
                         WPCD_PROT_UNDEFINED = 0,            //* undefined protocol.           */
                         //* Protocol back ends*/
                         WPCD_PROT_RAW,                      //* Raw protocol.                 */
                         WPCD_PROT_TELNET,                   //* TelNet protocol.              */
                         WPCD_PROT_RLOGIN,                   //* Remote Login protocol.        */
                         WPCD_PROT_SSH,                      //* SSH  protocol.                */
                         //* PROT_SERIAL is supported on a subset of platforms */
                         WPCD_PROT_SERIAL                    //* Serial Connection.            */
                        );

  //*===================== List of Data Types =============================*/
  CONNENUMPROC = function(hFrontEnd : WPCD_HINST; lpSid : Pointer; _lParam : LPARAM) : BOOL; stdcall;
  NOTIFYPROC = procedure(hFrontEnd : WPCD_HINST; lpSid : Pointer; _wParam : WPARAM; _lParam : LPARAM); stdcall;

const
  SESCFG_SIG_MAX = 256;
  SESCFG_KEX_MAX = 4;
  SESCFG_CIPHER_MAX = 6;
  SESCFG_RESERVED_MAX = 512;

type
  _tag_WPCD_SesCfg = record
    //* Signature and version */
    wpcd_signature : array[0..SESCFG_SIG_MAX - 1] of Ansichar;
    wpcd_ver_major : Integer;
    wpcd_ver_minor : Integer;

    //* Basic options */
    host : array[0..511] of Ansichar;
    port : integer;
    protocol : integer;
    addressfamily : integer;
    ping_interval : integer;		       //* in seconds */
    tcp_nodelay : integer;
    tcp_keepalives : integer;

    //* Proxy options */
    proxy_exclude_list : array[0..511] of Ansichar;
    proxy_dns : Integer;
    even_proxy_localhost : Integer;
    proxy_type : Integer;
    proxy_host : array[0..511] of Ansichar;
    proxy_port : Integer;
    proxy_username : array[0..127] of Ansichar;
    proxy_password : array[0..127] of Ansichar;
    proxy_telnet_command : array[0..511] of Ansichar;

    //* SSH options */
    remote_cmd : array[0..511] of Ansichar;

    nopty : Integer;
    compression : Integer;
    ssh_kexlist : array[0..SESCFG_KEX_MAX - 1] of Integer;
    ssh_rekey_time : Integer;	       //* in minutes */
    ssh_rekey_data : array[0..15] of Ansichar;
    tryagent : Integer;
    agentfwd : Integer;
    change_username : Integer;	       //* allow username switching in SSH-2   */
    ssh_cipherlist : array[0..SESCFG_CIPHER_MAX - 1] of integer;
    keyfile : array[0..254] of Ansichar;
    sshprot : Integer;		       //* use v1 or v2 when both available    */
    ssh2_des_cbc : Integer;		       //* "des-cbc" unrecommended SSH-2 cipher*/
    ssh_no_userauth : Integer;	       //* bypass "ssh-userauth" (SSH-2 only)  */
    try_tis_auth : Integer;
    try_ki_auth : Integer;
    ssh_subsys : Integer;		       //* run a subsystem rather than a command*/
    ssh_subsys2 : Integer;		       //* fallback to go with remote_cmd_ptr2 */
    ssh_no_shell : Integer;		       //* avoid running a shell               */
    ssh_nc_host : array[0..511] of Ansichar;	       //* host to connect to in `nc' mode     */
    ssh_nc_port : Integer;		       //* port to connect to in `nc' mode     */

    //* Telnet options */
    termtype : array[0..31] of Ansichar;
    termspeed : array[0..31] of Ansichar;
    ttymodes : array[0..767] of Ansichar;		       //* MODE\tVvalue\0MODE\tA\0\0           */
    environmt : array[0..1023] of Ansichar;	       //* VAR\tvalue\0VAR\tvalue\0\0          */
    username : array[0..99] of Ansichar;
    localusername : array[0..99] of Ansichar;
    rfc_environ : Integer;
    passive_telnet : Integer;

    //* Serial port options */
    serline : array[0..255] of Ansichar;
    serspeed : Integer;
    serdatabits : Integer;
    serstopbits : Integer;
    serparity : Integer;
    serflow : Integer;

    //* Logging */
    logfilename : array[0..254] of Ansichar;
    logtype : Integer;
    logxfovr : Integer;
    logflush : Integer;
    logomitpass : Integer;
    logomitdata : Integer;

    //* X11 forwarding */
    x11_forward : Integer;
    x11_display : array[0..127] of Ansichar;
    x11_auth : Integer;

    //* port forwarding */
    lport_acceptall : Integer; //*accept conns from hosts other than localhost*/
    rport_acceptall : Integer; //*same for remote forwarded ports (SSH-2 only)*/
    {* < _Simon_ _wrote_ _this_ >
    * The port forwarding string contains a number of
    * NUL-terminated substrings, terminated in turn by an empty
    * string (i.e. a second NUL immediately after the previous
    * one). Each string can be of one of the following forms:
    *
    *   [LR]localport\thost:port
    *   [LR]localaddr:localport\thost:port
    *   Dlocalport
    *   Dlocaladdr:localport
    }
    portfwd : array[0..1023] of AnsiChar;

    //* SSH bug compatibility modes */
    sshbug_ignore1 : Integer;
    sshbug_plainpw1 : Integer;
    sshbug_rsa1 : Integer;
    sshbug_hmac2 : Integer;
    sshbug_derivekey2 : Integer;
    sshbug_rsapad2 : Integer;
    sshbug_pksessid2 : Integer;
    sshbug_rekey2 : Integer;

    { Settings that are not configured By PuTTY dialog, but I think
    * we can save them in the session configuration.
    * you can use WPuttyCD_SetMode() to set them.
    }
    //* keyboard options */
    bksp_is_delete : Integer;            //* negotialte Backspace = Del(^H)   */
    {
    * switch #ifdef(__WPUTTYCD_KBDSNDTNCMD__), enables PuTTY
    * dialog to configure telnet_keyboard and telnet_newline
    * and enables handling for both options in RawTerm.term_send() which
    * then calls ldisc_send() instead of ((Ldisc)term->ldisc)->back->send()
    * also enables WPuttyCD_SetMode() to set them.
    }
    //* Telnet keyboard options */
    telnet_keyboard : Integer;
    telnet_newline : Integer;

    //* Reserved Section, This is reserved for new settings in future versions */
    wpcd_reserved : array[0..SESCFG_RESERVED_MAX - 1] of Integer;
  end;
  WPCD_SesCfg = _tag_WPCD_SesCfg;

  PWPCD_SesCfg = ^WPCD_SesCfg;


  //*============== Functions do not require connection handle ============*/
  function WPCD_GetVersion : DWORD; stdcall; external DLLName;
  function WPCD_GetInitCount : Integer; stdcall; external DLLName;
  function WPCD_Init : Integer; stdcall; external DLLName;
  function WPCD_Shutdown : Integer; stdcall; external DLLName;
  function WPCD_GetDefaultWindow : HWND; stdcall; external DLLName;
  function WPCD_SetDefaultWindow(hNewWnd : HWND) : HWND; stdcall; external DLLName;
  function WPCD_GetDefaultFlags : Integer; stdcall; external DLLName;
  function WPCD_SetDefaultFlags : Integer; stdcall; external DLLName;
  function WPCD_GetDefaultNotifyMode(iNewFlags : Integer) : Integer; stdcall; external DLLName;
  function WPCD_SetDefaultNotifyMode(iNewMode : Integer) : Integer; stdcall; external DLLName;
  function WPCD_GetDefaultNotifyMask : DWORD; stdcall; external DLLName;
  function WPCD_SetDefaultNotifyMask(dwNewMask : DWORD) : DWORD; stdcall; external DLLName;
  function WPCD_GetDefaultEvent(idEvent : Integer) : LRESULT; stdcall; external DLLName;
  function WPCD_SetDefaultEvent(idEvent : Integer; lValue : LPARAM) : LRESULT; stdcall; external DLLName;
  function WPCD_QueryLangID : Integer; stdcall; external DLLName;
  function WPCD_SetLangID(iLangID : Integer) : Integer; stdcall; external DLLName;
  procedure WPCD_ShowEventLog(hWndParent : HWND); stdcall; external DLLName;
  procedure WPCD_ShowAboutDlg(hWndParent : HWND); stdcall; external DLLName;
  function WPCD_SizeofSettings : Integer; stdcall; external DLLName;
  function WPCD_QueryDefaults(pDefSesCfg : PWPCD_SesCfg) : Integer; stdcall; external DLLName;
  function WPCD_SetDefaults(pDefSesCfg : PWPCD_SesCfg) : Integer; stdcall; external DLLName;
  function WPCD_DefaultsSetup(hWndParent : HWND; iNotify : Integer) : Integer; stdcall; external DLLName;
  function WPCD_EnumConnections(lpConcEnumFunc : CONNENUMPROC; _lParam : LPARAM) : Integer; stdcall; external DLLName;
  function WPCD_EnumConnected(lpConcEnumFunc : CONNENUMPROC; _lParam : LPARAM) : Integer; stdcall; external DLLName;
  function WPCD_CreateInstance(lpSid : Pointer; pSesCfg : PWPCD_SesCfg) : WP_INST; stdcall; external DLLName;
  function WPCD_GetLastError : Integer; stdcall; external DLLName;
  function WPCD_SetLastError(iErrCode : Integer) : Integer; stdcall; external DLLName;

  //*================== Functions require connection handle ===============*/
  function WPCD_DupInstance(hFrontEnd : WPCD_HINST; lpSid : Pointer) : WP_INST; stdcall; external DLLName;
  function WPCD_DestroyInstance(hFrontEnd : WPCD_HINST) : Integer; stdcall; external DLLName;
  function WPCD_GetCallerLastError(hFrontEnd : WPCD_HINST) : Integer; stdcall; external DLLName;
  function WPCD_SetCallerLastError(hFrontEnd : WPCD_HINST; iErrCode : Integer) : Integer; stdcall; external DLLName;
  function WPCD_GetServentLastError(hFrontEnd : WPCD_HINST) : Integer; stdcall; external DLLName;
  function WPCD_SetServentLastError(hFrontEnd : WPCD_HINST; iErrCode : Integer) : Integer; stdcall; external DLLName;
  function WPCD_QueryInstance(hFrontEnd : WPCD_HINST; pSesCfg : PWPCD_SesCfg) : Integer; stdcall; external DLLName;
  function WPCD_SetInstance(hFrontEnd : WPCD_HINST; pSesCfg : PWPCD_SesCfg) : Integer; stdcall; external DLLName;
  function WPCD_IsCfgDefaults(hFrontEnd : WPCD_HINST) : BOOL; stdcall; external DLLName;
  function WPCD_SetTermType(hFrontEnd : WPCD_HINST; lpTermType : LPCSTR) : Integer; stdcall; external DLLName;
  function WPCD_GetEvent(hFrontEnd : WPCD_HINST; idEvent : Integer) : LRESULT; stdcall; external DLLName;
  function WPCD_SetEvent(hFrontEnd : WPCD_HINST; idEvent : Integer; lValue : LPARAM) : LRESULT; stdcall; external DLLName;
  function WPCD_GetSync(hFrontEnd : WPCD_HINST; idSync : Integer) : DWORD; stdcall; external DLLName;
  function WPCD_SetSync(hFrontEnd : WPCD_HINST; idSync : Integer; dwValue : DWORD) : DWORD; stdcall; external DLLName;
  function WPCD_GetMode(hFrontEnd : WPCD_HINST; idMode : Integer) : BOOL; stdcall; external DLLName;
  function WPCD_SetMode(hFrontEnd : WPCD_HINST; idMode : Integer; dwValue : Integer) : DWORD; stdcall; external DLLName;
  function WPCD_SetNotifyMode(hFrontEnd : WPCD_HINST; iNewMode : Integer) : BOOL; stdcall; external DLLName;
  function WPCD_GetNotifyMask(hFrontEnd : WPCD_HINST) : DWORD; stdcall; external DLLName;
  function WPCD_SetNotifyMask(hFrontEnd : WPCD_HINST; dwNewMask : DWORD) : DWORD; stdcall; external DLLName;
  function WPCD_IsEventNotify(hFrontEnd : WPCD_HINST; idEvent : Integer) : BOOL; stdcall; external DLLName;
  function WPCD_SetEventNotify(hFrontEnd : WPCD_HINST; idEvent : Integer; bEnabled : BOOL) : BOOL; stdcall; external DLLName;
  function WPCD_SetAutoConnect(hFrontEnd : WPCD_HINST; bEnabled : BOOL) : BOOL; stdcall; external DLLName;
  function WPCD_NegotiateBackSpaceAsDel(hFrontEnd : WPCD_HINST; bEnabled : BOOL) : BOOL; stdcall; external DLLName;
  function WPCD_Setup(hFrontEnd : WPCD_HINST; hWndParent : HWND) : Integer; stdcall; external DLLName;
  function WPCD_Connect(hFrontEnd : WPCD_HINST; hWndSes : HWND) : Integer; stdcall; external DLLName;
  function WPCD_IsConfigChanged(hFrontEnd : WPCD_HINST) : BOOL; stdcall; external DLLName;
  function WPCD_IsConnected(hFrontEnd : WPCD_HINST) : BOOL; stdcall; external DLLName;
  function WPCD_IsBusy(hFrontEnd : WPCD_HINST) : BOOL; stdcall; external DLLName;
  function WPCD_GetSID(hFrontEnd : WPCD_HINST) : Pointer; stdcall; external DLLName;
  function WPCD_GetHWND(hFrontEnd : WPCD_HINST) : HWND; stdcall; external DLLName;
  function WPCD_GetStatus(hFrontEnd : WPCD_HINST) : Integer; stdcall; external DLLName;
  function WPCD_GetProtocol(hFrontEnd : WPCD_HINST) : Integer; stdcall; external DLLName;
  function WPCD_GetProtoclName(hFrontEnd : WPCD_HINST) : LPCSTR; stdcall; external DLLName;
  function WPCD_GetRealHostName(hFrontEnd : WPCD_HINST) : LPCSTR; stdcall; external DLLName;
  function WPCD_DisConnect(hFrontEnd : WPCD_HINST) : Integer; stdcall; external DLLName;
  function WPCD_ComRead(hFrontEnd : WPCD_HINST; lpBuffer : Pointer; iBufLen : Integer) : Integer; stdcall; external DLLName;
  function WPCD_ComWrite(hFrontEnd : WPCD_HINST; lpBuffer : Pointer; iBufLen : Integer) : Integer; stdcall; external DLLName;
  function WPCD_SendStr(hFrontEnd : WPCD_HINST; lpStr : LPCSTR) : Integer; stdcall; external DLLName;
  function WPCD_SendSpecialCmd(hFrontEnd : WPCD_HINST; pPopupPlace : PPoint) : Integer; stdcall; external DLLName;
  function WPCD_SendBreak(hFrontEnd : WPCD_HINST) : Integer; stdcall; external DLLName;
  function WPCD_TimerActivation(hFrontEnd : WPCD_HINST) : Integer; stdcall; external DLLName;
  function WPCD_EventHandler(hFrontEnd : WPCD_HINST; _hwnd : HWND; Msg : UINT; _wParam : WPARAM; _lParam : LPARAM) : Integer; stdcall; external DLLName;



implementation

function WPCD_ISVALIDEVENT(id_event : DWORD) : Boolean;
begin
  Result := (id_event >= WPCN_NMWPARAM_MIN) and
            (id_event <= WPCN_NMWPARAM_MAX);
end;

function _WPCD_ISEVENTNOTIFY(nm_mask, id_event : DWORD) : Boolean;
begin
  Result := (nm_mask and (1 shl (id_event - 1))) = 1;
end;

procedure _WPCD_SETEVENTNOTIFY(var nm_mask : DWORD; id_event : DWORD);
begin
  nm_mask := nm_mask or (1 shl (id_event - 1));
end;

procedure WPCD_RESETEVENTNOTIFY(var nm_mask : DWORD; id_event : DWORD);
begin
  nm_mask := nm_mask and (not (1 shl (id_event - 1)));
end;

end.
