unit vncserverwrapper;

{$R 'vncserverwrapper.res'}

interface

uses
  SysUtils, Classes, Windows, StrUtils;

type
  TZLibLevel = 1..9;
  TReverseSecurityType = (rstNone);
  TSecurityType = (stNone, stVncAuth);
  TDisconnectAction = (daNone, daLock, daLogOff);
  TUpdateMethod = (umPolling=0, umAppHook=1, umDriverHook=2);

  TVNCServerWrapper = class(TComponent)
  private
    FBinaryPath : String;
    FMaxCutText : LongWord; //Maximum permitted length of an incoming clipboard update (default=262144)
    FPollConsoleWindows : Boolean; //Server should poll console windows for updates (default=1)
    FZlibLevel : TZLibLevel; //Zlib compression level (default=1)
    FUseCaptureBlt : Boolean;//Use a slower capture method that ensures that alpha blended windows appear correctly (default=1)
    FDeadKeyAware : Boolean; //Whether to assume the viewer has already interpreted dead key sequences into latin-1 characters (default=1)
    FQueryConnect : Boolean; //Prompt the local user to accept or reject incoming connections. (default=0)
    FSendCutText : Boolean; //Send clipboard changes to clients. (default=1)
    FAcceptCutText : Boolean; //Accept clipboard updates from clients. (default=1)
    FAcceptPointerEvents : Boolean; //Accept pointer press and release events from clients.(default=1)
    FAcceptKeyEvents : Boolean; //Accept key press and release events from clients. (default=1)
    FDisconnectClients : Boolean; //Disconnect existing clients if an incoming connection is non-shared. If combined with NeverShared then new connections will be refused while there is a client active (default=1)
    FNeverShared : Boolean; //Never treat incoming connections as shared, regardless of the client-specified setting (default=0)
    FAlwaysShared : Boolean; //Always treat incoming connections as shared, regardless of the client-specified setting (default=0)
    FProtocol33 : Boolean; //Always use protocol version 3.3 for backwards compatibility with badly-behaved clients (default=0)
    FCompareFB : Boolean; //Perform pixel comparison on framebuffer to reduce unnecessary updates (default=1)
    FClientWaitTimeMillis : Word; //The number of milliseconds to wait for a client which is no longer responding (default=20000)
    FIdleTimeout : Word; //The number of seconds after which an idle VNC connection will be dropped (zero means no timeout) (default=3600)
    FRemapKeys : TStrings; //Comma-separated list of incoming keysyms to remap.  Mappings are expressed as two hex values, prefixed by 0x, and separated by -> (default=)
    FBlacklistTimeout : Word; //The initial timeout applied when a host is first black-listed.  The host cannot re-attempt a connection until the timeout expires. (default=10)
    FBlacklistThreshold : Byte; // The number of unauthenticated connection attempts allowed from any individual host before that host is black-listed (default=5)
    FPassword : String; //Obfuscated binary encoding of the password which clients must supply to access the server (default=)
    FPasswordFile : String; //Password file for VNC authentication (default=)
    FReverseSecurityType : TReverseSecurityType; //Specify encryption scheme to use for reverse connections (None) (default=None)
    FSecurityType : TSecurityType; //Specify which security scheme to use for incoming connections (None, VncAuth) (default=VncAuth)
    FDisableEffects : Boolean; //Disable desktop user interface effects when the server is in use. (default=0)
    FRemovePattern : Boolean; //Remove the desktop background pattern when the server is in use. (default=0)
    FRemoveWallpaper : Boolean; //Remove the desktop wallpaper when the server is in use. (default=0)
    FDisplayDevice : String; //Display device name of the monitor to be remoted, or empty to export the whole desktop. (default=)
    FDisconnectAction : TDisconnectAction; //Action to perform when all clients have disconnected. (None, Lock, Logoff) (default=None)
    FDisableLocalInputs : Boolean; //Disable local keyboard and pointer input while the server is in use (default=0)
    FUpdateMethod : TUpdateMethod; //How to discover desktop updates; 0 - Polling, 1 - Application hooking, 2 - Driver hooking. (default=1)
    FQueryOnlyIfLoggedOn : Boolean; //Only prompt for a local user to accept incoming connections if there is a user logged on (default=0)
    FLocalHost : Boolean; //Only accept connections from via the local loop-back network interface (default=0)
    FHosts : TStrings; //Filter describing which hosts are allowed access to this server (default=+0.0.0.0/0.0.0.0)
    FPortNumber : Word; //TCP/IP port on which the server will accept connections (default=5900)
    FHTTPPortNumber : Word; //TCP/IP port on which the server will serve the Java applet VNC Viewer  (default=5800)
    FDisableClose : Boolean; //Disable the Close entry in the VNC Server tray menu. (default=0)
    FDisableOptions : Boolean; //Disable the Options entry in the VNC Server tray menu. (default=0) - not exported
    FQueryConnectTimeout : Byte; //Number of seconds to show the Accept Connection dialog before rejecting the connection (default=10)}
    
    FProcInfo : TProcessInformation;

    procedure Set_BinaryPath(AValue : String);

    function StartProcess : Boolean;
    function StopProcess : Boolean;
    function ExtractFiles : Boolean;
    function DeleteFiles : Boolean;
    function GetParamString : String;
    function GetPassword : String;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy(); override;

    function IsActive : Boolean;
    function Start : Boolean;
    function Stop : Boolean;
    function Restart : Boolean;
  published
    property BinaryPath             : String                read FBinaryPath            write Set_BinaryPath;
    property MaxCutText             : LongWord              read FMaxCutText            write FMaxCutText           default 262144;
    property PollConsoleWindows     : Boolean               read FPollConsoleWindows    write FPollConsoleWindows   default true;
    property ZlibLevel              : TZLibLevel            read FZlibLevel             write FZlibLevel            default 1;
    property UseCaptureBlt          : Boolean               read FUseCaptureBlt         write FUseCaptureBlt        default true;
    property DeadKeyAware           : Boolean               read FDeadKeyAware          write FDeadKeyAware         default true;
    property QueryConnect           : Boolean               read FQueryConnect          write FQueryConnect         default false;
    property SendCutText            : Boolean               read FSendCutText           write FSendCutText          default true;
    property AcceptCutText          : Boolean               read FAcceptCutText         write FAcceptCutText        default true;
    property AcceptPointerEvents    : Boolean               read FAcceptPointerEvents   write FAcceptPointerEvents  default true;
    property AcceptKeyEvents        : Boolean               read FAcceptKeyEvents       write FAcceptKeyEvents      default true;
    property DisconnectClients      : Boolean               read FDisconnectClients     write FDisconnectClients    default true;
    property NeverShared            : Boolean               read FNeverShared           write FNeverShared          default false;
    property AlwaysShared           : Boolean               read FAlwaysShared          write FAlwaysShared         default false;
    property Protocol33             : Boolean               read FProtocol33            write FProtocol33           default false;
    property CompareFB              : Boolean               read FCompareFB             write FCompareFB            default true;
    property ClientWaitTimeMillis   : Word                  read FClientWaitTimeMillis  write FClientWaitTimeMillis default 20000;
    property IdleTimeout            : Word                  read FIdleTimeout           write FIdleTimeout          default 3600;
    property RemapKeys              : TStrings              read FRemapKeys             write FRemapKeys;
    property BlacklistTimeout       : Word                  read FBlacklistTimeout      write FBlacklistTimeout     default 10;
    property BlacklistThreshold     : Byte                  read FBlacklistThreshold    write FBlacklistThreshold   default 5;
    property Password               : String                read FPassword              write FPassword;
    property PasswordFile           : String                read FPasswordFile          write FPasswordFile;
    property ReverseSecurityType    : TReverseSecurityType  read FReverseSecurityType   write FReverseSecurityType  default rstNone;
    //property SecurityType           : TSecurityType         read FSecurityType          write FSecurityType         default stVncAuth;
    property DisableEffects         : Boolean               read FDisableEffects        write FDisableEffects       default false;
    property RemovePattern          : Boolean               read FRemovePattern         write FRemovePattern        default false;
    property RemoveWallpaper        : Boolean               read FRemoveWallpaper       write FRemoveWallpaper      default false;
    property DisplayDevice          : String                read FDisplayDevice         write FDisplayDevice;
    property DisconnectAction       : TDisconnectAction     read FDisconnectAction      write FDisconnectAction     default daNone;
    property DisableLocalInputs     : Boolean               read FDisableLocalInputs    write FDisableLocalInputs   default false;
    property UpdateMethod           : TUpdateMethod         read FUpdateMethod          write FUpdateMethod         default umAppHook;
    property QueryOnlyIfLoggedOn    : Boolean               read FQueryOnlyIfLoggedOn   write FQueryOnlyIfLoggedOn  default false;
    property LocalHost              : Boolean               read FLocalHost             write FLocalHost            default false;
    property Hosts                  : TStrings              read FHosts                 write FHosts;
    property PortNumber             : Word                  read FPortNumber            write FPortNumber           default 5900;
    property HTTPPortNumber         : Word                  read FHTTPPortNumber        write FHTTPPortNumber       default 5800;
    property DisableClose           : Boolean               read FDisableClose          write FDisableClose         default false;
    property QueryConnectTimeout    : Byte                  read FQueryConnectTimeout   write FQueryConnectTimeout  default 10;
  end;

procedure Register;

implementation

uses d3des;

procedure Register;
begin
  RegisterComponents('TUO', [TVNCServerWrapper]);
end;

{ TVNCServerWrapper }



constructor TVNCServerWrapper.Create(AOwner: TComponent);
begin
  inherited;
  FBinaryPath:=Trim(GetEnvironmentVariable('TEMP'));
  if AnsiSameText(FBinaryPath,'') then
    GetDir(0,FBinaryPath);
  FBinaryPath:=IncludeTrailingPathDelimiter(FBinaryPath);

  FRemapKeys:=TStringList.Create;
  FHosts:=TStringList.Create;

  MaxCutText:= 262144;
  PollConsoleWindows:= true;
  ZlibLevel:= 1;
  UseCaptureBlt:= true;
  DeadKeyAware:= true;
  QueryConnect:= false;
  SendCutText:= true;
  AcceptCutText:= true;
  AcceptPointerEvents:= true;
  AcceptKeyEvents:= true;
  DisconnectClients:= true;
  NeverShared:= false;
  AlwaysShared:= false;
  Protocol33:= false;
  CompareFB:= true;
  ClientWaitTimeMillis:= 20000;
  IdleTimeout:= 3600;
  BlacklistTimeout:= 10;
  BlacklistThreshold:= 5;
  FPassword:='passwort';
  PasswordFile:='';
  ReverseSecurityType:= rstNone;
  FSecurityType:= stVncAuth;
  DisableEffects:= false;
  RemovePattern:= false;
  RemoveWallpaper:= false;
  DisplayDevice:='';
  DisconnectAction:= daNone;
  DisableLocalInputs:= false;
  UpdateMethod:= umAppHook;
  QueryOnlyIfLoggedOn:= false;
  LocalHost:= false;
  Hosts.Text:='+0.0.0.0/0.0.0.0';
  PortNumber:= 5900;
  HTTPPortNumber:= 5800;
  DisableClose:= false;
  FDisableOptions:=true;
  QueryConnectTimeout:= 10;
end;

destructor TVNCServerWrapper.Destroy;
begin
  if not (csDesigning in Self.ComponentState) then
  begin
    Stop;
    DeleteFiles;
  end;

  FRemapKeys.Free;
  FHosts.Free;

  inherited;
end;

procedure TVNCServerWrapper.Set_BinaryPath(AValue: String);
begin
  if DirectoryExists(AValue) then
    FBinaryPath:=IncludeTrailingPathDelimiter(AValue);
end;

function TVNCServerWrapper.IsActive: Boolean;
var
  Exitcode : Cardinal;
begin
  Result:=false;
  if GetExitCodeProcess(FProcInfo.hProcess,Exitcode) then
    Result:=Exitcode=STILL_ACTIVE;
end;

function TVNCServerWrapper.ExtractFiles: Boolean;
var
  Rs : TResourceStream;
begin
  Result:=false;
  if not FileExists(FBinaryPath+'winvnc4.exe') then
  begin
    Rs:=TResourceStream.Create(HInstance,'WINVNC4_EXE',RT_RCDATA);
    rs.SaveToFile(FBinaryPath+'winvnc4.exe');
    rs.Free;
  end;
  if not FileExists(FBinaryPath+'wm_hooks.dll') then
  begin
    Rs:=TResourceStream.Create(HInstance,'WM_HOOKS_DLL',RT_RCDATA);
    rs.SaveToFile(FBinaryPath+'wm_hooks.dll');
    rs.Free;
  end;
  Result:=true;
end;

function TVNCServerWrapper.DeleteFiles: Boolean;
begin
  if FileExists(FBinaryPath+'winvnc4.exe') then
    Result:=DeleteFile(PChar(FBinaryPath+'winvnc4.exe'));
  if Result and FileExists(FBinaryPath+'wm_hooks.dll') then
    Result:=DeleteFile(PChar(FBinaryPath+'wm_hooks.dll'));
end;

function TVNCServerWrapper.GetParamString: String;
  procedure Add(AName : String; AValue : String);
  begin
    Result:=Result+' '+AName+'='+AValue;
  end;
begin
  Result:=' -noconsole';
  Add('MaxCutText',IntToStr(FMaxCutText));
  Add('PollConsoleWindows',IfThen(FPollConsoleWindows,'1','0'));
  Add('ZlibLevel',IntToStr(Integer(FZlibLevel)));
  Add('UseCaptureBlt',IfThen(FUseCaptureBlt,'1','0'));
  Add('DeadKeyAware',IfThen(FDeadKeyAware,'1','0'));
  Add('QueryConnect',IfThen(FQueryConnect,'1','0'));
  Add('SendCutText',IfThen(FSendCutText,'1','0'));
  Add('AcceptCutText',IfThen(FAcceptCutText,'1','0'));
  Add('AcceptPointerEvents',IfThen(FAcceptPointerEvents,'1','0'));
  Add('AcceptKeyEvents',IfThen(FAcceptKeyEvents,'1','0'));
  Add('DisconnectClients',IfThen(FDisconnectClients,'1','0'));
  Add('NeverShared',IfThen(FNeverShared,'1','0'));
  Add('AlwaysShared',IfThen(FAlwaysShared,'1','0'));
  Add('Protocol3.3',IfThen(FProtocol33,'1','0'));
  Add('CompareFB',IfThen(FCompareFB,'1','0'));
  Add('ClientWaitTimeMillis',IntToStr(FClientWaitTimeMillis));
  Add('IdleTimeout',IntToStr(FIdleTimeout));
  if Length(Trim(FRemapKeys.Text))>0 then
    Add('RemapKeys',FRemapKeys.CommaText);
  Add('BlacklistTimeout',IntToStr(FBlacklistTimeout));
  Add('BlacklistThreshold',IntToStr(FBlacklistThreshold));
  Add('Password',GetPassword);
  if Length(Trim(FPasswordFile))>0 then
    Add('PasswordFile',FPasswordFile);
  {case FSecurityType of
    stNone: Add('SecurityType','None');
    stVncAuth: Add('SecurityType','VncAuth');
  end;}
  Add('DisableEffects',IfThen(FDisableEffects,'1','0'));
  Add('RemovePattern',IfThen(FRemovePattern,'1','0'));
  Add('RemoveWallpaper',IfThen(FRemoveWallpaper,'1','0'));
  if Length(Trim(FDisplayDevice))>0 then
    Add('DisplayDevice',FDisplayDevice);
  Add('DisconnectAction',IntToStr(Integer(FDisconnectAction)));
  Add('DisableLocalInputs',IfThen(FDisableLocalInputs,'1','0'));
  Add('UpdateMethod',IntToStr(Integer(FUpdateMethod)));
  Add('QueryOnlyIfLoggedOn',IfThen(FQueryOnlyIfLoggedOn,'1','0'));
  Add('LocalHost',IfThen(FLocalHost,'1','0'));
  if Length(Trim(FHosts.Text))>0 then
    Add('Hosts',FHosts.CommaText);
  Add('PortNumber',IntToStr(FPortNumber));
  Add('HTTPPortNumber',IntToStr(FHTTPPortNumber));
  Add('DisableClose',IfThen(FDisableClose,'1','0'));
  Add('DisableOptions',IfThen(FDisableOptions,'1','0'));
  Add('QueryConnectTimeout',IntToStr(FQueryConnectTimeout));
end;

function TVNCServerWrapper.GetPassword: String;
var
  idx : Integer;
  Hash : String;
begin
  Hash:=DES_Encrypt(FPassword,VNC_DES_Key);
  for idx:=1 to Length(Hash) do
    Result:=Result+IntToHex(Ord(Hash[idx]),2);
end;


function TVNCServerWrapper.StartProcess: Boolean;
var
  SI : TStartupInfo;
begin
  if ExtractFiles then
  begin
    FillChar(SI, SizeOf(SI), #0);
    SI.cb:=SizeOf(SI);
    SI.dwFlags:= STARTF_USESHOWWINDOW;
    SI.wShowWindow:=SW_HIDE;
    Result := CreateProcess(NIL, PChar(FBinaryPath+'winvnc4.exe'+GetParamString), NIL, NIL, FALSE,
                           NORMAL_PRIORITY_CLASS, NIL,
                           PChar(FBinaryPath),
                               SI, FProcInfo);
  end;
end;

function TVNCServerWrapper.StopProcess: Boolean;
begin
  Result:=OpenProcess(PROCESS_TERMINATE,false,FProcInfo.dwProcessId)<>0;
  if Result then
    Result:=TerminateProcess(FProcInfo.hProcess,0);
end;

function TVNCServerWrapper.Start: Boolean;
begin
  if IsActive then
    Result:=false
  else
    Result:=StartProcess;
end;

function TVNCServerWrapper.Stop: Boolean;
begin
  Result:=not IsActive;
  if not Result then
    Result:=StopProcess;
end;

function TVNCServerWrapper.Restart: Boolean;
begin
  if IsActive then
  begin
    Result:=StopProcess;
    if Result then
      Result:=StartProcess;
  end
  else
    Result:=false;
end;

end.
