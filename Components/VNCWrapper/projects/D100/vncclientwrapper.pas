unit vncclientwrapper;

{$R 'vncclientwrapper.res'}

interface

uses
  Classes, Windows, StrUtils, SysUtils;

type
  TZLibLevel = 1..9;
  TMenuKey = (mkF1=1, mkfF2=2, mkfF3=3, mkfF4=4, mkfF5=5, mkfF6=6, mkfF7=7,
              mkfF8=8, mkfF9=9, mkfF10=10, mkfF11=11, mkfF12=12);
  TEncoding = (eZRLE, eHextile, eRaw);
  TColorLevel = (clVeryLow8, clLow64, clMedium256);

  TVNCClientWrapper = class(TComponent)
  private
    FHost : String;
    FPort : Word;
    FBinaryPath : String;
    FZlibLevel : TZLibLevel; //Zlib compression level (default=1)
    FListen : Boolean; //Accept incoming connections from VNC servers. (default=0)
    FAutoReconnect : Boolean; //Offer to reconnect to the remote server if the connectionis dropped because an error occurs. (default=1)
    FMenuKey : TMenuKey; //The key which brings up the popup menu (default=F8)
    FMonitor : String; //The monitor to open the VNC Viewer window on, if available.(default=)
    FAcceptBell : Boolean; //Produce a system beep when requested to by the server. (default=1)
    FEmulate3 : Boolean; //Emulate middle mouse button when left and right buttons are used simulatenously. (default=0)
    FPointerEventDelay : Word; //The interval to delay between sending one pointer event and the next. (default=0)
    FProtocol33 : Boolean; //Only use protocol version 3.3 (default=0)
    FDisableWinKeys : Boolean; //Pass special Windows keys directly to the server.(default=1)
    FServerCutText : Boolean; //Accept clipboard changes from the server. (default=1)
    FClientCutText : Boolean; //Send clipboard changes to the server. (default=1)
    FSendKeyEvents : Boolean; //Send key presses (and releases) to the server. (default=1)
    FSendPointerEvents : Boolean; //- Send pointer (mouse) events to the server. (default=1)
    FShared : Boolean; //Allow existing connections to the server to continue.(Default is to disconnect all other clients) (default=0)
    FAutoSelect: Boolean; // Auto select pixel format and encoding (default=1)
    FPreferredEncoding : TEncoding; //Preferred graphical encoding to use - overridden by AutoSelect if set. (ZRLE, Hextile or Raw) (default=ZRLE)
    FFullScreen : Boolean; //Use the whole display to show the remote desktop.(Press MenuKey to access the viewer menu) (default=0)
    FLowColourLevel : TColorLevel; //Colour level to use on slow connections. 0 = Very Low (8 colours), 1 = Low (64 colours), 2 = Medium (256 colours) (default=1)
    FFullColour : Boolean; //Use full colour (default is to use low colour unless auto select decides the link is fast enough). (default=0)
    FUseDesktopResize : Boolean; //Support dynamic desktop resizing (default=1)
    FUseLocalCursor : Boolean; //Render the mouse cursor locally (default=1)
    FDebugDelay : Word; //Milliseconds to display inverted pixel data - a debugging feature (default=0)

    FProcInfo : TProcessInformation;

    procedure Set_BinaryPath(AValue : String);

    function StartProcess : Boolean;
    function StopProcess : Boolean;
    function ExtractFiles : Boolean;
    function DeleteFiles : Boolean;
    function GetParamString : String;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy(); override;

    function IsActive : Boolean;
    function Start : Boolean;
    function Connect : Boolean; overload;
    function Connect(AHost : String; APort: Word=5900) : Boolean; overload;
    function Disconnect : Boolean;
  published
    property Host                 : String      read FHost                  write FHost;
    property Port                 : Word        read FPort                  write FPort                 default 5900;
    property BinaryPath           : String      read FBinaryPath            write Set_BinaryPath;
    property ZlibLevel            : TZLibLevel  read FZlibLevel             write FZlibLevel            default 1;
    property Listen               : Boolean     read FListen                write FListen               default false;
    property AutoReconnect        : Boolean     read FAutoReconnect         write FAutoReconnect        default true;
    property MenuKey              : TMenuKey    read FMenuKey               write FMenuKey              default mkfF8;
    property Monitor              : String      read FMonitor               write FMonitor;
    property AcceptBell           : Boolean     read FAcceptBell            write FAcceptBell           default true;
    property Emulate3             : Boolean     read FEmulate3              write FEmulate3             default false;
    property PointerEventDelay    : Word        read FPointerEventDelay     write FPointerEventDelay    default 0;
    property Protocol33           : Boolean     read FProtocol33            write FProtocol33           default false;
    property DisableWinKeys       : Boolean     read FDisableWinKeys        write FDisableWinKeys       default true;
    property ServerCutText        : Boolean     read FServerCutText         write FServerCutText        default true;
    property ClientCutText        : Boolean     read FClientCutText         write FClientCutText        default true;
    property SendKeyEvents        : Boolean     read FSendKeyEvents         write FSendKeyEvents        default true;
    property SendPointerEvents    : Boolean     read FSendPointerEvents     write FSendPointerEvents    default true;
    property Shared               : Boolean     read FShared                write FShared               default false;
    property AutoSelect           : Boolean     read FAutoSelect            write FAutoSelect           default true;
    property PreferredEncoding    : TEncoding   read FPreferredEncoding     write FPreferredEncoding    default eZRLE;
    property FullScreen           : Boolean     read FFullScreen            write FFullScreen           default false;
    property LowColourLevel       : TColorLevel read FLowColourLevel        write FLowColourLevel       default clLow64;
    property FullColour           : Boolean     read FFullColour            write FFullColour           default false;
    property UseDesktopResize     : Boolean     read FUseDesktopResize      write FUseDesktopResize     default true;
    property UseLocalCursor       : Boolean     read FUseLocalCursor        write FUseLocalCursor       default true;
    property DebugDelay           : Word        read FDebugDelay            write FDebugDelay           default 0;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TUO', [TVNCClientWrapper]);
end;

constructor TVNCClientWrapper.Create(AOwner: TComponent);
begin
  inherited;
  FBinaryPath:=Trim(GetEnvironmentVariable('TEMP'));
  if AnsiSameText(FBinaryPath,'') then
    GetDir(0,FBinaryPath);
  FBinaryPath:=IncludeTrailingPathDelimiter(FBinaryPath);

  FHost:='';
  FPort:=5900;

  FZlibLevel:=1;
  FListen:=false;
  FAutoReconnect:=true;
  FMenuKey:=mkfF8;
  FMonitor:='';
  FAcceptBell:=true;
  FEmulate3:=false;
  FPointerEventDelay:=0;
  FProtocol33:=false;
  FDisableWinKeys:=true;
  FServerCutText:=true;
  FClientCutText:=true;
  FSendKeyEvents:=true;
  FSendPointerEvents:=true;
  FShared:=false;
  FAutoSelect:=true;
  FPreferredEncoding:=eZRLE;
  FFullScreen:=false;
  FLowColourLevel:=clLow64;
  FFullColour:=false;
  FUseDesktopResize:=true;
  FUseLocalCursor:=true;
  FDebugDelay:=0;
end;

destructor TVNCClientWrapper.Destroy;
begin
  if not (csDesigning in Self.ComponentState) then
  begin
    Disconnect;
    DeleteFiles;
  end;
  inherited;
end;

procedure TVNCClientWrapper.Set_BinaryPath(AValue: String);
begin
  if DirectoryExists(AValue) then
    FBinaryPath:=IncludeTrailingPathDelimiter(AValue);
end;

function TVNCClientWrapper.IsActive: Boolean;
var
  Exitcode : Cardinal;
begin
  Result:=false;
  if GetExitCodeProcess(FProcInfo.hProcess,Exitcode) then
    Result:=Exitcode=STILL_ACTIVE;
end;

function TVNCClientWrapper.ExtractFiles: Boolean;
var
  Rs : TResourceStream;
begin
  Result:=false;
  if not FileExists(FBinaryPath+'vncviewer.exe') then
  begin
    Rs:=TResourceStream.Create(HInstance,'VNCVIEWER_EXE',RT_RCDATA);
    rs.SaveToFile(FBinaryPath+'vncviewer.exe');
    rs.Free;
  end;
  Result:=true;
end;

function TVNCClientWrapper.DeleteFiles: Boolean;
begin
  if FileExists(FBinaryPath+'vncviewer.exe') then
    Result:=DeleteFile(PAnsiChar(FBinaryPath+'vncviewer.exe'));
end;

function TVNCClientWrapper.GetParamString: String;
  procedure Add(AName : String; AValue : String);
  begin
    Result:=Result+' '+AName+'='+AValue;
  end;
begin
  if not AnsiSameText(Trim(FHost),'') then
  begin
    Result:=' '+FHost;
    if FPort<>0 then
      Result:=Result+':'+IntToStr(FPort);
  end;
  Add('ZlibLevel',IntToStr(Integer(FZlibLevel)));
  Add('Listen',IfThen(FListen,'1','0'));
  Add('AutoReconnect',IfThen(FAutoReconnect,'1','0'));
  Add('MenuKey','F'+IntToSTr(Integer(FMenuKey)));
  if not AnsiSameText(Trim(Monitor),'') then
    Add('Monitor',FMonitor);
  Add('AcceptBell',IfThen(FAcceptBell,'1','0'));
  Add('Emulate3',IfThen(FEmulate3,'1','0'));
  if FPointerEventDelay<>0 then
    Add('PointerEventDelay',IntToStr(FPointerEventDelay));
  Add('Protocol3.3',IfThen(FProtocol33,'1','0'));
  Add('DisableWinKeys',IfThen(FDisableWinKeys,'1','0'));
  Add('ServerCutText',IfThen(FServerCutText,'1','0'));
  Add('ClientCutText',IfThen(FClientCutText,'1','0'));
  Add('SendKeyEvents',IfThen(FSendKeyEvents,'1','0'));
  Add('SendPointerEvents',IfThen(FSendPointerEvents,'1','0'));
  Add('Shared',IfThen(FShared,'1','0'));
  Add('AutoSelect',IfThen(FAutoSelect,'1','0'));
  case FPreferredEncoding of
    eZRLE: Add('PreferredEncoding','ZRLE');
    eHextile: Add('PreferredEncoding','Hextile');
    eRaw: Add('PreferredEncoding','Raw');
  end;
  Add('FullScreen',IfThen(FFullScreen,'1','0'));
  Add('LowColourLevel',IntToStr(Integer(FLowColourLevel)));
  Add('FullColour',IfThen(FFullColour,'1','0'));
  Add('UseDesktopResize',IfThen(FUseDesktopResize,'1','0'));
  Add('UseLocalCursor',IfThen(FUseLocalCursor,'1','0'));
  if FDebugDelay<>0 then
    Add('DebugDelay',IntToStr(FDebugDelay));
end;

function TVNCClientWrapper.StartProcess: Boolean;
var
  SI : TStartupInfo;
begin
  if ExtractFiles then
  begin
    FillChar(SI, SizeOf(SI), #0);
    SI.cb:=SizeOf(SI);
    Result := CreateProcess(NIL, PChar(FBinaryPath+'vncviewer.exe'+GetParamString), NIL, NIL, FALSE,
                           NORMAL_PRIORITY_CLASS, NIL,
                           PChar(FBinaryPath),
                               SI, FProcInfo);
  end;
end;

function TVNCClientWrapper.StopProcess: Boolean;
begin
  Result:=OpenProcess(PROCESS_TERMINATE,false,FProcInfo.dwProcessId)<>0;
  if Result then
    Result:=TerminateProcess(FProcInfo.hProcess,0);
end;

function TVNCClientWrapper.Start: Boolean;
begin
  if IsActive then
    Result:=false
  else
    Result:=StartProcess;
end;

function TVNCClientWrapper.Connect: Boolean;
begin
  Result:=not AnsiSameText(Trim(FHost),'');
  if Result then
    Result:=Start;
end;

function TVNCClientWrapper.Connect(AHost: String; APort: Word): Boolean;
begin
  FHost:=AHost;
  FPort:=APort;
  Result:=Connect;
end;

function TVNCClientWrapper.Disconnect: Boolean;
begin
  Result:=not IsActive;
  if not Result then
    Result:=StopProcess;
end;

end.
