//**********************************************************
// Developed by TheUnkownOnes.net
//
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit uNetworkTools;

interface

uses
  Classes, Windows;

{$REGION 'WTSDefinitions'}

type
  TWtsInfoClass = ( WTSInitialProgram,
                      WTSApplicationName,
                      WTSWorkingDirectory,
                      WTSOEMId,
                      WTSSessionId,
                      WTSUserName,
                      WTSWinStationName,
                      WTSDomainName,
                      WTSConnectState,
                      WTSClientBuildNumber,
                      WTSClientName,
                      WTSClientDirectory,
                      WTSClientProductId,
                      WTSClientHardwareId,
                      WTSClientAddress,
                      WTSClientDisplay,
                      WTSClientProtocolType);

  TWTSQuerySessionInformationProc= function(hServer: THandle;
                                        SessionId: DWORD;
                                        WTSInfoClass: TWtsInfoClass;
                                        var ppBuffer: Pointer;
                                        var pBytesReturned: DWORD): BOOL; stdcall;

  WTS_CLIENT_ADDRESS = record
    AddressFamily: DWORD;           // AF_INET, AF_IPX, AF_NETBIOS, AF_UNSPEC
    Address: array [0..19] of Byte; // client network address
  end;


const
  WTS_CURRENT_SERVER_HANDLE = THandle(0);
  WTS_CURRENT_SESSION = DWORD(-1);
  AF_INET         = 2;               { internetwork: UDP, TCP, etc. }


{$ENDREGION}

{$REGION 'Network Routines'}
function GetUserIP: String;
{$ENDREGION}

implementation

uses
  JclSysInfo, WinSock, SysUtils;

{$REGION 'TerminalServer Routines'}
function GetUserIP: String;
var
  QuerySession : TWTSQuerySessionInformationProc;
  ReturnPointer: pointer;
  ByteCount: cardinal;

  ClientAddress: WTS_Client_Address;

  x: integer;
  IPAddress: string;
begin
  IPAddress := '';


  @QuerySession:=GetProcAddress(LoadLibrary('wtsapi.dll'),'WTSQuerySessionInformationA');
  if not Assigned(@QuerySession) then
    exit;

  if (GetSystemMetrics($1000)=0) then //Ist keine RemoteSession
    IPAddress:=GetIPAddress('')
  else
  begin 
    QuerySession(
      WTS_CURRENT_SERVER_HANDLE,
      WTS_CURRENT_SESSION,
      WTSClientAddress,
      ReturnPointer,
      ByteCount);

    ClientAddress := WTS_CLIENT_ADDRESS(ReturnPointer^);
          
    if ClientAddress.AddressFamily = AF_INET then begin
      for x:= 2 to 5 do begin
        IPAddress := IPAddress + IntToStr(ClientAddress.Address[x]);
        if x < 5 then IPAddress := IPAddress + '.';
      end;
    end;
  end;

  Result := IPAddress;
end;
{$ENDREGION}

end.
