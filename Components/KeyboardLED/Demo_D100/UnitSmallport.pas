//--------------------------------------------------------------------------
//     SmallPort ver 1.4 for Delphi 4 modified 21-August-2000
//     Windows NT/2000 compatible 
//     Author: A.Weitzman
//--------------------------------------------------------------------------

unit UnitSmallport;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Winsvc;

type

  TBytePort = record address: WORD; value: BYTE; end;
  TWordPort = record address: WORD; value: WORD; end;
  TDWordPort = record address: WORD; value: DWORD; end;

  TSmallPort = class(TComponent)
  private
   PortResult: boolean;
   dwLastError: DWORD;
   winver: DWORD;
   dwReadByteCode:   DWORD;
   dwWriteByteCode:  DWORD;
   dwReadWordCode:   DWORD;
   dwWriteWordCode:  DWORD;
   dwReadDWordCode:  DWORD;
   dwWriteDWordCode: DWORD;
   DriverName: string;
   bRemoveDevice: Boolean;
   bStopService: Boolean;
   procedure ReOpen(value: boolean);
   procedure OpenServiceManager;
   procedure CloseServiceManager;
   procedure InstallSmallPortDriver;
   procedure RemoveSmallPortDriver;
   procedure StartSmallPortService;
   procedure StopSmallPortService;
  protected
    hManager: SC_HANDLE;
    hDevice: SC_HANDLE;
    cbBytesReturned: DWORD;
    ByteData: TBytePort;
    WordData: TWordPort;
    DWordData: TDWordPort;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    function OpenDriver :boolean;
    procedure CloseDriver;
    function IsOpen: boolean;
    function ReadByte(Index: WORD): WORD;
    procedure WriteByte(Index: WORD;value: WORD);
    function ReadWord(port: WORD): WORD;
    procedure WriteWord(port: WORD;value: WORD);
    function ReadDWord(port: WORD): DWORD;
    function GetWindowsVersion: DWORD;
    procedure WriteDWord(port: WORD;value: DWORD);
    procedure Sound(wFrq: WORD; delay: DWORD);
    procedure SpeakerOn(wTone: WORD);
    procedure SpeakerOff;
    property Port[Index1:WORD] : Word read ReadByte write WriteByte;
    property PortW[Index1:WORD] : Word read ReadWord write WriteWord;
    property PortDW[Index1:WORD] : DWORD read ReadDWord write WriteDWord;
    property IOResult: Boolean read PortResult;
    property LastError: DWORD read dwLastError;
  published
    property Opened :Boolean read IsOpen write ReOpen default false;
    property DriverHandle :DWORD read hDevice default 0;
  end;

procedure Register;

const
  SMALLPORT_TYPE      = 61696;

  METHOD_BUFFERED     = 0;
  FILE_READ_ACCESS    = 1;
  FILE_WRITE_ACCESS   = 2;

  VER_WINDOWS_UNKNOWN = 0;
  VER_WINDOWS_31      = 1;
  VER_WINDOWS_95      = 2;
  VER_WINDOWS_98      = 3;
  VER_WINDOWS_NT      = 4;
  VER_WINDOWS_2000    = 5;
  VER_WINDOWS_ME      = 6;

implementation

procedure Register;
begin
  RegisterComponents('System', [TSmallPort]);
end;

function CTL_CODE(DeviceType: integer; func: integer; meth: integer; access: integer): DWORD;
Begin
  result := (DeviceType shl 16) or (Access shl 14) or (func shl 2) or (meth);
end;

constructor TSmallPort.Create(aOwner: TComponent);
begin
 inherited Create(aOwner);
 hDevice := INVALID_HANDLE_VALUE;
 hManager := 0;
 DriverName := 'Smport';
 bRemoveDevice := TRUE;
 bStopService := TRUE;
 winver := GetWindowsVersion;
 if(winver = VER_WINDOWS_NT)or(winver = VER_WINDOWS_2000) then
 begin
   dwReadByteCode   := CTL_CODE(SMALLPORT_TYPE, $901, METHOD_BUFFERED, FILE_READ_ACCESS);
   dwWriteByteCode  := CTL_CODE(SMALLPORT_TYPE, $902, METHOD_BUFFERED, FILE_WRITE_ACCESS);
   dwReadWordCode   := CTL_CODE(SMALLPORT_TYPE, $903, METHOD_BUFFERED, FILE_READ_ACCESS);
   dwWriteWordCode  := CTL_CODE(SMALLPORT_TYPE, $904, METHOD_BUFFERED, FILE_WRITE_ACCESS);
   dwReadDWordCode  := CTL_CODE(SMALLPORT_TYPE, $905, METHOD_BUFFERED, FILE_READ_ACCESS);
   dwWriteDWordCode := CTL_CODE(SMALLPORT_TYPE, $906, METHOD_BUFFERED, FILE_WRITE_ACCESS);
 end
 else
 begin
   dwReadByteCode   := 2;
   dwWriteByteCode  := 3;
   dwReadWordCode   := 4;
   dwWriteWordCode  := 5;
   dwReadDWordCode  := 6;
   dwWriteDWordCode := 7;
 end;
 PortResult:=False;
 dwLastError := 0;
end;

destructor TSmallPort.Destroy;
begin
  if(hDevice <> INVALID_HANDLE_VALUE) then CloseDriver;
  if(winver = VER_WINDOWS_NT)or(winver = VER_WINDOWS_2000) then
  begin
    OpenServiceManager;
    if hManager <> 0 then
    begin
      if bStopService then StopSmallPortService;
      if bRemoveDevice then RemoveSmallPortDriver;
      CloseServiceManager;
    end;  
  end;
  inherited Destroy;
end;

function TSmallPort.OpenDriver :boolean;
var buf: PChar; s: string;
begin
 Result:=false;
 buf:='';
 if hDevice <> INVALID_HANDLE_VALUE then CloseDriver;
 if(winver = VER_WINDOWS_NT)or(winver = VER_WINDOWS_2000) then
 begin
   OpenServiceManager;
   if dwLastError = ERROR_SUCCESS then
   begin
     InstallSmallPortDriver;
     if dwLastError <> ERROR_SUCCESS then
     begin
       if dwLastError = ERROR_SERVICE_EXISTS then
       begin
         bRemoveDevice := FALSE;
         dwLastError := ERROR_SUCCESS;
       end
       else CloseServiceManager;
     end;
     if dwLastError = ERROR_SUCCESS then
     begin
       StartSmallPortService;
       if dwLastError <> ERROR_SUCCESS then
       begin
         if dwLastError = ERROR_SERVICE_ALREADY_RUNNING then
         begin
           bStopService := FALSE;
           dwLastError := ERROR_SUCCESS;
         end;
       end;
       CloseServiceManager;
       if dwLastError = ERROR_SUCCESS then
       begin
         hDevice := CreateFile(PChar('\\.\'+ DriverName),
                               GENERIC_READ OR GENERIC_WRITE,
                               0,
                               PSECURITY_DESCRIPTOR(nil),
                               OPEN_EXISTING,
                               FILE_ATTRIBUTE_NORMAL,
                               0);
        if hDevice <> INVALID_HANDLE_VALUE then Result := TRUE
        else dwLastError := GetLastError;
       end;
     end;
   end;
 end
 else
 begin
   hDevice := CreateFile( '\\.\smport.vxd', 0, 0, nil,0,0,0);
   if hDevice = INVALID_HANDLE_VALUE then
   begin
    GetSystemDirectory(buf,MAX_PATH);
    s:=buf; s:='\\.\'+s+'\smport.vxd';
    hDevice := CreateFile(PChar(s),0,0,nil,0,0,0);
    if hDevice = INVALID_HANDLE_VALUE then
    begin
      dwLastError := GetLastError;
    end;
   end
   else begin PortResult:=false; Result:=true end;
 end;
end;

procedure TSmallPort.CloseDriver;
begin
 if hDevice <> INVALID_HANDLE_VALUE then
 begin
  CloseHandle(hDevice);
  hDevice := INVALID_HANDLE_VALUE;
 end;
end;

function TSmallPort.IsOpen: boolean;
begin
 if hDevice = INVALID_HANDLE_VALUE then Result := FALSE
 else Result := TRUE;
end;

procedure TSmallPort.ReOpen(value: boolean);
begin
  if IsOpen then CloseDriver
  else OpenDriver;
end;

function TSmallPort.ReadByte(Index: WORD): WORD;
var
 value: word;
begin
 if hDevice = INVALID_HANDLE_VALUE then
 begin
   PortResult:=false;
   Result:=0;
   dwLastError := ERROR_INVALID_HANDLE;
 end
 else
  begin
   PortResult:=DeviceIoControl(hDevice, dwReadByteCode, @Index, sizeof(Index),
                               @value, sizeof(value), cbBytesReturned,nil);
   if PortResult then Result:=Lo(value)
   else
   begin
     dwLastError := GetLastError;
     Result:=0;
   end;
  end;
end;

procedure TSmallPort.WriteByte(Index: WORD; value: WORD);
begin
  if hDevice = INVALID_HANDLE_VALUE then
  begin
    PortResult:=false;
    dwLastError := ERROR_INVALID_HANDLE;
  end
 else
  begin
   ByteData.address:=Index;
   ByteData.value:=Lo(value);
   PortResult:=DeviceIoControl(hDevice, dwWriteByteCode, @ByteData, sizeof(ByteData),
                               @value,sizeof(value),cbBytesReturned,nil);
   if not PortResult then dwLastError := GetLastError;
  end;
end;

function TSmallPort.ReadWord(port: WORD): WORD;
var
 value: word;
begin
 if hDevice = INVALID_HANDLE_VALUE then
 begin
   PortResult:=false;
   Result:=0;
   dwLastError := ERROR_INVALID_HANDLE;
 end
 else
  begin
   PortResult:=DeviceIoControl(hDevice, dwReadWordCode, @port, sizeof(port),
                               @value, sizeof(value), cbBytesReturned,nil);
   if PortResult then Result := value
   else
   begin
     dwLastError := GetLastError;
     Result := 0;
   end;
  end;
end;

procedure TSmallPort.WriteWord(port: WORD; value: WORD);
begin
  if hDevice = INVALID_HANDLE_VALUE then
  begin
    PortResult:=false;
    dwLastError := ERROR_INVALID_HANDLE;
  end  
  else
  begin
   WordData.address:=port;
   WordData.value:=value;
   PortResult:=DeviceIoControl(hDevice, dwWriteWordCode, @WordData, sizeof(WordData),
                               @port, sizeof(port), cbBytesReturned,nil);
   if not PortResult then dwLastError := GetLastError;
  end;
end;

function TSmallPort.ReadDWord(port: WORD): DWORD;
var
 value: DWORD;
begin
 if hDevice = INVALID_HANDLE_VALUE then
 begin
   PortResult:=false;
   Result:=0;
   dwLastError := ERROR_INVALID_HANDLE;
 end
 else
  begin
    PortResult:=DeviceIoControl(hDevice, dwReadDWordCode, @port, sizeof(port),
                                @value, sizeof(value), cbBytesReturned, nil);
    if PortResult then Result := value
    else
    begin
      dwLastError := GetLastError;
      Result:=0;
    end;
  end;
end;

procedure TSmallPort.WriteDWord(port: WORD; value: DWORD);
begin
  if hDevice = INVALID_HANDLE_VALUE then
  begin
    PortResult:=false;
    dwLastError := ERROR_INVALID_HANDLE;
  end
 else
  begin
    DWordData.address:=port;
    DWordData.value:=value;
    PortResult:=DeviceIoControl(hDevice, dwWriteDWordCode, @DWordData, sizeof(DWordData),
                                @port, sizeof(port), cbBytesReturned, nil);
    if not PortResult then dwLastError := GetLastError;
  end;
end;

procedure TSmallPort.SpeakerOn(wTone: WORD);
begin
  Port[$61]:=(Port[$61]OR $03);
  Port[$43]:=$B6;
  Port[$42]:=Lo(wTone);
  Port[$42]:=Hi(wTone);
end;

procedure TSmallPort.SpeakerOff;
begin
  Port[$61]:=(Port[$61]AND $FC);
end;

procedure TSmallPort.Sound(wFrq: WORD; delay: DWORD);
var Tone: WORD;
begin
  Tone:=Round(1193180.0/wFrq);
  SpeakerOn(Tone);
  Sleep(delay);
  SpeakerOff();
end;

function TSmallPort.GetWindowsVersion: DWORD;
var
  ver: TOSVersionInfoA;
begin
  Result := VER_WINDOWS_UNKNOWN;
  ver.dwOSVersionInfoSize := sizeof(TOSVersionInfoA);
  GetVersionExA(ver);
  case ver.dwPlatformId of
    VER_PLATFORM_WIN32s: Result := VER_WINDOWS_31;
    VER_PLATFORM_WIN32_WINDOWS:       begin
                                        case ver.dwMinorVersion of
                                           0: Result := VER_WINDOWS_95;
                                          10: Result := VER_WINDOWS_98;
                                          90: Result := VER_WINDOWS_ME;
                                        end;
                                      end;
    VER_PLATFORM_WIN32_NT:            begin
                                        case ver.dwMajorVersion of
                                          4: Result := VER_WINDOWS_NT;
                                          5: Result := VER_WINDOWS_2000;
                                        end;
                                      end;
  end;
end;

procedure TSmallPort.OpenServiceManager;
begin
  if hManager = 0 then
  begin
    dwLastError := 0;
    hManager := OpenSCManager(nil, nil, SC_MANAGER_ALL_ACCESS);
    if hManager = 0 then dwLastError := GetLastError;
  end;
end;

procedure TSmallPort.CloseServiceManager;
begin
  if(hManager <> 0)then
  begin
    CloseServiceHandle(hManager);
    hManager := 0;
  end;
end;

procedure TSmallPort.InstallSmallPortDriver;
var
  hService: SC_HANDLE;
  DriverPath: string;
  buf: array[0..MAX_PATH - 1]of char;
begin
  dwLastError := 0;
  DriverPath := GetCurrentDir + '\' + DriverName + '.sys';
  if not FileExists(DriverPath)then
  begin
    GetSystemDirectory(buf, MAX_PATH);
    DriverPath := StrPas(buf) + '\' + DriverName + '.sys';
  end;
  hService := CreateService(hManager, PChar(DriverName),PChar(DriverName),
                            SERVICE_ALL_ACCESS,
                            SERVICE_KERNEL_DRIVER,
                            SERVICE_DEMAND_START,
                            SERVICE_ERROR_NORMAL,
                            PChar(DriverPath),
                            nil, nil, nil, nil, nil);
  if hService = 0 then dwLastError := GetLastError
  else
  begin
    CloseServiceHandle(hService);
    dwLastError := 0;
  end;
end;

procedure TSmallPort.RemoveSmallPortDriver;
var
  hService: SC_HANDLE;
begin
  dwLastError := 0;
  hService := OpenService(hManager, PChar(DriverName), SERVICE_ALL_ACCESS);
  if hService <> 0 then
  begin
    if not DeleteService(hService) then dwLastError := GetLastError;
  end
  else dwLastError := GetLastError();
  if hService <> 0 then CloseServiceHandle(hService);
end;

procedure TSmallPort.StartSmallPortService;
var
  hService: SC_HANDLE;
  lpVectors: PChar;
begin
  dwLastError := 0;
  lpVectors := nil;
  hService := OpenService(hManager, PChar(DriverName), SERVICE_ALL_ACCESS);
  if hService <> 0 then
  begin
    if not StartService(hService, 0, lpVectors)then dwLastError := GetLastError;
  end
  else dwLastError := GetLastError;
  if (hService <> 0) then CloseServiceHandle(hService);
end;

procedure TSmallPort.StopSmallPortService;
var
  hService: SC_HANDLE;
  Status: TServiceStatus;
begin
  dwLastError := 0;
  hService := OpenService(hManager, PChar(DriverName), SERVICE_ALL_ACCESS);
  if hService <> 0 then
  begin
    if not ControlService(hService, SERVICE_CONTROL_STOP, Status) then
      dwLastError := GetLastError;
  end
  else dwLastError := GetLastError;
  if (hService <> 0) then CloseServiceHandle(hService);
end;

end.
 
