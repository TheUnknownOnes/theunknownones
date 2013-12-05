unit uSimpleComport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  {$IFDEF UNIX}
  BaseUnix, TermIO
  {$ENDIF}
  {$IFDEF WINDOWS}
  Windows
  {$ENDIF};

type

  TcsSpeed = ({$IFDEF UNIX}css50, css75,{$ENDIF}
              css110,
              {$IFDEF UNIX}css134, css150, css200,{$ENDIF}
              css300,
              css600,
              css1200,
              {$IFDEF UNIX}css1800,{$ENDIF}
              css2400,
              css4800,
              css9600,
              {$IFDEF WINDOWS}css14400,{$ENDIF}
              css19200,
              css38400,
              {$IFDEF WINDOWS}css56000,{$ENDIF}
              css57600,
              css115200,
              {$IFDEF WINDOWS}css128000,{$ENDIF}
              {$IFDEF UNIX}css230400,{$ENDIF}
              {$IFDEF WINDOWS}css256000{$ENDIF}
              {$IFDEF UNIX}css460800{$ENDIF}
             );

  TcsByteSize = (csb5, csb6, csb7, csb8);
  TcsParity = (cspNone, cspOdd, cspEven);
  TcsStopBits = (cssb1, {$IFDEF WINDOWS}cssb15,{$ENDIF} cssb2);

  { TSimpleComport }

  TSimpleComport = class
  protected
    FPort : {$IFDEF UNIX}cint{$ENDIF}{$IFDEF WINDOWS}Handle{$ENDIF};
    FConfig : {$IFDEF UNIX}Termios{$ENDIF}{$IFDEF WINDOWS}TDCB{$ENDIF};
    {$IFDEF WINDOWS}FTimeouts : TCOMMTIMEOUTS;{$ENDIF}
    FInConfigUpdate : Integer;
    FPortName : String;

    procedure ReadConfig; virtual;
    procedure WriteConfig; virtual;

    function GetByteSize: TcsByteSize; virtual;
    function GetSpeed: TcsSpeed; virtual;
    procedure SetByteSize(AValue: TcsByteSize); virtual;
    procedure SetSpeed(AValue: TcsSpeed); virtual;
    function GetParity: TcsParity; virtual;
    function GetStopBits: TcsStopBits; virtual;
    procedure SetParity(AValue: TcsParity); virtual;
    procedure SetStopBits(AValue: TcsStopBits); virtual;
    {$IFDEF UNIX}
    function GetCC(AIndex: Integer): Byte; virtual;
    procedure SetCC(AIndex: Integer; AValue: Byte); virtual;
    {$ENDIF}
    {$IFDEF WINDOWS}
    function GetTimeout(AIndex: Integer): DWORD; virtual;
    procedure SetTimeout(AIndex: Integer; AValue: DWORD); virtual;
    {$ENDIF}
  public
    constructor Create(APort : String); virtual;
    destructor Destroy; override;

    function ReadAnsiString(out AString : AnsiString; AChunkSize : Word = 32) : Boolean;
    function WriteAnsiString(const AString : AnsiString) : Boolean;

    procedure BeginUpdateConfig; virtual;
    procedure EndUpdateConfig; virtual;

    property PortName : String read FPortName;
    property Speed : TcsSpeed read GetSpeed write SetSpeed;
    property ByteSize : TcsByteSize read GetByteSize write SetByteSize;
    property Parity : TcsParity read GetParity write SetParity;
    property StopBits : TcsStopBits read GetStopBits write SetStopBits;

    {$IFDEF UNIX}
    property MinBytes : Byte index VMIN read GetCC write SetCC;
    property Timeout : Byte index VTIME read GetCC write SetCC;
    {$ENDIF}
    {$IFDEF WINDOWS}
    property ReadIntervalTimeout : DWORD index 0 read GetTimeout write SetTimeout;
    property ReadTotalTimeoutMultiplier : DWORD index 1 read GetTimeout write SetTimeout;
    property ReadTotalTimeoutConstant : DWORD index 2 read GetTimeout write SetTimeout;
    property WriteTotalTimeoutMultiplier : DWORD index 3 read GetTimeout write SetTimeout;
    property WriteTotalTimeoutConstant : DWORD index 4 read GetTimeout write SetTimeout;
    {$ENDIF}
  end;

implementation

const
  SPEEDS : array[Low(TcsSpeed)..High(TcsSpeed)] of Cardinal = (
    {$IFDEF UNIX}
    B50,
    B75,
    B110,
    B134,
    B150,
    B200,
    B300,
    B600,
    B1200,
    B1800,
    B2400,
    B4800,
    B9600,
    B19200,
    B38400,
    B57600,
    B115200,
    B230400,
    B460800
    {$ENDIF}
    {$IFDEF WINDOWS}
    CBR_110,
    CBR_300,
    CBR_600,
    CBR_1200,
    CBR_2400,
    CBR_4800,
    CBR_9600,
    CBR_14400,
    CBR_19200,
    CBR_38400,
    CBR_56000,
    CBR_57600,
    CBR_115200,
    CBR_128000,
    CBR_256000
    {$ENDIF}
  );

  BYTESIZES : array[Low(TcsByteSize)..High(TcsByteSize)] of {$IFDEF UNIX}Cardinal{$ENDIF}{$IFDEF WINDOWS}Byte{$ENDIF} = (
    {$IFDEF UNIX}CS5, CS6, CS7, CS8{$ENDIF}
    {$IFDEF WINDOWS}5, 6, 7, 8{$ENDIF}
  );

{ TSimpleComport }

function TSimpleComport.ReadAnsiString(out AString: AnsiString; AChunkSize: Word): Boolean;
var
  buffer : AnsiString;
  bytes_read : {$IFDEF UNIX}TSsize{$ENDIF}
               {$IFDEF WINDOWS}Cardinal{$ENDIF};
begin
  Result := false;
  AString := '';
  repeat
    SetLength(buffer, AChunkSize);
    {$IFDEF UNIX}
    bytes_read := FpRead(FPort, buffer[1], AChunkSize);
    if bytes_read > 0 then
    {$ENDIF}
    {$IFDEF WINDOWS}
    if ReadFile(FPort, buffer[1], AChunkSize, bytes_read, nil) and (bytes_read > 0) then
    {$ENDIF}
    begin
      SetLength(buffer, bytes_read);
      AString := AString + buffer;
      Result := true;
    end;
  until bytes_read <= 0;
end;

function TSimpleComport.WriteAnsiString(const AString: AnsiString): Boolean;
{$IFDEF WINDOWS}
var
  bytes_written : Cardinal;
{$ENDIF}
begin
  {$IFDEF UNIX}
  Result := FpWrite(FPort, AString[1], Length(AString)) = Length(AString);
  {$ENDIF}
  {$IFDEF WINDOWS}
  Result := WriteFile(FPort, AString[1], Length(AString), bytes_written, nil);
  {$ENDIF}
end;

procedure TSimpleComport.BeginUpdateConfig;
begin
  Inc(FInConfigUpdate);
end;

procedure TSimpleComport.EndUpdateConfig;
begin
  Dec(FInConfigUpdate);
  if FInConfigUpdate < 0 then
    FInConfigUpdate := 0;
  if FInConfigUpdate = 0 then
    WriteConfig;
end;

function TSimpleComport.GetByteSize: TcsByteSize;
var
  idx : TcsByteSize;
begin
  for idx := Low(TcsByteSize) to High(TcsByteSize) do
  begin
    {$IFDEF UNIX}
    if (FConfig.c_cflag and BYTESIZES[idx]) = BYTESIZES[idx] then Result := idx;
    {$ENDIF}
    {$IFDEF WINDOWS}
    if FConfig.ByteSize = BYTESIZES[idx] then Result := idx;
    {$ENDIF}
  end;
end;

function TSimpleComport.GetSpeed: TcsSpeed;
var
  idx : TcsSpeed;
begin
  for idx := Low(TcsSpeed) to High(TcsSpeed) do
  begin
    {$IFDEF UNIX}
    if (FConfig.c_cflag and SPEEDS[idx]) = SPEEDS[idx] then Result := idx;
    {$ENDIF}
    {$IFDEF WINDOWS}
    if FConfig.BaudRate = SPEEDS[idx] then Result := idx;
    {$ENDIF}
  end;
end;

procedure TSimpleComport.SetByteSize(AValue: TcsByteSize);
begin
  {$IFDEF UNIX}
  FConfig.c_cflag := FConfig.c_cflag and BYTESIZES[AValue];
  {$ENDIF}
  {$IFDEF WINDOWS}
  FConfig.ByteSize := BYTESIZES[AValue];
  {$ENDIF}
  WriteConfig;
end;

procedure TSimpleComport.SetSpeed(AValue: TcsSpeed);
begin
  {$IFDEF UNIX}
  FConfig.c_cflag := FConfig.c_cflag and SPEEDS[AValue];
  {$ENDIF}
  {$IFDEF WINDOWS}
  FConfig.BaudRate := SPEEDS[AValue];
  {$ENDIF}
  WriteConfig;
end;

function TSimpleComport.GetParity: TcsParity;
begin
  {$IFDEF UNIX}
  if (FConfig.c_cflag and PARENB) = PARENB then
  begin
    if (FConfig.c_cflag and PARODD) = PARODD then
      Result := cspOdd
    else
      Result := cspEven;
  end
  else
    Result := cspNone;
  {$ENDIF}
  {$IFDEF WINDOWS}
  case FConfig.Parity of
    NOPARITY : Result := cspNone;
    ODDPARITY : Result := cspOdd;
    EVENPARITY : Result := cspEven;
  end;
  {$ENDIF}
end;

function TSimpleComport.GetStopBits: TcsStopBits;
begin
  {$IFDEF UNIX}
  if (FConfig.c_cflag or CSTOPB) = CSTOPB then
    Result := cssb1
  else
    Result := cssb2;
  {$ENDIF}
  {$IFDEF WINDOWS}
  case FConfig.StopBits of
    ONESTOPBIT : Result := cssb1;
    ONE5STOPBITS : Result := cssb15;
    TWOSTOPBITS : Result := cssb2;
  end;
  {$ENDIF}
end;

procedure TSimpleComport.SetParity(AValue: TcsParity);
begin
  {$IFDEF UNIX}
  case AValue of
    cspNone : FConfig.c_cflag := FConfig.c_cflag and (not (PARENB or PARODD));
    cspOdd : FConfig.c_cflag := FConfig.c_cflag and (PARENB or PARODD);
    cspEven : FConfig.c_cflag := (FConfig.c_cflag and PARENB) and (not PARODD);
  end;
  {$ENDIF}
  {$IFDEF WINDOWS}
  case AValue of
    cspNone : FConfig.Parity := NOPARITY;
    cspOdd : FConfig.Parity := ODDPARITY;
    cspEven : FConfig.Parity := EVENPARITY;
  end;
  {$ENDIF}
  WriteConfig;
end;

procedure TSimpleComport.SetStopBits(AValue: TcsStopBits);
begin
  {$IFDEF UNIX}
  case AValue of
    cssb1 : FConfig.c_cflag := FConfig.c_cflag and (not CSTOPB);
    cssb2 : FConfig.c_cflag := FConfig.c_cflag or CSTOPB;
  end;
  {$ENDIF}
  {$IFDEF WINDOWS}
  case AValue of
    cssb1 : FConfig.StopBits := ONESTOPBIT;
    cssb15 : FConfig.StopBits := ONE5STOPBITS;
    cssb2 : FConfig.StopBits := TWOSTOPBITS;
  end;
  {$ENDIF}
  WriteConfig;
end;

{$IFDEF WINDOWS}
function TSimpleComport.GetTimeout(AIndex: Integer): DWORD;
begin
  case AIndex of
    0: Result := FTimeouts.ReadIntervalTimeout;
    1: Result := FTimeouts.ReadTotalTimeoutMultiplier;
    2: Result := FTimeouts.ReadTotalTimeoutConstant;
    3: Result := FTimeouts.WriteTotalTimeoutMultiplier;
    4: Result := FTimeouts.WriteTotalTimeoutConstant;
  end;
end;
{$ENDIF}

{$IFDEF WINDOWS}
procedure TSimpleComport.SetTimeout(AIndex: Integer; AValue: DWORD);
begin
  case AIndex of

    0: FTimeouts.ReadIntervalTimeout := AValue;
    1: FTimeouts.ReadTotalTimeoutMultiplier := AValue;
    2: FTimeouts.ReadTotalTimeoutConstant := AValue;
    3: FTimeouts.WriteTotalTimeoutMultiplier := AValue;
    4: FTimeouts.WriteTotalTimeoutConstant := AValue;
  end;
  WriteConfig;
end;
{$ENDIF}

{$IFDEF UNIX}
function TSimpleComport.GetCC(AIndex: Integer): Byte;
begin
  Result := FConfig.c_cc[AIndex];
end;
{$ENDIF}

{$IFDEF UNIX}
procedure TSimpleComport.SetCC(AIndex: Integer; AValue: Byte);
begin
  FConfig.c_cc[AIndex] := AValue;
  WriteConfig
end;
{$ENDIF}

procedure TSimpleComport.ReadConfig;
begin
  {$IFDEF UNIX}
  if TCGetAttr(FPort, FConfig) <> 0 then
  {$ENDIF}
  {$IFDEF WINDOWS}
  FConfig.DCBlength := SizeOf(FConfig);
  if not (GetCommState(FPort, FConfig) and GetCommTimeouts(FPort, FTimeouts)) then
  {$ENDIF}
    RaiseLastOSError;
end;

procedure TSimpleComport.WriteConfig;
var
  FOldConfig : {$IFDEF UNIX}Termios{$ENDIF}{$IFDEF WINDOWS}TDCB{$ENDIF};
  {$IFDEF WINDOWS}FOldTimeouts : TCOMMTIMEOUTS;{$ENDIF}
begin
  if FInConfigUpdate > 0 then exit;

  FOldConfig := FConfig;
  {$IFDEF WINDOWS}
  FOldTimeouts := FTimeouts;
  {$ENDIF}

  {$IFDEF UNIX}
  TCFlush(FPort, TCIOFLUSH);
  if TCSetAttr(FPort, TCSANOW, FConfig) <> 0 then
  {$ENDIF}
  {$IFDEF WINDOWS}
  if not (SetCommState(FPort, FConfig) and SetCommTimeouts(FPort, FTimeouts)) then
  {$ENDIF}
  begin
    FConfig := FOldConfig;
    {$IFDEF WINDOWS}
    FTimeouts := FOldTimeouts;
    {$ENDIF}
    RaiseLastOSError;
  end;
end;

constructor TSimpleComport.Create(APort : String);
begin
  inherited Create();

  FInConfigUpdate := 0;

  FPortName := APort;
  {$IFDEF UNIX}
  FPort := FpOpen(APort, O_RDWR or O_NOCTTY or O_NONBLOCK);
  {$ENDIF}
  {$IFDEF WINDOWS}
  FPort := CreateFile(PChar(APort),
                      GENERIC_READ or GENERIC_WRITE,
                      0,
                      nil,
                      OPEN_EXISTING,
                      0,
                      0);
  {$ENDIF}

  {$IFDEF UNIX}
  if FPort < 0 then
  {$ENDIF}
  {$IFDEF WINDOWS}
  if FPort = INVALID_HANDLE_VALUE then
  {$ENDIF}
    raise Exception.Create('ComPort "' + APort + '" konnte nicht geöffnet werden');

  {$IFDEF UNIX}
  FConfig.c_cflag := FConfig.c_cflag and CLOCAL and CREAD;
  WriteConfig;
  {$ENDIF}
end;

destructor TSimpleComport.Destroy;
begin
  {$IFDEF UNIX}
  FpClose(FPort);
  {$ENDIF}

  {$IFDEF WINDOWS}
  CloseHandle(FPort);
  {$ENDIF}

  inherited Destroy;
end;

end.

