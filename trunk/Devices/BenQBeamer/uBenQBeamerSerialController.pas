unit uBenQBeamerSerialController;

interface

uses
  Classes, Windows, SysUtils, StrUtils;

type
  EBenqBeamer = type Exception;
  EBBLostConnection = type EBenqBeamer;
  EBBCommandNotSupported = type EBenqBeamer;
  EBBBlocked = type EBenqBeamer;
  EBBIllegalFomat = type EBenqBeamer;

  TBBAnswerType = (atOK, atUnsupported, atBlocked, atIllegalFormat, atOther);
  TBBParsedAnswer = record
    AnswerType : TBBAnswerType;
    RawAnswer,
    Name,
    Value : String;
  end;

  TBBCommandIDs = (ciPow,
                   ciModelName);

  TBenQBeamerSerialController = class
  protected
    FComport: String;
    FPort : THandle;
    FBaud: DWORD;

    procedure SetBaud(const Value: DWORD);
    function GetOnOffValue(const Index: Integer): Boolean;
    procedure SetOnOffValue(const Index: Integer; const Value: Boolean);
    function GetStringValue(const Index: Integer): String;
    procedure SetStringValue(const Index: Integer; const Value: String);

    procedure NeedConnection;
    function DefaultCommandCode(ACommand : String) : TBBParsedAnswer;

    function GetIsOpen: Boolean;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Open();
    procedure Close();

    property ComPort : String read FComport write FComport;
    property IsOpen : Boolean read GetIsOpen;

    property PowerOn : Boolean index ciPow read GetOnOffValue write SetOnOffValue;

    property ModelName : String index ciModelName read GetStringValue;
    property Baud : DWORD read FBaud write SetBaud;

    class function OpenPort(AComPort : String) : THandle;

    class function SendCommand(AComPort : String;
                               ABaud : DWORD;
                               ACommand : AnsiString) : String; overload;

    class function SendCommand(AComPort : THandle;
                               ACommand : AnsiString) : String; overload;

    class procedure ConfigurePort(AComPort : THandle;
                                  ABaud : DWORD);

    class function ParseAnswer(AAnswer : String) : TBBParsedAnswer;

    class procedure RaiseAnswerException(AAnswer : TBBParsedAnswer);
  end;

const
  BBCommands : array[Low(TBBCommandIDs)..High(TBBCommandIDs)] of AnsiString =
    ('pow', 'modelname');

implementation

const
  EBBLostConnectionMessage = 'Lost connection to beamer.';
  EBBCommandNotSupportedMessage = 'Command not supported';
  EBBBlockedMessage = 'Beamer is blocked';
  EBBIllegalFormatMessage = 'Illegal format';

{ TBenQSerialController }

procedure TBenQBeamerSerialController.Close;
begin
  if FPort <> INVALID_HANDLE_VALUE then
    CloseHandle(FPort);

  FPort := INVALID_HANDLE_VALUE;
end;

class procedure TBenQBeamerSerialController.ConfigurePort(AComPort: THandle;
  ABaud: DWORD);
var
  cs : TDCB;
  cto : TCommTimeouts;
begin
  cs.DCBlength := SizeOf(cs);
  GetCommState(AComPort, cs);
  cs.BaudRate := ABaud;
  cs.XonLim := 0;
  cs.XoffLim := 0;
  cs.ByteSize := 8;
  cs.Parity := NOPARITY;
  cs.StopBits := ONESTOPBIT;

  if not SetCommState(AComPort, cs) then
    raise EBenqBeamer.Create('Can not configure comport');

  cto.ReadIntervalTimeout := 50;
  cto.ReadTotalTimeoutMultiplier := 50;
  cto.ReadTotalTimeoutConstant := 500;
  cto.WriteTotalTimeoutMultiplier := 50;
  cto.WriteTotalTimeoutConstant := 500;
  if not SetCommTimeouts(AComPort, cto) then
    raise EBenqBeamer.Create('Can not set timeouts on comport');
end;

constructor TBenQBeamerSerialController.Create;
begin
  inherited;
  FPort := INVALID_HANDLE_VALUE;
  FBaud := 115200;
end;

function TBenQBeamerSerialController.DefaultCommandCode(
  ACommand: String): TBBParsedAnswer;
begin
  NeedConnection;
  Result := ParseAnswer(SendCommand(FPort, ACommand));
  RaiseAnswerException(Result);
end;

destructor TBenQBeamerSerialController.Destroy;
begin
  Close;
  inherited;
end;

function TBenQBeamerSerialController.GetIsOpen: Boolean;
begin
  Result := FPort <> INVALID_HANDLE_VALUE;
end;

function TBenQBeamerSerialController.GetOnOffValue(
  const Index: Integer): Boolean;
var
  a : TBBParsedAnswer;
begin
  a := DefaultCommandCode(BBCommands[TBBCommandIDs(Index)] + '=?');
  if SameText(a.Value, 'on') then
    Result := true
  else
    Result := false;
end;

function TBenQBeamerSerialController.GetStringValue(
  const Index: Integer): String;
var
  a : TBBParsedAnswer;
begin
  a := DefaultCommandCode(BBCommands[TBBCommandIDs(Index)] + '=?');
  Result := a.Value;
end;

procedure TBenQBeamerSerialController.NeedConnection;
begin
  if not IsOpen then
    raise EBenqBeamer.Create('Connection not open');
end;

procedure TBenQBeamerSerialController.Open();
begin
  Close;
  try
    FPort := OpenPort(FComport);
    ConfigurePort(FPort, FBaud);
  except
    Close;
    raise;
  end;
end;

class function TBenQBeamerSerialController.OpenPort(AComPort: String): THandle;
begin
  Result := CreateFile(PChar(AComPort),
                       GENERIC_READ or GENERIC_WRITE,
                       0,
                       nil,
                       OPEN_EXISTING,
                       FILE_ATTRIBUTE_NORMAL,
                       0);
  if Result = INVALID_HANDLE_VALUE then
    raise EBenqBeamer.Create('Can not open ' + AComPort);
end;

class function TBenQBeamerSerialController.ParseAnswer(
  AAnswer: String): TBBParsedAnswer;
var
  p : Integer;
begin
  Result.RawAnswer := AAnswer;
  Result.Name := EmptyStr;
  Result.Value := EmptyStr;

  p := Pos('=', AAnswer);

  if SameText(AAnswer, 'Unsupported item') then
  begin
    Result.AnswerType := atUnsupported;
  end
  else
  if SameText(AAnswer, 'Block item') then
  begin
    Result.AnswerType := atBlocked;
  end
  else
  if SameText(AAnswer, 'Illegal format') then
  begin
    Result.AnswerType := atIllegalFormat;
  end
  else
  if p > 0 then
  begin
    Result.AnswerType := atOK;
    Result.Name := Copy(AAnswer, 1, p - 1);
    Result.Value := Copy(AAnswer, p + 1, Length(AAnswer) - p);
  end
  else
  begin
    Result.AnswerType := atOther;
  end;
end;

class procedure TBenQBeamerSerialController.RaiseAnswerException(
  AAnswer: TBBParsedAnswer);
begin
  case AAnswer.AnswerType of
    atUnsupported: raise EBBCommandNotSupported.Create(EBBCommandNotSupportedMessage);
    atBlocked: raise EBBBlocked.Create(EBBBlockedMessage);
    atIllegalFormat : raise EBBIllegalFomat.Create(EBBIllegalFormatMEssage);
  end;
end;

class function TBenQBeamerSerialController.SendCommand(AComPort: THandle;
  ACommand: AnsiString): String;
var
  bytes_written,
  bytes_read : DWORD;
  buffer : AnsiString;
begin
  ACommand := #13 + '*' + ACommand + '#' + #13;

  if (WriteFile(AComPort, ACommand[1], Length(ACommand), bytes_written, nil) and (bytes_written > 0)) then
  begin
    while bytes_written > 0 do
    begin
      if not (ReadFile(AComPort, ACommand[1], bytes_written, bytes_read, nil) and (bytes_read > 0)) then
        raise EBBLostConnection.Create(EBBLostConnectionMessage);

      Dec(bytes_written, bytes_read);
    end;

    SetLength(buffer, 50);
    if not (ReadFile(AComPort, buffer[1], Length(buffer), bytes_read, nil) and (bytes_read > 0)) then
        raise EBBLostConnection.Create(EBBLostConnectionMessage);

    SetLength(buffer, bytes_read);
    Result := Trim(Buffer);
    SetLength(buffer, 0);

    if (Length(Result) > 0) and (Result[1] = '*') and (Result[Length(Result)] = '#') then
      Result := Copy(Result, 2, Length(Result) - 2);
  end
  else
    EBBLostConnection.Create(EBBLostConnectionMessage);
end;

procedure TBenQBeamerSerialController.SetBaud(const Value: DWORD);
begin
  if IsOpen then
  begin
    try
      DefaultCommandCode('baud=' + IntToStr(Value));
    except
      on EBBLostConnection do
      begin
        Close;
        FBaud := Value;
        Open;
      end
      else
        raise;
    end;
  end
  else
    FBaud := Value;
end;

procedure TBenQBeamerSerialController.SetOnOffValue(const Index: Integer;
  const Value: Boolean);
begin
  DefaultCommandCode(BBCommands[ciPow] + '=' + IfThen(Value, 'on', 'off'));
end;

procedure TBenQBeamerSerialController.SetStringValue(const Index: Integer;
  const Value: String);
begin
  DefaultCommandCode(BBCommands[ciPow] + '=' + Value);
end;

class function TBenQBeamerSerialController.SendCommand(AComPort: String;
  ABaud: DWORD; ACommand: AnsiString): String;
var
  Port : THandle;
begin
   Port := OpenPort(AComPort);
  try
    ConfigurePort(Port, ABaud);
    Result := SendCommand(Port, ACommand);
  finally
    CloseHandle(Port);
  end;
end;

end.
