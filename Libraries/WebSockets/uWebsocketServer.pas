unit uWebsocketServer;

interface

uses
  Classes,
  Controls,
  SysUtils,
  StrUtils,
  IdGlobal,
  IdTCPConnection,
  IdContext,
  IdBaseComponent,
  IdComponent,
  IdYarn,
  IdCustomTCPServer,
  IdHashMessageDigest,
  idHash,
  RegExpr,
  IdException,
  IdExceptionCore;

  //Magic Bytes
const
  MBFrameStart = $00;
  MBFrameEnd = $FF;

type
  TWebsocketServer = class;
  TWebsocketConnection = class;

  TWebsocketInitialRequest = class
  protected
    FValues : array[0..6] of String;
    FChallengeData : TBytes;

    function GetValue(const Index: Integer): String;
  public
    constructor Create(AContext : TIdContext); virtual;

    property Document : String index 0 read GetValue;
    property HTTPVersion : String index 1 read GetValue;
    property Host : String index 2 read GetValue;
    property WebsocketProtocol : String index 3 read GetValue;
    property WebsocketKey1 : String index 4 read GetValue;
    property WebsocketKey2 : String index 5 read GetValue;
    property Origin : String index 6 read GetValue;

    property ChallengeData : TBytes read FChallengeData;
  end;


  TWebsocketConnection = class(TIdServerContext)
  protected
    FInitialRequest : TWebsocketInitialRequest;

    procedure DoProcessIncomingStringData(AData : String); virtual;

    procedure InitialAnswer; virtual;
  public
    constructor Create(AConnection: TIdTCPConnection; AYarn: TIdYarn; AList: TThreadList = nil); override;
    destructor Destroy(); override;

    procedure SendData(AData : String); overload;

    class function DecodeSecWebSocketKey(AKey: String): Integer;
    class function IntToIntBigEndian(AInt : Integer) : Integer;

    property InitialRequest : TWebsocketInitialRequest read FInitialRequest;
  end;

  TWebsocketConnectionClass = class of TWebsocketConnection;



  TWebsocketServer = class(TIdCustomTCPServer)
  protected const
    DefaultReadTimeout = 1000;
  protected var
    FReadTimeout: Integer;

    function DoExecute(AContext : TIdContext): Boolean; override;
    procedure DoConnect(AContext: TIdContext); override;
    procedure DoDisconnect(AContext: TIdContext); override;

    function GetClientClass: TWebsocketConnectionClass;
    procedure SetClientClass(const Value: TWebsocketConnectionClass); virtual;
  public
    procedure AfterConstruction; override;

    property ReadTimeout : Integer read FReadTimeout write FReadTimeout;

    property ContextClass : TWebsocketConnectionClass read GetClientClass write SetClientClass;
  end;



implementation

{ TWebsocketServer }

procedure TWebsocketServer.AfterConstruction;
begin
  inherited;

  ContextClass := TWebsocketConnection;
  FReadTimeout := DefaultReadTimeout;
end;

procedure TWebsocketServer.DoConnect(AContext: TIdContext);
begin
  inherited;

  TWebsocketConnection(AContext).Connection.Socket.ReadTimeout := FReadTimeout;
end;

procedure TWebsocketServer.DoDisconnect(AContext: TIdContext);
begin
  inherited;
end;

function TWebsocketServer.DoExecute(AContext: TIdContext): Boolean;
var
  StringData : String;
  Buffer : TBytes;
  idxBuffer : Integer;
  b : Byte;
  len : Integer;
  b_v : Byte;
const
  BufferInc = 1024;
begin
  Result := true;

  try
    b := AContext.Connection.Socket.ReadByte;
  except
    on E : EIdReadTimeout do
      exit;
  end;

  if (b shr 7) = 0 then
  begin

    case b of
      MBFrameStart:
      begin
        SetLength(Buffer, BufferInc);
        idxBuffer := -1;

        repeat
          Inc(idxBuffer);

          if idxBuffer > High(Buffer) then
            SetLength(Buffer, Length(Buffer) + BufferInc);

          Buffer[idxBuffer] := AContext.Connection.Socket.ReadByte;
        until (Buffer[idxBuffer] = MBFrameEnd);

        SetLength(Buffer, idxBuffer);

        StringData := TEncoding.UTF8.GetString(Buffer);

        TWebsocketConnection(AContext).DoProcessIncomingStringData(StringData);
      end;
    end;
  end
  else
  begin
    len := 0;
    repeat
      b := AContext.Connection.Socket.ReadByte;

      if b > $00 then
      begin
        b_v := b and $7F;
        len := len * 128 + b_v;

      end;
    until (b and $80) <> $80;

    AContext.Connection.Socket.ReadBytes(Buffer, len, false);
  end;
end;

function TWebsocketServer.GetClientClass: TWebsocketConnectionClass;
begin
  Result := TWebsocketConnectionClass(inherited ContextClass);
end;

procedure TWebsocketServer.SetClientClass(
  const Value: TWebsocketConnectionClass);
begin
  inherited ContextClass := Value;
end;

{ TWebsocketInitialRequest }

constructor TWebsocketInitialRequest.Create(AContext: TIdContext);
var
  line : String;
  RegEx : TRegExpr;
const
  Expr_Document = 'GET (/.*) HTTP/(.+)';
  Expr_Host = 'Host: (.*)';
  Expr_Key = 'Sec-WebSocket-Key(\d): (.+)';
  Expr_Prot = 'Sec-WebSocket-Protocol: (.+)';
  Expr_Origin = 'Origin: (.+)';
begin
  RegEx := TRegExpr.Create;
  try
    RegEx.ModifierM := false;
    RegEx.ModifierI := true;

    line := '-';
    while line <> '' do
    begin
      line := AContext.Connection.Socket.ReadLn(#13#10, TEncoding.UTF8);
      RegEx.InputString := line;

      RegEx.Expression := Expr_Document;
      if RegEx.Exec then
      begin
        FValues[0] := RegEx.Substitute('$1');
        FValues[1] := RegEx.Substitute('$2');
      end;

      RegEx.Expression := Expr_Host;
      if RegEx.Exec then
      begin
        FValues[2] := RegEx.Substitute('$1');
      end;

      RegEx.Expression := Expr_Key;
      if RegEx.Exec then
      begin
        case RegEx.Substitute('$1')[1] of
          '1': FValues[4] := RegEx.Substitute('$2');
          '2': FValues[5] := RegEx.Substitute('$2');
        end;
      end;

      RegEx.Expression := Expr_Prot;
      if RegEx.Exec then
      begin
        FValues[3] := RegEx.Substitute('$1');
      end;

      RegEx.Expression := Expr_Origin;
      if RegEx.Exec then
      begin
        FValues[6] := RegEx.Substitute('$1');
      end;
    end;
  finally
    RegEx.Free;
  end;

  AContext.Connection.Socket.ReadBytes(FChallengeData, 8, false);
end;

function TWebsocketInitialRequest.GetValue(const Index: Integer): String;
begin
  Result := FValues[Index];
end;

{ TWebsocketConnection }



constructor TWebsocketConnection.Create(AConnection: TIdTCPConnection; AYarn: TIdYarn; AList: TThreadList = nil);
begin
  inherited;

  FInitialRequest := TWebsocketInitialRequest.Create(Self);
  InitialAnswer;
end;

destructor TWebsocketConnection.Destroy;
begin
  FInitialRequest.Free;

  inherited;
end;


procedure TWebsocketConnection.DoProcessIncomingStringData(AData: String);
begin
end;

class function TWebsocketConnection.DecodeSecWebSocketKey(AKey : String) : Integer;
var
  KeyStr : String;
  KeySpaces : Int64;
  KeyNumber : Int64;
  c : Char;
begin
  KeyStr := '';
  KeySpaces := 0;

  for c in AKey do
  begin
    case c of
      ' ': Inc(KeySpaces);
      '0'..'9': KeyStr := KeyStr + c;
    end;
  end;
  KeyNumber := StrToInt64(KeyStr);
  Result := Integer(KeyNumber div KeySpaces);
end;

procedure TWebsocketConnection.InitialAnswer;
var
  b : TIdBytes;
  md5 : TIdHashMessageDigest5;
  Enc : TEncoding;
begin
  Enc := TEncoding.UTF8;

  b := ToBytes(IntToIntBigEndian(DecodeSecWebSocketKey(InitialRequest.WebsocketKey1)));
  AppendBytes(b, ToBytes(IntToIntBigEndian(DecodeSecWebSocketKey(InitialRequest.WebsocketKey2))));
  AppendBytes(b, InitialRequest.ChallengeData);

  md5 := TIdHashMessageDigest5.Create;
  try
    b := md5.HashBytes(b);
  finally
    md5.Free;
  end;

  with Connection.Socket do
  begin
    WriteLn('HTTP/1.1 101 WebSocket Protocol Handshake', Enc);
    WriteLn('Upgrade: WebSocket', Enc);
    WriteLn('Connection: Upgrade', Enc);
    WriteLn('Sec-WebSocket-Origin: ' + InitialRequest.Origin, Enc);
    WriteLn('Sec-WebSocket-Location: ws://' + InitialRequest.Host + InitialRequest.Document, Enc);
    if InitialRequest.WebsocketProtocol <> '' then
      WriteLn('Sec-WebSocket-Protocol: ' + InitialRequest.WebsocketProtocol, Enc);
    WriteLn(Enc);
    Write(b);
  end;
end;

class function TWebsocketConnection.IntToIntBigEndian(AInt: Integer): Integer;
var
  FromInt : array[0..3] of Byte absolute AInt;
  ToInt : array[0..3] of Byte absolute Result;
begin
  ToInt[0] := FromInt[3];
  ToInt[1] := FromInt[2];
  ToInt[2] := FromInt[1];
  ToInt[3] := FromInt[0];
end;

procedure TWebsocketConnection.SendData(AData: String);
var
  b : TIdBytes;
begin

  AppendBytes(b, ToBytes(Byte(MBFrameStart)));
  AppendBytes(b, ToBytes(AData, TEncoding.UTF8));
  AppendBytes(b, ToBytes(Byte(MBFrameEnd)));

  Connection.Socket.WriteBufferOpen;
  try
    Connection.Socket.WriteDirect(b);
  finally
    Connection.Socket.WriteBufferClose;
  end;
end;

end.
