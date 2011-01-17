unit uWebsocketServer;

interface

uses
  Classes,
  Controls,
  SysUtils,
  StrUtils,
  IdGlobal,
  IdContext,
  IdBaseComponent,
  IdComponent,
  IdCustomTCPServer,
  IdHashMessageDigest,
  idHash,
  RegExpr;

const
  FrameStart = $00;
  FrameEnd = $FF;
  FrameSizeStart = $80;

type
  TCustomWebsocketServer = class;

  TWebsocketInitialRequest = class
  protected
    FValues : array[0..6] of String;
    FChallengeData : TBytes;

    function GetValue(const Index: Integer): String;
  public
    constructor Create(AContext : TIdContext;
                       AServer : TCustomWebsocketServer); virtual;

    property Document : String index 0 read GetValue;
    property HTTPVersion : String index 1 read GetValue;
    property Host : String index 2 read GetValue;
    property WebsocketProtocol : String index 3 read GetValue;
    property WebsocketKey1 : String index 4 read GetValue;
    property WebsocketKey2 : String index 5 read GetValue;
    property Origin : String index 6 read GetValue;

    property ChallengeData : TBytes read FChallengeData;
  end;

  TCustomWebsocketClient = class
  protected
    FInitialRequest : TWebsocketInitialRequest;
    FContext : TIdContext;

    procedure InitialAnswer; virtual;
  public
    constructor Create(AContext : TIdContext;
                       AInitialRequest : TWebsocketInitialRequest); virtual;
    destructor Destroy(); override;

    procedure SendData(AData : String); overload;

    class function DecodeSecWebSocketKey(AKey: String): Integer;
    class function IntToIntBigEndian(AInt : Integer) : Integer;

    property InitialRequest : TWebsocketInitialRequest read FInitialRequest;
  end;

  TWebsocketClientEvent = procedure(AClient : TCustomWebsocketClient) of Object;
  TWebsocketStringDataReceivedEvent = procedure(AClient : TCustomWebsocketClient; AData : String) of Object;

  TCustomWebsocketServer = class(TIdCustomTCPServer)
  protected const
    DefaultReadTimeout = 1000;


  protected var
    FReadTimeout: Integer;

    FOnClientConnected: TWebsocketClientEvent;
    FOnStringDataReceived: TWebsocketStringDataReceivedEvent;
    FOnClientDisconnected: TWebsocketClientEvent;

    procedure OnTCPConnect(AContext : TIdContext); virtual;
    procedure OnTCPDisconnect(AContext : TIdContext); virtual;
    procedure OnTCPExecute(AContext : TIdContext); virtual;
  public
    constructor Create(AOwner: TComponent);

    property ReadTimeout : Integer read FReadTimeout write FReadTimeout;

    property OnClientConnected : TWebsocketClientEvent read FOnClientConnected write FOnClientConnected;
    property OnClientDisconnected : TWebsocketClientEvent read FOnClientDisconnected write FOnClientDisconnected;
    property OnStringDataReceived : TWebsocketStringDataReceivedEvent read FOnStringDataReceived write FOnStringDataReceived;
  end;



implementation

{ TCustomWebsocketServer }

constructor TCustomWebsocketServer.Create(AOwner: TComponent);
begin
  inherited;

  FReadTimeout := DefaultReadTimeout;

  OnConnect := OnTCPConnect;
  OnExecute := OnTCPExecute;
  OnDisconnect := OnTCPDisconnect;
end;


procedure TCustomWebsocketServer.OnTCPConnect(AContext: TIdContext);
var
  client : TCustomWebsocketClient;
begin
  client := TCustomWebsocketClient.Create(AContext, TWebsocketInitialRequest.Create(AContext, Self));

  if Assigned(FOnClientConnected) then
    FOnClientConnected(client);
end;

procedure TCustomWebsocketServer.OnTCPDisconnect(AContext: TIdContext);
begin
  if Assigned(FOnClientDisconnected) then
    FOnClientDisconnected(TCustomWebsocketClient(AContext.Data));
end;

procedure TCustomWebsocketServer.OnTCPExecute(AContext: TIdContext);
var
  StringData : String;
  Buffer : TBytes;
  idxBuffer : Integer;
const
  BufferInc = 1024;
begin
  SetLength(Buffer, BufferInc);
  idxBuffer := -1;

  case AContext.Connection.Socket.ReadByte of
    FrameStart:
    begin
      repeat
        Inc(idxBuffer);

        if idxBuffer > High(Buffer) then
          SetLength(Buffer, Length(Buffer) + BufferInc);

        Buffer[idxBuffer] := AContext.Connection.Socket.ReadByte;
      until Buffer[idxBuffer] = FrameEnd;

      SetLength(Buffer, idxBuffer);

      StringData := TEncoding.UTF8.GetString(Buffer);

      if Assigned(FOnStringDataReceived) then
        FOnStringDataReceived(TCustomWebsocketClient(AContext.Data), StringData);

      TCustomWebsocketClient(AContext.Data).SendData('I''ve recüved säh vollowing: ' + StringData);
    end;
  end;

end;

{ TWebsocketInitialRequest }

constructor TWebsocketInitialRequest.Create(AContext: TIdContext;
                                            AServer : TCustomWebsocketServer);
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
      line := AContext.Connection.Socket.ReadLn(#13#10, TEncoding.ASCII);
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

{ TCustomWebsocketClient }

constructor TCustomWebsocketClient.Create(AContext : TIdContext;
                                          AInitialRequest : TWebsocketInitialRequest);
begin
  FInitialRequest := AInitialRequest;
  FContext := AContext;

  InitialAnswer;

  AContext.Data := Self;
end;

destructor TCustomWebsocketClient.Destroy;
begin
  FInitialRequest.Free;

  inherited;
end;

class function TCustomWebsocketClient.DecodeSecWebSocketKey(AKey : String) : Integer;
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

procedure TCustomWebsocketClient.InitialAnswer;
var
  b : TIdBytes;
  md5 : TIdHashMessageDigest5;
begin
  b := ToBytes(IntToIntBigEndian(DecodeSecWebSocketKey(InitialRequest.WebsocketKey1)));
  AppendBytes(b, ToBytes(IntToIntBigEndian(DecodeSecWebSocketKey(InitialRequest.WebsocketKey2))));
  AppendBytes(b, InitialRequest.ChallengeData);

  md5 := TIdHashMessageDigest5.Create;
  try
    b := md5.HashBytes(b);
  finally
    md5.Free;
  end;

  with FContext.Connection.Socket do
  begin
    WriteLn('HTTP/1.1 101 WebSocket Protocol Handshake', TEncoding.ASCII);
    WriteLn('Upgrade: WebSocket', TEncoding.ASCII);
    WriteLn('Connection: Upgrade', TEncoding.ASCII);
    WriteLn('Sec-WebSocket-Origin: ' + InitialRequest.Origin, TEncoding.ASCII);
    WriteLn('Sec-WebSocket-Location: ws://' + InitialRequest.Host + InitialRequest.Document, TEncoding.ASCII);
    if InitialRequest.WebsocketProtocol <> '' then
      WriteLn('Sec-WebSocket-Protocol: ' + InitialRequest.WebsocketProtocol, TEncoding.ASCII);
    WriteLn(TEncoding.ASCII);
    Write(b);
  end;
end;

class function TCustomWebsocketClient.IntToIntBigEndian(AInt: Integer): Integer;
var
  FromInt : array[0..3] of Byte absolute AInt;
  ToInt : array[0..3] of Byte absolute Result;
begin
  ToInt[0] := FromInt[3];
  ToInt[1] := FromInt[2];
  ToInt[2] := FromInt[1];
  ToInt[3] := FromInt[0];
end;

procedure TCustomWebsocketClient.SendData(AData: String);
var
  b : TIdBytes;
begin

  AppendBytes(b, ToBytes(Byte(FrameStart)));
  AppendBytes(b, ToBytes(AData, TEncoding.UTF8));
  AppendBytes(b, ToBytes(Byte(FrameEnd)));

  FContext.Connection.Socket.WriteBufferOpen;
  try
    FContext.Connection.Socket.WriteDirect(b);
  finally
    FContext.Connection.Socket.WriteBufferClose;
  end;
end;

end.
