unit uJSDirect;

interface

uses
  Classes,
  SysUtils,
  StrUtils,
  DateUtils,
  Windows,
  Variants,
  Math,
  IdUri,
  uWebsocketServer;

type
  TjsdApplication = class;

  TjsdContext = class(TWebsocketContext)
  protected
    FApplication : TjsdApplication;

    procedure DoProcessIncomingStringData(AData : String); override;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

  TjsdElement = class
  protected
    FApplication : TjsdApplication;
    F_GUID : String;
    F_JSVar : String;
  public
    constructor Create(AApplication : TjsdApplication); virtual;

    property _JSVar : String read F_JSVar;
    property _GUID : String read F_GUID;
  end;

  TjsdResponseType = (rtOk, rtError);
  TjsdResponseResult = (rrNotResponded, rrResponded, rrRespondedWithError);

  TjsdRespondingElement = class(TjsdElement)
  protected
    FResponseResult : TjsdResponseResult;
    FResponseError : String;

    procedure DoResponse(AData : String; AResponseType : TjsdResponseType); virtual;
  public const
    DefaultWaitForTimeout = 0;
  public
    Async : Boolean;

    constructor Create(Application : TjsdApplication); override;
    destructor Destroy(); override;

    function WaitFor(ATimeout : Integer = DefaultWaitForTimeout) : Boolean;
  end;

  TjsdCommand = class(TjsdElement)
  public
    constructor Create(AApplication : TjsdApplication; ACommand : String); reintroduce;
  end;

  TjsdRespondingCommand = class(TjsdRespondingElement)
  protected
    procedure DoResponse(AData : String; AResponseType : TjsdResponseType); override;
  public
    Result : String;

    constructor Create(AApplication : TjsdApplication; ACommand : String); reintroduce;
  end;


  TjsdFunctionHandlerParams = class(TStringList)
  private
    function GetValue(AName: String): String;
    procedure SetValue(AName: String; const Value: String);
  published
  public
    constructor Create(AData : String);

    property Values[AName : String] : String read GetValue write SetValue;
  end;

  TjsdFunctionHandlerProc = procedure(AParams : TjsdFunctionHandlerParams) of object;

  TjsdFunction = class(TjsdRespondingElement)
  protected
    FReturnParamName : String;
    FReturnParamValue : String;

    procedure DoResponse(AData : String; AResponseType : TjsdResponseType); override;
  public
    HandlerProc : TjsdFunctionHandlerProc;

    constructor Create(AAplication : TjsdApplication;
                       AResponseParams : String; //'RetVal1=arguments[0];RetVal2=MyVar'
                       AHandlerProc : TjsdFunctionHandlerProc = nil;
                       ABody : String = ''); reintroduce;

    destructor Destroy(); override;

    function WaitFor(AReturnParamName : String;
                     AReturnParamDefault : String = '';
                     ATimeout : Integer = TjsdRespondingElement.DefaultWaitForTimeout) : String;

  public const
    ParamDelimiter = '&';
  end;

  TjsdBaseObject = class(TjsdElement)
  protected
    procedure SetPropertyValue(AProperty : String; AValue : Variant; AValueIsJSCode: Boolean = False); virtual;

    function GetPropertyValue(AProperty : String) : String; overload;
    procedure GetPropertyValue(AProperty : String; out AValue : String; ADefault : String = ''; ACheckNullUndefined : Boolean = true); overload; virtual;
    procedure GetPropertyValue(AProperty : String; out AValue : WideString; ADefault : String = ''; ACheckNullUndefined : Boolean = true); overload; virtual;
    procedure GetPropertyValue(AProperty : String; out AValue : OleVariant; ADefault : String = ''; ACheckNullUndefined : Boolean = true); overload; virtual;
    procedure GetPropertyValue(AProperty : String; out AValue : Smallint; ADefault : Integer = 0); overload; virtual;
    procedure GetPropertyValue(AProperty : String; out AValue : Integer; ADefault : Integer = 0); overload; virtual;
    procedure GetPropertyValue(AProperty : String; out AValue : Int64; ADefault : Integer = 0); overload; virtual;
    procedure GetPropertyValue(AProperty : String; out AValue : Double; ADefault : Double = 0); overload; virtual;
    procedure GetPropertyValue(AProperty : String; out AValue : Single; ADefault : Double = 0); overload; virtual;
    procedure GetPropertyValue(AProperty : String; out AValue : Boolean; ADefault : Boolean = false); overload; virtual;
    procedure GetPropertyValue(AProperty : String; out AValue : WordBool; ADefault : Boolean = false); overload; virtual;

  public
    function ExecMethod(AMethod : String; ABlocking : Boolean = false) : String;
  end;

  TjsdObject = class(TjsdBaseObject)
  public
    constructor Create(AApplication : TjsdApplication); override;
    destructor Destroy(); override;
  end;

  TjsdObjectEx = class(TjsdBaseObject)
  protected
    FRefsExisting : Boolean;
  public
    constructor Create(AApplication : TjsdApplication;
                       ACreateCommand : String;
                       ARefsExisting : Boolean); reintroduce; virtual;
  end;


  TjsdAppEvent = class
  protected
    procedure Call(AApplication : TjsdApplication); virtual;
  end;

  TjsdaeDisconnected = class(TjsdAppEvent)
  protected
    procedure Call(AApplication : TjsdApplication); override;
  end;

  TjsdaeReconnected = class(TjsdAppEvent)
  protected
    procedure Call(AApplication : TjsdApplication); override;
  end;

  TjsdaeElementResponse = class(TjsdAppEvent)
  protected
    FElement : TjsdRespondingElement;
    FData : String;
    FResponseType : TjsdResponseType;

    procedure Call(AApplication : TjsdApplication); override;
  public
    constructor Create(AElement : TjsdRespondingElement; AData : String; AResponseType : TjsdResponseType);
  end;


  TjsdApplication = class(TThread)
  protected
    FContext : TjsdContext;
    FInactive : Boolean;
    FInactiveSince : TDateTime;

    FAppEvents : TList;
    FAppEventListLock : TRTLCriticalSection;

    FGUID : String;

    FElements : TStringList;
    FElementsLock : TRTLCriticalSection;

    procedure Execute(); override;
    procedure AddEvent(AEvent : TjsdAppEvent);
    procedure AddElement(AElement : TjsdRespondingElement);
    procedure DeleteElement(AElement : TjsdElement);

    procedure ProcessIncomingMessage(AMessage : String); virtual; //called from jsdContext

    procedure DoDisconnected; virtual;
    procedure DoReconnected; virtual;
    procedure DoError(AException : Exception); virtual;
    procedure DoCreated(); virtual;
    procedure DoTerminated(); virtual;
    function HasTimeout : Boolean; virtual;
  public
    constructor Create(AContext : TjsdContext); reintroduce; virtual;
    destructor Destroy(); override;

    function NeedConnection : Boolean; //blocks, until connection is established; returns true if connection can be used

    function Exec(ACommand : String; ABlocking : Boolean = false) : String;
    property GUID : String read FGUID;
  end;


  TjsdServer = class(TWebsocketServer)
  private
    property ContextClass;
  public
    procedure AfterConstruction; override;
  end;

  TjsdApplicationClass = class of TjsdApplication;

procedure RegisterJSDApplication(AName : String; AClass : TjsdApplicationClass);
procedure UnregisterJSDApplication(AName : String; AClass : TjsdApplicationClass);


implementation

uses uJSHelper;

type
  TjsdSweeper = class(TThread)
  protected
    procedure Execute(); override;
  public
    constructor Create();
  end;

var
  jsdApplications : TStringList;
  jsdAppInstances : TThreadList;
  jsdSweeper : TjsdSweeper;

procedure RegisterJSDApplication(AName : String; AClass : TjsdApplicationClass);
begin
  jsdApplications.AddObject(AName, TObject(AClass))
end;

procedure UnregisterJSDApplication(AName : String; AClass : TjsdApplicationClass);
var
  idx : Integer;
begin
  idx := jsdApplications.IndexOf(AName);
  if idx > -1 then
    jsdApplications.Delete(idx);
end;

{ TjsdServer }

procedure TjsdServer.AfterConstruction;
begin
  inherited;
  ContextClass := TjsdContext;
end;

{ TjsdContext }

procedure TjsdContext.AfterConstruction;
var
  idx : Integer;
  lst : TList;
  GUID : String;
  cls : TjsdApplicationClass;
begin
  inherited;

  FApplication := nil;

  GUID := InitialRequest.Param['GUID'];

  if GUID <> '' then
  begin
    lst := jsdAppInstances.LockList;
    try
      for idx := 0 to lst.Count - 1 do
      begin
        if TjsdApplication(lst[idx]).GUID = GUID then
        begin
          FApplication := TjsdApplication(lst[idx]);
          FApplication.FContext := Self;
          FApplication.FInactive := false;
          FApplication.AddEvent(TjsdaeReconnected.Create());
          break;
        end;
      end;
    finally
      jsdAppInstances.UnlockList;
    end;
  end;

  if not Assigned(FApplication) then  
  begin
    idx := jsdApplications.IndexOf(InitialRequest.Document);
    if idx = -1 then
      Connection.Disconnect(true)
    else
    begin
      cls := TjsdApplicationClass(jsdApplications.Objects[idx]);
      FApplication := cls.Create(Self);
    end;
  end;
end;

procedure TjsdContext.BeforeDestruction;
begin
  inherited;

  FApplication.FContext := nil;
  FApplication.FInactive := true;
  FApplication.FInactiveSince := Now;
  FApplication.AddEvent(TjsdaeDisconnected.Create);
end;

procedure TjsdContext.DoProcessIncomingStringData(AData: String);
begin
  FApplication.ProcessIncomingMessage(AData);
end;

{ TjsdApplication }

procedure TjsdApplication.AddElement(AElement: TjsdRespondingElement);
begin
  EnterCriticalSection(FElementsLock);
  try
    FElements.AddObject(AElement._GUID, AElement);
  finally
    LeaveCriticalSection(FElementsLock);
  end;
end;

procedure TjsdApplication.AddEvent(AEvent: TjsdAppEvent);
begin
  EnterCriticalSection(FAppEventListLock);
  try
    FAppEvents.Add(AEvent);
  finally
    LeaveCriticalSection(FAppEventListLock);
  end;
end;

constructor TjsdApplication.Create(AContext : TjsdContext);
var
  lGUID : TGUID;
begin
  inherited Create(true);

  FInactive := false;

  CreateGUID(lGUID);
  FGUID := GUIDToString(lGUID);

  FContext := AContext;

  FAppEvents := TList.Create;
  InitializeCriticalSection(FAppEventListLock);

  InitializeCriticalSection(FElementsLock);
  FElements := TStringList.Create;
  FElements.Duplicates := dupIgnore;
  FElements.Sorted := true;

  FreeOnTerminate := true;

  jsdAppInstances.Add(Self);

  FContext.SendData(FContext.InitialRequest.Param['vGUID'] + '="' + GUID + '";');

  Resume;
end;

procedure TjsdApplication.DeleteElement(AElement: TjsdElement);
var
  idx : Integer;
begin
  EnterCriticalSection(FElementsLock);
  try
    idx := FElements.IndexOf(AElement._GUID);
    if idx > -1 then
      FElements.Delete(idx);
  finally
    LeaveCriticalSection(FElementsLock);
  end;
end;

destructor TjsdApplication.Destroy;
begin
  jsdAppInstances.Remove(Self);
  DeleteCriticalSection(FAppEventListLock);
  FAppEvents.Free;

  DeleteCriticalSection(FElementsLock);
  FElements.Free;

  inherited;
end;

procedure TjsdApplication.DoCreated;
begin
end;

procedure TjsdApplication.DoDisconnected;
begin
end;

procedure TjsdApplication.DoError(AException: Exception);
begin
end;

procedure TjsdApplication.DoReconnected;
begin
end;

procedure TjsdApplication.DoTerminated;
begin
end;

function TjsdApplication.Exec(ACommand: String; ABlocking : Boolean) : String;
var
  rc : TjsdRespondingCommand;
begin
  if ABlocking then
  begin
    rc := TjsdRespondingCommand.Create(Self, ACommand);
    try
      rc.WaitFor;
      Result := rc.Result;
    finally
      rc.Free;
    end;
  end
  else
    TjsdCommand.Create(Self, ACommand).Free;
end;

procedure TjsdApplication.Execute;
var
  NextSleep : Cardinal;
  Eve : TjsdAppEvent;
begin
  DoCreated;

  while not Terminated do
  begin
    try

      if FAppEvents.Count > 0 then
      begin
        NextSleep := 0;

        EnterCriticalSection(FAppEventListLock);
        try
          Eve := TjsdAppEvent(FAppEvents[0]);
          FAppEvents.Delete(0);
        finally
          LeaveCriticalSection(FAppEventListLock);
        end;
      end
      else
      begin
        NextSleep := 20;
        Eve := nil;
      end;

      if Assigned(Eve) then
      begin
        try
          Eve.Call(Self);
        finally
          Eve.Free;
        end;
      end;

      if NextSleep > 0 then
        Sleep(NextSleep);

    except
      on E : Exception do
        DoError(E);
    end;
  end;

  DoTerminated;
end;


function TjsdApplication.HasTimeout: Boolean;
begin
  Result := SecondsBetween(FInactiveSince, Now) > 300;
end;

function TjsdApplication.NeedConnection : Boolean;
begin
  while (not Terminated) and (not Assigned(FContext)) do
    Sleep(100);

  Result := (not Terminated) and Assigned(FContext);
end;

procedure TjsdApplication.ProcessIncomingMessage(AMessage: String);
var
  idx : Integer;
  e : TjsdRespondingElement;
  guid : String;
  ResponseTypeChar : Char;
  ResponseType : TjsdResponseType;
begin
  if (Length(AMessage) >= 39) and
     (CharInSet(AMessage[1], ['O', 'E'])) and
     (AMessage[2] = '{') and
     (AMessage[39] = '}') then
  begin
    ResponseTypeChar := AMessage[1];
    guid := Copy(AMessage, 2, 38);
    Delete(AMessage, 1, 39);

    e := nil;

    case ResponseTypeChar of
      'O': ResponseType := rtOk;
      'E' : ResponseType := rtError
      else
        ResponseType := rtError;
    end;

    EnterCriticalSection(FElementsLock);
    try
      idx := FElements.IndexOf(guid);
      if idx > -1 then
      begin
        e := TjsdRespondingElement(FElements.Objects[idx]);
      end;
    finally
      LeaveCriticalSection(FElementsLock);
    end;

    if Assigned(e) then
    begin
      if e.Async then
        e.DoResponse(AMessage, ResponseType)
      else
        AddEvent(TjsdaeElementResponse.Create(e, AMessage, ResponseType));
    end;

  end;
end;

{ TjsdaeReconnected }

procedure TjsdaeReconnected.Call(AApplication: TjsdApplication);
begin
  AApplication.DoReconnected;
end;

{ TjsdaeDisconnected }

procedure TjsdaeDisconnected.Call(AApplication: TjsdApplication);
begin
  AApplication.DoDisconnected;
end;


{ TjsdAppEvent }

procedure TjsdAppEvent.Call(AApplication: TjsdApplication);
begin
end;

{ TjsdSweeper }

constructor TjsdSweeper.Create;
begin
  inherited Create(true);

  FreeOnTerminate := true;
  Resume;
end;

procedure TjsdSweeper.Execute;
var
  lst : TList;
  p : Pointer;
  appinstance : TjsdApplication absolute p;
begin
  while not Terminated do
  begin
    lst := jsdAppInstances.LockList;
    try
      for p in lst do
      begin
        if appinstance.FInactive and appinstance.HasTimeout then
          appinstance.Terminate;
      end;
    finally
      jsdAppInstances.UnlockList;
    end;

    Sleep(1000);
  end;
end;

{ TjsdElement }

constructor TjsdElement.Create(AApplication : TjsdApplication);
var
  lGUID : TGUID;
begin
  FApplication := AApplication;

  CreateGUID(lGUID);
  F_GUID := GUIDToString(lGUID);

  F_JSVar := '_' + StringReplace(Copy(_GUID, 2, 36), '-', '', [rfReplaceAll]);
end;

{ TjsdaeElementResponse }

procedure TjsdaeElementResponse.Call(AApplication: TjsdApplication);
begin
  FElement.DoResponse(FData, FResponseType);
end;

constructor TjsdaeElementResponse.Create(AElement: TjsdRespondingElement;
  AData: String; AResponseType : TjsdResponseType);
begin
  FElement := AElement;
  FData := AData;
  FResponseType := AResponseType;
end;

{ TjsdCommand }

constructor TjsdCommand.Create(AApplication: TjsdApplication; ACommand: String);
begin
  inherited Create(AApplication);

  if FApplication.NeedConnection then
    FApplication.FContext.SendData(ACommand);
end;

{ TjsdFunction }

constructor TjsdFunction.Create(AAplication : TjsdApplication;
                               AResponseParams : String;
                               AHandlerProc : TjsdFunctionHandlerProc = nil;
                               ABody : String = '');
var
  sl : TStringList;
  idx : Integer;
  cmd : String;
begin
  inherited Create(AAplication);

  cmd := _JSVar + ' = function()';
  cmd := cmd + '{' + ABody + ' ';

  cmd := cmd + FApplication.FContext.InitialRequest.Param['cResponse'] + '("O' + _GUID + '"';

  sl := TStringList.Create;
  try
    sl.Delimiter := ';';
    sl.StrictDelimiter := true;
    sl.DelimitedText := AResponseParams;

    for idx := 0 to sl.Count - 1 do
    begin
      if idx > 0 then
        cmd := cmd + '+"&"';
      cmd := cmd + '+"' + sl.Names[idx] + '="+encodeURIComponent(' + sl.ValueFromIndex[idx] + ')';
    end;
  finally
    sl.Free;
  end;

  cmd := cmd + ');}';

  FApplication.Exec(cmd);

  HandlerProc := AHandlerProc;
end;

destructor TjsdFunction.Destroy;
begin
  FApplication.Exec(_JSVar + '=null');

  inherited;
end;

procedure TjsdFunction.DoResponse(AData: String; AResponseType : TjsdResponseType);
var
  Params : TjsdFunctionHandlerParams;
begin
  Params := TjsdFunctionHandlerParams.Create(AData);
  try
    if (FReturnParamName <> EmptyStr) then
      FReturnParamValue := Params.Values[FReturnParamName];

    if Assigned(HandlerProc) then
      HandlerProc(Params);
  finally
    Params.Free;
  end;

  inherited;
end;

function TjsdFunction.WaitFor(AReturnParamName, AReturnParamDefault: String;
  ATimeout: Integer): String;
begin
  Result := AReturnParamDefault;

  FReturnParamValue := EmptyStr;
  try
    FReturnParamName := AReturnParamName;
    FReturnParamValue := EmptyStr;

    if (inherited WaitFor(ATimeout)) and
       (FReturnParamValue <> EmptyStr) then
      Result := FReturnParamValue;
  finally
    FReturnParamName := EmptyStr;
    FReturnParamValue := EmptyStr;
  end;
end;

{ TjsdFunctionHandlerParams }

constructor TjsdFunctionHandlerParams.Create(AData: String);
begin
  Delimiter := TjsdFunction.ParamDelimiter;
  StrictDelimiter := true;
  DelimitedText := AData;
end;

function TjsdFunctionHandlerParams.GetValue(AName: String): String;
begin
  Result := TIdURI.URLDecode(inherited Values[AName]);
end;

procedure TjsdFunctionHandlerParams.SetValue(AName: String; const Value: String);
begin
  inherited Values[AName] := TIdURI.ParamsEncode(Value);
end;

{ TjsdRespondingElement }

constructor TjsdRespondingElement.Create(Application: TjsdApplication);
begin
  inherited;

  FApplication.AddElement(Self);
end;

destructor TjsdRespondingElement.Destroy;
begin
  FApplication.DeleteElement(Self);
  inherited;
end;

procedure TjsdRespondingElement.DoResponse(AData: String; AResponseType : TjsdResponseType);
begin
  if AResponseType = rtError then
  begin
    FResponseError := AData;
    FResponseResult := rrRespondedWithError;
  end
  else
    FResponseResult := rrResponded;
end;

function TjsdRespondingElement.WaitFor(ATimeout: Integer) : Boolean;
var
  lOldAsync : Boolean;
  tm : Integer;
const
  SleepTime = 10;
begin
  lOldAsync := Async;
  Async := true;
  tm := ATimeout;
  FResponseResult := rrNotResponded;

  while (not FApplication.Terminated) and (FResponseResult = rrNotResponded) do
  begin
    if ATimeout > DefaultWaitForTimeout then
    begin
      Dec(tm, SleepTime);
      if  tm <= 0 then
        break;
    end;
    Sleep(SleepTime);
  end;

  Async := lOldAsync;

  case FResponseResult of
    rrNotResponded: Result := false;
    rrResponded: Result := true;
    rrRespondedWithError: raise Exception.Create(FResponseError);
    else
      Result := false;
  end;

end;

{ TjsdBaseObject }

procedure TjsdBaseObject.GetPropertyValue(AProperty: String; out AValue: Integer;
  ADefault: Integer);
begin
  AValue := StrToIntDef(GetPropertyValue(AProperty), ADefault);
end;

procedure TjsdBaseObject.GetPropertyValue(AProperty: String; out AValue: String;
  ADefault: String; ACheckNullUndefined : Boolean);
var
  s : String;
begin
  s := GetPropertyValue(AProperty);
  AValue := IfThen(ACheckNullUndefined and ((s = 'null') or (s = 'undefined')), ADefault, s);
end;

function TjsdBaseObject.GetPropertyValue(AProperty: String): String;
begin
  Result := FApplication.Exec(_JSVar + '.' + AProperty, true);
end;

function TjsdBaseObject.ExecMethod(AMethod: String; ABlocking: Boolean): String;
begin
  if Copy(AMethod, Length(AMethod), 1) <> ')' then
    AMethod := AMethod + '()';

  Result := FApplication.Exec(_JSVar + '.' + AMethod, ABlocking);
end;

procedure TjsdBaseObject.GetPropertyValue(AProperty: String; out AValue: Boolean;
  ADefault: Boolean);
begin
  AValue := StrToBoolDef(GetPropertyValue(AProperty), ADefault);
end;

procedure TjsdBaseObject.GetPropertyValue(AProperty: String; out AValue: WideString;
  ADefault: String; ACheckNullUndefined: Boolean);
var
  s : String;
begin
  GetPropertyValue(AProperty, s, ADefault, ACheckNullUndefined);
  AValue:=s;
end;

procedure TjsdBaseObject.GetPropertyValue(AProperty: String; out AValue: Double;
  ADefault: Double);
begin
  AValue := StrToFloatDef(GetPropertyValue(AProperty), ADefault);
end;

procedure TjsdBaseObject.GetPropertyValue(AProperty: String; out AValue: Int64;
  ADefault: Integer);
begin
  AValue := StrToInt64Def(GetPropertyValue(AProperty), ADefault);
end;

procedure TjsdBaseObject.SetPropertyValue(AProperty : String; AValue : Variant; AValueIsJSCode: Boolean);
var
  Value: String;
begin
  if AValueIsJSCode then
    Value:=AValue
  else
    Value:=ToJSCode(AValue);

  FApplication.Exec(_JSVar + '.' + AProperty + '=' + Value);
end;

procedure TjsdBaseObject.GetPropertyValue(AProperty: String; out AValue: WordBool;
  ADefault: Boolean);
var
  b : Boolean;
begin
  GetPropertyValue(AProperty, b, ADefault);
  AValue:=b;
end;

procedure TjsdBaseObject.GetPropertyValue(AProperty: String; out AValue: Single;
  ADefault: Double);
var
  db : Double;
begin
  GetPropertyValue(AProperty, db, ADefault);
  AValue:=db;
end;

procedure TjsdBaseObject.GetPropertyValue(AProperty: String;
  out AValue: Smallint; ADefault: Integer);
var
  i : INteger;
begin
  GetPropertyValue(AProperty, i, ADefault);
  AValue:=i;
end;

procedure TjsdBaseObject.GetPropertyValue(AProperty: String; out AValue: OleVariant;
  ADefault: String; ACheckNullUndefined: Boolean);
var
  s : String;
begin
  GetPropertyValue(AProperty, s, ADefault, ACheckNullUndefined);
  AValue:=s;
end;

{ TjsdRespondingCommand }

constructor TjsdRespondingCommand.Create(AApplication: TjsdApplication;
  ACommand: String);
begin
  inherited Create(AApplication);

  if FApplication.NeedConnection then
    FApplication.FContext.SendData(_GUID + ACommand);
end;

procedure TjsdRespondingCommand.DoResponse(AData: String; AResponseType : TjsdResponseType);
begin
  Result := AData;
  inherited;
end;

{ TjsExistingObject }

constructor TjsdObjectEx.Create(AApplication : TjsdApplication;
                       ACreateCommand : String;
                       ARefsExisting : Boolean);
begin
  inherited Create(AApplication);

  FRefsExisting := ARefsExisting;

  FApplication.Exec(_JSVar + '=' + ACreateCommand, true);
end;

{ TjsdObject }

constructor TjsdObject.Create(AApplication: TjsdApplication);
begin
  inherited;

  FApplication.Exec(_JSVar + '=new Object', true);
end;

destructor TjsdObject.Destroy;
begin
  FApplication.Exec(_JSVar + '=null');

  inherited;
end;

initialization
  jsdApplications := TStringList.Create;
  jsdApplications.Duplicates := dupIgnore;
  jsdApplications.Sorted := true;

  jsdAppInstances := TThreadList.Create;

  jsdSweeper := TjsdSweeper.Create;

finalization
  jsdSweeper.Terminate;

  jsdApplications.Free;
  jsdAppInstances.Free;

end.
