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

const
  jsElementNamePlaceHolder = '%Name%';
  jsElementGUIDPlaceHolder = '%GUID%';

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

  TjsElement = class
  protected
    FApplication : TjsdApplication;
    FInitialJSCommand : String;
    FFinalJSCommand : String;

    function ExpandPlaceholders(AString : String) : String;

    procedure DoResponse(AData : String); virtual;
  public
    _GUID : String;
    _Name : String;

    constructor Create(AApplication : TjsdApplication); virtual;

    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

  TjsRespondingElement = class(TjsElement)
  protected
    FResponded : Boolean;

    procedure DoResponse(AData : String); override;
  public const
    DefaultWaitForTimeout = 0;
  public
    Async : Boolean;

    constructor Create(Application : TjsdApplication); override;
    destructor Destroy(); override;

    function WaitFor(ATimeout : Integer = DefaultWaitForTimeout) : Boolean;
  end;

  TjsCommand = class(TjsElement)
  public
    constructor Create(AApplication : TjsdApplication; ACommand : String); reintroduce;
  end;

  TjsRespondingCommand = class(TjsRespondingElement)
  protected
    procedure DoResponse(AData : String); override;
  public
    Result : String;
    constructor Create(AApplication : TjsdApplication; ACommand : String); reintroduce;
  end;


  TjsFunctionHandlerParams = class(TStringList)
  private
    function GetValue(AName: String): String;
    procedure SetValue(AName: String; const Value: String);
  published
  public
    constructor Create(AData : String);

    property Values[AName : String] : String read GetValue write SetValue;
  end;

  TjsFunctionHandlerProc = procedure(AParams : TjsFunctionHandlerParams) of object;

  TjsFunction = class(TjsRespondingElement)
  protected
    FReturnParamName : String;
    FReturnParamValue : String;

    procedure DoResponse(AData : String); override;
  public
    HandlerProc : TjsFunctionHandlerProc;

    constructor Create(AAplication : TjsdApplication;
                       AParams : String; //'P1,P2,P3'
                       AResponseParams : String; //'RetVal1=P1;RetVal2=MyVar'
                       ABody : String = ''); reintroduce;

    function WaitFor(AReturnParamName : String;
                     AReturnParamDefault : String = '';
                     ATimeout : Integer = TjsRespondingElement.DefaultWaitForTimeout) : String;

  public const
    ParamDelimiter = '&';
  end;

  TjsObject = class(TjsElement)
  protected
    procedure SetPropertyValue(AProperty : String; AValue : Variant); virtual;

    function GetPropertyValue(AProperty : String) : String; overload;
    procedure GetPropertyValue(AProperty : String; out AValue : String; ADefault : String = ''; ACheckNullUndefined : Boolean = true); overload; virtual;
    procedure GetPropertyValue(AProperty : String; out AValue : Integer; ADefault : Integer = 0); overload; virtual;
    procedure GetPropertyValue(AProperty : String; out AValue : Int64; ADefault : Integer = 0); overload; virtual;
    procedure GetPropertyValue(AProperty : String; out AValue : Double; ADefault : Double = 0); overload; virtual;
    procedure GetPropertyValue(AProperty : String; out AValue : Boolean; ADefault : Boolean = false); overload; virtual;
  public
    constructor Create(AApplication : TjsdApplication); override;
  end;

  TjsObjectEx = class(TjsObject)
  public
    constructor Create(AApplication : TjsdApplication; ACreateCommand : String); reintroduce; virtual;
  end;


  TjsdAppEvent = class
  protected
    procedure Call(AApplication : TjsdApplication); virtual;
  end;

  TaeDisconnected = class(TjsdAppEvent)
  protected
    procedure Call(AApplication : TjsdApplication); override;
  end;

  TaeReconnected = class(TjsdAppEvent)
  protected
    procedure Call(AApplication : TjsdApplication); override;
  end;

  TaeElementResponse = class(TjsdAppEvent)
  protected
    FElement : TjsElement;
    FData : String;

    procedure Call(AApplication : TjsdApplication); override;
  public
    constructor Create(AElement : TjsElement; AData : String);
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
    procedure AddElement(AElement : TjsRespondingElement);
    procedure DeleteElement(AElement : TjsElement);

    procedure NeedConnection; //blocks, until connection is established

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

    procedure ExecJSCommand(ACommand : String);
    function ExecBlockingCommand(ACommand : String) : String;
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
          FApplication.AddEvent(TaeReconnected.Create());
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
  FApplication.AddEvent(TaeDisconnected.Create);
end;

procedure TjsdContext.DoProcessIncomingStringData(AData: String);
begin
  FApplication.ProcessIncomingMessage(AData);
end;

{ TjsdApplication }

procedure TjsdApplication.AddElement(AElement: TjsRespondingElement);
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

procedure TjsdApplication.DeleteElement(AElement: TjsElement);
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

function TjsdApplication.ExecBlockingCommand(ACommand: String) : String;
var
  rc : TjsRespondingCommand;
begin
  rc := TjsRespondingCommand.Create(Self, ACommand);
  try
    rc.WaitFor;
    Result := rc.Result;
  finally
    rc.Free;
  end;
end;

procedure TjsdApplication.ExecJSCommand(ACommand: String);
begin
  TjsCommand.Create(Self, ACommand).Free;
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

procedure TjsdApplication.NeedConnection;
begin
  while (not Terminated) and (not Assigned(FContext)) do
    Sleep(100);
end;

procedure TjsdApplication.ProcessIncomingMessage(AMessage: String);
var
  idx : Integer;
  e : TjsRespondingElement;
  guid : String;
begin
  if (Length(AMessage) >= 38) and
     (AMessage[1] = '{') and
     (AMessage[38] = '}') then
  begin
    guid := Copy(AMessage, 1, 38);
    Delete(AMessage, 1, 38);

    e := nil;

    EnterCriticalSection(FElementsLock);
    try
      idx := FElements.IndexOf(guid);
      if idx > -1 then
      begin
        e := TjsRespondingElement(FElements.Objects[idx]);
      end;
    finally
      LeaveCriticalSection(FElementsLock);
    end;

    if Assigned(e) then
    begin
      if e.Async then
        e.DoResponse(AMessage)
      else
        AddEvent(TaeElementResponse.Create(e, AMessage));
    end;

  end;
end;

{ TaeReconnected }

procedure TaeReconnected.Call(AApplication: TjsdApplication);
begin
  AApplication.DoReconnected;
end;

{ TaeDisconnected }

procedure TaeDisconnected.Call(AApplication: TjsdApplication);
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

{ TjsElement }

procedure TjsElement.AfterConstruction;
begin
  inherited;

  if FInitialJSCommand <> EmptyStr then
  begin
    FApplication.FContext.SendData(ExpandPlaceholders(FInitialJSCommand));
  end;
end;

procedure TjsElement.BeforeDestruction;
begin
  if Assigned(FApplication.FContext) and
    (FFinalJSCommand <> EmptyStr) then
  begin
    FApplication.FContext.SendData(ExpandPlaceholders(FFinalJSCommand));
  end;

  inherited;
end;

constructor TjsElement.Create(AApplication : TjsdApplication);
var
  lGUID : TGUID;
begin
  FApplication := AApplication;

  CreateGUID(lGUID);
  _GUID := GUIDToString(lGUID);

  _Name := '_' + StringReplace(Copy(_GUID, 2, 36), '-', '', [rfReplaceAll]);
end;

procedure TjsElement.DoResponse(AData: String);
begin

end;

function TjsElement.ExpandPlaceholders(AString: String): String;
begin
  Result := StringReplace(AString, jsElementNamePlaceHolder, _Name, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, jsElementGUIDPlaceHolder, _GUID, [rfReplaceAll, rfIgnoreCase]);
end;

{ TaeElementResponse }

procedure TaeElementResponse.Call(AApplication: TjsdApplication);
begin
  FElement.DoResponse(FData);
end;

constructor TaeElementResponse.Create(AElement: TjsElement;
  AData: String);
begin
  FElement := AElement;
  FData := AData;
end;

{ TjsCommand }

constructor TjsCommand.Create(AApplication: TjsdApplication; ACommand: String);
begin
  inherited Create(AApplication);

  AApplication.NeedConnection;
  AApplication.FContext.SendData(ACommand);
end;

{ TjsFunction }

constructor TjsFunction.Create(AAplication : TjsdApplication;
                               AParams : String;
                               AResponseParams : String;
                               ABody : String = '');
var
  sl : TStringList;
  idx : Integer;
begin
  inherited Create(AAplication);

  FInitialJSCommand := _Name + ' = function(' + AParams + ')';
  FInitialJSCommand := FInitialJSCommand + '{' + ABody;

  FInitialJSCommand := FInitialJSCommand + FApplication.FContext.InitialRequest.Param['cResponse'] + '("' + _GUID + '"';

  sl := TStringList.Create;
  try
    sl.Delimiter := ';';
    sl.StrictDelimiter := true;
    sl.DelimitedText := AResponseParams;

    for idx := 0 to sl.Count - 1 do
    begin
      if idx > 0 then
        FInitialJSCommand := FInitialJSCommand + '+"&"';
      FInitialJSCommand := FInitialJSCommand + '+"' + sl.Names[idx] + '="+encodeURIComponent(' + sl.ValueFromIndex[idx] + ')';
    end;
  finally
    sl.Free;
  end;

  FInitialJSCommand := FInitialJSCommand + ');}';

  FFinalJSCommand := _Name + '=null';

  HandlerProc := nil;
end;

procedure TjsFunction.DoResponse(AData: String);
var
  Params : TjsFunctionHandlerParams;
begin
  Params := TjsFunctionHandlerParams.Create(AData);
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

function TjsFunction.WaitFor(AReturnParamName, AReturnParamDefault: String;
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

{ TjsFunctionHandlerParams }

constructor TjsFunctionHandlerParams.Create(AData: String);
begin
  Delimiter := TjsFunction.ParamDelimiter;
  StrictDelimiter := true;
  DelimitedText := AData;
end;

function TjsFunctionHandlerParams.GetValue(AName: String): String;
begin
  Result := TIdURI.URLDecode(inherited Values[AName]);
end;

procedure TjsFunctionHandlerParams.SetValue(AName: String; const Value: String);
begin
  inherited Values[AName] := TIdURI.ParamsEncode(Value);
end;

{ TjsRespondingElement }

constructor TjsRespondingElement.Create(Application: TjsdApplication);
begin
  inherited;

  FApplication.AddElement(Self);
end;

destructor TjsRespondingElement.Destroy;
begin
  FApplication.DeleteElement(Self);
  inherited;
end;

procedure TjsRespondingElement.DoResponse(AData: String);
begin
  inherited;

  FResponded := true;
end;

function TjsRespondingElement.WaitFor(ATimeout: Integer) : Boolean;
var
  lOldAsync : Boolean;
  tm : Integer;
const
  SleepTime = 10;
begin
  lOldAsync := Async;
  Async := true;
  tm := ATimeout;
  FResponded := false;

  while (not FApplication.Terminated) and (not FResponded) do
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
  Result := FResponded;
end;

{ TjsObject }

constructor TjsObject.Create(AApplication: TjsdApplication);
begin
  inherited;

  FInitialJSCommand := _Name + '=new Object';
  FFinalJSCommand := _Name + '=null';
end;

procedure TjsObject.GetPropertyValue(AProperty: String; out AValue: Integer;
  ADefault: Integer);
begin
  AValue := StrToIntDef(GetPropertyValue(AProperty), ADefault);
end;

procedure TjsObject.GetPropertyValue(AProperty: String; out AValue: String;
  ADefault: String; ACheckNullUndefined : Boolean);
var
  s : String;
begin
  s := GetPropertyValue(AProperty);
  AValue := IfThen(ACheckNullUndefined and ((s = 'null') or (s = 'undefined')), ADefault, s);
end;

function TjsObject.GetPropertyValue(AProperty: String): String;
begin
  Result := FApplication.ExecBlockingCommand(_Name + '.' + AProperty);
end;

procedure TjsObject.GetPropertyValue(AProperty: String; out AValue: Boolean;
  ADefault: Boolean);
begin
  AValue := StrToBoolDef(GetPropertyValue(AProperty), ADefault);
end;

procedure TjsObject.GetPropertyValue(AProperty: String; out AValue: Double;
  ADefault: Double);
begin
  AValue := StrToFloatDef(GetPropertyValue(AProperty), ADefault);
end;

procedure TjsObject.GetPropertyValue(AProperty: String; out AValue: Int64;
  ADefault: Integer);
begin
  AValue := StrToInt64Def(GetPropertyValue(AProperty), ADefault);
end;

procedure TjsObject.SetPropertyValue(AProperty : String; AValue : Variant);
begin
  FApplication.ExecJSCommand(_Name + '.' + AProperty + '=' + ToJSCode(AValue));
end;

{ TjsRespondingCommand }

constructor TjsRespondingCommand.Create(AApplication: TjsdApplication;
  ACommand: String);
begin
  inherited Create(AApplication);

  Result := EmptyStr;

  FApplication.NeedConnection;
  FApplication.FContext.SendData(_GUID + ACommand);
end;

procedure TjsRespondingCommand.DoResponse(AData: String);
begin
  Result := AData;
  inherited;
end;

{ TjsExistingObject }

constructor TjsObjectEx.Create(AApplication: TjsdApplication;
  ACreateCommand: String);
begin
  inherited Create(AApplication);

  FInitialJSCommand := _Name + '=' + ACreateCommand;
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
