//**********************************************************
// Developed by TheUnkownOnes.net
//
// for more information look at www.TheUnknownOnes.net
//**********************************************************

unit uServiceStatusChange;

interface

uses
  Classes, Windows, WinSvc,
  Generics.Collections,
  SyncObjs;

const
  SERVICE_NOTIFY_STOPPED = $00000001;
  SERVICE_NOTIFY_START_PENDING = $00000002;
  SERVICE_NOTIFY_STOP_PENDING = $00000004;
  SERVICE_NOTIFY_RUNNING = $00000008;
  SERVICE_NOTIFY_CONTINUE_PENDING = $00000010;
  SERVICE_NOTIFY_PAUSE_PENDING = $00000020;
  SERVICE_NOTIFY_PAUSED = $00000040;
  SERVICE_NOTIFY_CREATED = $00000080;
  SERVICE_NOTIFY_DELETED = $00000100;
  SERVICE_NOTIFY_DELETE_PENDING = $00000200;

  _SERVICE_NOTIFY_ALL = SERVICE_NOTIFY_STOPPED or
                        SERVICE_NOTIFY_START_PENDING or
                        SERVICE_NOTIFY_STOP_PENDING or
                        SERVICE_NOTIFY_RUNNING or
                        SERVICE_NOTIFY_CONTINUE_PENDING or
                        SERVICE_NOTIFY_PAUSE_PENDING or
                        SERVICE_NOTIFY_PAUSED or
                        SERVICE_NOTIFY_DELETE_PENDING;

  _SERVICE_MANAGER_NOTIFY_ALL = SERVICE_NOTIFY_CREATED or
                                 SERVICE_NOTIFY_DELETED;

  SERVICE_NOTIFY_STATUS_CHANGE = $00000002;

function Service_NotifyToString(AServiceNotify : DWORD) : String;

type
  SC_NOTIFY_CALLBACK = procedure(ANotify : Pointer); stdcall;
  TSC_NOTIFY_CALLBACK = SC_NOTIFY_CALLBACK;

  SERVICE_STATUS_PROCESS = record
    dwServiceType,
    dwCurrentState,
    dwControlsAccepted,
    dwWin32ExitCode,
    dwServiceSpecificExitCode,
    dwCheckPoint,
    dwWaitHint,
    dwProcessId,
    dwServiceFlags : DWORD;
  end;
  TSERVICE_STATUS_PROCESS = SERVICE_STATUS_PROCESS;
  PSERVICE_STATUS_PROCESS = ^SERVICE_STATUS_PROCESS;

  SERVICE_NOTIFYW = record
    dwVersion: DWORD;
    pfnNotifyCallback: TSC_NOTIFY_CALLBACK;
    pContext: Pointer;
    dwNotificationStatus: DWORD;
    ServiceStatus: TSERVICE_STATUS_PROCESS;
    dwNotificationTriggered: DWORD;
    pszServiceNames: PWideChar;
  end;
  TSERVICE_NOTIFYW = SERVICE_NOTIFYW;
  PSERVICE_NOTIFYW = ^SERVICE_NOTIFYW;

  SERVICE_NOTIFYA = record
    dwVersion: DWORD;
    pfnNotifyCallback: TSC_NOTIFY_CALLBACK;
    pContext: Pointer;
    dwNotificationStatus: DWORD;
    ServiceStatus: TSERVICE_STATUS_PROCESS;
    dwNotificationTriggered: DWORD;
    pszServiceNames: PAnsiChar;
  end;
  TSERVICE_NOTIFYA = SERVICE_NOTIFYA;
  PSERVICE_NOTIFYA = ^SERVICE_NOTIFYA;

  TService_Notify = TSERVICE_NOTIFYW;
  PService_Notify = ^TService_Notify;

function NotifyServiceStatusChangeW(hService : SC_HANDLE; dwNotifyMask : DWORD; var pNotifyBuffer : TSERVICE_NOTIFYW) : DWORD; stdcall;
function NotifyServiceStatusChangeA(hService : SC_HANDLE; dwNotifyMask : DWORD; var pNotifyBuffer : TSERVICE_NOTIFYA) : DWORD; stdcall;
function NotifyServiceStatusChange(hService : SC_HANDLE; dwNotifyMask : DWORD; var pNotifyBuffer : TService_Notify) : DWORD; stdcall;

type
  TCustomServiceStatusChangeHandler = class
  protected
    FRegistered,
    FNotified : Boolean; //Both should only be used in this unit or with sense for the code :)
    FAutoCloseServiceHandle : Boolean;

    function Register : Boolean; virtual; abstract;
    //Called(out of the thread) to register this handler for notifications
    //Returns true if registration was successfull

    function Notify : Boolean; virtual; abstract;
    //Called(out of the thread) if a notification occured
    //Returns true if more notifications are desired

    function TestNotificationTriggered(ANotification : DWORD) : Boolean;
  public
    ServiceHandle : SC_HANDLE;
    ServiceNotify : TService_Notify;

    constructor Create; virtual;
    destructor Destroy; override;
  end;

  TServiceStatusChangeThread = class(TThread)
  protected
    FHandler : TList<TCustomServiceStatusChangeHandler>;
    FHandlerLock : TCriticalSection;

    procedure Execute; override;
  public
    constructor Create();
    destructor Destroy; override;

    procedure AddHandler(AHandler : TCustomServiceStatusChangeHandler);
    procedure RemoveHandler(AHandler : TCustomServiceStatusChangeHandler);

    property Handler : TList<TCustomServiceStatusChangeHandler> read FHandler;
    property HandlerLock : TCriticalSection read FHandlerLock;
  end;

  TServiceManagerStatusChangeHandler = class(TCustomServiceStatusChangeHandler)
  protected
    function Register : Boolean; override;
  end;

  TServiceStatusChangeHandler = class(TCustomServiceStatusChangeHandler)
  protected
    function Register : Boolean; override;
    function Notify : Boolean; override;
    procedure Deleted; virtual;
  end;


  //Classes for the GUI --------------------------------------------------------

  TsscnManagerHandler = class;
  TsscnServiceHandler = class;
  TServiceStatusChangeNotificator = class;

  TsscnOpenServiceManangerEvent = function(ASender : TServiceStatusChangeNotificator) : SC_HANDLE of object;
  TsscnManagerEvent = procedure(ASender : TServiceStatusChangeNotificator;
                                AHandler : TsscnManagerHandler) of object;
  TsscnServiceInstallEvent = procedure(ASender : TServiceStatusChangeNotificator;
                                       AServices : TStringList) of object;
  TsccnOpenServiceEvent = function(ASender : TServiceStatusChangeNotificator;
                                   AServiceName : String) : SC_HANDLE of object;
  TsscnServiceStatusEvent = procedure(ASender : TServiceStatusChangeNotificator;
                                      AHandler : TsscnServiceHandler) of object;

  TServiceStatusChangeNotificator = class(TComponent)
  protected
    FThread : TServiceStatusChangeThread;
    FManagerHandler : TsscnManagerHandler;
    FServiceNames: TStringList;

    FOnManagerEvent: TsscnManagerEvent;
    FOnOpenServiceManager: TsscnOpenServiceManangerEvent;
    FOnServiceInstalled: TsscnServiceInstallEvent;
    FOnServiceUninstalled: TsscnServiceInstallEvent;
    FOnOpenService: TsccnOpenServiceEvent;
    FOnServiceStatus: TsscnServiceStatusEvent;
    FOnServiceRunning: TsscnServiceStatusEvent;
    FOnServicePausePending: TsscnServiceStatusEvent;
    FOnServicePaused: TsscnServiceStatusEvent;
    FOnServiceStopPending: TsscnServiceStatusEvent;
    FOnServiceStartPending: TsscnServiceStatusEvent;
    FOnServiceDeletePending: TsscnServiceStatusEvent;
    FOnServiceContinuePending: TsscnServiceStatusEvent;
    FOnServiceStopped: TsscnServiceStatusEvent;

    function DoOpenServiceManager : SC_HANDLE; virtual;
    procedure DoManangerEvent(AHandler : TsscnManagerHandler); virtual;
    function DoOpenService(AHandler : TsscnServiceHandler) : SC_HANDLE; virtual;
    procedure DoServiceStatus(AHandler : TsscnServiceHandler); virtual;

    procedure OnServiceNamesChange(Sender : TObject);

    procedure SetServiceNames(const Value: TStringList);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetServiceManagerHandle : SC_HANDLE;
  published
    property ServiceNames : TStringList read FServiceNames write SetServiceNames;

    property OnOpenServiceManager : TsscnOpenServiceManangerEvent read FOnOpenServiceManager write FOnOpenServiceManager;
    property OnManagerEvent : TsscnManagerEvent read FOnManagerEvent write FOnManagerEvent;
    property OnServiceInstalled : TsscnServiceInstallEvent read FOnServiceInstalled write FOnServiceInstalled;
    property OnServiceUninstalled : TsscnServiceInstallEvent read FOnServiceUninstalled write FOnServiceUninstalled;
    property OnOpenService : TsccnOpenServiceEvent read FOnOpenService write FOnOpenService;
    property OnServiceStatus : TsscnServiceStatusEvent read FOnServiceStatus write FOnServiceStatus;
    property OnServiceStopped : TsscnServiceStatusEvent read FOnServiceStopped write FOnServiceStopped;
    property OnServiceStartPending : TsscnServiceStatusEvent read FOnServiceStartPending write FOnServiceStartPending;
    property OnServiceStopPending : TsscnServiceStatusEvent read FOnServiceStopPending write FOnServiceStopPending;
    property OnServiceRunning : TsscnServiceStatusEvent read FOnServiceRunning write FOnServiceRunning;
    property OnServiceContinuePending : TsscnServiceStatusEvent read FOnServiceContinuePending write FOnServiceContinuePending;
    property OnServicePausePending : TsscnServiceStatusEvent read FOnServicePausePending write FOnServicePausePending;
    property OnServicePaused : TsscnServiceStatusEvent read FOnServicePaused write FOnServicePaused;
    property OnServiceDeletePending : TsscnServiceStatusEvent read FOnServiceDeletePending write FOnServiceDeletePending;
  end;

  TsscnManagerHandler = class(TServiceManagerStatusChangeHandler)
  protected
    FOwner : TServiceStatusChangeNotificator;
    function Notify : Boolean; override;
  public
    constructor Create(AOwner : TServiceStatusChangeNotificator); reintroduce;
  end;

  TsscnServiceHandler = class(TServiceStatusChangeHandler)
  protected
    FOwner : TServiceStatusChangeNotificator;
    FServiceName : String;
    function Notify : Boolean; override;
    function Register : Boolean; override;
  public
    constructor Create(AOwner : TServiceStatusChangeNotificator; AServiceName : String); reintroduce;

    property ServiceName : String read FServiceName;
  end;

procedure Register;

implementation

uses
  StrUtils;

procedure Register;
begin
  RegisterComponents('TUO', [TServiceStatusChangeNotificator]);
end;

const
  advapi32 = 'advapi32.dll';

function NotifyServiceStatusChangeW; external advapi32 name 'NotifyServiceStatusChangeW';
function NotifyServiceStatusChangeA; external advapi32 name 'NotifyServiceStatusChangeA';
function NotifyServiceStatusChange; external advapi32 name 'NotifyServiceStatusChangeW';

function Service_NotifyToString(AServiceNotify : DWORD) : String;
  procedure Add(AString : String);
  begin
    if Result <> '' then
      Result := Result + ',';
    Result := Result + AString;
  end;
begin
  Result := '';

  if (AServiceNotify and SERVICE_NOTIFY_STOPPED) = SERVICE_NOTIFY_STOPPED then Add('stopped');
  if (AServiceNotify and SERVICE_NOTIFY_START_PENDING) = SERVICE_NOTIFY_START_PENDING then Add('start pending');
  if (AServiceNotify and SERVICE_NOTIFY_STOP_PENDING) = SERVICE_NOTIFY_STOP_PENDING then Add('stop pending');
  if (AServiceNotify and SERVICE_NOTIFY_RUNNING) = SERVICE_NOTIFY_RUNNING then Add('running');
  if (AServiceNotify and SERVICE_NOTIFY_CONTINUE_PENDING) = SERVICE_NOTIFY_CONTINUE_PENDING then Add('continue pending');
  if (AServiceNotify and SERVICE_NOTIFY_PAUSE_PENDING) = SERVICE_NOTIFY_PAUSE_PENDING then Add('pause pending');
  if (AServiceNotify and SERVICE_NOTIFY_PAUSED) = SERVICE_NOTIFY_PAUSED then Add('paused');
  if (AServiceNotify and SERVICE_NOTIFY_DELETE_PENDING) = SERVICE_NOTIFY_DELETE_PENDING then Add('delete pending');
end;

procedure NotificationCallback(ANotify : Pointer); stdcall;
var
  Notify : PService_Notify absolute ANotify;
begin
  with TCustomServiceStatusChangeHandler(Notify.pContext) do
  begin
    FNotified := true;
    FRegistered := false;
  end;
end;

{ TServiceStatusChangeThread }

procedure TServiceStatusChangeThread.AddHandler(
  AHandler: TCustomServiceStatusChangeHandler);
begin
  if not FHandler.Contains(AHandler) then
    FHandler.Add(AHandler);
end;

constructor TServiceStatusChangeThread.Create;
begin
  FreeOnTerminate := false;

  FHandler := TList<TCustomServiceStatusChangeHandler>.Create;
  FHandlerLock := TCriticalSection.Create;

  inherited Create(false);
end;

destructor TServiceStatusChangeThread.Destroy;
begin
  FHandlerLock.Free;
  FHandler.Free;

  inherited;
end;

procedure TServiceStatusChangeThread.Execute;
var
  Handler : TCustomServiceStatusChangeHandler;
begin
  while not Terminated do
  begin
    FHandlerLock.Enter;
    try
      for Handler in FHandler do
      begin
        if not Handler.FRegistered then
        begin
          try
            Handler.FRegistered := Handler.Register;
          except
            Handler.FRegistered := false;
          end;
        end;
      end;
    finally
      FHandlerLock.Leave;
    end;

    if SleepEx(50, true) = WAIT_IO_COMPLETION then
    begin
      FHandlerLock.Enter;
      try
        for Handler in FHandler do
        begin
          if Handler.FNotified then
          begin
            Handler.FNotified := false;
            try
              if Handler.Notify then
                Handler.FRegistered := Handler.Register;
            except
              Handler.FRegistered := false;
            end;
          end;
        end;
      finally
        FHandlerLock.Leave;
      end;
    end;
  end;
end;

procedure TServiceStatusChangeThread.RemoveHandler(
  AHandler: TCustomServiceStatusChangeHandler);
begin
  FHandler.Extract(AHandler);
end;

{ TCustomServiceStatusChangeHandler }

constructor TCustomServiceStatusChangeHandler.Create;
begin
  ServiceNotify.dwVersion := SERVICE_NOTIFY_STATUS_CHANGE;
  ServiceNotify.pfnNotifyCallback := NotificationCallback;
  ServiceNotify.pContext := Self;

  FNotified := false;
  FRegistered := false;
  FAutoCloseServiceHandle := true;
end;

destructor TCustomServiceStatusChangeHandler.Destroy;
begin
  if FAutoCloseServiceHandle then
    CloseServiceHandle(ServiceHandle);
  inherited;
end;

function TCustomServiceStatusChangeHandler.TestNotificationTriggered(
  ANotification: DWORD): Boolean;
begin
  Result := (ANotification and ServiceNotify.dwNotificationTriggered) = ANotification;
end;

{ TServiceManagerStatusChangeHandler }

function TServiceManagerStatusChangeHandler.Register: Boolean;
begin
  Result := NotifyServiceStatusChange(ServiceHandle, _SERVICE_MANAGER_NOTIFY_ALL, ServiceNotify) = ERROR_SUCCESS;
end;

{ TServiceStatusChangeHandler }

procedure TServiceStatusChangeHandler.Deleted;
begin
  CloseServiceHandle(ServiceHandle);
  ServiceHandle := 0;
end;

function TServiceStatusChangeHandler.Notify: Boolean;
begin
  if TestNotificationTriggered(SERVICE_NOTIFY_DELETE_PENDING) then
  begin
    //if we dont close our handle to the service, we'll block the other app removing this service
    Deleted;
    Result := false;
  end
  else
    Result := true;
end;

function TServiceStatusChangeHandler.Register: Boolean;
begin
  Result := NotifyServiceStatusChange(ServiceHandle, _SERVICE_NOTIFY_ALL, ServiceNotify) = ERROR_SUCCESS;
end;

{ TServiceStatusChangeNotificator }

constructor TServiceStatusChangeNotificator.Create(AOwner: TComponent);
begin
  inherited;

  if csDesigning in ComponentState then
  begin
    FThread := nil
  end
  else
  begin
    FThread := TServiceStatusChangeThread.Create;
    FManagerHandler := TsscnManagerHandler.Create(Self);
    FThread.AddHandler(FManagerHandler);
  end;

  FServiceNames := TStringList.Create;
  FServiceNames.OnChange := OnServiceNamesChange;
end;

destructor TServiceStatusChangeNotificator.Destroy;
begin
  if Assigned(FThread) then
  begin
    FThread.Terminate;
    FThread.WaitFor;
    FThread.Free;
  end;

  if Assigned(FManagerHandler) then
    FManagerHandler.Free;

  FServiceNames.OnChange := nil;
  while FServiceNames.Count > 0 do
  begin
    if Assigned(FServiceNames.Objects[0]) then
      TsscnServiceHandler(FServiceNames.Objects[0]).Free;
    FServiceNames.Delete(0);
  end;
  FServiceNames.Free;

  inherited;
end;

procedure TServiceStatusChangeNotificator.DoManangerEvent(
  AHandler: TsscnManagerHandler);
var
  slCreated,
  slDeleted : TStringList;
  c : PWideChar;
begin
  if Assigned(FOnManagerEvent) then
    FOnManagerEvent(Self, AHandler);

  slCreated := TStringList.Create;
  slDeleted := TStringList.Create;
  try
    c := AHandler.ServiceNotify.pszServiceNames;
    while lstrlen(c) > 0 do
    begin
      if StartsStr('/', c)  then
        slCreated.Add(Copy(c, 2, lstrlen(c)))
      else
        slDeleted.Add(c);

      Inc(c, lstrlen(c) + 1)
    end;

    if Assigned(FOnServiceInstalled) and (slCreated.Count > 0) then
      FOnServiceInstalled(Self, slCreated);

    if Assigned(FOnServiceUninstalled) and (slDeleted.Count > 0) then
      FOnServiceUninstalled(Self, slDeleted);
  finally
    slCreated.Free;
    slDeleted.Free;
  end;
end;

function TServiceStatusChangeNotificator.DoOpenService(
  AHandler: TsscnServiceHandler): SC_HANDLE;
begin
  if Assigned(FOnOpenService) then
    Result := FOnOpenService(Self, AHandler.ServiceName)
  else
    Result := OpenService(GetServiceManagerHandle, PWideChar(WideString(AHandler.ServiceName)), SERVICE_QUERY_STATUS);
end;

function TServiceStatusChangeNotificator.DoOpenServiceManager: SC_HANDLE;
begin
  if Assigned(FOnOpenServiceManager) then
    Result := FOnOpenServiceManager(Self)
  else
    Result := OpenSCManager(nil, SERVICES_ACTIVE_DATABASE, SC_MANAGER_ENUMERATE_SERVICE);
end;

procedure TServiceStatusChangeNotificator.DoServiceStatus(
  AHandler: TsscnServiceHandler);
begin
  if Assigned(FOnServiceStatus) then
    FOnServiceStatus(Self, AHandler);

  if AHandler.TestNotificationTriggered(SERVICE_NOTIFY_STOPPED) and
     Assigned(FOnServiceStopped) then
    FOnServiceStopped(Self, AHandler);

  if AHandler.TestNotificationTriggered(SERVICE_NOTIFY_START_PENDING) and
     Assigned(FOnServiceStartPending) then
    FOnServiceStopped(Self, AHandler);

  if AHandler.TestNotificationTriggered(SERVICE_NOTIFY_STOP_PENDING) and
     Assigned(FOnServiceStopPending) then
    FOnServiceStopped(Self, AHandler);

  if AHandler.TestNotificationTriggered(SERVICE_NOTIFY_RUNNING) and
     Assigned(FOnServiceRunning) then
    FOnServiceStopped(Self, AHandler);

  if AHandler.TestNotificationTriggered(SERVICE_NOTIFY_CONTINUE_PENDING) and
     Assigned(FOnServiceContinuePending) then
    FOnServiceStopped(Self, AHandler);

  if AHandler.TestNotificationTriggered(SERVICE_NOTIFY_PAUSE_PENDING) and
     Assigned(FOnServicePausePending) then
    FOnServiceStopped(Self, AHandler);

  if AHandler.TestNotificationTriggered(SERVICE_NOTIFY_PAUSED) and
     Assigned(FOnServicePaused) then
    FOnServiceStopped(Self, AHandler);

  if AHandler.TestNotificationTriggered(SERVICE_NOTIFY_DELETE_PENDING) and
     Assigned(FOnServiceDeletePending) then
    FOnServiceDeletePending(Self, AHandler);
end;

function TServiceStatusChangeNotificator.GetServiceManagerHandle: SC_HANDLE;
begin
  if Assigned(FManagerHandler) then
    Result := FManagerHandler.ServiceHandle
  else
    Result := 0;
end;

procedure TServiceStatusChangeNotificator.OnServiceNamesChange(Sender: TObject);
var
  idx : Integer;
  handler : TsscnServiceHandler;
  h : TCustomServiceStatusChangeHandler;
begin
  if csDesigning in ComponentState then exit;

  for idx := 0 to FServiceNames.Count - 1 do
  begin
    if FServiceNames[idx] = '' then continue;

    handler := TsscnServiceHandler(FServiceNames.Objects[idx]);
    if not Assigned(handler) then
    begin
      handler := TsscnServiceHandler.Create(Self, FServiceNames[idx]);
      FThread.AddHandler(handler);
      FServiceNames.Objects[idx] := handler;
    end;
  end;

  FThread.HandlerLock.Enter;
  try
    for h in FThread.Handler do
    begin
      if h is TsscnServiceHandler then
      begin
        handler := TsscnServiceHandler(h);
        if FServiceNames.IndexOf(handler.ServiceName) = -1 then
        begin
          FThread.Handler.Extract(h);
          h.Free;
        end;
      end;
    end;
  finally
    FThread.HandlerLock.Leave;
  end;
end;

procedure TServiceStatusChangeNotificator.SetServiceNames(
  const Value: TStringList);
begin
  FServiceNames.Assign(Value);
end;

{ TsscnManagerHandler }

constructor TsscnManagerHandler.Create(AOwner: TServiceStatusChangeNotificator);
begin
  inherited Create;
  FOwner := AOwner;

  ServiceHandle := AOwner.DoOpenServiceManager;
end;

function TsscnManagerHandler.Notify: Boolean;
begin
  TThread.Synchronize(nil, procedure begin FOwner.DoManangerEvent(Self); end);
  Result := true;
end;

{ TsscnServiceHandler }

constructor TsscnServiceHandler.Create(AOwner: TServiceStatusChangeNotificator; AServiceName : String);
begin
  inherited Create;
  FOwner := AOwner;
  FServiceName := AServiceName;

  ServiceHandle := FOwner.DoOpenService(Self);
end;

function TsscnServiceHandler.Notify: Boolean;
begin
  inherited;
  Result := true;
  TThread.Synchronize(nil, procedure begin FOwner.DoServiceStatus(Self); end);
end;

function TsscnServiceHandler.Register: Boolean;
begin
  Result := inherited;

  if (not Result) and (ServiceHandle = 0) then
    TThread.Synchronize(nil, procedure begin ServiceHandle := FOwner.DoOpenService(Self); end);
end;

end.
