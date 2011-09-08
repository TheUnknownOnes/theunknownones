unit uServiceStatusNotify;

interface

uses
  Classes,
  SysUtils,
  StrUtils,
  Generics.Collections,
  SyncObjs,
  jwaWindows;

type


  TCustomServiceStatusNotify = class
  protected
    FServiceHandle : SC_HANDLE;
    FNotifyMask : Cardinal;
    FNotifyBuffer : SERVICE_NOTIFY;
    FSynchronizeNotification : Boolean;

    procedure DoRegister; virtual;
      //called out of TMainThread
    function DoNotfication(ANotify : SERVICE_NOTIFY) : Boolean; virtual;
      //return true, to get more notifications
  public
    constructor Create(AServiceHandle : SC_HANDLE); virtual;
    destructor Destroy; override;
  end;

  TCustomServiceManagerNotify = class(TCustomServiceStatusNotify)
  public
    constructor Create; reintroduce; overload; virtual;
    constructor Create(AMachine : PChar); reintroduce; overload; virtual;
  end;

  TCustomServiceNotify = class(TCustomServiceStatusNotify)
  protected
    FServiceManager : SC_HANDLE;
  public
    constructor Create(AMachine : PChar; AName : PChar); reintroduce; overload; virtual;
    constructor Create(AName : PChar); reintroduce; overload; virtual;
    constructor Create(AServiceManager : SC_HANDLE; AName : PChar); reintroduce; overload; virtual;
    constructor Create(AServiceHandle : SC_HANDLE); reintroduce; overload; virtual;
    destructor Destroy; override;
  end;

implementation

type
  TMainThread = class(TThread)
  protected
    FNotifiers : TList<TCustomServiceStatusNotify>;
    FEvent : TEvent;
    procedure OnNotifiersChanged(Sender: TObject; const Item: TCustomServiceStatusNotify; Action: TCollectionNotification);
    procedure Execute; override;
  public
    constructor Create();
    destructor Destroy; override;
  end;

  TNotificationThread = class(TThread)
  private
    procedure OnNotificationsChanged(Sender: TObject;
      const Item: SERVICE_NOTIFY; Action: TCollectionNotification);
  protected
    FNotifications : TList<SERVICE_NOTIFY>;
    FEvent : TEvent;

    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

var
  MainThread : TMainThread;
  NotificationThread : TNotificationThread;

procedure NotificationCallback(ANotify : Pointer); stdcall;
var
  Notify : PService_Notify absolute ANotify;
begin
  NotificationThread.FNotifications.Add(Notify^);
end;

{ TCustomServiceStatusNotify }

constructor TCustomServiceStatusNotify.Create(AServiceHandle: SC_HANDLE);
begin
  FServiceHandle := AServiceHandle;
  FSynchronizeNotification := true;

  //dont change these values if you dont know what you do :-)
  FNotifyBuffer.dwVersion := SERVICE_NOTIFY_STATUS_CHANGE;
  FNotifyBuffer.pfnNotifyCallback := NotificationCallback;
  FNotifyBuffer.pContext := Self;

  MainThread.FNotifiers.Add(Self);
end;

destructor TCustomServiceStatusNotify.Destroy;
begin
  CloseServiceHandle(FServiceHandle);

  inherited;
end;

function TCustomServiceStatusNotify.DoNotfication(
  ANotify: SERVICE_NOTIFY): Boolean;
begin
  Result := true;
end;

procedure TCustomServiceStatusNotify.DoRegister;
begin
  if NotifyServiceStatusChange(FServiceHandle, FNotifyMask, FNotifyBuffer) <> ERROR_SUCCESS then
    RaiseLastOSError;
end;

{ TMainThread }

constructor TMainThread.Create;
begin
  FNotifiers := TList<TCustomServiceStatusNotify>.Create;
  inherited Create(true);
  FreeOnTerminate := false;
  FNotifiers.OnNotify := OnNotifiersChanged;
  FEvent := TEvent.Create;
  Resume;
end;

destructor TMainThread.Destroy;
begin
  FEvent.Free;
  FNotifiers.Free;

  inherited;
end;

procedure TMainThread.Execute;
var
  n : TCustomServiceStatusNotify;
begin
  while not Terminated do
  begin
    case WaitForSingleObjectEx(FEvent.Handle, 50, true) of
      WAIT_OBJECT_0:
      begin
        for n in FNotifiers do
        begin
          try
            n.DoRegister;
          except
          end;
        end;

        FNotifiers.Clear;
        FEvent.ResetEvent;
      end;
    end;

  end;
end;

procedure TMainThread.OnNotifiersChanged(Sender: TObject;
  const Item: TCustomServiceStatusNotify; Action: TCollectionNotification);
begin
  FEvent.SetEvent;
end;

{ TNotificationThread }

constructor TNotificationThread.Create;
begin
  FNotifications := TList<SERVICE_NOTIFY>.Create;
  FNotifications.OnNotify := OnNotificationsChanged;
  inherited Create(true);
  FreeOnTerminate := false;
  FEvent := TEvent.Create;
  Resume;
end;

destructor TNotificationThread.Destroy;
begin
  FNotifications.Free;
  FEvent.Free;
  inherited;
end;

procedure TNotificationThread.Execute;
var
  n : SERVICE_NOTIFY;
  ReRegister : Boolean;
  p : TThreadProcedure;
begin
  while not Terminated do
  begin
    if WaitForSingleObject(FEvent.Handle, 50) = WAIT_OBJECT_0 then
    begin
      for n in FNotifications do
      begin
        try
          p := procedure
               begin
                 ReRegister := TCustomServiceStatusNotify(n.pContext).DoNotfication(n);
               end;

          if TCustomServiceStatusNotify(n.pContext).FSynchronizeNotification then
            Synchronize(p)
          else
            p;

          if ReRegister then
            MainThread.FNotifiers.Add(TCustomServiceStatusNotify(n.pContext));
        except
        end;
      end;

      FNotifications.Clear;
      FEvent.ResetEvent;
    end;
  end;
end;

procedure TNotificationThread.OnNotificationsChanged(Sender: TObject;
  const Item: SERVICE_NOTIFY; Action: TCollectionNotification);
begin
  FEvent.SetEvent;
end;

{ TServiceManagerNotify }

constructor TCustomServiceManagerNotify.Create;
begin
  Create(nil);
end;

constructor TCustomServiceManagerNotify.Create(AMachine: PChar);
begin
  FNotifyMask := SERVICE_NOTIFY_CREATED or SERVICE_NOTIFY_DELETED;
  inherited Create(OpenSCManager(AMachine, SERVICES_ACTIVE_DATABASE, SC_MANAGER_ENUMERATE_SERVICE));
end;

{ TCustomServiceNotify }

constructor TCustomServiceNotify.Create(AMachine, AName: PChar);
begin
  FServiceManager := OpenSCManager(AMachine, SERVICES_ACTIVE_DATABASE, SC_MANAGER_ENUMERATE_SERVICE);
  Create(FServiceManager, AName);
end;

constructor TCustomServiceNotify.Create(AName: PChar);
begin
  Create(nil, AName);
end;

constructor TCustomServiceNotify.Create(AServiceManager: SC_HANDLE;
  AName: PChar);
begin
  Create(OpenService(AServiceManager, AName, SERVICE_QUERY_STATUS));
end;

constructor TCustomServiceNotify.Create(AServiceHandle: SC_HANDLE);
begin
  FServiceHandle := AServiceHandle;

  FNotifyMask := SERVICE_NOTIFY_STOPPED or
                 SERVICE_NOTIFY_START_PENDING or
                 SERVICE_NOTIFY_STOP_PENDING or
                 SERVICE_NOTIFY_RUNNING or
                 SERVICE_NOTIFY_CONTINUE_PENDING or
                 SERVICE_NOTIFY_PAUSE_PENDING or
                 SERVICE_NOTIFY_PAUSED or
                 SERVICE_NOTIFY_DELETE_PENDING;

  inherited Create(FServiceHandle);
end;

destructor TCustomServiceNotify.Destroy;
begin
  inherited;

  if FServiceManager > 0 then
    CloseServiceHandle(FServiceManager);
end;

initialization
  MainThread := TMainThread.Create;
  NotificationThread := TNotificationThread.Create;

finalization
  MainThread.Terminate; MainThread.WaitFor; MainThread.Free;
  NotificationThread.Terminate; NotificationThread.WaitFor; NotificationThread.Free;

end.
