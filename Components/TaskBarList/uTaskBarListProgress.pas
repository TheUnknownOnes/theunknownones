unit uTaskBarListProgress;

interface

uses
  uTaskBarList,
  jwaShlObj,
  Controls,
  Classes,
  AppEvnts,
  Windows,
  jwaWinType,
  ComCtrls,
  CommCtrl;

type
  TTaskbarListProgressState = (psNoProgress,
                               psIndeterminate,
                               psNormal,
                               psPaused,
                               psError);

  TTaskbarListProgress = class(TTaskBarListComponent)
  private
    FAppEvents : TApplicationEvents;
    FMin,
    FMax,
    FPosition : ULONGLONG;

    FProgressbar : TProgressbar;
    FState: TTaskbarListProgressState;

    procedure OnMessage(var Msg: TMsg; var Handled: Boolean);
    procedure SetMax(const Value: ULONGLONG);
    procedure SetMin(const Value: ULONGLONG);
    procedure SetPosition(const Value: ULONGLONG);
    procedure SetState(const Value: TTaskbarListProgressState);
    procedure SetProgressbar(const Value: TProgressBar);
  protected
    procedure DoUpdate; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Min : ULONGLONG read FMin write SetMin default 0;
    property Max : ULONGLONG read FMax write SetMax default 100;
    property Position : ULONGLONG read FPosition write SetPosition default 0;
    property State : TTaskbarListProgressState read FState write SetState default psNormal;
    property ProgressBar : TProgressBar read FProgressBar write SetProgressbar;
  end;

implementation

{ TTaskbarListProgress }

constructor TTaskbarListProgress.Create(AOwner: TComponent);
begin
  inherited;

  FInitialized := true;

  FState := psNormal;
  FMin := 0;
  FMax := 100;
  FPosition := 0;
  FProgressbar := nil;

  if not (csDesigning in ComponentState) then
  begin
    FAppEvents := TApplicationEvents.Create(Self);
    FAppEvents.OnMessage := OnMessage;
  end;
end;

destructor TTaskbarListProgress.Destroy;
begin

  inherited;
end;

procedure TTaskbarListProgress.DoUpdate;
var
  NewState : DWORD;
begin
  inherited;

  if Assigned(FProgressbar) then
  begin
    FMin := FProgressbar.Min;
    FMax := FProgressbar.Max;
    FPosition := FProgressbar.Position;
  end;

  if Assigned(FTaskbarList3) then
  begin
    FTaskbarList3.SetProgressValue(TaskBarEntryHandle, FPosition - FMin, FMax - FMin);

    NewState := TBPF_NOPROGRESS;

    if FPosition <> FMin then
    begin
      case FState of
        psNoProgress: NewState := TBPF_NOPROGRESS;
        psIndeterminate: NewState := TBPF_INDETERMINATE;
        psNormal: NewState := TBPF_NORMAL;
        psPaused: NewState := TBPF_PAUSED;
        psError: NewState := TBPF_ERROR;
      end;
    end;

    FTaskbarList3.SetProgressState(TaskBarEntryHandle, NewState);
  end;
end;

procedure TTaskbarListProgress.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;

  if (AComponent = FProgressbar) and (Operation = opRemove) then
    FProgressbar := nil;
end;

procedure TTaskbarListProgress.OnMessage(var Msg: TMsg; var Handled: Boolean);
begin
  Handled := false;

  if Assigned(FProgressbar) and
     FProgressbar.HandleAllocated and
     (FProgressbar.Handle = Msg.hwnd)  then
    PostUpdateMessage;
end;

procedure TTaskbarListProgress.SetMax(const Value: ULONGLONG);
begin
  FMax := Value;
  PostUpdateMessage;
end;

procedure TTaskbarListProgress.SetMin(const Value: ULONGLONG);
begin
  FMin := Value;
  PostUpdateMessage;
end;

procedure TTaskbarListProgress.SetPosition(const Value: ULONGLONG);
begin
  FPosition := Value;
  PostUpdateMessage;
end;

procedure TTaskbarListProgress.SetProgressbar(const Value: TProgressBar);
begin
  if Assigned(FProgressbar) then
    FProgressbar.RemoveFreeNotification(Self);

  FProgressBar := Value;

  if Assigned(FProgressbar) then
  begin
    FProgressbar.FreeNotification(Self);
    PostUpdateMessage;
  end;
end;

procedure TTaskbarListProgress.SetState(const Value: TTaskbarListProgressState);
begin
  FState := Value;
  PostUpdateMessage;
end;

end.
