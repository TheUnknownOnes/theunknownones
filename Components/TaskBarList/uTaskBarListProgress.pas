unit uTaskBarListProgress;

interface

uses
  uTaskBarList,
  jwaShlObj,
  Controls,
  Classes,
  Windows,
  jwaWinType,
  ComCtrls,
  Messages,
  CommCtrl;

{$i JEDI.inc}

type
  TTaskbarListProgressState = (psNoProgress,
                               psNormal,
                               psPaused,
                               psError);

  TTaskbarListProgress = class(TTaskBarListComponent)
  private
    FMin,
    FMax,
    FPosition : ULONGLONG;

    FProgressbar : TProgressbar;
    FOrigProgressWndProc : ULong;
    FState: TTaskbarListProgressState;
    FMarquee: Boolean;

    procedure SetMax(const Value: ULONGLONG);
    procedure SetMin(const Value: ULONGLONG);
    procedure SetPosition(const Value: ULONGLONG);
    procedure SetState(const Value: TTaskbarListProgressState);
    procedure SetProgressbar(const Value: TProgressBar);
    procedure SetMarquee(const Value: Boolean);
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
    property Marquee : Boolean read FMarquee write SetMarquee default false;
  end;

implementation

var
  Progresses : TList;

function MyWndProc(hwnd : HWND; uMsg : UINT; wParam : WPARAM; lParam : LPARAM) : LRESULT; stdcall;
var
  idx : Integer;
  prog : TTaskbarListProgress;
begin
  Result:=0;
  for idx := 0 to Progresses.Count - 1 do
  begin
    prog := TTaskbarListProgress(Progresses[idx]);

    if Assigned(Prog.ProgressBar) and
       (hwnd = Prog.ProgressBar.Handle) then
    begin
      Result := CallWindowProc(Pointer(Prog.FOrigProgressWndProc), hwnd, uMsg, wParam, lParam);

      case uMsg of
        PBM_SETRANGE..PBM_SETRANGE32
        {$IFDEF DELPHI12_UP}, PBM_SETMARQUEE, PBM_SETSTATE{$ENDIF}:
          Prog.PostUpdateMessage;
      end;

    end;
  end;
end;

{ TTaskbarListProgress }

constructor TTaskbarListProgress.Create(AOwner: TComponent);
begin
  inherited;

  FState := psNormal;
  FMin := 0;
  FMax := 100;
  FPosition := 0;
  FProgressbar := nil;
  FMarquee := false;

  Progresses.Add(Self);

  FInitialized:=True;
end;

destructor TTaskbarListProgress.Destroy;
begin
  Progresses.Remove(Self);
  ProgressBar := nil;
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

    {$IFDEF DELPHI12_UP}
    case FProgressbar.State of
      pbsNormal: FState := psNormal;
      pbsError: FState := psError;
      pbsPaused: FState := psPaused;
    end;
    FMarquee := FProgressbar.Style = pbstMarquee;
    {$ENDIF}
  end;

  if Assigned(FTaskbarList3) then
  begin
    if not FMarquee then
      FTaskbarList3.SetProgressValue(TaskBarEntryHandle, FPosition - FMin, FMax - FMin);

    NewState := TBPF_NOPROGRESS;

    if FPosition <> FMin then
    begin
      case FState of
        psNoProgress: NewState := TBPF_NOPROGRESS;
        psNormal: NewState := TBPF_NORMAL;
        psPaused: NewState := TBPF_PAUSED;
        psError: NewState := TBPF_ERROR;
      end;
    end;


    FTaskbarList3.SetProgressState(TaskBarEntryHandle, NewState);
    if FMarquee then
      FTaskbarList3.SetProgressState(TaskBarEntryHandle, TBPF_INDETERMINATE);
  end;
end;

procedure TTaskbarListProgress.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;

  if (AComponent = FProgressbar) and (Operation = opRemove) then
    FProgressbar := nil;
end;

procedure TTaskbarListProgress.SetMarquee(const Value: Boolean);
begin
  FMarquee := Value;
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
  begin
    SetWindowLong(FProgressbar.Handle, GWL_WNDPROC, FOrigProgressWndProc);
    FProgressbar.RemoveFreeNotification(Self);
  end;

  FProgressBar := Value;

  if Assigned(FProgressbar) then
  begin
    FProgressbar.FreeNotification(Self);
    FOrigProgressWndProc := SetWindowLong(FProgressbar.Handle, GWL_WNDPROC, Integer(@MyWndProc));
    PostUpdateMessage;
  end;
end;

procedure TTaskbarListProgress.SetState(const Value: TTaskbarListProgressState);
begin
  FState := Value;
  PostUpdateMessage;
end;

initialization
  Progresses := TList.Create;

finalization
  Progresses.Free;

end.
