unit uTaskBarListTab;

interface

uses
  uTaskBarList, JwaWinUser, Classes, Controls, Windows, jwaDWMAPI, Messages,
  JwaWinGDI, SysUtils, Graphics, Forms, Math, AppEvnts, JwaShlObj;

type
  TPreviewMode = (pmTaskBar, pmLive);

  TActivateTabEvent = procedure(Sender: TObject; Index: Integer) of object;
  TCloseTabEvent = procedure(Sender: TObject; var CanClose: Boolean) of object;
  TOnDrawPreview = procedure(Sender: TObject; PreviewMode: TPreviewMode; Canvas: TCanvas; Rect: TRect; var Handled: Boolean) of object;
  TOnGetPreviewRect = procedure(Sender: TObject; PreviewMode: TPreviewMode; var Rect: TRect) of object;

  TTabProperty = (tpUseAppThumbnailAlways,
                  tpUseAppThumbnailWhenActive,
                  tpUseAppPeekAlways,
                  tpUseAppPeekWhenActive);

  TTabProperties = set of TTabProperty;

  TTaskbarListTab = class(TTaskBarListComponent)
  private
    FControl: TControl;
    FProxyHandle: HWND;

    FIsActive: Boolean;
    FIsActiveWindow: Boolean;

    FIcon : TIcon;
    FOnGetPreviewRect : TOnGetPreviewRect;
    FOnDrawPreview : TOnDrawPreview;
    FOnCloseTab : TCloseTabEvent;
    FOnActivateTab: TNotifyEvent;
    FTabProps: TTabProperties;
    FInsertBefore: TTaskbarListTab;

    procedure SetControl(const Value: TControl);
    procedure SetActive(const Value: Boolean);
    procedure ProxyWndProc(var Message: TMessage);

    procedure DoWMActivate;
    procedure DoWMClose;
    procedure DoWMInvalidate;
    procedure DoActivateWindow;
    procedure DoCreatePreview(Message: TMessage);
    procedure SetIcon(const Value: TIcon);
    procedure DoGetPreviewRect(PreviewMode: TPreviewMode; var Rect: TRect);
    function DoDrawPreview(PreviewMode: TPreviewMode; Canvas: TCanvas; Rect: TRect): Boolean;
    procedure SetAppProps(const Value: TTabProperties);
    procedure SetInsertBefore(const Value: TTaskbarListTab);
  protected
    procedure DoRegisterTab;
    function GetIcon: TIcon; virtual;
    function DoCloseWindow: Boolean; virtual;
    function GetWindowCaption: String; virtual;

    property Control : TControl read FControl write SetControl;
    property Icon: TIcon read GetIcon write SetIcon;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure DoInitialize; override;
    procedure DoUpdate; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ActivateTaskWindow;
    procedure DeactivateTaskWindow;
    procedure UpdateTaskWindow;
    property Active: Boolean read FIsActive write SetActive;

    procedure SetTabActive;
  published
    property OnCloseTab: TCloseTabEvent read FOnCloseTab write FOnCloseTab;
    property OnActivateTab: TNotifyEvent read FOnActivateTab write FOnActivateTab;
    property OnGetPreviewRect: TOnGetPreviewRect read FOnGetPreviewRect write FOnGetPreviewRect;
    property OnDrawPreview: TOnDrawPreview read FOnDrawPreview write FOnDrawPreview;

    property AutoInitialize;

    property TabProperties: TTabProperties read FTabProps write SetAppProps;
    property InsertBefore: TTaskbarListTab read FInsertBefore write SetInsertBefore;
  end;

  TTaskbarListFormTab = class(TTaskbarListTab)
  protected
    function GetIcon: TIcon; override;
    function DoCloseWindow: Boolean; override;
    function GetWindowCaption: String; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TTaskbarListControlTab = class(TTaskbarListTab)
  private
    FWindowCaption: String;
  protected
    function GetWindowCaption: String; override;
  published
    property Control;
    property Icon;
    property Caption : String read FWindowCaption write FWindowCaption;
  end;

implementation

{ TTaskbarListFormTab }

const
  WM_INVALIDATE = WM_USER + $0001;

type
  TWMDwmSendIconicThumbnail = packed record
    Msg : Cardinal;
    Unused : Integer;
    Height, Width : Word;
    Result: LongInt;
  end;

constructor TTaskbarListFormTab.Create(AOwner: TComponent);
begin
  if not (AOwner is TCustomForm) then
    raise ETaskBarListError.Create('Owner of TTaskbarListFormTab must be a TCustomForm descendant');

  inherited;

  FControl:=TWinControl(AOwner);
end;

function TTaskbarListFormTab.DoCloseWindow: Boolean;
begin
  Result:=False;
  if Assigned(FOnCloseTab) then
    Result:=inherited DoCloseWindow
  else
  begin
    Result:=TCustomForm(FControl).CloseQuery;
    if Result then
      TCustomForm(FControl).Close;
  end;
end;

function TTaskbarListFormTab.GetIcon: TIcon;
begin
  Result:=nil;
  if FControl is TForm then
    Result:=TForm(FControl).Icon;
end;

function TTaskbarListFormTab.GetWindowCaption: String;
begin
  Result:=TCustomForm(Control).Caption;
end;

{ TTaskbarListTab }

procedure TTaskbarListTab.ProxyWndProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_ACTIVATE: begin
                   DoWMActivate;
                 end;
    WM_CLOSE:   begin
                  DoWMClose;
                  Exit;
                end;
    WM_DWMSENDICONICTHUMBNAIL,
    WM_DWMSENDICONICLIVEPREVIEWBITMAP:
                begin
                  if Control <> nil then
                    DoCreatePreview(Message);
                  Exit;
                end;
    WM_INVALIDATE:
                begin
                  DoWMInvalidate;
                  Exit;
                end;
  end;
  Message.Result := DefWindowProc(FProxyHandle, Message.Msg, Message.WParam, Message.LParam);
end;

procedure TTaskbarListTab.ActivateTaskWindow;
begin
  if (FIsActive) or (not Assigned(Control)) then
    Exit;

  if Assigned(FTaskbarList3) and CheckWin32Version(6,1) then
  begin
    FIsActive := True;
    inherited;

    DoRegisterTab;

    if FIsActiveWindow then
    begin
      FTaskbarList3.SetTabActive(FProxyHandle, TaskBarEntryHandle, 0);
    end;
  end;
end;

constructor TTaskbarListTab.Create(AOwner: TComponent);
var
  B : LongBool;
begin
  inherited;
  FIcon:=TIcon.Create;
  FIsActive := False;
  FProxyHandle:= AllocateHWnd(ProxyWndProc);

  FOnGetPreviewRect := nil;
  FOnDrawPreview := nil;

  B := True;
  if CheckWin32Version(6,1) then
  begin
    DwmSetWindowAttribute(FProxyHandle, DWMWA_HAS_ICONIC_BITMAP, @B, SizeOf(B));
    DwmSetWindowAttribute(FProxyHandle, DWMWA_FORCE_ICONIC_REPRESENTATION, @B, SizeOf(B));
  end;
end;

procedure TTaskbarListTab.DeactivateTaskWindow;
begin
  if not FIsActive then
    Exit;

  if Assigned(FTaskbarList3) and CheckWin32Version(6,1) then
  begin
    FTaskbarList3.UnregisterTab(FProxyHandle);

    FIsActive := False;
  end;
end;

destructor TTaskbarListTab.Destroy;
begin
  DeactivateTaskWindow;
  FIcon.Free;
  DeallocateHWnd(FProxyHandle);
  inherited;
end;

procedure TTaskbarListTab.DoActivateWindow;
begin
  if Assigned(FTaskbarList3) and CheckWin32Version(6,1) then
  begin
    FTaskbarList3.SetTabActive(FProxyHandle, TaskBarEntryHandle, 0);
    if Assigned(FOnActivateTab) then
      FOnActivateTab(Self);
  end;
end;

function TTaskbarListTab.DoCloseWindow: Boolean;
var
  CanClose
    : Boolean;
begin
  CanClose := True;

  if Assigned(FOnCloseTab) then
    FOnCloseTab(Self, CanClose);

  Result := CanClose;
end;

procedure TTaskbarListTab.DoCreatePreview(Message: TMessage);
var
  PreviewWindow,
  PreviewArea,
  PreviewThumb   : TBitmap;
  Pt             : TPoint;
  AbsoulteParent : TControl;
  W, H           : Integer;
  FrameAspect,
  ThumbAspect    : Double;
  OldVisibility  : Boolean;
  PreviewRect    : TRect;
  PreviewMode    : TPreviewMode;
begin
  if Message.Msg = WM_DWMSENDICONICLIVEPREVIEWBITMAP then
    PreviewMode := pmLive
  else
    PreviewMode := pmTaskBar;

  PreviewWindow := TBitmap.Create;
  PreviewArea := TBitmap.Create;
  try
    DoGetPreviewRect(PreviewMode, PreviewRect);
    PreviewWindow.Width := Control.Width;
    PreviewWindow.Height := Control.Height;
    PreviewWindow.PixelFormat := pf24bit;
    if Control is TWinControl then
        PreviewWindow.Canvas.Brush := TWinControl(Control).Brush;
    PreviewWindow.Canvas.FillRect(Control.ClientRect);
    PreviewWindow.Canvas.Lock;
    try
      if not DoDrawPreview(PreviewMode, PreviewWindow.Canvas, PreviewRect) then
      begin
        if Control.Parent <> nil then
        begin
          Control.Parent.Perform(WM_SETREDRAW, 0, 0);
        end;
        try
          OldVisibility := Control.Visible;
          Control.Visible := True;
          try
            if Control is TWinControl then
              TWinControl(Control).PaintTo(PreviewWindow.Canvas.Handle, 0, 0)
            else
              Control.Perform(WM_PAINT, PreviewWindow.Canvas.Handle, 0);
          finally
            Control.Visible := OldVisibility;
          end;
        finally
          if Control.Parent <> nil then
          begin
            Control.Parent.Perform(WM_SETREDRAW, 1, 0);
          end;
        end;
      end;
    finally
      PreviewWindow.Canvas.Unlock;
    end;

    PreviewArea.PixelFormat := PreviewWindow.PixelFormat;
    PreviewArea.Width := PreviewRect.Right ;//;- PreviewRect.Left + 1;
    PreviewArea.Height := PreviewRect.Bottom;// - PreviewRect.Top + 1;
    StretchBlt(PreviewArea.Canvas.Handle, 0, 0, PreviewArea.Width, PreviewArea.Height, PreviewWindow.Canvas.Handle, 0, 0, PreviewRect.Right, PreviewRect.Bottom, SRCCOPY);
    PreviewArea.PixelFormat := pf32bit;

    if Message.Msg = WM_DWMSENDICONICLIVEPREVIEWBITMAP then
    begin

      AbsoulteParent := Control;

      while AbsoulteParent.Parent <> nil do
      begin
        AbsoulteParent:=AbsoulteParent.Parent;
      end;

      if AbsoulteParent<>Control then
        pt:=Control.ClientToParent(Point(0,0), TWinControl(AbsoulteParent))
      else
      begin
        pt:=Point(0,0);

        if (Control is TForm) and
           (TForm(Control).Handle <> Self.TaskBarEntryHandle)
        then
        begin
          pt.X:=TForm(control).ClientOrigin.X-Application.MainForm.ClientOrigin.X;
          pt.Y:=TForm(control).ClientOrigin.Y-Application.MainForm.ClientOrigin.Y;
        end
      end;
      DwmSetIconicLivePreviewBitmap(FProxyHandle, PreviewArea.Handle, @Pt, 0);
    end
    else
    begin
      W := TWMDwmSendIconicThumbnail(Message).Width;
      H := TWMDwmSendIconicThumbnail(Message).Height;
      FrameAspect := PreviewArea.Width / PreviewArea.Height;
      ThumbAspect := W / H;
      if FrameAspect > ThumbAspect then
        H := Trunc(H * ThumbAspect / FrameAspect)
      else
        W := Trunc(W * FrameAspect / ThumbAspect);
      PreviewThumb := TBitmap.Create;
      try
        PreviewThumb.PixelFormat := pf32bit;
        PreviewThumb.Width := W;
        PreviewThumb.Height := H;
        SetStretchBltMode(PreviewThumb.Canvas.Handle, HALFTONE);
        StretchBlt(PreviewThumb.Canvas.Handle, 0, 0, W, H, PreviewArea.Canvas.Handle, 0, 0, PreviewArea.Width, PreviewArea.Height, SRCCOPY);
        DwmSetIconicThumbnail(FProxyHandle, PreviewThumb.Handle, 0);
      finally
        PreviewThumb.Free;
      end;
    end;
  finally
    PreviewWindow.Free;
    PreviewArea.Free;
  end;
end;


function TTaskbarListTab.DoDrawPreview(PreviewMode: TPreviewMode;
  Canvas: TCanvas; Rect: TRect): Boolean;
begin
  Result := False;
  if Assigned(FOnDrawPreview) then
    FOnDrawPreview(Self, PreviewMode, Canvas, Rect, Result);
end;

procedure TTaskbarListTab.DoGetPreviewRect(PreviewMode: TPreviewMode;
  var Rect: TRect);
begin
  Rect:=Control.ClientRect;
  if Assigned(FOnGetPreviewRect) then
    FOnGetPreviewRect(Self, PreviewMode, Rect);
end;

procedure TTaskbarListTab.DoInitialize;
begin
  inherited;
  ActivateTaskWindow;
  PostUpdateMessage;
end;

procedure TTaskbarListTab.DoRegisterTab;
begin
  if Assigned(FTaskbarList3) and CheckWin32Version(6,1) then
  begin
    FTaskbarList3.RegisterTab(FProxyHandle, TaskBarEntryHandle);
    if Assigned(FInsertBefore) and (FInsertBefore<>Self) then
    begin
      if FTaskbarList3.SetTabOrder(fProxyHandle, FInsertBefore.FProxyHandle)<>S_OK then
        FTaskbarList3.SetTabOrder(fProxyHandle, 0);
    end
    else
      FTaskbarList3.SetTabOrder(fProxyHandle, 0);
  end;

  PostUpdateMessage;
end;

procedure TTaskbarListTab.DoUpdate;
var
  Icon : TIcon;
  Flags : DWORD;
begin
  inherited;
  if CheckWin32Version(6,1) then
  begin
    if Assigned(FTaskbarList4) then
    begin
      Flags:=STPF_NONE;
      if tpUseAppThumbnailAlways in FTabProps then Flags:=Flags or STPF_USEAPPTHUMBNAILALWAYS;
      if tpUseAppThumbnailWhenActive in FTabProps then Flags:=Flags or STPF_USEAPPTHUMBNAILWHENACTIVE;
      if tpUseAppPeekAlways in FTabProps then Flags:=Flags or STPF_USEAPPPEEKALWAYS;
      if tpUseAppPeekWhenActive in FTabProps then Flags:=Flags or STPF_USEAPPPEEKWHENACTIVE;

      FTaskbarList4.SetTabProperties(FProxyHandle, Flags);
    end;

    DefWindowProc(FProxyHandle, WM_SETTEXT, 0 , LPARAM(PChar(GetWindowCaption)));

    Icon:=GetIcon;
    if Assigned(Icon) then
      SendMessage(FProxyHandle, WM_SETICON, ICON_SMALL, Icon.Handle)
    else
      SendMessage(FProxyHandle, WM_SETICON, ICON_SMALL, 0)
  end;
end;

procedure TTaskbarListTab.DoWMActivate;
begin
  if Application.MainForm.WindowState = wsMinimized then
    ShowWindow(Application.MainForm.Handle, SW_RESTORE);
  Application.MainForm.SetFocus;

  DoActivateWindow;
  PostMessage(FProxyHandle, WM_INVALIDATE, 0, 0);
end;

procedure TTaskbarListTab.DoWMClose;
begin
  if DoCloseWindow then
    Self.DeactivateTaskWindow;
end;

procedure TTaskbarListTab.DoWMInvalidate;
begin
  if Control is TWinControl then
    RedrawWindow(TWinControl(Control).Handle, nil, 0, RDW_ERASE or RDW_FRAME or RDW_INVALIDATE or RDW_ALLCHILDREN);
end;

function TTaskbarListTab.GetIcon: TIcon;
begin
  Result:=nil;
  if Assigned(FIcon) then
    Result:=FIcon
  else
    inherited;
end;

function TTaskbarListTab.GetWindowCaption: String;
begin
  Result:='';
end;

procedure TTaskbarListTab.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation=opRemove then
  begin
    if AComponent=Control then
      Control:=Nil;
    if AComponent=InsertBefore then
      InsertBefore:=Nil;
  end;
end;

procedure TTaskbarListTab.SetActive(const Value: Boolean);
begin
  if Value then
    ActivateTaskWindow
  else
    DeactivateTaskWindow;
end;

procedure TTaskbarListTab.SetAppProps(const Value: TTabProperties);
begin
  FTabProps := Value;
  PostUpdateMessage;
end;

procedure TTaskbarListTab.SetControl(const Value: TControl);
begin
  if Assigned(FControl) then
    FControl.RemoveFreeNotification(self);

  FControl := Value;

  if Assigned(FControl) then
    FControl.FreeNotification(self);

  UpdateTaskWindow;
end;

procedure TTaskbarListTab.SetIcon(const Value: TIcon);
begin
  if Assigned(FIcon) then
    FIcon.Assign(Value);

  PostUpdateMessage;
end;

procedure TTaskbarListTab.SetInsertBefore(const Value: TTaskbarListTab);
var
  OldActive : Boolean;
begin
  OldActive:=Self.Active;
  DeactivateTaskWindow;
  if Assigned(FInsertBefore) then
    FInsertBefore.RemoveFreeNotification(Self);

  FInsertBefore := Value;

  if Assigned(FInsertBefore) then
    FInsertBefore.FreeNotification(Self);

  if OldActive then
    ActivateTaskWindow;
end;

procedure TTaskbarListTab.SetTabActive;
begin
  if CheckWin32Version(6,1) and Assigned(FTaskbarList3) then
    FTaskbarList3.SetTabActive(FProxyHandle, TaskBarEntryHandle, 0);
end;

procedure TTaskbarListTab.UpdateTaskWindow;
begin
  if CheckWin32Version(6,1) then
  begin
    DwmInvalidateIconicBitmaps(FProxyHandle);
    PostUpdateMessage;
  end;
end;

{ TTaskbarListControlTab }

function TTaskbarListControlTab.GetWindowCaption: String;
begin
  Result:=FWindowCaption;
end;

end.
