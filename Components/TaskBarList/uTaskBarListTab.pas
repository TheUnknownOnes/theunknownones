unit uTaskBarListTab;

interface

uses
  uTaskBarList, Classes, Controls, Windows, jwaDWMAPI, Messages, JwaWinGDI,
  Graphics;

type
  TPreviewMode = (pmTaskBar, pmLive);

  TActivateTabEvent = procedure(Sender: TObject; Index: Integer) of object;
  TCloseTabEvent = procedure(Sender: TObject; var CanClose: Boolean) of object;
  TOnDrawPreview = procedure(Sender: TObject; PreviewMode: TPreviewMode; Canvas: TCanvas; Rect: TRect; var Handled: Boolean) of object;
  TOnGetPreviewRect = procedure(Sender: TObject; PreviewMode: TPreviewMode; var Rect: TRect) of object;

  TTaskbarListTab = class(TTaskBarListComponent)
  private
    FControl: TControl;
    FProxyHandle: HWND;

    FIsActive: Boolean;
    FIsActiveWindow: Boolean;

    FIcon : TIcon;
    FCaption : TCaption;
    FOnGetPreviewRect : TOnGetPreviewRect;
    FOnDrawPreview : TOnDrawPreview;
    FOnCloseTab : TCloseTabEvent;
    FOnActivateTab: TNotifyEvent;

    procedure SetControl(const Value: TControl);
    procedure SetActive(const Value: Boolean);
    procedure ProxyWndProc(var Message: TMessage);

    procedure DoWMActivate;
    procedure DoWMClose;
    procedure DoWMInvalidate;
    procedure DoActivateWindow;
    procedure DoUpdateWindowCaption;
    procedure DoUpdateWindowIcon;
    procedure DoCreatePreview(Message: TMessage);
    procedure SetIcon(const Value: TIcon);
    procedure DoGetPreviewRect(PreviewMode: TPreviewMode; var Rect: TRect);
    function DoDrawPreview(PreviewMode: TPreviewMode; Canvas: TCanvas; Rect: TRect): Boolean;
  protected
    procedure DoRegisterTab;
    function GetIcon: TIcon; virtual;
    function DoCloseWindow: Boolean; virtual;
    function GetWindowCaption: String; virtual;

    property Control : TControl read FControl write SetControl;
    property Icon: TIcon read GetIcon write SetIcon;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ActivateTaskWindow;
    procedure DeactivateTaskWindow;
    procedure UpdateTaskWindow;
    property Active: Boolean read FIsActive write SetActive;
  published
    property OnCloseTab: TCloseTabEvent read FOnCloseTab write FOnCloseTab;
    property OnActivateTab: TNotifyEvent read FOnActivateTab write FOnActivateTab;
    property OnGetPreviewRext: TOnGetPreviewRect read FOnGetPreviewRect write FOnGetPreviewRect;
    property OnDrawPreview: TOnDrawPreview read FOnDrawPreview write FOnDrawPreview;
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

uses
  Forms, JwaWinUser, uSysTools, Math;

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
  if Assigned(FOnCloseTab) then
    inherited
  else
  begin
    Result:=TCustomForm(FControl).CloseQuery;
    if Result then
      TCustomForm(FControl).Close;
  end;
end;

function TTaskbarListFormTab.GetIcon: TIcon;
begin
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
  if FIsActive then
    Exit;

  if Assigned(FTaskbarList3) then
  begin
    DoRegisterTab;

    if FIsActiveWindow then
    begin
      FTaskbarList3.SetTabActive(FProxyHandle, TaskBarEntryHandle, 0);
    end;

    FIsActive := True;
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
  DwmSetWindowAttribute(FProxyHandle, DWMWA_HAS_ICONIC_BITMAP, @B, SizeOf(B));
  DwmSetWindowAttribute(FProxyHandle, DWMWA_FORCE_ICONIC_REPRESENTATION, @B, SizeOf(B));
end;

procedure TTaskbarListTab.DeactivateTaskWindow;
begin
  if not FIsActive then
    Exit;

  if Assigned(FTaskbarList3) then
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
  if Assigned(FTaskbarList3) then
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
  PreviewWindow
  , PreviewArea
  , PreviewThumb
    : TBitmap;
  Pt
    : TPoint;
  CtlParent,
  AbsoulteParent
    : TControl;
  W
  , H ,i
    : Integer;
  FrameAspect
  , ThumbAspect
    : Double;
  OldVisibility
    : Boolean;
  PreviewRect
    : TRect;
  PreviewMode
    : TPreviewMode;
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
        pt:=Point(0,0);

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

procedure TTaskbarListTab.DoRegisterTab;
begin
  if Assigned(FTaskbarList3) then
  begin
    FTaskbarList3.RegisterTab(FProxyHandle, TaskBarEntryHandle);
    FTaskbarList3.SetTabOrder(fProxyHandle, 0);
  end;

  DoUpdateWindowCaption;
  DoUpdateWindowIcon;
end;

procedure TTaskbarListTab.DoUpdateWindowCaption;
begin
  DefWindowProc(FProxyHandle, WM_SETTEXT, 0 , LPARAM(PChar(GetWindowCaption)));
end;

procedure TTaskbarListTab.DoUpdateWindowIcon;
var
  Icon : TIcon;
begin
  Icon:=GetIcon;
  if Assigned(Icon) then
    SendMessage(FProxyHandle, WM_SETICON, ICON_SMALL, Icon.Handle)
  else
    SendMessage(FProxyHandle, WM_SETICON, ICON_SMALL, 0)

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
  if Assigned(FIcon) then
    Result:=FIcon
  else
    inherited;
end;

function TTaskbarListTab.GetWindowCaption: String;
begin
  Result:='';
end;

procedure TTaskbarListTab.SetActive(const Value: Boolean);
begin
  if Value then
    ActivateTaskWindow
  else
    DeactivateTaskWindow;
end;

procedure TTaskbarListTab.SetControl(const Value: TControl);
begin
  FControl := Value;
end;

procedure TTaskbarListTab.SetIcon(const Value: TIcon);
begin
  if Assigned(FIcon) then
    FIcon.Assign(Value);
end;

procedure TTaskbarListTab.UpdateTaskWindow;
begin
  DwmInvalidateIconicBitmaps(FProxyHandle);
end;

{ TTaskbarListControlTab }

function TTaskbarListControlTab.GetWindowCaption: String;
begin
  Result:=FWindowCaption;
end;

end.
