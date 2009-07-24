unit Unit3;

interface

uses 
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons, Types, ComCtrls, ToolWin, jwauxtheme,
  xpman, uEffectPNGToolbar, GDIPOBJ, gdipapi, AXCtrls, ActiveX, uDWMHelper,
  Jwadwmapi;

type 
  TForm1 = class(TForm)
    btn1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    Button1: TButton;

    procedure FormActivate(Sender: TObject); 
    procedure FormPaint(Sender: TObject); 
    procedure FormCreate(Sender: TObject);
    procedure btn1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure btn1CustomDraw(Sender: TToolBar; const ARect: TRect;
      var DefaultDraw: Boolean);
    procedure ToolButton2Click(Sender: TObject);
    procedure btn1AdvancedCustomDrawButton(Sender: TToolBar;
      Button: TToolButton; State: TCustomDrawState; Stage: TCustomDrawStage;
      var Flags: TTBCustomDrawFlags; var DefaultDraw: Boolean);
    procedure FormDestroy(Sender: TObject);

  private 
    { Private declarations }
    FBtnImages : TStringList;

    dwmMargins  : MARGINS;
    _marginOk   : Boolean;
    _AeroEnabled: Boolean;
     FRectText: TRect;
    FDown:boolean;
    procedure WndProc(var Message : TMessage) ; override ;
    function HitTestNCA(hwnd:HWND;wparam:WPARAM;lparam:LPARAM):Integer;

    
  public 
    { Public declarations } 
    function AeroEnabled: Boolean; 
    procedure CheckGlassEnabled; 
  end; 

var 
  Form1: TForm1;

implementation 

{$R *.dfm} 

function TForm1.AeroEnabled: Boolean; 
begin
  Result:=_AeroEnabled 
end;


procedure TForm1.btn1AdvancedCustomDrawButton(Sender: TToolBar;
  Button: TToolButton; State: TCustomDrawState; Stage: TCustomDrawStage;
  var Flags: TTBCustomDrawFlags; var DefaultDraw: Boolean);
var
  graphics : TGPGraphics;
  brush : TGPSolidBrush;
  image : TGPImage;
  resstr : TResourceStream;
  sa : activex.IStream;

  function GetImage(AName : String): TGPImage;
  begin
    if FBtnImages.IndexOf(AName)<0 then
    begin
      Result:=TGPImage.Create(IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName))+AName+'.png');
    end
    else
      Result:=TGPImage(FBtnImages.Objects[FBtnImages.IndexOf(AName)]);
  end;

begin
  if Stage=cdPostPaint then
  begin
    image:=GetImage(Button.Caption); //TGPImage.Create(IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName))+'kanne.png');
    graphics:=TGPGraphics.Create(sender.Canvas.Handle);
    try
      graphics.DrawImage(image,Button.BoundsRect.Left+3,Button.BoundsRect.Top+3)
    finally
      graphics.Free;
    end;
    DefaultDraw:=false;
  end
  else
    DefaultDraw:=true;
end;

procedure TForm1.btn1CustomDraw(Sender: TToolBar; const ARect: TRect;
  var DefaultDraw: Boolean);
begin
  btn1.Canvas.Brush.Color:=clBlack;
  btn1.Canvas.FillRect(ARect);

  DefaultDraw:=True;
end;

procedure TForm1.btn1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  Self.Invalidate;
end;

procedure TForm1.CheckGlassEnabled;
var 
  Response: Integer;
  Enabled : bool; 
begin
  if (Win32MajorVersion >= 6) then begin 
     Enabled:=False; 
     response :=DwmIsCompositionEnabled(Enabled);
     _aeroEnabled:= enabled; 
  end; 
end; 

procedure TForm1.FormActivate(Sender: TObject);
begin 
   inherited;
   if (dwmMargins.cyTopHeight < (btn1.Height+btn1.Top)) then
       dwmMargins.cyTopHeight:= (btn1.Height+btn1.Top);
   DwmExtendFrameIntoClientArea(Handle,dwmMargins);
end;

procedure TForm1.FormCreate(Sender: TObject); 
begin
  FBtnImages:=TStringList.Create;
  DoubleBuffered:=true;
  CheckGlassEnabled;
end; 

procedure TForm1.FormDestroy(Sender: TObject);
begin
  while FBtnImages.Count>0 do
  begin
    FBtnImages.Objects[0].Free;
    FBtnImages.Delete(0);
  end;
end;

procedure TForm1.FormPaint(Sender: TObject);
var 
  r : TRect; 
begin 
  inherited; 
  if _aeroEnabled then begin
      Canvas.Brush.Color:=TransparentColorValue; 
      Canvas.FillRect(Form1.ClientRect);
  end 
  else begin 
      Canvas.Brush.Color:=RGB($00C2, $00D9, $00F7); 
      Canvas.FillRect(Form1.ClientRect); 
  end; 
  r.Left:=dwmMargins.cxLeftWidth - 0; 
  r.Top:= dwmMargins.cyTopHeight - 0; 
  r.Right:= Width - dwmMargins.cxRightWidth - 0; 
  r.Bottom:= Height - dwmMargins.cyBottomHeight - 0;

  Canvas.Brush.Color:=clBtnface;
  Canvas.FillRect(r);
end; 


function TForm1.HitTestNCA(hwnd:HWND; wparam:WParam; lparam:LPARAM):Integer;
var
  P : TPoint;
  rect : TRect;

function PointInRect(APoint: TPoint; ARect : TRect) : Boolean;
begin
  Result:=(P.X>=ARect.Left) and (P.X<=ARect.Right+ARect.Left) and (P.Y>=ARect.Top) and (P.Y<=ARect.Bottom+ARect.Top);
end;

function RectangleToScreen(ALeft, ATop, AWidth, AHeight: Integer): TRect;
begin
  Result.TopLeft:=ClientToScreen(Types.Point(ALeft, ATop));
  Result.BottomRight:=Types.Point(AWidth, AHeight);
end;

begin
  Result:=HTCLIENT;

  P.X:=LoWord(integer(lparam));
  P.Y:=HiWord(integer(lparam));


  rect:=RectangleToScreen(0,0,dwmMargins.cxLeftWidth, dwmMargins.cxLeftWidth);
  if PointInRect(P, rect) then
  begin
    Result:=HTTOPLEFT;
    exit;
  end;

  rect := RectangleToScreen(Width - dwmMargins.cxRightWidth, 0, dwmMargins.cxRightWidth, dwmMargins.cxRightWidth);
  if PointInRect(P, rect) then
  begin
    Result:=HTTOPRIGHT;
    exit;
  end;

  rect:=RectangleToScreen(0, Height - dwmMargins.cyBottomHeight, dwmMargins.cxLeftWidth, dwmMargins.cyBottomHeight);
  if PointInRect(P, rect) then
  begin
    Result:=HTBOTTOMLEFT;
    exit;
  end;

  rect:=RectangleToScreen(Width - dwmMargins.cxRightWidth, Height - dwmMargins.cyBottomHeight, dwmMargins.cxRightWidth, dwmMargins.cyBottomHeight);
  if PointInRect(P, rect) then
  begin
    Result:=HTBOTTOMRIGHT;
    exit;
  end;

  rect:=RectangleToScreen(0, 0, Width, dwmMargins.cxLeftWidth);
  if PointInRect(P, rect) then
  begin
    Result:=HTTOP;
    exit;
  end;

  rect:=RectangleToScreen(0, dwmMargins.cxLeftWidth, Width, dwmMargins.cyTopHeight - dwmMargins.cxLeftWidth);
  if PointInRect(P, rect) then
  begin
    Result:=HTCAPTION;
    exit;
  end;

  rect:=RectangleToScreen(0, 0, dwmMargins.cxLeftWidth, Height);
  if PointInRect(P, rect) then
  begin
    Result:=HTLEFT;
    exit;
  end;

  rect:=RectangleToScreen(Width - dwmMargins.cxRightWidth, 0, dwmMargins.cxRightWidth, Height);
  if PointInRect(P, rect) then
  begin
    Result:=HTRIGHT;
    exit;
  end;

  rect:=RectangleToScreen(0, Height - dwmMargins.cyBottomHeight, Width, dwmMargins.cyBottomHeight);
  if PointInRect(P, rect) then
  begin
    Result:=HTBOTTOM;
    exit;
  end;
end;

procedure TForm1.ToolButton2Click(Sender: TObject);
begin
  ShowMessage('Guckst du ;-)');
end;

procedure TForm1.WndProc(var Message : TMessage);
const 
    WM_NCCALCSIZE = $0083; 
    WM_NCHITTEST =  $0084;
var
    Result : LResult; 
    dwmHandled : bool;

    msgCalcSize : TWMNCCalcSize;
begin
    dwmHandled:=DwmDefWindowProc(Form1.Handle, Message.Msg, Message.WParam, Message.LParam, result);
    if (dwmHandled = true) then begin
        message.Result:=result;
        exit;
    end;
    if ( (Message.Msg = WM_NCCALCSIZE) and (message.WParam = 1)) then
    begin
       if (_marginOk=false) then begin
             //Set what client area would be for passing to DwmExtendIntoClientArea
           dwmMargins.cyTopHeight:= 10;
           dwmMargins.cxLeftWidth:= 10;
           dwmMargins.cyBottomHeight:= 10;
           dwmMargins.cxRightWidth:= 10;
           _marginOk := true;
       end;

       self.invalidate;
    end 
    else if (message.Msg = WM_NCHITTEST) and (Integer(Message.Result)= 0) then 
       Message.Result:= HitTestNCA(Form1.Handle, Message.WParam, message.LParam) 
    else
       inherited;
end;

procedure PatchINT3;
var
  NOP : Byte;
  NTDLL: THandle;
  BytesWritten: DWORD;
  Address: Pointer;
begin
  if Win32Platform <> VER_PLATFORM_WIN32_NT then Exit;
  NTDLL := GetModuleHandle('NTDLL.DLL');
  if NTDLL = 0 then Exit;
  Address := GetProcAddress(NTDLL, 'DbgBreakPoint');
  if Address = nil then Exit;
  try
    if Char(Address^) <> #$CC then Exit;

    NOP := $90;
    if windows.WriteProcessMemory(GetCurrentProcess, Address, @NOP, 1, BytesWritten) and
      (BytesWritten = 1) then
      FlushInstructionCache(GetCurrentProcess, Address, 1);
  except
    //Do not panic if you see an EAccessViolation here, it is perfectly harmless!
    on EAccessViolation do ;
    else raise;
  end;
end;

initialization
  if DebugHook<>0 then
    PatchINT3;
end.
