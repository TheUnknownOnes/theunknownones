//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit uDWMHelper;

interface

uses
  Windows, Graphics, Jwadwmapi, jwauxtheme;

function DWM_EnableBlurBehind(hwnd : HWND; AEnable: Boolean; hRgnBlur : HRGN = 0; ATransitionOnMaximized: Boolean = False; AFlags: Cardinal = 1): HRESULT;
function DWM_ExtendFrameIntoClientArea(hwnd: HWND; ATopHeight, ALeftWidth, ABottomHeight, ARightWidth: Integer): HRESULT;
function DWM_SheetOfGlass(hwnd: HWND): HRESULT;

implementation

function DWM_EnableBlurBehind(hwnd : HWND; AEnable: Boolean; hRgnBlur : HRGN = 0; ATransitionOnMaximized: Boolean = False; AFlags: Cardinal = 1): HRESULT;
var
  bb : DWM_BLURBEHIND;
begin
  bb.dwFlags:=AFlags;
  bb.fEnable:=AEnable;
  bb.hRgnBlur:=hRgnBlur;
  bb.fTransitionOnMaximized:=ATransitionOnMaximized;

  Result:=DwmEnableBlurBehindWindow(hwnd, bb);
end;

function DWM_ExtendFrameIntoClientArea(hwnd: HWND; ATopHeight, ALeftWidth, ABottomHeight, ARightWidth: Integer): HRESULT;
var
  lMargins : Margins;
begin
   lMargins.cyTopHeight := ATopHeight;
   lMargins.cyBottomHeight := ABottomHeight;
   lMargins.cxLeftWidth := ALeftWidth;
   lMargins.cxRightWidth := ARightWidth;

   Result := DwmExtendFrameIntoClientArea(hwnd, lMargins);
end;

function DWM_SheetOfGlass(hwnd: HWND): HRESULT;
begin
   DWM_ExtendFrameIntoClientArea(hwnd, -1, -1, -1, -1);
end;

end.
