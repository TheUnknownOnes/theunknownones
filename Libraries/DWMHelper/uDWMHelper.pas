//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit uDWMHelper;

interface

uses
  Windows, Graphics, Jwadwmapi, JwaUxTheme;

function DWM_EnableBlurBehind(hwnd : HWND; AEnable: Boolean; hRgnBlur : HRGN = 0; ATransitionOnMaximized: Boolean = False; AFlags: Cardinal = 1): HRESULT;
function DWM_ExtendFrameIntoClientArea(hwnd: HWND; ATopHeight, ALeftWidth, ABottomHeight, ARightWidth: Integer): HRESULT;
function DWM_ExtendFrameIntoAll(hwnd: HWND): HRESULT;

implementation

function DWM_EnableBlurBehind(hwnd : HWND; AEnable: Boolean; hRgnBlur : HRGN = 0; ATransitionOnMaximized: Boolean = False; AFlags: Cardinal = 1): HRESULT;
var
  bb : DWM_BLURBEHIND;
begin
  Result:=S_OK;

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
   Result := S_OK;

   //extend frame on bottom of client area
   Result := DwmExtendFrameIntoClientArea(hwnd, lMargins);
end;

function DWM_ExtendFrameIntoAll(hwnd: HWND): HRESULT;
var
  Margins : TMargins;
begin
   Margins.cyTopHeight := -1;
   Margins.cyBottomHeight := -1;
   Margins.cxLeftWidth := -1;
   Margins.cxRightWidth := -1;
   Result := S_OK;

   Result := DwmExtendFrameIntoClientArea(hwnd, margins);
end;

end.
