//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit DWMApi;

interface

uses
  Windows, Graphics;

const
  WM_DWMCOLORIZATIONCOLORCHANGED = $0320;

  DWM_BB_ENABLE = $00000001;     //Indicates a value for fEnable has been specified.
  DWM_BB_BLURREGION = $00000002; //Indicates a value for hRgnBlur has been specified.
  DWM_BB_TRANSITIONONMAXIMIZED = $00000004;  //Indicates a value for fTransitionOnMaximized has been specified.

type
  _MARGINS = packed record
    cxLeftWidth: Integer;
    cxRightWidth: Integer;
    cyTopHeight: Integer;
    cyBottomHeight: Integer;
  end;
  PMargins = ^_MARGINS;
  TMargins = _MARGINS;

  _DWM_BLURBEHIND = packed record
    dwFlags : DWORD;
    fEnable : BOOL;
    hRgnBlur : HRGN;
    fTransitionOnMaximized : Boolean;
  end;
  PDWM_BLURBEHIND = ^_DWM_BLURBEHIND;
  TDWM_BLURBEHIND = _DWM_BLURBEHIND;

  TDwmExtendFrameIntoClientAreaFunc = function(hWnd: HWND; const pMarInset: PMargins): HRESULT; stdcall;
  TDwmEnableBlurBehindWindow = function(hWnd: HWND; bb : PDWM_BLURBEHIND): HRESULT; stdcall;
  TDwmIsCompositionEnabledFunc = function(pfEnabled: PBOOL): HRESULT; stdcall;
  TDwmDefWindowProc = procedure(hwnd : HWND; msg: UINT; wParam: WPARAM; lParam: LPARAM; out lResult: Integer); stdcall;
  TDwmGetColorizationColor = function(out pcrColorization: DWORD; out pfOpaqueBlend: BOOL): HRESULT; stdcall;

var
  DwmIsCompositionEnabled: TDwmIsCompositionEnabledFunc;
  DwmExtendFrameIntoClientArea: TDwmExtendFrameIntoClientAreaFunc;
  DwmEnableBlurBehindWindow : TDwmEnableBlurBehindWindow;
  DwmExtendFrameIntoClientAreaFunc : TDwmExtendFrameIntoClientAreaFunc;
  DwmDefWindowProc: TDwmDefWindowProc;
  DwmGetColorizationColor: TDwmGetColorizationColor;

function DWM_EnableBlurBehind(hwnd : HWND; AEnable: Boolean; hRgnBlur : HRGN = 0; ATransitionOnMaximized: Boolean = False; AFlags: Cardinal = 1): HRESULT;
function DWM_ExtendFrameIntoClientArea(hwnd: HWND; ATopHeight, ALeftWidth, ABottomHeight, ARightWidth: Integer): HRESULT;
function DWM_ExtendFrameIntoAll(hwnd: HWND): HRESULT;

implementation

var
  hDWMDLL : THandle = 0;

function InitDWMFuncs: HRESULT;
var
  osVinfo: TOSVersionInfo;
begin
  Result:=S_FALSE;
  ZeroMemory(@osVinfo, SizeOf(osVinfo));
  OsVinfo.dwOSVersionInfoSize := SizeOf(TOSVERSIONINFO);

  if ((GetVersionEx(osVInfo) = True) and (osVinfo.dwPlatformId = VER_PLATFORM_WIN32_NT) and (osVinfo.dwMajorVersion >= 5)) then
  begin
    hDWMDLL := LoadLibrary('dwmapi.dll');

    if hDWMDLL <> 0 then
    begin
      @DwmIsCompositionEnabled := GetProcAddress(hDWMDLL, 'DwmIsCompositionEnabled');
      @DwmExtendFrameIntoClientArea := GetProcAddress(hDWMDLL, 'DwmExtendFrameIntoClientArea');
      @DwmEnableBlurBehindWindow := GetProcAddress(hDWMDLL, 'DwmEnableBlurBehindWindow');
      @DwmDefWindowProc:= GetProcAddress(hDWMDLL, 'DwmDefWindowProc');
      @DwmGetColorizationColor:= GetProcAddress(hDWMDLL, 'DwmGetColorizationColor');

      Result:=S_OK;
    end;
  end;
end;

procedure UnloadDWMFuncs;
begin
  if hDWMDLL<>0 then
    FreeLibrary(hDWMDLL);

  hDWMDLL:=0;
end;

function DWM_EnableBlurBehind(hwnd : HWND; AEnable: Boolean; hRgnBlur : HRGN = 0; ATransitionOnMaximized: Boolean = False; AFlags: Cardinal = 1): HRESULT;
var
  bb : TDWM_BLURBEHIND;
begin
  Result:=S_OK;

  bb.dwFlags:=AFlags;
  bb.fEnable:=AEnable;
  bb.hRgnBlur:=hRgnBlur;
  bb.fTransitionOnMaximized:=ATransitionOnMaximized;

  Result:=DwmEnableBlurBehindWindow(hwnd, @bb);
end;

function DWM_ExtendFrameIntoClientArea(hwnd: HWND; ATopHeight, ALeftWidth, ABottomHeight, ARightWidth: Integer): HRESULT;
var
  Margins : TMargins;
begin
   Margins.cyTopHeight := ATopHeight;
   Margins.cyBottomHeight := ABottomHeight;
   Margins.cxLeftWidth := ALeftWidth;
   Margins.cxRightWidth := ARightWidth;
   Result := S_OK;

   //extend frame on bottom of client area
   Result := DwmExtendFrameIntoClientArea(hwnd,@margins);
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

   Result := DwmExtendFrameIntoClientArea(hwnd,@margins);
end;

initialization
  InitDWMFuncs;

finalization
  UnloadDWMFuncs;
  
end.
