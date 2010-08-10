{ Original file taken from "Windows 7 Controls for Delphi" by Daniel Wischnewski
  http://www.gumpi.com/Blog/2009/01/20/Alpha1OfWindows7ControlsForDelphi.aspx
  MPL licensed }

{ D2/D3 support and correct IID consts added by Martijn Laan for Inno Setup }

{$IFDEF VER90}
  {$DEFINE DELPHI2}
{$ENDIF}

unit uTaskBarList_Intf;

interface

uses
  Windows {$IFDEF DELPHI2}, OLE2 {$ENDIF};

const
  CLSID_TaskbarList: TGUID = (
    D1:$56FDF344; D2:$FD6D; D3:$11D0; D4:($95,$8A,$00,$60,$97,$C9,$A0,$90));
  IID_TaskbarList: TGUID = (
    D1:$56FDF342; D2:$FD6D; D3:$11D0; D4:($95,$8A,$00,$60,$97,$C9,$A0,$90));
  IID_TaskbarList2: TGUID = (
    D1:$602D4995; D2:$B13A; D3:$429B; D4:($A6,$6E,$19,$35,$E4,$4F,$43,$17));
  IID_TaskbarList3: TGUID = (
    D1:$EA1AFB91; D2:$9E28; D3:$4B86; D4:($90,$E9,$9E,$9F,$8A,$5E,$EF,$AF));
  IID_TaskbarList4: TGUID = (
    D1:$C43DC798; D2:$95D1; D3:$4BEA; D4:($90,$30,$BB,$99,$E2,$98,$3A,$1A));
const
  THBF_ENABLED = $0000;
  THBF_DISABLED = $0001;
  THBF_DISMISSONCLICK = $0002;
  THBF_NOBACKGROUND = $0004;
  THBF_HIDDEN = $0008;

const
  THB_BITMAP = $0001;
  THB_ICON = $0002;
  THB_TOOLTIP = $0004;
  THB_FLAGS = $0008;

const
  THBN_CLICKED = $1800;

const
  TBPF_NOPROGRESS = $00;
  TBPF_INDETERMINATE = $01;
  TBPF_NORMAL = $02;
  TBPF_ERROR = $04;
  TBPF_PAUSED = $08;

const
  TBATF_USEMDITHUMBNAIL: DWORD = $00000001;
  TBATF_USEMDILIVEPREVIEW: DWORD = $00000002;

const
  WM_DWMSENDICONICTHUMBNAIL = $0323;
  WM_DWMSENDICONICLIVEPREVIEWBITMAP = $0326;

const
  STPF_NONE                        = $0000;
  STPF_USEAPPTHUMBNAILALWAYS       = $0001;
  STPF_USEAPPTHUMBNAILWHENACTIVE   = $0002;
  STPF_USEAPPPEEKALWAYS            = $0004;
  STPF_USEAPPPEEKWHENACTIVE        = $0008;

type
  TTipString = array[0..259] of WideChar;
  PTipString = ^TTipString;
  tagTHUMBBUTTON = packed record
    dwMask: DWORD;
    iId: UINT;
    iBitmap: UINT;
    hIcon: HICON;
    szTip: TTipString;
    dwFlags: DWORD;
  end;
  THUMBBUTTON = tagTHUMBBUTTON;
  THUMBBUTTONLIST = ^THUMBBUTTON;

  dwInteger64 = record
    Lo, Hi: Cardinal;
  end;

type
{$IFDEF DELPHI2}
  ITaskbarList = class(IUnknown)
    procedure HrInit; virtual; stdcall; abstract;
    procedure AddTab(hwnd: Cardinal); virtual; stdcall; abstract;
    procedure DeleteTab(hwnd: Cardinal); virtual; stdcall; abstract;
    procedure ActivateTab(hwnd: Cardinal); virtual; stdcall; abstract;
    procedure SetActiveAlt(hwnd: Cardinal); virtual; stdcall; abstract;
  end;

  ITaskbarList2 = class(ITaskbarList)
    procedure MarkFullscreenWindow(hwnd: Cardinal; fFullscreen: Bool); virtual; stdcall; abstract;
  end;

  ITaskbarList3 = class(ITaskbarList2)
    procedure SetProgressValue(hwnd: Cardinal; ullCompleted, ullTotal: dwInteger64); virtual; stdcall; abstract;
    procedure SetProgressState(hwnd: Cardinal; tbpFlags: DWORD); virtual; stdcall; abstract;
    procedure RegisterTab(hwndTab: Cardinal; hwndMDI: Cardinal); virtual; stdcall; abstract;
    procedure UnregisterTab(hwndTab: Cardinal); virtual; stdcall; abstract;
    procedure SetTabOrder(hwndTab: Cardinal; hwndInsertBefore: Cardinal); virtual; stdcall; abstract;
    procedure SetTabActive(hwndTab: Cardinal; hwndMDI: Cardinal; tbatFlags: DWORD); virtual; stdcall; abstract;
    procedure ThumbBarAddButtons(hwnd: Cardinal; cButtons: UINT; Button: THUMBBUTTONLIST); virtual; stdcall; abstract;
    procedure ThumbBarUpdateButtons(hwnd: Cardinal; cButtons: UINT; pButton: THUMBBUTTONLIST); virtual; stdcall; abstract;
    procedure ThumbBarSetImageList(hwnd: Cardinal; himl: Cardinal); virtual; stdcall; abstract;
    procedure SetOverlayIcon(hwnd: Cardinal; hIcon: HICON; pszDescription: LPCWSTR); virtual; stdcall; abstract;
    procedure SetThumbnailTooltip(hwnd: Cardinal; pszTip: LPCWSTR); virtual; stdcall; abstract;
    procedure SetThumbnailClip(hwnd: Cardinal; prcClip: PRect); virtual; stdcall; abstract;
  end;
{$ELSE}
  ITaskbarList = interface
    ['{56FDF342-FD6D-11D0-958A-006097C9A090}']
    procedure HrInit; safecall;
    procedure AddTab(hwnd: Cardinal); safecall;
    procedure DeleteTab(hwnd: Cardinal); safecall;
    procedure ActivateTab(hwnd: Cardinal); safecall;
    procedure SetActiveAlt(hwnd: Cardinal); safecall;
  end;

  ITaskbarList2 = interface(ITaskbarList)
    ['{602D4995-B13A-429B-A66E-1935E44F4317}']
    procedure MarkFullscreenWindow(hwnd: Cardinal; fFullscreen: Bool); safecall;
  end;

  ITaskbarList3 = interface(ITaskbarList2)
    ['{EA1AFB91-9E28-4B86-90E9-9E9F8A5EEFAF}']
    procedure SetProgressValue(hwnd: Cardinal; ullCompleted, ullTotal: dwInteger64); safecall;
    procedure SetProgressState(hwnd: Cardinal; tbpFlags: DWORD); safecall;
    procedure RegisterTab(hwndTab: Cardinal; hwndMDI: Cardinal); safecall;
    procedure UnregisterTab(hwndTab: Cardinal); safecall;
    procedure SetTabOrder(hwndTab: Cardinal; hwndInsertBefore: Cardinal); safecall;
    procedure SetTabActive(hwndTab: Cardinal; hwndMDI: Cardinal; tbatFlags: DWORD); safecall;
    procedure ThumbBarAddButtons(hwnd: Cardinal; cButtons: UINT; Button: THUMBBUTTONLIST); safecall;
    procedure ThumbBarUpdateButtons(hwnd: Cardinal; cButtons: UINT; pButton: THUMBBUTTONLIST); safecall;
    procedure ThumbBarSetImageList(hwnd: Cardinal; himl: Cardinal); safecall;
    procedure SetOverlayIcon(hwnd: Cardinal; hIcon: HICON; pszDescription: LPCWSTR); safecall;
    procedure SetThumbnailTooltip(hwnd: Cardinal; pszTip: LPCWSTR); safecall;
    procedure SetThumbnailClip(hwnd: Cardinal; prcClip: PRect); safecall;
  end;

  ITaskbarList4 = interface(ITaskbarList3)
    ['{C43DC798-95D1-4BEA-9030-BB99E2983A1A}']
    procedure SetTabProperties(hwndTab: Cardinal; stpFlags: DWORD);
  end;
{$ENDIF}

  function GetTaskbarList4Interface: ITaskbarList4;

implementation

uses
  ActiveX;

function GetTaskbarList4Interface: ITaskbarList4;
begin
  if CoCreateInstance(CLSID_TaskbarList, nil, CLSCTX_INPROC_SERVER, IID_TaskbarList4, Result) = S_OK then
  begin
    Result.HrInit;
  end
  else
  begin
    Result:=nil;
  end;
end;

end.

