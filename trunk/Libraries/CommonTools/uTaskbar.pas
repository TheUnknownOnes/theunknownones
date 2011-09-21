//**********************************************************
// Developed by TheUnkownOnes.net
//
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit uTaskbar;

interface

uses
  Classes, Windows, Messages, CommCtrl, SysUtils, Math, Controls, uSysTools;


type
  ETaskbarError = class(Exception);
  ETaskbarAllocMemError = class(ETaskbarError);

  TTaskbarButton = class(TObject)
  {-this represents a TBBUTTON Structure
   -use this to "store" a button in you application}
  public
    Bitmap : Integer;
    Command : Integer;
    State : Byte;
    Style : Byte;
    Data : NativeUInt;
    Text : Integer;
  end;

  TTaskbarButtonList = class(TList)
  private
    function Get(AIndex: Integer): TTaskbarButton;
    procedure Put(AIndex: Integer; const Value: TTaskbarButton);
    function Add(const AButton : TTaskbarButton) : Integer; reintroduce;
    procedure Delete(const AIndex : Integer); reintroduce;
    procedure Insert(const AIndex : Integer; const AButton : TTaskbarButton); reintroduce;
    function Remove(const AButton : TTaskbarButton) : Integer; reintroduce;
  published
  public
    destructor Destroy(); override;

    property Items[AIndex : Integer] : TTaskbarButton read Get write Put; default;
    function First : TTaskbarButton; reintroduce;
    function IndexOf(const AButton : TTaskbarButton) : Integer; reintroduce;
    function Last : TTaskbarButton; reintroduce;

  end;

  TTaskbar = class(TObject)
  strict private
    function CreateButtonFromTaskbar(AIndex : Integer) : TTaskbarButton;

    procedure ReadFromTaskbar;
    function IndexOfButtonWith(ACommand : Integer; AData : NativeUInt) : Integer;
  public
    Buttons : TTaskbarButtonList;

    constructor Create(); reintroduce;
    destructor Destroy; override;

    procedure Refresh;
      //reloads all info from the taskbar
    function Rect : TRect;
      //returns the rect of the taskbar relative to the screen

    {$REGION 'button access ... most interesting for you'}
    function ButtonExists(AButton : TTaskbarButton) : Boolean;
    function GetButtonText(AButton : TTaskbarButton) : WideString;
    function ButtonVisible (AButton : TTaskbarButton) : Boolean;
    procedure SetButtonVisible(AButton : TTaskbarButton; AVisible : Boolean = true);
    function ButtonIsGroupButton(AButton : TTaskbarButton) : Boolean;
    procedure MoveButton(OldIndex, NewIndex : Integer); overload;
    procedure MoveButton(AButton : TTaskbarButton; NewIndex : Integer); overload;
    function AddButtonIconToImageList(AButton : TTaskbarButton; AImageList : TImageList) : Integer;
    function ButtonRect(AButton : TTaskbarButton) : TRect;
    procedure DrawButton(AButton: TTaskbarButton; ADC: HDC; ARect : TRect);
    {$ENDREGION}

    {$REGION 'class functions ... use it if u know what u do :)'}
    class procedure CGetButton(AButtonIndex : Integer; AButton : PTBButton);
      //retrieves the raw button data
    class procedure CGetButtonRect(AButtonIndex : Integer; ARect : PRect);
      //gets the rect of the button relative to the Taskbar
    class function COpenTaskbarProcess : NativeUInt;
      //dont forget to close the handle
    class function CButtonCount : Integer;
      //the real amount ... maybe our object isnt up to date :)
    class function CWindowHandle : HWND;
      //the window handle of the taskbar control
    class function CButtonText (ACommand : Integer) : WideString;
      //the text u can see on the button
    class function CAppWindowOfButton(AData : NativeUint) : HWND;
      //retrieves the window handle the button points to
    {$ENDREGION}
  end;

var
  Taskbar : TTaskbar;
  
implementation

uses
  Graphics;

{$REGION 'Helper'}
function ReasonSysError: String;
begin
  Result:=' (Reason: '+SysErrorMessage(GetLastError)+')';
end;

procedure InvalidButton;
begin
  raise ETaskbarError.Create('Invalid button');
end;

function get_window_icon_small(hwnd: HWND): HICON;
var
  Icon: HICON;
begin
  Icon := 0;
  SendMessageTimeout(hwnd, WM_GETICON, ICON_SMALL2, 0, SMTO_ABORTIFHUNG, 1000, PDWORD_PTR(@Icon));

  if Icon = 0 then
    SendMessageTimeout(hwnd, WM_GETICON, ICON_SMALL, 0, SMTO_ABORTIFHUNG, 1000, PDWORD_PTR(@Icon));

  if Icon = 0 then
    SendMessageTimeout(hwnd, WM_GETICON, ICON_BIG, 0, SMTO_ABORTIFHUNG, 1000, PDWORD_PTR(@Icon));

  if Icon = 0 then
    Icon := HICON(GetClassLong(hwnd, GCL_HICONSM));

  if Icon = 0 then
    Icon := HICON(GetClassLong(hwnd, GCL_HICON));

  if Icon = 0 then
    SendMessageTimeout(hwnd, WM_QUERYDRAGICON, 0, 0, 0, 1000, PDWORD_PTR(@Icon));

  Result := Icon;
end;


function get_window_icon_big(hwnd: HWND; allow_from_class: Boolean): HICON;
var
  Icon: HICON;
begin
  Icon := 0;
  SendMessageTimeout(hwnd, WM_GETICON, ICON_BIG, 0, SMTO_ABORTIFHUNG, 1000, PDWORD_PTR(@Icon));

  if Icon = 0 then
    SendMessageTimeout(hwnd, WM_GETICON, ICON_SMALL2, 0, SMTO_ABORTIFHUNG, 1000, PDWORD_PTR(@Icon));

  if Icon = 0 then
    SendMessageTimeout(hwnd, WM_GETICON, ICON_SMALL, 0, SMTO_ABORTIFHUNG, 1000, PDWORD_PTR(@Icon));

  if allow_from_class then
  begin
    if Icon = 0 then
      Icon := HICON(GetClassLong(hwnd, GCL_HICON));

    if Icon = 0 then
      Icon := HICON(GetClassLong(hwnd, GCL_HICONSM));
  end;

  if Icon = 0 then
    SendMessageTimeout(hwnd, WM_QUERYDRAGICON, 0, 0, 0, 1000, PDWORD_PTR(@Icon));

  Result := Icon;
end;
{$ENDREGION}

{$region 'TTaskbarButtonList'}

function TTaskbarButtonList.Add(const AButton: TTaskbarButton): Integer;
begin
  Result:=inherited Add(AButton);
end;

procedure TTaskbarButtonList.Delete(const AIndex: Integer);
begin
  Items[AIndex].Free;
  inherited Delete(AIndex);
end;

destructor TTaskbarButtonList.Destroy;
var
  LIndex : Integer;
begin
  for LIndex:=Count-1 downto 0 do
    Delete(LIndex);
  inherited;
end;

function TTaskbarButtonList.First: TTaskbarButton;
begin
  Result:=inherited First;
end;

function TTaskbarButtonList.Get(AIndex: Integer): TTaskbarButton;
begin
  Result:=inherited Get(AIndex);
end;

function TTaskbarButtonList.IndexOf(const AButton: TTaskbarButton): Integer;
begin
  Result:=inherited IndexOf(AButton);
end;

procedure TTaskbarButtonList.Insert(const AIndex: Integer;
  const AButton: TTaskbarButton);
begin
  inherited Insert(AIndex,AButton);
end;

function TTaskbarButtonList.Last: TTaskbarButton;
begin
  Result:=inherited Last;
end;

procedure TTaskbarButtonList.Put(AIndex: Integer; const Value: TTaskbarButton);
begin
  inherited Put(AIndex, Value);
end;

function TTaskbarButtonList.Remove(const AButton: TTaskbarButton): Integer;
begin
  Result:=inherited Remove(AButton);
end;

{$endregion}


{$region 'TTaskbar'}

function TTaskbar.AddButtonIconToImageList(AButton: TTaskbarButton;
  AImageList: TImageList): Integer;
var
  LIcon : Ticon;
begin
  if ButtonExists(AButton) then
  begin
    LIcon:=TIcon.Create;
    LIcon.Handle:=get_window_icon_small(TTaskbar.CAppWindowOfButton(AButton.Data));
    Result:=AImageList.AddIcon(LIcon);
    LIcon.Free;
  end
  else
    InvalidButton;
end;

function TTaskbar.ButtonExists(AButton: TTaskbarButton): Boolean;
begin
  Result:=Assigned(AButton) and (IndexOfButtonWith(AButton.Command,AButton.Data)>-1);
end;

function TTaskbar.ButtonIsGroupButton(AButton: TTaskbarButton): Boolean;
begin
  Result:=false;
  if ButtonExists(AButton) then
    Result:=TTaskbar.CAppWindowOfButton(AButton.Data)=0
  else
    InvalidButton;
end;

function TTaskbar.ButtonRect(AButton: TTaskbarButton): TRect;
begin
  if ButtonExists(AButton) then
    Taskbar.CGetButtonRect(Buttons.IndexOf(AButton),@Result)
  else
    InvalidButton;
end;

function TTaskbar.ButtonVisible(AButton: TTaskbarButton): Boolean;
begin
  if ButtonExists(AButton) then
    Result:=SendMessage(TTaskbar.CWindowHandle,
                        TB_ISBUTTONHIDDEN,
                        WParam(AButton.Command),
                        0)=0
  else
    InvalidButton;
end;

class function TTaskbar.CAppWindowOfButton(AData: NativeUInt): HWND;
//gets the handle of the window, the button points to
var
  LTBProcessHandle : NativeUInt;
  LWindowHandle : HWND;
  LBytesRead : NativeUInt;
begin
  Result:=0;
  
  //open remote process
  LTBProcessHandle:=TTaskBar.COpenTaskbarProcess;

  ReadProcessMemory(LTBProcessHandle,
                    Pointer(AData),
                    @LWindowHandle,
                    SizeOf(HWND),
                    LBytesRead);
  if LBytesRead<>SizeOf(HWND) then
    raise ETaskbarError.Create('Error while reading window handle'+ReasonSysError);

  Result:=LWindowHandle;

  //close remote process
  CloseHandle(LTBProcessHandle);
end;

class function TTaskbar.CButtonText(ACommand: Integer): WideString;
//retrives the text of the button
var
  LTBProcessHandle : THandle;
  LRemoteBuffer : Pointer;
  LLocalBuffer  : array of WideChar;
  LBytesRead : NativeUInt;
  LTextLen : Integer;
begin
  LTBProcessHandle:=TTaskBar.COpenTaskbarProcess;

  //get the length of the text
  LTextLen:=SendMessage(TTaskbar.CWindowHandle,
                        TB_GETBUTTONTEXTW,
                        WParam(ACommand),
                        LParam(0))*SizeOf(WideChar)+SizeOf(WideChar);

  //allocate memory for the text in the remote process
  LRemoteBuffer:=VirtualAllocEx(LTBProcessHandle,
                                nil,
                                LTextLen,
                                MEM_COMMIT,
                                PAGE_READWRITE);
  if not Assigned(LRemoteBuffer) then
    raise ETaskbarAllocMemError.Create('Error while reading button text'+ReasonSysError);

  //tell taskbar, to write out the text to the remote buffer
  if SendMessage(TTaskbar.CWindowHandle,
                 TB_GETBUTTONTEXTW,
                 WParam(ACommand),
                 LParam(LRemoteBuffer))=0 then
    raise ETaskbarError.Create('Error while fetching button text');

  //we need space in our process to store the button data
  SetLength(LLocalBuffer,LTextLen);
  try
    //copy the button data from the remote process into our process
    ReadProcessMemory(LTBProcessHandle,
                      LRemoteBuffer,
                      LLocalBuffer,
                      LTextLen,
                      LBytesRead);
    if LBytesRead<>LTextLen then
      raise ETaskbarError.Create('Error while reading button text'+ReasonSysError);

    Result:=PWideChar(LLocalBuffer);

    //clean the remote process
    VirtualFreeEx(LTBProcessHandle,
                  LRemoteBuffer,
                  LTextLen,
                  MEM_RELEASE);

    //close remote process
    CloseHandle(LTBProcessHandle);
  finally
    SetLength(LLocalBuffer,0);
  end;
end;

constructor TTaskbar.Create;
begin
  inherited;

  Buttons:=TTaskbarButtonList.Create;
end;

function TTaskbar.CreateButtonFromTaskbar(AIndex : Integer): TTaskbarButton;
var
  LButton : PTBButton;
begin
  New(LButton);
  try
    TTaskbar.CGetButton(AIndex,LButton);

    Result:=TTaskbarButton.Create;

    Result.Bitmap:=Lbutton.iBitmap;
    Result.Command:=LButton.idCommand;
    Result.State:=LButton.fsState;
    Result.Style:=LButton.fsStyle;
    Result.Data:=LButton.dwData;
    Result.Text:=LButton.iString;
  finally
    Dispose(LButton)
  end;
end;

destructor TTaskbar.Destroy;
begin
  Buttons.Free;
  
  inherited;
end;

procedure TTaskbar.DrawButton(AButton: TTaskbarButton; ADC: HDC; ARect : TRect);
var
  FullBitmap: TBitmap;
  LRect : TRect;
begin
  LRect:=Taskbar.ButtonRect(AButton);

  FullBitmap:=TBitmap.Create;
  FullBitmap.PixelFormat:=pf32bit;
  FullBitmap.Width:=Taskbar.Rect.Right-Taskbar.Rect.Left;
  FullBitmap.Height:=Taskbar.Rect.Bottom-Taskbar.Rect.Top;

  PrintWindow(TTaskbar.CWindowHandle, FullBitmap);

  BitBlt(ADC,
         ARect.Left,
         ARect.Top,
         ARect.Right-ARect.Left,
         ARect.Bottom-ARect.Top,
         FullBitmap.Canvas.Handle,
         LRect.Left, LRect.Top, SRCCOPY);
  FullBitmap.Free;
end;

function TTaskbar.GetButtonText(AButton: TTaskbarButton): WideString;
begin
  if ButtonExists(AButton) then
    Result:=TTaskbar.CButtonText(AButton.Command)
  else
    InvalidButton;
end;

function TTaskbar.IndexOfButtonWith(ACommand: Integer;
  AData: NativeUInt): Integer;
var
  LIndex : Integer;
begin
  Result:=-1;

  for LIndex:=0 to Buttons.Count-1 do
  begin
    if (Buttons[LIndex].Command=ACommand) and
       (Buttons[LIndex].Data=AData) then
    begin
      Result:=LIndex;
      break;
    end;
  end;
end;

procedure TTaskbar.MoveButton(OldIndex, NewIndex: Integer);
begin
  if SendMessage(TTaskbar.CWindowHandle,
                 TB_MOVEBUTTON,
                 WParam(OldIndex),
                 LParam(NewIndex))<>0 then
    ReadFromTaskbar;
end;

procedure TTaskbar.MoveButton(AButton: TTaskbarButton; NewIndex: Integer);
begin
  if ButtonExists(AButton) then
    MoveButton(Buttons.IndexOf(AButton),NewIndex);
end;

procedure TTaskbar.ReadFromTaskbar;
var
  LIndex : Integer;
  LButton : TTaskbarButton;
  LLocalIndex : Integer;
begin
  for LIndex:=0 to TTaskbar.CButtonCount-1 do
  begin
    LButton:=CreateButtonFromTaskbar(LIndex);
    LLocalIndex:=IndexOfButtonWith(LButton.Command,LButton.Data);
    if LLocalIndex=-1 then //the button is new to us
      Buttons.Insert(LIndex,LButton)
    else //we know this button, but maby at another index
    begin
      Buttons.Move(LLocalIndex,LIndex);
      LButton.Free;
    end;
  end;

  //every button in our list, which index is greater then the real button count is a ghost
  for LIndex:= Buttons.Count-1 downto TTaskbar.CButtonCount do
    Buttons.Delete(LIndex);
end;

function TTaskbar.Rect: TRect;
begin
  GetWindowRect(TTaskbar.CWindowHandle,Result);
end;

procedure TTaskbar.Refresh;
begin
  Self.ReadFromTaskbar;
end;

procedure TTaskbar.SetButtonVisible(AButton: TTaskbarButton; AVisible : Boolean);
begin
  if ButtonExists(AButton) then
    SendMessage(TTaskbar.CWindowHandle,
                TB_HIDEBUTTON,
                WParam(AButton.Command),
                IfThen(AVisible,0,1))
  else
    InvalidButton;
end;

class procedure TTaskbar.CGetButton(AButtonIndex: Integer; AButton: PTBButton);
//retrives the raw button data
var
  LTBProcessHandle : NativeUInt;
  LRemoteButton  : PTBButton;
  LBytesRead : NativeUInt;
  LButton : PTBButton;
begin
  LTBProcessHandle:=TTaskbar.COpenTaskbarProcess;

  //allocate memory for the button data in the remote process
  LRemoteButton:=VirtualAllocEx(LTBProcessHandle,
                                nil,
                                SizeOf(TTBButton),
                                MEM_COMMIT,
                                PAGE_READWRITE);
  if not Assigned(LRemoteButton) then
    raise ETaskbarAllocMemError.Create('Error while reading button data'+ReasonSysError);

  //tell taskbar, to write out the button data
  if SendMessage(TTaskbar.CWindowHandle,
                 TB_GETBUTTON,
                 WParam(AButtonIndex),
                 LParam(LRemoteButton))=0 then
    raise ETaskbarError.Create('Fetching button data failed'+ReasonSysError);

  //we need space in our process to store the button data
  New(LButton);
  try
    //copy the button data from the remote process into our process
    ReadProcessMemory(LTBProcessHandle,
                      LRemoteButton,
                      LButton,
                      SizeOf(TTBButton),
                      LBytesRead);
    if LBytesRead<>SizeOf(TTBButton) then
      raise ETaskbarError.Create('Reading button data failed'+ReasonSysError);

    //copy the button data into the supplied parameter
    CopyMemory(AButton,LButton,SizeOf(TTBButton)); 

    //clean the remote process
    VirtualFreeEx(LTBProcessHandle,
                  LRemoteButton,
                  SizeOf(TTBButton),
                  MEM_RELEASE);

    //close remote process
    CloseHandle(LTBProcessHandle);
  finally
    Dispose(LButton);
  end;
end;

class procedure TTaskbar.CGetButtonRect(AButtonIndex: Integer; ARect: PRect);
var
  LTBProcessHandle : NativeUInt;
  LRemoteRect : Pointer;
  LLocalRect : PRect;
  LBytesRead : NativeUInt;
begin
  //open remote process
  LTBProcessHandle:=TTaskbar.COpenTaskbarProcess;

  LRemoteRect:=VirtualAllocEx(LTBProcessHandle,
                              nil,
                              SizeOf(TRect),
                              MEM_COMMIT,
                              PAGE_READWRITE);
  if not Assigned(LRemoteRect) then
    raise ETaskbarAllocMemError.Create('Error while reading button rect'+ReasonSysError);

  SendMessage(TTaskbar.CWindowHandle,
              TB_GETITEMRECT,
              WParam(AButtonIndex),
              LParam(LRemoteRect));

  New(LLocalRect);

  ReadProcessMemory(LTBProcessHandle,
                    LRemoteRect,
                    LLocalRect,
                    SizeOf(TRect),
                    LBytesRead);
  if LBytesRead<>SizeOf(TRect) then
    raise ETaskbarError.Create('Reading button rect failed');

  CopyMemory(ARect,LLocalRect,SizeOf(TRect));

  Dispose(LLocalRect);

  VirtualFreeEx(LTBProcessHandle,
                LRemoteRect,
                SizeOf(TRect),
                MEM_RELEASE);

  //close remote process
  CloseHandle(LTBProcessHandle);
end;

class function TTaskbar.COpenTaskbarProcess: NativeUInt;
//opens the taskbars owner process for any task ;)
var
  LTBProcessID     : DWORD;
  LTBProcessHandle : HWND;
begin
  //find the PID of the owner process of the taskbar
  GetWindowThreadProcessId(TTaskbar.CWindowHandle,LTBProcessID);
  if LTBProcessID=0 then
    raise ETaskbarError.Create('PID of taskbar process not found');

  //open the process
  LTBProcessHandle:=OpenProcess(PROCESS_ALL_ACCESS,true,LTBProcessID);
  if LTBProcessHandle=0 then
    raise ETaskbarError.Create('Open taskbar process failed'+ReasonSysError);

  Result:=LTBProcessHandle;
end;

class function TTaskbar.CButtonCount: Integer;
//gets the amount of buttons in the taskbar (includes also invisible group-buttons)
begin
  Result:=SendMessage(TTaskbar.CWindowHandle,TB_BUTTONCOUNT,0,0);
end;

class function TTaskbar.CWindowHandle: HWND;
//Retrieves the window handle of the taskbar
var
  LHandle : HWND;
begin
  LHandle:=GetDesktopWindow;
  LHandle:=FindWindowEx(LHandle, 0, 'Shell_TrayWnd', nil);
  LHandle:=FindWindowEx(LHandle, 0, 'ReBarWindow32', nil);
  LHandle:=FindWindowEx(LHandle, 0, 'MSTaskSwWClass', nil);
  LHandle:=FindWindowEx(LHandle, 0, 'ToolbarWindow32', nil);

  if LHandle=0 then raise ETaskbarError.Create('Taskbar not found!');

  Result:=LHandle;
end;

{$endregion}



initialization
begin
  Taskbar:=TTaskbar.Create;
end;

finalization
begin
  Taskbar.Free;
end;

end. { Designed by TheUnknownOnes.net }