//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit uDeskbandTypes;

interface

uses
  Windows, Messages, Classes, ActiveX, ComServ, ComObj, ShlObj,
  SHDocVw,
  Graphics, Forms, Contnrs, Sysutils, Registry;

type
  TDeskBandFormModeFlag=(mfNormal,
                         mfVariableHeight,
                         mfDebossed,
                         mfBKColor);
  TDeskBandFormModeFlags = set of TDeskBandFormModeFlag;

  TDeskBand = class;

  TDeskbandForm = class(TForm)
  private
    FDeskBand : TDeskBand;
  public
    {$REGION 'Override this'}
    class function _GetGUID : TGUID; virtual;
    //return a GUID for this deskband

    class function _GetBandName : WideString; virtual;
    //return a name for the band (will be shown in the "Add band" menu)

    class function _GetBandTitle : WideString; virtual;
    //return a title for the band (max. 254 chars)

    procedure _GetSizingSteps(out X, Y : Longint); virtual;
    //only used if mfVariableHeight is set
    function _GetBackColor : TColor; virtual;
    //only used if mfBKColor is set
    function _GetModeFlags : TDeskBandFormModeFlags; virtual;

    //use to notify Form of de-/activation
    procedure _NotifyUIActivate(AActive : Boolean); Virtual;
    {$ENDREGION}
  published
    property DeskBand : TDeskBand read FDeskBand;
  end;

  TDeskBandFormClass = class of TDeskBandForm;



  TDeskBandFormFactory = class(TComObjectFactory)
  private
    FDeskBandFormClass : TDeskBandFormClass;
  public
    constructor Create(ADeskBandFormClass : TDeskBandFormClass); reintroduce;

    procedure UpdateRegistry(Register: Boolean); override;

    property DeskBandFormClass : TDeskBandFormClass read FDeskBandFormClass;
  end;

  

  TDeskBand = class(TComObject, IDeskBand, IPersist, IPersistStream,
                    IPersistStreamInit, IObjectWithSite, IContextMenu, IInputObject)
  private
    FDeskBandFormClass : TDeskBandFormClass;
    FDeskBandForm : TDeskbandForm;

    FParentWindow : HWND;
    FInternetExplorer : IWebBrowser2;
    FSite : IInputObjectSite;
    FCommandTarget : IOleCommandTarget;

    FBandID : DWORD; //comes via GetBandInfo

    FHasFocus : Boolean;

    OrigWndProc : TWndMethod;

    procedure WndProc(var Msg : TMessage);

    procedure CreateDeskBandForm;
    procedure HookDeskBandForm;
    procedure FreeDeskBandForm;

    procedure FocusChange(AHasFocus: Boolean);

    function GetWindow(out wnd: HWnd): HResult; stdcall;
    function ContextSensitiveHelp(fEnterMode: BOOL): HResult; stdcall;
    function ShowDW(fShow: BOOL): HResult; stdcall;
    function CloseDW(dwReserved: DWORD): HResult; stdcall;
    function ResizeBorderDW(var prcBorder: TRect; punkToolbarSite: IUnknown; fReserved: BOOL): HResult; stdcall;
    function GetBandInfo(dwBandID, dwViewMode: DWORD; var pdbi: TDeskBandInfo): HResult; stdcall;

    function GetClassID(out classID: TCLSID): HResult; stdcall;

    function IsDirty: HResult; stdcall;
    function Load(const stm: IStream): HResult; stdcall;
    function Save(const stm: IStream; fClearDirty: BOOL): HResult; stdcall;
    function GetSizeMax(out cbSize: Largeint): HResult; stdcall;

    function InitNew: HResult; stdcall;

    function SetSite(const pUnkSite: IUnknown ):HResult; stdcall;
    function GetSite(const riid: TIID; out site: IUnknown):HResult; stdcall;

    function QueryContextMenu(Menu: HMENU; indexMenu, idCmdFirst, idCmdLast, uFlags: UINT): HResult; stdcall;
    function InvokeCommand(var lpici: TCMInvokeCommandInfo): HResult; stdcall;
    function GetCommandString(idCmd, uType: UINT; pwReserved: PUINT; pszName: LPSTR; cchMax: UINT): HResult; stdcall;

    function UIActivateIO(fActivate: BOOL; var lpMsg: TMsg): HResult; stdcall;
    function HasFocusIO: HResult; stdcall;
    function TranslateAcceleratorIO(var lpMsg: TMsg): HResult; stdcall;
    function GetFocused: Boolean;
  public
    destructor Destroy(); override;

    procedure Initialize; override;

    procedure UpdateBandInfo;

    property InternetExplorer : IWebBrowser2 read FInternetExplorer;
    property Focused : Boolean read GetFocused;
  end;


  TDeskBandFormClassEnum = class;

  TDeskBandFormClassList = class(TClassList)
  private
    function GetItems(Index: Integer): TDeskBandFormClass;
    procedure SetItems(Index: Integer; const Value: TDeskBandFormClass);
  public
    function Add(AClass: TDeskBandFormClass): Integer;
    function Extract(Item: TDeskBandFormClass): TDeskBandFormClass;
    function Remove(AClass: TDeskBandFormClass): Integer;
    function IndexOf(AClass: TDeskBandFormClass): Integer;
    function First: TDeskBandFormClass;
    function GetEnumerator: TDeskBandFormClassEnum;
    function Last: TDeskBandFormClass;
    procedure Insert(Index: Integer; AClass: TDeskBandFormClass);
    property Items[Index: Integer]: TDeskBandFormClass read GetItems write SetItems; default;
  end;

  TDeskBandFormClassEnum = class
  private
    FIndex: Integer;
    FList: TDeskBandFormClassList;
  public
    constructor Create(AList : TDeskBandFormClassList);
    function GetCurrent: TDeskBandFormClass;
    function MoveNext: Boolean;
    property Current: TDeskBandFormClass read GetCurrent;
  end;

implementation

uses Controls;

const
  REGKEY_DESK_BAND = '{00021492-0000-0000-C000-000000000046}';

{ TDeskbandForm }

function TDeskbandForm._GetBackColor: TColor;
begin
  Result:=clNone;
end;

class function TDeskbandForm._GetBandName: WideString;
begin
  Result:='DeskbandForm-Name';
end;

class function TDeskbandForm._GetBandTitle: WideString;
begin
  Result:='DeskBandForm-Title'
end;

class function TDeskbandForm._GetGUID: TGUID;
begin
  Assert(true, 'Supply a valid GUID via "_GetGUID" (press Shift-Ctrl-G in the IDE)');
end;

function TDeskbandForm._GetModeFlags: TDeskBandFormModeFlags;
begin
  Result:=[mfNormal, mfVariableHeight, mfDebossed];
end;

procedure TDeskbandForm._NotifyUIActivate(AActive : Boolean); 
begin

end;

procedure TDeskbandForm._GetSizingSteps(out X, Y: Integer);
begin
  Assert(true, 'Override "_GetSizingSteps" please');
end;

{ TDeskBandFormClassList }

function TDeskBandFormClassList.Add(AClass: TDeskBandFormClass): Integer;
begin
  Result:=inherited Add(AClass);
end;

function TDeskBandFormClassList.Extract(Item: TDeskBandFormClass): TDeskBandFormClass;
begin
  Result:=TDeskBandFormClass(inherited Extract(Item));
end;

function TDeskBandFormClassList.First: TDeskBandFormClass;
begin
  Result:=TDeskBandFormClass(inherited First);
end;

function TDeskBandFormClassList.GetEnumerator: TDeskBandFormClassEnum;
begin
  Result:=TDeskBandFormClassEnum.Create(Self);
end;

function TDeskBandFormClassList.GetItems(Index: Integer): TDeskBandFormClass;
begin
  Result:=TDeskBandFormClass(inherited GetItems(Index));
end;

function TDeskBandFormClassList.IndexOf(AClass: TDeskBandFormClass): Integer;
begin
  Result:=inherited IndexOf(AClass);
end;

procedure TDeskBandFormClassList.Insert(Index: Integer; AClass: TDeskBandFormClass);
begin
  inherited Insert(Index, AClass);
end;

function TDeskBandFormClassList.Last: TDeskBandFormClass;
begin
  Result:=TDeskBandFormClass(inherited Last);
end;

function TDeskBandFormClassList.Remove(AClass: TDeskBandFormClass): Integer;
begin
  Result:=inherited Remove(AClass);
end;

procedure TDeskBandFormClassList.SetItems(Index: Integer; const Value: TDeskBandFormClass);
begin
  inherited SetItems(Index, Value);
end;


{ TDeskBandFactory }

constructor TDeskBandFormFactory.Create(ADeskBandFormClass: TDeskBandFormClass);
begin
  FDeskBandFormClass := ADeskBandFormClass;

  inherited Create(ComServ.ComServer,
                   TDeskBand,
                   FDeskBandFormClass._GetGUID,
                   '',
                   FDeskBandFormClass._GetBandTitle,
                   ciMultiInstance);
end;

{ TDeskBand }

function TDeskBand.CloseDW(dwReserved: DWORD): HResult;
begin
  if Assigned(FDeskBandForm) then
    FDeskBandForm.Hide;
    
  Result := S_OK;
end;

function TDeskBand.ContextSensitiveHelp(fEnterMode: BOOL): HResult;
begin
  Result := E_NOTIMPL;
end;

procedure TDeskBand.CreateDeskBandForm;
begin
  FDeskBandForm:=FDeskBandFormClass.CreateParented(FParentWindow);
  FDeskBandForm.FDeskBand:=Self;
  FDeskBandForm.Show;
end;

destructor TDeskBand.Destroy;
begin
  FreeDeskBandForm;

  FSite:=nil;
  FCommandTarget:=nil;
  FInternetExplorer:=nil;
  
  inherited;
end;

procedure TDeskBand.FocusChange(AHasFocus: Boolean);
begin
  if FSite <> nil then
    FSite.OnFocusChangeIS(Self, FHasFocus);

  FDeskBandForm._NotifyUIActivate(AHasFocus);
end;

procedure TDeskBand.FreeDeskBandForm;
begin
  if Assigned(FDeskBandForm) then
  begin
    FDeskBandForm.Free;
    FDeskBandForm:=nil;
  end;
end;

function TDeskBand.GetBandInfo(dwBandID, dwViewMode: DWORD;
  var pdbi: TDeskBandInfo): HResult;
var
  ModeFlags : TDeskBandFormModeFlags;
  Title : WideString;
begin
  FBandID:=dwBandID;

  if Assigned(FDeskBandForm) then
  begin
    if pdbi.dwMask or DBIM_MINSIZE <> 0 then
    begin
      pdbi.ptMinSize.x := FDeskBandForm.Constraints.MinWidth;
      pdbi.ptMinSize.y := FDeskBandForm.Constraints.MinHeight;
    end;

    if pdbi.dwMask or DBIM_MAXSIZE <> 0 then
    begin
      if FDeskBandForm.Constraints.MaxWidth>0 then
        pdbi.ptMaxSize.x := FDeskBandForm.Constraints.MaxWidth
      else
        pdbi.ptMaxSize.x := -1;

      if FDeskBandForm.Constraints.MaxHeight>0 then
        pdbi.ptMaxSize.y:=FDeskBandForm.Constraints.MaxHeight
      else
        pdbi.ptMaxSize.y := -1;
    end;

    if pdbi.dwMask or DBIM_INTEGRAL <> 0 then
      FDeskBandForm._GetSizingSteps(pdbi.ptMinSize.x, pdbi.ptMinSize.y);

    if pdbi.dwMask or DBIM_ACTUAL <> 0 then
    begin
      pdbi.ptMinSize.x := FDeskBandForm.Width;
      pdbi.ptMinSize.y := FDeskBandForm.Height;
    end;

    if pdbi.dwMask or DBIM_TITLE <> 0 then
    begin
      Title := FDeskBandForm._GetBandTitle;
      FillChar(pdbi.wszTitle, Length(pdbi.wszTitle) * SizeOf(pdbi.wszTitle[0]), #0);
      FillChar(pdbi.wszTitle, SizeOf(Title) + 1, ' ');

      StringToWideChar(Title, @pdbi.wszTitle, Length(Title) + 1);
    end;

    if pdbi.dwMask or DBIM_BKCOLOR <> 0 then
      pdbi.crBkgnd := FDeskBandForm._GetBackColor;

    if pdbi.dwMask or DBIM_MODEFLAGS <>0 then
    begin
      ModeFlags := fDeskBandForm._GetModeFlags;

      pdbi.dwModeFlags:=0;
      if mfNormal in ModeFlags then
        pdbi.dwModeFlags := pdbi.dwModeFlags or DBIMF_NORMAL;

      if mfVariableHeight in ModeFlags then
        pdbi.dwModeFlags := pdbi.dwModeFlags or DBIMF_VARIABLEHEIGHT;

      if mfDebossed in ModeFlags then
        pdbi.dwModeFlags := pdbi.dwModeFlags or DBIMF_DEBOSSED;

      if mfBKColor in ModeFlags then
        pdbi.dwModeFlags := pdbi.dwModeFlags or DBIMF_BKCOLOR;
    end;
    
    Result:=NOERROR;
  end
  else
    Result:=E_FAIL;
end;

function TDeskBand.GetClassID(out classID: TCLSID): HResult;
begin
  classID := FDeskBandFormClass._GetGUID;
  Result:=S_OK;
end;

function TDeskBand.GetCommandString(idCmd, uType: UINT; pwReserved: PUINT;
  pszName: LPSTR; cchMax: UINT): HResult;
begin
  Result := NOERROR;
end;

function TDeskBand.GetFocused: Boolean;
begin
  Result:=HasFocusIO=1;
end;

function TDeskBand.GetSite(const riid: TIID;
  out site: IInterface): HResult;
begin
  if Site <> nil then
    Result := Site.QueryInterface(riid, site)
  else
    Result := E_FAIL;
end;

function TDeskBand.GetSizeMax(out cbSize: Largeint): HResult;
begin
  cbSize:=256;
  Result:=S_OK;
end;

function TDeskBand.GetWindow(out wnd: HWnd): HResult;
begin
  CreateDeskBandForm;
  HookDeskBandForm;

  wnd:=FDeskBandForm.Handle;
  Result:=S_OK;
end;

function TDeskBand.HasFocusIO: HResult;
begin
  Result := Integer(not FHasFocus);
end;

procedure TDeskBand.HookDeskBandForm;
begin
  OrigWndProc:=FDeskBandForm.WindowProc;
  FDeskBandForm.WindowProc:=WndProc;  
end;

procedure TDeskBand.Initialize;
begin
  inherited;

  if Factory is TDeskBandFormFactory then
    FDeskBandFormClass:=TDeskBandFormFactory(Factory).DeskBandFormClass
  else
    raise Exception.Create('Can not create Deskband without DeskBandFormFactory');

  FDeskBandForm:=nil;
end;

function TDeskBand.InitNew: HResult;
begin
  Result:=S_OK;
end;

function TDeskBand.InvokeCommand(var lpici: TCMInvokeCommandInfo): HResult;
begin
  Result:=E_NOTIMPL;
  //todo: implement
end;

function TDeskBand.IsDirty: HResult;
begin
  Result:=S_OK;
end;

function TDeskBand.Load(const stm: IStream): HResult;
begin
  Result:=S_OK;
end;

function TDeskBand.QueryContextMenu(Menu: HMENU; indexMenu, idCmdFirst,
  idCmdLast, uFlags: UINT): HResult;
begin
  Result:=0;
end;

function TDeskBand.ResizeBorderDW(var prcBorder: TRect;
  punkToolbarSite: IInterface; fReserved: BOOL): HResult;
begin
  Result:=E_NOTIMPL;
end;

function TDeskBand.Save(const stm: IStream; fClearDirty: BOOL): HResult;
begin
  Result:=S_OK;
end;

function TDeskBand.SetSite(const pUnkSite: IInterface): HResult;
begin
  if pUnkSite <> nil then
  begin
    FSite := pUnkSite as IInputObjectSite;
    (pUnkSite as IOleWindow).GetWindow(FParentWindow);
    FCommandTarget := pUnkSite as IOleCommandTarget;
    (FCommandTarget as IServiceProvider).QueryService(IWebbrowserApp, IWebbrowser2, FInternetExplorer);
  end;
  Result := S_OK;
end;

function TDeskBand.ShowDW(fShow: BOOL): HResult;
begin
  FHasFocus:=fShow;
  FocusChange(FHasFocus);
  Result:=S_OK;
end;

function TDeskBand.TranslateAcceleratorIO(var lpMsg: TMsg): HResult;
begin
  if lpMsg.WParam <> VK_TAB then
  begin
    TranslateMessage(lpMSg);
    DispatchMessage(lpMsg);
    Result := S_OK;
  end
  else
  begin
    Result := S_FALSE;
  end;
end;

function TDeskBand.UIActivateIO(fActivate: BOOL; var lpMsg: TMsg): HResult;
begin
  FHasFocus := fActivate;
  FocusChange(FHasFocus);
  if FHasFocus then
    if Assigned(FDeskBandForm) then
      FDeskBandForm.SetFocus;

  Result := S_OK;
end;

procedure TDeskBand.UpdateBandInfo;
var
  vain, vaOut: OleVariant;
  PtrGuid: PGUID;
begin
  vaIn := Variant(FBandID);
  New(PtrGUID);
  PtrGUID^ := IDESKBAND;
  FCommandTarget.Exec(PtrGUID, DBID_BANDINFOCHANGED, OLECMDEXECOPT_DODEFAULT, vaIn, vaOut);
  Dispose(PtrGUID);
end;

procedure TDeskBand.WndProc(var Msg: TMessage);
begin
  if (Msg.Msg = WM_PARENTNOTIFY)  then
  begin
    FHasFocus := True;
    FocusChange(FHasFocus);
  end;

  OrigWndProc(Msg);
end;

{ TDeskBandFormClassEnum }

constructor TDeskBandFormClassEnum.Create(AList: TDeskBandFormClassList);
begin
  FList:=AList;
  FIndex:=-1;
end;

function TDeskBandFormClassEnum.GetCurrent: TDeskBandFormClass;
begin
  Result:=FList[Findex];
end;

function TDeskBandFormClassEnum.MoveNext: Boolean;
begin
  Result:=FIndex<Pred(FList.Count);
  if Result then
    Inc(FIndex);
end;

procedure TDeskBandFormFactory.UpdateRegistry(Register: Boolean);
var
  GUID: string;
begin
  inherited UpdateRegistry(Register);
  GUID := GUIDToString(DeskBandFormClass._GetGUID);
  with TRegistry.Create do
  try
    if Register then
    begin
      // das Desk-Band wird installiert

      // Registrierung der COM Komponente
      RootKey := HKEY_CLASSES_ROOT;
      if OpenKey('CLSID\' + GUID, True) then
      try
        WriteString('', DeskBandFormClass._GetBandName);
      finally
        CloseKey;
      end;
      if OpenKey('CLSID\' + GUID + '\InProcServer32', True) then
      try
        WriteString('ThreadingModel', 'Apartment');
      finally
        CloseKey;
      end;
      if OpenKey('CLSID\' + GUID + '\Implemented Categories\' + REGKEY_DESK_BAND, True) then
        CloseKey;

      // Registrierung der COM Komponente im Internet Explorer
      RootKey := HKEY_LOCAL_MACHINE;
      if OpenKey('SOFTWARE\Microsoft\Internet Explorer\Toolbar', True) then
      try
        WriteString(GUID, '');
      finally
        CloseKey;
      end;
    end
    else
    begin
      // das Desk-Band wird deinstalliert
      RootKey := HKEY_CLASSES_ROOT;
      DeleteKey('Component Categories\' + REGKEY_DESK_BAND + '\Enum');
      DeleteKey('CLSID\' + GUID + '\Implemented Categories\' + REGKEY_DESK_BAND);
      DeleteKey('CLSID\' + GUID + '\InProcServer32');
      DeleteKey('CLSID\' + GUID);
      CloseKey;

      RootKey := HKEY_LOCAL_MACHINE;
      if OpenKey('Software\Microsoft\Internet Explorer\Toolbar', False) then
      try
        DeleteValue(GUID);
      finally
        CloseKey;
      end;
    end;
  finally
    Free;
    Halt(0);
  end;
end;

end.
