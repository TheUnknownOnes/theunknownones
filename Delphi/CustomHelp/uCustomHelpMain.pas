unit uCustomHelpMain;

interface

uses
  Classes,
  Forms,
  Dialogs,
  ToolsAPI,
  Menus,
  Registry,
  HelpIntfs, Windows;

type
  TCustomHelp = class
  protected
    FHelpManager : IHelpManager;

    FMenuItem : TMenuItem;

    FProvider : TStringList;
    FShowMSHelpOnWP : Boolean;
    FShowCustHelpOnWP : Boolean;

    procedure ConnectToIDE;
    procedure DisconnectFromIDE;
    function GetHelpMenu : TMenuItem;

    procedure LoadProviderFromRegistry;
    procedure LoadSettingsFromRegistry;

    procedure OnMenuItemClick(Sender : TObject);
  public
    ViewerID : Integer;

    constructor Create();
    destructor Destroy(); override;

    property ProviderList : TStringList read FProvider;

    property ShowMsHelpOnWelcomePage: Boolean read FShowMSHelpOnWP;
    property ShowCustomHelpOnWelcomePage: Boolean read FShowCustHelpOnWP;

    class function DecodeURL(const URL: String; out Caption: String;
      out Description: String; out Link: String): boolean;

    class procedure WriteProviderToRegistry(AKeyName, AName, ADesc, AURL : String); 
    class procedure WriteSettingToRegistry(AName, AValue: String);
    class procedure ReadSettingsFromRegistry(const ANameValueList: TStrings);
  end;

  TMyViewer = class(TInterfacedObject, ICustomHelpViewer)
  private
    {$REGION 'ICustomHelpViewer'}
    function  GetViewerName : String;
    function  UnderstandsKeyword(const HelpString: String): Integer;
    function  GetHelpStrings(const HelpString: String): TStringList;
    function  CanShowTableOfContents : Boolean;
    procedure ShowTableOfContents;
    procedure ShowHelp(const HelpString: String);
    procedure NotifyID(const ViewerID: Integer);
    procedure SoftShutDown;
    procedure ShutDown;
    {$ENDREGION}
  end;

const
  CPROT = 'CustomHelp://';
  PROVIDER_ROOT_KEY = '\Software\TheUnknownOnes\Delphi\CustomHelp';
  VALUE_NAME = 'Name';
  VALUE_DESCR = 'Description';
  VALUE_URL = 'URL';

  SETTINGS_MSHELPWP = 'MsHelpOnWP';
  SETTINGS_CUSTHELPWP = 'CustomHelpOnWP';

var
  GlobalCustomHelp : TCustomHelp;

implementation

uses
  SysUtils, uCustomHelpSelector, StrUtils, ShellAPI, uFormConfigCustomHelp,
  uCustomHelpIDEIntegration, Graphics;

var
  vi : Integer;

{ TMyViewer }

function TMyViewer.CanShowTableOfContents: Boolean;
begin
  Result := true;
end;

function TMyViewer.GetHelpStrings(const HelpString: String): TStringList;
var
  idy : Integer;
  e : String;
  c, d, u : String;

  function EncodedHelpString: String;
  var
    idx: integer;
  begin
    Result:='';
    for idx := 1 to Length(HelpString)do
      Result:=Result + '%'+Format('%.2x', [Ord(HelpString[idx])]);
  end;

begin
  Result := TStringList.Create;
  Result.Assign(GlobalCustomHelp.ProviderList);

  e := EncodedHelpString;

  for idy := 0 to Result.Count - 1 do
  begin
    TCustomHelp.DecodeURL(Result[idy], c, d, u);

    if Pos('://', u)>0 then
      Result[idy] := Result[idy] + e
    else
    if AnsiSameText(ExtractFileExt(u),'.hlp') then
    begin
      TCustomHelp.DecodeURL(Result[idy], c, d, u);
      Result[idy] := CPROT + c + '|' + d + '|' + 'winhlp://-k '+HelpString+' '+u;
    end;
  end;
end;

function TMyViewer.GetViewerName: String;
begin
  Result := 'CustomHelpViewer';
end;

procedure TMyViewer.NotifyID(const ViewerID: Integer);
begin
  vi := ViewerID;
end;                  

procedure TMyViewer.ShowHelp(const HelpString: String);
var
  c,d,u: String;
  alternativeNavigate : boolean;
begin
  if TCustomHelp.DecodeURL(HelpString, c, d, u) then
  begin
    if Pos('winhlp://', u)=1 then
    begin
      Delete(u,1,9);
      ShellExecute(Application.Handle,
                   'open',
                   'winhlp32',
                   PChar(u),
                   PChar(ExtractFileDir(Application.ExeName)),
                   SW_SHOWNORMAL);
    end
    else
    begin
      alternativeNavigate := False;
      if GlobalCustomHelp.ShowCustomHelpOnWelcomePage then
      begin
        if not WelcomePageNavigate(u) then
          alternativeNavigate:=True;
      end
      else
        alternativeNavigate:=True;

      if alternativeNavigate then
        ShellExecute(Application.Handle, 'open', PChar(u), '', '', SW_SHOWNORMAL);
    end;
  end
end;

procedure TMyViewer.ShowTableOfContents;
begin
end;

procedure TMyViewer.ShutDown;
begin
end;

procedure TMyViewer.SoftShutDown;
begin  
end;

function TMyViewer.UnderstandsKeyword(const HelpString: String): Integer;
var
  hs : IHelpSystem;
begin
  Result := 1;

  if GetHelpSystem(hs) then
  begin
    hs.AssignHelpSelector(THelpSelector.Create);
  end;
end;

{ TCustomHelp }

procedure TCustomHelp.ConnectToIDE;
var
  HelpMenu : TMenuItem;
begin
  HelpMenu := GetHelpMenu;

  if Assigned(HelpMenu) then
  begin
    FMenuItem := TMenuItem.Create(HelpMenu);
    FMenuItem.Caption := 'Configure Custom Help ...';
    FMenuItem.OnClick := OnMenuItemClick;
    HelpMenu.Insert(1, FMenuItem);
  end;
end;

constructor TCustomHelp.Create;
var
  intf : ICustomHelpViewer;
begin
  FHelpManager := nil;
  intf:=TMyViewer.Create;

  FProvider := TStringList.Create;

  RegisterViewer(intf, FHelpManager);
  ViewerID:=vi;

  ConnectToIDE;

  LoadProviderFromRegistry;

  if FProvider.Count = 0 then
  begin
    WriteProviderToRegistry('1',
                            'DP DelphiReference',
                            'Search with Daniels Cool Tool',
                            'http://ref.dp200x.de/dp_reference.php?sourceid=captaincaveman&wsproencoding=ISO-8859-1&securitytoken=guest&tabbed=1&sbutton=Search&query=');

    WriteProviderToRegistry('2',
                            'Koders.com',
                            'Search at koders.com',
                            'http://www.koders.com/default.aspx?submit=Search&la=Delphi&li=*&s=');

    WriteProviderToRegistry('3',
                            'Google Codesearch',
                            'Search using Google Codesearch',
                            'http://www.google.com/codesearch?btnG=Code+suchen&hl=de&as_lang=pascal&as_license_restrict=i&as_license=&as_package=&as_filename=&as_case=&as_q=');

    LoadProviderFromRegistry;
  end;
end;

class function TCustomHelp.DecodeURL(const URL: String; out Caption,
  Description, Link: String): boolean;
var
  sl : TStringList;
begin
  Result:=False;
  if AnsiStartsText(CPROT, URL) then
  begin
    sl:=TStringList.Create;
    sl.QuoteChar:=#0;
    sl.Delimiter:='|';
    sl.StrictDelimiter:=True;
    sl.DelimitedText:=Copy(URL, Length(CProt)+1, Length(URL));
    Caption:=Sl[0];
    Description:=sl[1];
    Link:=sl[2];
    sl.Free;
    Result:=True;
  end;           
end;

destructor TCustomHelp.Destroy;
begin
  DisconnectFromIDE;

  FProvider.Free;

  if Assigned(FHelpManager) then
    FHelpManager.Release(ViewerID);
  
  inherited;
end;

procedure TCustomHelp.DisconnectFromIDE;
begin
  if Assigned(FMenuItem) then
    FMenuItem.Free;
end;

function TCustomHelp.GetHelpMenu: TMenuItem;
var
  NTA : INTAServices;
  idx : Integer;
begin
  Result := nil;

  if Supports(BorlandIDEServices, INTAServices, NTA) then
  begin
    for idx := 0 to NTA.MainMenu.Items.Count - 1 do
    begin
      if NTA.MainMenu.Items[idx].Name = 'HelpMenu' then
      begin
        Result := NTA.MainMenu.Items[idx];
        break;
      end;
    end;
  end;

end;

procedure TCustomHelp.LoadProviderFromRegistry;
var
  Reg : TRegistry;
  sl : TStringList;
  s : String;
begin
  LoadSettingsFromRegistry;

  FProvider.Clear;

  sl := TStringList.Create;

  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;

    if Reg.OpenKey(PROVIDER_ROOT_KEY, true) then
    begin
      Reg.GetKeyNames(sl);
      Reg.CloseKey;
    end;

    for s in sl do
    begin
      if Reg.OpenKey(PROVIDER_ROOT_KEY + '\' + s, false) then
      begin
        if trim(reg.ReadString(VALUE_NAME))<>EmptyStr then
          FProvider.Add(CPROT + Reg.ReadString(VALUE_NAME) + '|' +
                              Reg.ReadString(VALUE_DESCR) + '|' +
                              Reg.ReadString(VALUE_URL));

        Reg.CloseKey;
      end;
    end;
      
  finally
    Reg.Free;
    sl.Free;
  end;
end;

procedure TCustomHelp.LoadSettingsFromRegistry;
var
  sl : TStringList;
begin
  sl:=TStringList.Create;
  try
    ReadSettingsFromRegistry(sl);
    FShowMSHelpOnWP:=sl.Values[SETTINGS_MSHELPWP]='1';
    FShowCustHelpOnWP:=sl.Values[SETTINGS_MSHELPWP]='1';
  finally
    sl.Free;
  end;
end;

procedure TCustomHelp.OnMenuItemClick(Sender: TObject);
begin
  Tform_Config.Execute;
  LoadProviderFromRegistry;
end;

class procedure TCustomHelp.ReadSettingsFromRegistry(const ANameValueList: TStrings);
var
  Reg : TRegistry;
  idx: Integer;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;

    if Reg.OpenKey(PROVIDER_ROOT_KEY + '\Settings', true) then
    begin
      Reg.GetValueNames(ANameValueList);

      for idx := 0 to ANameValueList.count - 1 do
        ANameValueList[idx]:=ANameValueList[idx]+'='+Reg.ReadString(ANameValueList[idx]);

      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

class procedure TCustomHelp.WriteSettingToRegistry(AName, AValue: String);
var
  Reg : TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;

    if Reg.OpenKey(PROVIDER_ROOT_KEY + '\Settings', true) then
    begin
      Reg.WriteString(AName, AValue);

      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

class procedure TCustomHelp.WriteProviderToRegistry(AKeyName, AName, ADesc,
  AURL: String);
var
  Reg : TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;

    if Reg.OpenKey(PROVIDER_ROOT_KEY + '\' + AKeyName, true) then
    begin
      Reg.WriteString(VALUE_NAME, AName);
      Reg.WriteString(VALUE_DESCR, ADesc);
      Reg.WriteString(VALUE_URL, AURL);

      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

{$R Images.res}

var
  bmp : TBitmap;

procedure AddSpashBitmap;
begin
  bmp:=TBitmap.Create;
  bmp.LoadFromResourceName(hinstance, 'Splash');
  SplashScreenServices.AddPluginBitmap(
             'Custom Help',
             bmp.Handle,
             false,
             'Free and OpenSource',
             'by TheUnknownOnes');

end;

initialization
  GlobalCustomHelp:=TCustomHelp.Create;
  AddSpashBitmap;

finalization
  GlobalCustomHelp.Free;
  If Assigned(bmp) then
    bmp.Free;

end.
