{-----------------------------------------------------------------------------
 Purpose: The main unit of the custom help expert 

 (c) by TheUnknownOnes
 see http://www.TheUnknownOnes.net
-----------------------------------------------------------------------------}

unit uCustomHelpMain;

interface

uses
  Classes,
  Forms,
  Dialogs,
  ToolsAPI,
  Menus,
  Registry,
  HelpIntfs,
  Windows,
  uMSHelpServices;

type

  //Das Hauptobjekt
  TCustomHelp = class
  private
    function GetNamespaces: IHxRegNamespaceList;
  protected
    FHelpManager : IHelpManager;

    FMenuItem : TMenuItem;

    FProvider : TStringList;
    FShowMSHelpOnWP : Boolean;
    FShowCustHelpOnWP : Boolean;
    FEnabledIndices : TInterfaceList;

    //In die IDE einhacken (Menu-Eintrag, ...)
    procedure ConnectToIDE;
    procedure DisconnectFromIDE;
    function GetHelpMenu : TMenuItem;

    procedure LoadProviderFromRegistry;
    procedure LoadSettingsFromRegistry;
    procedure LoadEnabledNamespacesFromRegistry;

    procedure OnMenuItemClick(Sender : TObject);
  published
  public
    constructor Create();
    destructor Destroy(); override;

    property ProviderList : TStringList read FProvider;
    property Namespaces : IHxRegNamespaceList read GetNamespaces;
    property EnabledIndices : TInterfaceList read FEnabledIndices;

    property ShowMsHelpOnWelcomePage: Boolean read FShowMSHelpOnWP;
    property ShowCustomHelpOnWelcomePage: Boolean read FShowCustHelpOnWP;

    class function DecodeURL(const URL: String; out Caption: String;
      out Description: String; out Link: String; out Order: Integer): boolean;
    class function EncodeURL(Caption, Description, Link: String; Order: Integer): String;

    class procedure WriteProviderToRegistry(AKeyName, AName, ADesc, AURL : String);
    class procedure WriteNamespacesToRegistry(ANamespace: String; AEnabled: Boolean);
    class procedure ReadEnabledNamespacesFromRegistry(const ANamesList: TStrings);
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
  {$IfDef VER170}
    PROVIDER_ROOT_KEY = '\Software\TheUnknownOnes\Delphi\VER170\CustomHelp';
  {$EndIf}
  {$IfDef VER180}
    PROVIDER_ROOT_KEY = '\Software\TheUnknownOnes\Delphi\VER180\CustomHelp';
  {$EndIf}
  {$IfDef VER185}
    PROVIDER_ROOT_KEY = '\Software\TheUnknownOnes\Delphi\VER185\CustomHelp';
  {$EndIf}
  {$IfDef VER200}
    PROVIDER_ROOT_KEY = '\Software\TheUnknownOnes\Delphi\VER200\CustomHelp';
  {$EndIf}
  {$IfDef VER210}
    PROVIDER_ROOT_KEY = '\Software\TheUnknownOnes\Delphi\VER210\CustomHelp';
  {$EndIf}

  CPROT = 'CustomHelp://';
  VALUE_NAME = 'Name';
  VALUE_DESCR = 'Description';
  VALUE_URL = 'URL';

  SETTINGS_CUSTHELPWP = 'CustomHelpOnWP';

  NAMESPACES_ROOT = 'Namespaces';
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
  Result := false;
end;

function TMyViewer.GetHelpStrings(const HelpString: String): TStringList;
var
  idx, idy : Integer;
  e : String;
  c, d, u : String;
  o, order : Integer;
  Index : IHxIndex;
  Topics : IHxTopicList;
  slot : integer;

  function EncodedHelpString: String;
  var
    i: integer;
  begin
    Result:='';
    for i := 1 to Length(HelpString)do
      Result:=Result + '%'+Format('%.2x', [Ord(HelpString[i])]);
  end;

begin
  //Weil wir bei UnderstandsKeyword gesagt haben, das wir das Keyword verstehen (Result = 1)
  //werden wir jetzt gefragt, welche Hilfethemen wir zu diesem Keyword liefern können
  //Die StringList wird vom Hilfesystem wieder freigegeben

  Result := TStringList.Create;
  Result.Assign(GlobalCustomHelp.ProviderList);

  //Das Keyword in Hex kodieren ... der Einfachheit halber einfach alle Zeichen
  e := EncodedHelpString;

  order := 0;

  for idx := 0 to Result.Count - 1 do
  begin
    TCustomHelp.DecodeURL(Result[idx], c, d, u, o);
    inc(order);

    if Pos('://', u)>0 then
      Result[idx] := TCustomHelp.EncodeURL(c,d,u+EncodedHelpString, order)
    else
    if AnsiSameText(ExtractFileExt(u),'.hlp') then
    begin
      Result[idx] := TCustomHelp.EncodeURL(c,d,'winhlp://-k '+HelpString+' '+u, order);
    end;
  end;

  //Und jetzt noch die eigentlichen Hilfe-Namespaces durchsuchen
  for idx := 0 to GlobalCustomHelp.EnabledIndices.Count-1 do
  begin
    if supports(GlobalCustomHelp.EnabledIndices[idx], IHxIndex, Index) then
    begin
      slot:=Index.GetSlotFromString(HelpString);

      if AnsiContainsText(Index.GetStringFromSlot(slot),HelpString) then
      begin
        Topics:=Index.GetTopicsFromSlot(slot);

        for idy := 1 to Topics.Count do
        begin
          inc(order);
          Result.Add(TCustomHelp.EncodeURL(Topics.Item(idy).Title[HxTopicGetRLTitle,0],
                                                Topics.Item(idy).Location,
                                                Topics.Item(idy).URL,
                                                order));
        end;
      end;
    end;
  end;

end;

function TMyViewer.GetViewerName: String;
begin
  Result := 'CustomHelpViewer';
end;

procedure TMyViewer.NotifyID(const ViewerID: Integer);
begin
  //Das Hilfesystem sagt uns, welche ID wir bekommen haben
  //Die brauchen wir am Ende zum freigeben
  vi := ViewerID;
end;

procedure TMyViewer.ShowHelp(const HelpString: String);
var
  c,d,u: String;
  o: Integer;
  alternativeNavigate : boolean;
begin
  //Hier gehts dann wirklich um die Wurst
  //Wir bekommen den Hilfestring übergeben, den der
  //Nutzer aus der Liste, die wir bei GetHelpStrings gebaut haben,
  //gewählt hat. Natürlich bekommen wir hier nur die, die wir auch definiert haben

  if TCustomHelp.DecodeURL(HelpString, c, d, u, o) then
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
  //Das Hilfesystem fragt uns: Verstehst du dieses Keyword (der Begriff unter dem Cursor)?

  Result := 1; //ja!

  if GetHelpSystem(hs) then
  begin
    //Noch schnell dem Hilfesystem sagen, das wir einen eigenen Auswahldialog für die
    //verschiedenen Hilfethemen haben 
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
  FEnabledIndices:=TInterfaceList.Create;
  FHelpManager := nil;
  intf:=TMyViewer.Create;

  FProvider := TStringList.Create;

  RegisterViewer(intf, FHelpManager);

  ConnectToIDE;
  LoadProviderFromRegistry;

  //Falls es keine Provider gibt, schreiben wir mal die Standards rein
  if FProvider.Count = 0 then
  begin
    WriteProviderToRegistry('1',
                            'DP DelphiReference',
                            'Search with Daniels Cool Tool',
                            'http://ref.dp200x.de/dp_reference.php?securitytoken=guest&tabbed=1&sbutton=Search&query=');

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
  Description, Link: String; out Order: Integer): boolean;
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
    Order:=StrToIntDef(sl[3],0);
    sl.Free;
    Result:=True;
  end;           
end;

destructor TCustomHelp.Destroy;
begin
  DisconnectFromIDE;

  FEnabledIndices.Free;
  FProvider.Free;

  if Assigned(FHelpManager) then
    FHelpManager.Release(vi);
  
  inherited;
end;

procedure TCustomHelp.DisconnectFromIDE;
begin
  if Assigned(FMenuItem) then
    FMenuItem.Free;
end;

class function TCustomHelp.EncodeURL(Caption, Description, Link: String;
  Order: Integer): String;
begin
  Result:=CPROT+Caption+'|'+Description+'|'+Link+'|'+IntToStr(Order);
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

function TCustomHelp.GetNamespaces: IHxRegNamespaceList;
begin
  Result:=CoHxRegistryWalker.Create.RegisteredNamespaceList[''];
end;

procedure TCustomHelp.LoadProviderFromRegistry;
var
  Reg : TRegistry;
  sl : TStringList;
  s : String;
begin
  LoadSettingsFromRegistry;
  LoadEnabledNamespacesFromRegistry;

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
          FProvider.Add(TCustomHelp.EncodeURL(
                              Reg.ReadString(VALUE_NAME),
                              Reg.ReadString(VALUE_DESCR),
                              Reg.ReadString(VALUE_URL), 0));

        Reg.CloseKey;
      end;
    end;

  finally
    Reg.Free;
    sl.Free;
  end;
end;

procedure TCustomHelp.LoadEnabledNamespacesFromRegistry;
var
  sl : TStringList;
  idx : integer;      
  hxSession: IHxSession;
  hxIndex: IHxIndex;
begin
  FEnabledIndices.Clear;
  sl:=TStringList.Create;
  try
    ReadEnabledNamespacesFromRegistry(sl);
    
    for idx := 1 to Namespaces.Count do
    begin
      if sl.IndexOf(Namespaces.Item(idx).Name)>=0 then
      begin
        hxSession:=CoHxSession.Create;
        hxSession.Initialize('ms-help://'+Namespaces.Item(idx).Name,0);

        if Supports(hxSession.GetNavigationObject('!DefaultKeywordIndex',''),
                    IID_IHxIndex,
                    hxIndex) then
        begin
          FEnabledIndices.Add(hxIndex);
        end;
      end;
    end;
  finally
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
    FShowCustHelpOnWP:=sl.Values[SETTINGS_CUSTHELPWP]='1';
  finally
    sl.Free;
  end;
end;

procedure TCustomHelp.OnMenuItemClick(Sender: TObject);
begin
  Tform_Config.Execute;
  LoadProviderFromRegistry;
end;

class procedure TCustomHelp.ReadEnabledNamespacesFromRegistry(
  const ANamesList: TStrings);
var
  Reg : TRegistry;
  idx: Integer;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;

    if Reg.OpenKey(PROVIDER_ROOT_KEY + '\'+NAMESPACES_ROOT, true) then
    begin
      Reg.GetValueNames(ANamesList);

      for idx := ANamesList.count - 1 downto 0 do
        if not Reg.ReadBool(ANamesList[idx]) then
          ANamesList.Delete(idx);

      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
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

class procedure TCustomHelp.WriteNamespacesToRegistry(ANamespace: String;
  AEnabled: Boolean);
var
  Reg : TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;

    if Reg.OpenKey(PROVIDER_ROOT_KEY + '\'+NAMESPACES_ROOT, true) then
    begin
      Reg.WriteBool(ANamespace, AEnabled);

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
