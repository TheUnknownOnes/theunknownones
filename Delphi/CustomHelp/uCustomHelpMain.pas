{-----------------------------------------------------------------------------
 Purpose: The main unit of the custom help expert 

 (c) by TheUnknownOnes
 see http://www.TheUnknownOnes.net
-----------------------------------------------------------------------------}

unit uCustomHelpMain;

interface

uses
  Classes, Forms, Dialogs, ToolsAPI, Menus, Registry, HelpIntfs, Windows,
  uMSHelpServices, uHtmlHelp;

type

  //Das Hauptobjekt
  TCustomHelp = class
  private
    FLastKeyword: String;
    function GetNamespaces: IHxRegNamespaceList;
    procedure SetFullTextSearch(const Value: Boolean);
    procedure SetShowCustomHelpOnWP(const Value: Boolean);
  protected
    FHelpManager : IHelpManager;

    FMenuItem : TMenuItem;

    FProvider : TStringList;
    FShowCustHelpOnWP : Boolean;
    FFullTextSearch : Boolean;
    FEnabledhxSessions : TInterfaceList;

    //In die IDE einhacken (Menu-Eintrag, ...)
    procedure ConnectToIDE;
    procedure DisconnectFromIDE;
    function GetHelpMenu : TMenuItem;

    procedure LoadProviderFromRegistry;
    procedure LoadSettingsFromRegistry;
    procedure LoadEnabledNamespacesFromRegistry;

    procedure OnMenuItemClick(Sender : TObject);
  public
    constructor Create();
    destructor Destroy(); override;

    property ProviderList : TStringList read FProvider;
    property Namespaces : IHxRegNamespaceList read GetNamespaces;
    property EnabledhxSessions : TInterfaceList read FEnabledhxSessions;

    property ShowCustomHelpOnWelcomePage: Boolean read FShowCustHelpOnWP write SetShowCustomHelpOnWP;
    property PerformFullTextSearch: Boolean read FFullTextSearch write SetFullTextSearch;
    property LastHelpCallKeyword: String read FLastKeyword write FLastKeyword;

    class function DecodeURL(const URL: String; out Caption: String;
      out Description: String; out Link: String; out Group: String): boolean;
    class function EncodeURL(Caption, Description, Link, Group: String): String;

    class procedure WriteProviderToRegistry(AKeyName, AName, ADesc, AURL : String);
    class procedure WriteNamespacesToRegistry(ANamespace: String; AEnabled: Boolean);
    class procedure ReadEnabledNamespacesFromRegistry(const ANamesList: TStrings);
    class procedure WriteSettingToRegistry(AName, AValue: String);
    class procedure ReadSettingsFromRegistry(const ANameValueList: TStrings);
  end;

  TMyViewer = class(TInterfacedObject, ICustomHelpViewer)
  private
    procedure ShowHTMLHelp(AURL: String);
    function ForceSelector(const HelpString: String): String;
    function GetNamespaceTitle(Session : IHxSession) : String;
    procedure SearchInHxSession(hxSession: IHxSession; const HelpString: string;
      var Result: TStringList; hxIndex: IHxIndex);
    procedure QueryInHxSession(hxSession: IHxSession; const HelpString: string;
      var Result: TStringList);
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
    REG_ROOT_KEY = '\Software\TheUnknownOnes\Delphi\VER170\CustomHelp';
  {$EndIf}
  {$IfDef VER180}
    REG_ROOT_KEY = '\Software\TheUnknownOnes\Delphi\VER180\CustomHelp';
  {$EndIf}
  {$IfDef VER185}
    REG_ROOT_KEY = '\Software\TheUnknownOnes\Delphi\VER185\CustomHelp';
  {$EndIf}
  {$IfDef VER200}
    REG_ROOT_KEY = '\Software\TheUnknownOnes\Delphi\VER200\CustomHelp';
  {$EndIf}
  {$IfDef VER210}
    REG_ROOT_KEY = '\Software\TheUnknownOnes\Delphi\VER210\CustomHelp';
  {$EndIf}

  CPROT = 'CustomHelp://';
  VALUE_NAME = 'Name';
  VALUE_DESCR = 'Description';
  VALUE_URL = 'URL';
  PROVIDER_SUB_KEY = '\Provider';
  SETTINGS_SUB_KEY= '\Settings';
  NAMESPACES_SUB_KEY = SETTINGS_SUB_KEY + '\NAMESPACES';
  EXPANDEDITEMS_SUB_KEY = SETTINGS_SUB_KEY + '\EXPANDED';

  URL_SPLITTER = #1;

  SETTINGS_CUSTHELPWP = 'CustomHelpOnWP';
  SETTINGS_FULLTEXTSEARCH = 'FullTextSearch';

  GROUP_LABEL_DEFAULT = 'Available Search engines';
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
  c, d, u, g : String;
  hxSession : IHxSession;
  hxIndex : IHxIndex;
  Topics : IHxTopicList;
  slot : integer;
  s : String;

  function EncodedHelpString: String;
  var
    i: integer;
  begin
    Result:='';
    for i := 1 to Length(HelpString)do
      Result:=Result + '%'+Format('%.2x', [Ord(HelpString[i])]);
  end;

begin
  GlobalCustomHelp.LastHelpCallKeyword:=HelpString;

  //Weil wir bei UnderstandsKeyword gesagt haben, das wir das Keyword verstehen (Result = 1)
  //werden wir jetzt gefragt, welche Hilfethemen wir zu diesem Keyword liefern können
  //Die StringList wird vom Hilfesystem wieder freigegeben

  Result := TStringList.Create;
  Result.Duplicates:=dupIgnore;
  //Result.Sorted:=True;
  Result.Assign(GlobalCustomHelp.ProviderList);

  //Das Keyword in Hex kodieren ... der Einfachheit halber einfach alle Zeichen
  e := EncodedHelpString;


  for idx := 0 to Result.Count - 1 do
  begin
    TCustomHelp.DecodeURL(Result[idx], c, d, u, g);

    if Pos('://', u)>0 then
      Result[idx] := TCustomHelp.EncodeURL(c,d,u+EncodedHelpString, g)
    else
    if AnsiSameText(ExtractFileExt(u),'.hlp') then
    begin
      Result[idx] := TCustomHelp.EncodeURL(c,d,'winhlp://-k '+HelpString+' '+u, g);
    end
    else
    if AnsiSameText(ExtractFileExt(u),'.chm') then
    begin
      Result[idx] := TCustomHelp.EncodeURL(c,d,'htmlhlp://'+HelpString+URL_SPLITTER+u, g);
    end;
  end;

  //Und jetzt noch die eigentlichen Hilfe-Namespaces durchsuchen
  for idx := 0 to GlobalCustomHelp.EnabledhxSessions.Count-1 do
  begin
    if Supports(GlobalCustomHelp.EnabledhxSessions[idx], IHxSession, hxSession) then
    begin
      //Soll nach dem kompletten Text gesucht werden?
      if GlobalCustomHelp.PerformFullTextSearch then
      begin
        QueryInHxSession(hxSession, HelpString, Result);  
      end
      else
      //oder nur im Index der Hilfe
        if Supports(hxSession.GetNavigationObject('!DefaultKeywordIndex',''),
                    IID_IHxIndex,
                    hxIndex) then
        begin
          SearchInHxSession(hxSession, HelpString, Result, hxIndex);

          s:=HelpString;
          while Pos('.', s) > 0 do
            Delete(s, 1, Pos('.', s));

          if ((s<>'') and (s<>HelpString)) then
            SearchInHxSession(hxSession, s, Result, hxIndex);
        end;
  end;
  end;
end;

function TMyViewer.GetNamespaceTitle(Session: IHxSession): String;
var
  NamespaceName : String;
  idx : integer;
begin
  NamespaceName:=Session.Collection.GetProperty(HxCollectionProp_NamespaceName);
  for idx := 1 to GlobalCustomHelp.GetNamespaces.Count do
  begin
    if GlobalCustomHelp.GetNamespaces.Item(idx).Name=NamespaceName then
    begin
      Result:=GlobalCustomHelp.GetNamespaces.Item(idx).GetProperty(HxRegNamespaceDescription);
      break;
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

procedure TMyViewer.ShowHTMLHelp(AURL: String);
var
  SearchRecord : tagHH_AKLINK;
  sl : TStringList;
begin
  sl:=TStringList.Create;
  try
  sl.Delimiter:=URL_SPLITTER;
  sl.QuoteChar:=#0;
  sl.StrictDelimiter:=True;
  sl.DelimitedText:=AURL;
  sl.Add('');
  sl.Add('');

  SearchRecord.cbStruct:=sizeof(SearchRecord);
  SearchRecord.fReserved:=False;
  SearchRecord.pszKeywords:=PChar(sl[0]);
  SearchRecord.pszUrl:=nil;
  SearchRecord.pszMsgText:=nil;
  SearchRecord.pszMsgTitle:=nil;
  SearchRecord.pszWindow:=nil;
  SearchRecord.fIndexOnFail:=True;

  HtmlHelp(Application.Handle, PChar(sl[1]), HH_DISPLAY_INDEX, 0);
  HtmlHelp(Application.Handle, PChar(sl[1]), HH_KEYWORD_LOOKUP, cardinal(@SearchRecord));
  finally
    sl.Free;
  end;                 
end;

function TMyViewer.ForceSelector(const HelpString: String): String;
var
  sl : TStringList;
  i : integer;
  u : String;
begin                    
  Result:='';
  sl:=GetHelpStrings(HelpString);
  if TFormHelpSelector.Execute(sl, i, u) then
    Result:=u;
end;

procedure TMyViewer.ShowHelp(const HelpString: String);
var
  c,d,u,g: String;
  o: Integer;
  alternativeNavigate : boolean;
begin
  if HelpString<>'' then
  begin
    //Hier gehts dann wirklich um die Wurst
    //Wir bekommen den Hilfestring übergeben, den der
    //Nutzer aus der Liste, die wir bei GetHelpStrings gebaut haben,
    //gewählt hat. Natürlich bekommen wir hier nur die, die wir auch definiert haben

    if TCustomHelp.DecodeURL(HelpString, c, d, u, g) then
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
      if Pos('htmlhlp://', u)=1 then
      begin
        Delete(u,1,10);
        ShowHTMLHelp(u);
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
    else
    begin
      //oops das haben wir jetzt nicht verstanden....
      //das heißt wir sind die letzte instanz Doch noch Hilfe zu bekommen.
      //Wir erzwingen einen Selektor und rufen uns selbst noch mal auf
      ShowHelp(ForceSelector(HelpString));
    end;
  end;
end;

procedure TMyViewer.ShowTableOfContents;
begin
end;

procedure TMyViewer.ShutDown;
begin
end;

procedure TMyViewer.SearchInHxSession(hxSession: IHxSession;
  const HelpString: string; var Result: TStringList;
  hxIndex: IHxIndex);
var
  Topics: IHxTopicList;
  idx: Integer;
  slot: Integer;
  s : String;
  g: string;
begin
  slot := hxIndex.GetSlotFromString(HelpString);
  if AnsiContainsText(hxIndex.GetStringFromSlot(slot), HelpString) then
  begin
    Topics := hxIndex.GetTopicsFromSlot(slot);
    g := GetNamespaceTitle(hxSession);
    for idx := 1 to Topics.Count do
    begin
      s:=TCustomHelp.EncodeURL(Topics.Item(idx).Title[HxTopicGetRLTitle, 0], Topics.Item(idx).Location, Topics.Item(idx).URL, g);
      Result.Add(s);
    end;
  end;
end;

procedure TMyViewer.QueryInHxSession(hxSession: IHxSession;
  const HelpString: string; var Result: TStringList);
var
  Topics: IHxTopicList;
  idx: Integer;
  s : String;
  g: string;
begin
  Topics:=hxSession.Query(HelpString, '!DefaultFullTextSearch', HxQuery_No_Option, '');

  g := GetNamespaceTitle(hxSession);
  for idx := 1 to Topics.Count do
  begin
    s:=TCustomHelp.EncodeURL(Topics.Item(idx).Title[HxTopicGetRLTitle, 0], Topics.Item(idx).Location, Topics.Item(idx).URL, g);
    Result.Add(s);
  end;
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
  hs : IHelpSystem;
begin
  FEnabledhxSessions:=TInterfaceList.Create;
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
  Description, Link: String; out Group: String): boolean;
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
    Group:=sl[3];
    sl.Free;
    Result:=True;
  end;           
end;

destructor TCustomHelp.Destroy;
begin
  DisconnectFromIDE;

  FEnabledhxSessions.Free;
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

class function TCustomHelp.EncodeURL(Caption, Description, Link, Group: String): String;
begin
  Result:=CPROT+Caption+'|'+Description+'|'+Link+'|'+Group;
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

    if Reg.OpenKey(REG_ROOT_KEY + PROVIDER_SUB_KEY, true) then
    begin
      Reg.GetKeyNames(sl);
      Reg.CloseKey;
    end;

    for s in sl do
    begin
      if Reg.OpenKey(REG_ROOT_KEY + PROVIDER_SUB_KEY + '\' + s, false) then
      begin
        if trim(reg.ReadString(VALUE_NAME))<>EmptyStr then
          FProvider.Add(TCustomHelp.EncodeURL(
                              Reg.ReadString(VALUE_NAME),
                              Reg.ReadString(VALUE_DESCR),
                              Reg.ReadString(VALUE_URL),
                              GROUP_LABEL_DEFAULT));

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
begin
  FEnabledhxSessions.Clear;
  sl:=TStringList.Create;
  try
    ReadEnabledNamespacesFromRegistry(sl);
    
    for idx := 1 to Namespaces.Count do
    begin
      if sl.IndexOf(Namespaces.Item(idx).Name)>=0 then
      begin
        hxSession:=CoHxSession.Create;
        hxSession.Initialize('ms-help://'+Namespaces.Item(idx).Name,0);

        FEnabledhxSessions.Add(hxSession);
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
    FFullTextSearch:=sl.Values[SETTINGS_FULLTEXTSEARCH]='1';
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

    if Reg.OpenKey(REG_ROOT_KEY + NAMESPACES_SUB_KEY, true) then
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

    if Reg.OpenKey(REG_ROOT_KEY + SETTINGS_SUB_KEY, true) then
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

procedure TCustomHelp.SetFullTextSearch(const Value: Boolean);
begin
  FFullTextSearch := Value;
  TCustomHelp.WriteSettingToRegistry(SETTINGS_FULLTEXTSEARCH, IntToStr(byte(Value)));
end;

procedure TCustomHelp.SetShowCustomHelpOnWP(const Value: Boolean);
begin
  FShowCustHelpOnWP := Value;
  TCustomHelp.WriteSettingToRegistry(SETTINGS_CUSTHELPWP, IntToStr(byte(Value)));
end;

class procedure TCustomHelp.WriteSettingToRegistry(AName, AValue: String);
var
  Reg : TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;

    if Reg.OpenKey(REG_ROOT_KEY + SETTINGS_SUB_KEY, true) then
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

    if Reg.OpenKey(REG_ROOT_KEY + NAMESPACES_SUB_KEY, true) then
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

    if Reg.OpenKey(REG_ROOT_KEY + PROVIDER_SUB_KEY + '\' + AKeyName, true) then
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
