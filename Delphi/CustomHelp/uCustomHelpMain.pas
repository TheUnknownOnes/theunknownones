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
  TNamespaceTrimOption = (nstoNoTrim=0, nstoTrimFirst, nstoTrimAll);

  TCustomHelpViewer = class;

  //Das Hauptobjekt
  TCustomHelp = class
  private
    FLastKeyword: String;

    function GetNamespaces: IHxRegNamespaceList;
    procedure SetFullTextSearch(const Value: Boolean);
    procedure SetShowCustomHelpOnWP(const Value: Boolean);
    procedure SetTrimNamespaces(const Value: TNamespaceTrimOption);
  protected
    FMenuItem : TMenuItem;

    FProvider : TStringList;
    FShowCustHelpOnWP : Boolean;
    FFullTextSearch : Boolean;
    FEnabledhxSessions : TInterfaceList;
    FTrimNamespaces: TNamespaceTrimOption;

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
    property TrimNamespacesUntilResultFound: TNamespaceTrimOption read FTrimNamespaces write SetTrimNamespaces;

    class function DecodeURL(const URL: String; out Caption: String;
      out Description: String; out Link: String; out Group: String; out TrimOption: TNamespaceTrimOption): boolean;
    class function EncodeURL(Caption, Description, Link, Group: String; TrimOption: TNamespaceTrimOption): String;

    class procedure WriteProviderToRegistry(AKeyName, AName, ADesc, AURL : String; ATrimNamespaces: TNamespaceTrimOption);
    class procedure WriteNamespacesToRegistry(ANamespace: String; AEnabled: Boolean);
    class procedure ReadEnabledNamespacesFromRegistry(const ANamesList: TStrings);
    class procedure WriteSettingToRegistry(AName, AValue: String);
    class procedure ReadSettingsFromRegistry(const ANameValueList: TStrings);
  end;

  TCustomHelpViewer = class(TInterfacedObject, ICustomHelpViewer)
  private
    FHelpManager: IHelpManager;
    FViewerID: Integer;
    procedure ShowHTMLHelp(AURL: String);
    function ForceSelector(const HelpString: String): String;
    function GetNamespaceTitle(Session : IHxSession) : String;
    function SearchInHxSession(hxSession: IHxSession; const HelpString: string;
      var AResult: TStringList; hxIndex: IHxIndex): Boolean;
    function QueryInHxSession(hxSession: IHxSession; const HelpString: string;
      var AResult: TStringList): Boolean;
    function TrimNamespace(var s: string; ATrimOption: TNamespaceTrimOption): Boolean;
    procedure InternalShutDown;
  protected
    constructor Create;
    destructor Destroy; override;
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

    property HelpManager : IHelpManager read FHelpManager write FHelpManager;
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
  VALUE_TRIMNAMESPACE = 'TrimNamespaces';

  PROVIDER_SUB_KEY = '\Provider';
  SETTINGS_SUB_KEY= '\Settings';
  NAMESPACES_SUB_KEY = SETTINGS_SUB_KEY + '\NAMESPACES';
  EXPANDEDITEMS_SUB_KEY = SETTINGS_SUB_KEY + '\EXPANDED';

  URL_SPLITTER = #1;

  SETTINGS_CUSTHELPWP = 'CustomHelpOnWP';
  SETTINGS_FULLTEXTSEARCH = 'FullTextSearch';
  SETTINGS_TRIMNAMESPACES = 'TrimNamespaces';

  GROUP_LABEL_DEFAULT = 'Available Search engines';

var
  GlobalCustomHelp : TCustomHelp;

  HelpViewer: TCustomHelpViewer;
  HelpViewerIntf: ICustomHelpViewer;

implementation

uses
  SysUtils, uCustomHelpSelector, StrUtils, ShellAPI, uFormConfigCustomHelp,
  uCustomHelpIDEIntegration, Graphics;

{ TCustomHelpViewer }

function TCustomHelpViewer.CanShowTableOfContents: Boolean;
begin      
  Result := false;
end;

function TCustomHelpViewer.GetHelpStrings(const HelpString: String): TStringList;
var
  idx, idy : Integer;
  c, d, u, g : String;
  hxSession : IHxSession;
  hxIndex : IHxIndex;
  Topics : IHxTopicList;
  slot : integer;
  s : String;
  loop : byte;
  TrimOption: TNamespaceTrimOption;

  function EncodedHelpString(AHelpString: String): String;
  var
    i: integer;
  begin
    Result:='';
    for i := 1 to Length(AHelpString)do
      Result:=Result + '%'+Format('%.2x', [Ord(AHelpString[i])]);
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

  for idx := 0 to Result.Count - 1 do
  begin
    TCustomHelp.DecodeURL(Result[idx], c, d, u, g, TrimOption);
    s:=HelpString;
    TrimNamespace(s, TrimOption);

    if Pos('://', u)>0 then
    begin
      Result[idx] := TCustomHelp.EncodeURL(c,d,u+EncodedHelpString(s), g, TrimOption);
    end
    else
    if AnsiSameText(ExtractFileExt(u),'.hlp') then
    begin
      Result[idx] := TCustomHelp.EncodeURL(c,d,'winhlp://-k '+s+' '+u, g, TrimOption);
    end
    else
    if AnsiSameText(ExtractFileExt(u),'.chm') then
    begin
      Result[idx] := TCustomHelp.EncodeURL(c,d,'htmlhlp://'+s+URL_SPLITTER+u, g, TrimOption);
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
        s:=HelpString;
        loop:=0;
        if (not QueryInHxSession(hxSession, s, Result)) and
           (GlobalCustomHelp.TrimNamespacesUntilResultFound<>nstoNoTrim) then
        repeat
          inc(loop);

          if (loop>1) and (GlobalCustomHelp.TrimNamespacesUntilResultFound<>nstoTrimAll) then
            break;

          if not TrimNamespace(s, nstoTrimFirst) then
            break;
        until QueryInHxSession(hxSession, s, Result);
      end
      else
      //oder nur im Index der Hilfe
      begin
        if Supports(hxSession.GetNavigationObject('!DefaultKeywordIndex',''),
                    IID_IHxIndex,
                    hxIndex) then
        begin
          loop:=0;
          s:=HelpString;
          if (not SearchInHxSession(hxSession, s, Result, hxIndex)) and
             (GlobalCustomHelp.TrimNamespacesUntilResultFound<>nstoNoTrim) then
          repeat
            inc(loop);

            if (loop>1) and (GlobalCustomHelp.TrimNamespacesUntilResultFound<>nstoTrimAll) then
              break;
            if not TrimNamespace(s, nstoTrimFirst) then
              break;
          until SearchInHxSession(hxSession, s, Result, hxIndex);
        end;
      end;
    end;
  end;
end;

function TCustomHelpViewer.GetNamespaceTitle(Session: IHxSession): String;
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

function TCustomHelpViewer.GetViewerName: String;
begin
  Result := 'CustomHelpViewer';
end;

procedure TCustomHelpViewer.NotifyID(const ViewerID: Integer);
begin
  //Das Hilfesystem sagt uns, welche ID wir bekommen haben
  //Die brauchen wir am Ende zum freigeben
  FViewerID := ViewerID;
end;

procedure TCustomHelpViewer.ShowHTMLHelp(AURL: String);
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

constructor TCustomHelpViewer.Create;
begin
  inherited Create;
  HelpViewerIntf := Self;
end;

destructor TCustomHelpViewer.Destroy;
begin
  HelpViewer := nil;
  inherited Destroy;
end;

function TCustomHelpViewer.ForceSelector(const HelpString: String): String;
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

procedure TCustomHelpViewer.ShowHelp(const HelpString: String);
var
  c,d,u,g: String;
  o: Integer;
  alternativeNavigate : boolean;
  TrimOption : TNamespaceTrimOption;
begin
  if HelpString<>'' then
  begin
    //Hier gehts dann wirklich um die Wurst
    //Wir bekommen den Hilfestring übergeben, den der
    //Nutzer aus der Liste, die wir bei GetHelpStrings gebaut haben,
    //gewählt hat. Natürlich bekommen wir hier nur die, die wir auch definiert haben

    if TCustomHelp.DecodeURL(HelpString, c, d, u, g, TrimOption) then
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

procedure TCustomHelpViewer.ShowTableOfContents;
begin
end;

procedure TCustomHelpViewer.ShutDown;
begin
  SoftShutDown;
  if Assigned(FHelpManager) then
    HelpManager := nil;
end;

function TCustomHelpViewer.TrimNamespace(var s: string; ATrimOption: TNamespaceTrimOption): Boolean;
begin
  Result:=True;
  if ATrimOption=nstoNoTrim then
    exit;

  Result:=Pos('.', s) > 0;
  if Result then
    Delete(s, 1, Pos('.', s));

  if Result and (ATrimOption=nstoTrimAll) then
    TrimNamespace(s, nstoTrimAll);
end;

function TCustomHelpViewer.SearchInHxSession(hxSession: IHxSession;
  const HelpString: string; var AResult: TStringList;
  hxIndex: IHxIndex): Boolean;
var
  Topics: IHxTopicList;
  idx: Integer;
  slot: Integer;
  s : String;
  g: string;
begin
  Result:=False;
  slot := hxIndex.GetSlotFromString(HelpString);
  if AnsiContainsText(hxIndex.GetStringFromSlot(slot), HelpString) then
  begin
    Topics := hxIndex.GetTopicsFromSlot(slot);
    g := GetNamespaceTitle(hxSession);
    for idx := 1 to Topics.Count do
    begin
      s:=TCustomHelp.EncodeURL(Topics.Item(idx).Title[HxTopicGetRLTitle, 0], Topics.Item(idx).Location, Topics.Item(idx).URL, g, nstoNoTrim);
      AResult.Add(s);
      Result:=True;
    end;
  end;
end;

procedure TCustomHelpViewer.InternalShutDown;
var
  hs : IHelpSystem;
begin
  SoftShutDown;
  if GetHelpSystem(hs) then
  begin
    hs.AssignHelpSelector(nil);
  end;
  if Assigned(FHelpManager) then
  begin
    HelpManager.Release(FViewerID);
    HelpManager := nil;
  end;
end;

function TCustomHelpViewer.QueryInHxSession(hxSession: IHxSession;
  const HelpString: string; var AResult: TStringList): Boolean;
var
  Topics: IHxTopicList;
  idx: Integer;
  s : String;
  g: string;
begin
  Result:=False;
  Topics:=hxSession.Query(HelpString, '!DefaultFullTextSearch', HxQuery_No_Option, '');

  g := GetNamespaceTitle(hxSession);
  for idx := 1 to Topics.Count do
  begin
    s:=TCustomHelp.EncodeURL(Topics.Item(idx).Title[HxTopicGetRLTitle, 0], Topics.Item(idx).Location, Topics.Item(idx).URL, g, nstoNoTrim);
    AResult.Add(s);
    Result:=True;
  end;
end;

procedure TCustomHelpViewer.SoftShutDown;
begin  
end;

function TCustomHelpViewer.UnderstandsKeyword(const HelpString: String): Integer;
var
  hs : IHelpSystem;
begin
  //Das Hilfesystem fragt uns: Verstehst du dieses Keyword (der Begriff unter dem Cursor)?

  if AnsiContainsText(HelpString, 'erroneous type') then
  begin
    Result := 0;
    Exit;
  end;

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
  hs : IHelpSystem;
begin
  FEnabledhxSessions:=TInterfaceList.Create;
  FProvider := TStringList.Create;

  HelpViewer:=TCustomHelpViewer.Create;
  RegisterViewer(HelpViewerIntf, HelpViewer.FHelpManager);

  ConnectToIDE;
  LoadProviderFromRegistry;

  //Falls es keine Provider gibt, schreiben wir mal die Standards rein
  if FProvider.Count = 0 then
  begin
    WriteProviderToRegistry('1',
                            'DP DelphiReference',
                            'Search with Daniels Cool Tool',
                            'http://ref.dp200x.de/dp_reference.php?securitytoken=guest&tabbed=1&sbutton=Search&query=',
                            nstoTrimFirst);

    WriteProviderToRegistry('2',
                            'Koders.com',
                            'Search at koders.com',
                            'http://www.koders.com/default.aspx?submit=Search&la=Delphi&li=*&s=',
                            nstoTrimFirst);

    WriteProviderToRegistry('3',
                            'Google Codesearch',
                            'Search using Google Codesearch',
                            'http://www.google.com/codesearch?btnG=Code+suchen&hl=de&as_lang=pascal&as_license_restrict=i&as_license=&as_package=&as_filename=&as_case=&as_q=',
                            nstoTrimFirst);

    LoadProviderFromRegistry;
  end;
end;

class function TCustomHelp.DecodeURL(const URL: String; out Caption,
  Description, Link: String; out Group: String;
  out TrimOption: TNamespaceTrimOption): boolean;
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
    TrimOption:=TNamespaceTrimOption(StrToIntDef(sl[4],0));
    sl.Free;
    Result:=True;
  end;
end;

destructor TCustomHelp.Destroy;
begin
  DisconnectFromIDE;

  FEnabledhxSessions.Free;
  FProvider.Free;

  if Assigned(HelpViewer.FHelpManager) then
    HelpViewer.InternalShutDown; 
  HelpViewer := nil;

  inherited;
end;

procedure TCustomHelp.DisconnectFromIDE;
begin
  if Assigned(FMenuItem) then
    FMenuItem.Free;
end;

class function TCustomHelp.EncodeURL(Caption, Description, Link, Group: String; TrimOption: TNamespaceTrimOption): String;
begin
  Result:=CPROT+Caption+'|'+Description+'|'+Link+'|'+Group+'|'+IntToStr(Integer(TrimOption));
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
                              GROUP_LABEL_DEFAULT,
                              TNamespaceTrimOption(StrToIntDef(Reg.ReadString(VALUE_TRIMNAMESPACE),0))));

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
    FTrimNamespaces:=TNamespaceTrimOption(StrToIntDef(sl.Values[SETTINGS_TRIMNAMESPACES], 0));
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

procedure TCustomHelp.SetTrimNamespaces(const Value: TNamespaceTrimOption);
begin
  FTrimNamespaces := Value;
  TCustomHelp.WriteSettingToRegistry(SETTINGS_TRIMNAMESPACES, IntToStr(byte(Value)));
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
  AURL: String; ATrimNamespaces: TNamespaceTrimOption);
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
      Reg.WriteString(VALUE_TRIMNAMESPACE, IntToStr(Integer(ATrimNamespaces)));

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
