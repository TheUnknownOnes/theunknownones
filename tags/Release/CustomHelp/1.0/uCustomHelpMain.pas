{-----------------------------------------------------------------------------
 Purpose: The main unit of the custom help expert

 (c) by TheUnknownOnes under dwywbdbu license - see http://theunknownones.googlecode.com/svn/ for the license
 see http://www.TheUnknownOnes.net
-----------------------------------------------------------------------------}

unit uCustomHelpMain;

interface

uses
  Classes, Forms, Dialogs, ToolsAPI, Menus, Registry, HelpIntfs, Windows,
  uMSHelpServices, uHtmlHelp, SyncObjs, uCustomHelpSelector;

type
  TNamespaceTrimOption = (nstoNoTrim=0, nstoTrimFirst, nstoTrimAll);

  TCustomHelpViewer = class;

  //Main object
  TCustomHelp = class
  private
    FLastKeyword: String;
    FLastHelpErrors: string;
    FSessionLock: TCriticalSection;
    FCustomHelpViewer:TCustomHelpViewer;
    FHandledSchemes: TStringList;
    FReplaceDefaultViewer: boolean;
    FShowOHSAtTop: boolean;
    FCheckWinHelpGid: Boolean;

    procedure SetReplaceDefaultViewer(const Value: boolean);
    function GetRedirectSchemes: string;
    procedure SetRedirectSchemes(const Value: string);
    procedure SetShowOHSAtTop(const Value: boolean);
    procedure SetCheckWinHelpGid(const Value: Boolean);
    class function GetNamespaces: IHxRegNamespaceList;
    function GetEnabledhxSession(Index: Integer): IHxSession;
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

    //hack into the IDE (Menu-entry, ...)
    procedure ConnectToIDE;
    procedure DisconnectFromIDE;
    function GetHelpMenu : TMenuItem;

    procedure LoadProviderFromRegistry;
    procedure LoadSettingsFromRegistry;
    procedure LoadEnabledNamespacesFromRegistry;

    procedure OnMenuItemClick(Sender : TObject);

    class function GetNamespaceTitle(Session : IHxSession) : String;
    class function GetNamespaceName(Session: IHxSession): string;
    class procedure TrimNamespace(var s: string; ATrimOption: TNamespaceTrimOption);
    class function CheckIndexInHxSession(hxSession: IHxSession; var hxIndex: IHxIndex): boolean;
    function SearchInHxSession(hxSession: IHxSession;
      const HelpString: string; AResult: TStringList;
      hxIndex: IHxIndex): Boolean;
    function QueryInHxSession(hxSession: IHxSession;
      const HelpString: string; const AResult: TStringList): Boolean;
    function PerformInHxSession(HelpString: string; SessionIndex: integer; const AResult: TStringList): Boolean;
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

    property EnabledhxSession[Index: integer]: IHxSession read GetEnabledhxSession;
    property LastHelpErrors: string read FLastHelpErrors write FLastHelpErrors;

    procedure InitHelpSelector(const HelpString: string);
    function IsHandledByDefaultViewer(const AKeyword: string): boolean;
    property ReplaceDefaultViewer: boolean read FReplaceDefaultViewer write SetReplaceDefaultViewer;
    property ShowOHSAtTop: boolean read FShowOHSAtTop write SetShowOHSAtTop;
    property CheckWinHelpGid: Boolean read FCheckWinHelpGid write SetCheckWinHelpGid;
    property RedirectSchemes: string read GetRedirectSchemes write SetRedirectSchemes;

    class function DecodeURL(const URL: String; out Caption: String;
      out Description: String; out Link: String; out Group: String;
      out TrimOption: TNamespaceTrimOption): boolean; overload;
    class function EncodeURL(Caption, Description, Link, Group: String; TrimOption: TNamespaceTrimOption): String;
    class procedure WriteProviderToRegistry(AKeyName, AName, ADesc, AURL : String; ATrimNamespaces: TNamespaceTrimOption);
    class procedure WriteNamespacesToRegistry(ANamespace: String; AEnabled: Boolean);
    class procedure ReadEnabledNamespacesFromRegistry(const ANamesList: TStrings);
    class procedure WriteSettingToRegistry(AName, AValue: String);
    class procedure ReadSettingsFromRegistry(const ANameValueList: TStrings);
    class function GetTopicFromURL(const URL: string; var Group: string): IHxTopic; overload;
    class function GetTopicFromURL(hxHierarchy: IHxHierarchy; const URL: string): IHxTopic; overload;
    class function GetTopicInfo(const URL: string; out Caption, Description, Link, Group: string;
      out TrimOption: TNamespaceTrimOption): boolean;
    class function DecodeURL(const URL: String; out Link: String): boolean; overload;
    class function CheckGidFile(AWinHelpFile: String): Boolean; static;
  end;

  TCustomHelpViewer = class(TInterfacedObject, ICustomHelpViewer)
  private
    FHelpManager: IHelpManager;
    FViewerID: Integer;
    procedure ShowHTMLHelp(AURL: String);
    function ForceSelector(const HelpString: String): String;
  protected
    {$REGION 'ICustomHelpViewer'}
    function  GetViewerName : String;
    function  UnderstandsKeyword(const HelpString: String): Integer;
    function  GetHelpStrings(const HelpString: String): TStringList;
    function  CanShowTableOfContents : Boolean;
    procedure ShowTableOfContents;     
    procedure NotifyID(const ViewerID: Integer);
    procedure SoftShutDown;
    procedure ShutDown;
    {$ENDREGION}
  public
    constructor Create;
    destructor Destroy; override;
    procedure ShowHelp(const HelpString: String);
    property HelpManager : IHelpManager read FHelpManager write FHelpManager;
    property ViewerID: Integer read FViewerID;
    property Name: String read GetViewerName;
  end;

const
  {$IfDef VER170}
    REG_ROOT_KEY = '\Software\TheUnknownOnes\Delphi\VER170\CustomHelp';
  {$EndIf}
  {$IfDef VER180}
    {$IfDef VER185}
      REG_ROOT_KEY = '\Software\TheUnknownOnes\Delphi\VER185\CustomHelp';
    {$Else}
      REG_ROOT_KEY = '\Software\TheUnknownOnes\Delphi\VER180\CustomHelp';
    {$EndIf}
  {$EndIf}
  {$IfDef VER200}
    REG_ROOT_KEY = '\Software\TheUnknownOnes\Delphi\VER200\CustomHelp';
  {$EndIf}
  {$IfDef VER210}
    REG_ROOT_KEY = '\Software\TheUnknownOnes\Delphi\VER210\CustomHelp';
  {$EndIf}

  PROTPREFIX_CUSTOMHELP = 'CustomHelp://';
  PROTPREFIX_MSHELP = 'ms-help://';
  PROTPREFIX_HTMLHELP = 'htmlhlp://';
  PROTPREFIX_WINHELP = 'winhlp://';
  PROTPREFIX_UNKNOWNHELP = 'unknown://';

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
  SETTINGS_OHSATTOP = 'DisplayOHSAtTop';
  SETTING_WINHELPGIDCHECK = 'CheckWinHelpGID';
  SETTINGS_HANDLEDSCHEMES = 'HandledSchemes';
  SETTINGS_REPLACEDEFAULT = 'ReplaceDefaultViewer';

  GROUP_LABEL_DEFAULT = 'Available Search engines';
  GROUP_LABEL_STANDARD = 'Other Help Providers';

  ENVVAR_TOKEN_START = '$(';
  ENVVAR_TOKEN_END = ')';
  ENVVAR_NAME_KEYWORD = 'HelpString';

  ENVVAR_NAME_FORMATTED_KEYWORD = ENVVAR_TOKEN_START+ENVVAR_NAME_KEYWORD+ENVVAR_TOKEN_END;

var
  GlobalCustomHelp : TCustomHelp;

  //We keep these global ... as it is done in WinHelpViewer.pas
  HelpViewer: TCustomHelpViewer;
  HelpViewerIntf: ICustomHelpViewer;

implementation

uses
  SysUtils, StrUtils, ShellAPI, uFormConfigCustomHelp,
  uCustomHelpIDEIntegration, Graphics, ActiveX, Variants, Types, uUtils;

{ TCustomHelpViewer }

function TCustomHelpViewer.CanShowTableOfContents: Boolean;
begin
  Result := false;
end;

function TCustomHelpViewer.GetHelpStrings(const HelpString: String): TStringList;
var
  idx : Integer;
  c, d, u, g : String;
  ShortHelpString : String;
  HelpStrings, errmsgs: TStringList;
  TrimOption: TNamespaceTrimOption;
  sl : TStringList;

  function EncodedHelpString(AHelpString: String): String;
  var
    i: integer;
  begin
    Result:='';
    for i := 1 to Length(AHelpString)do
      Result:=Result + '%'+Format('%.2x', [Ord(AHelpString[i])]);
  end;

  function PerformSearch(AHelpString: string): Boolean;
  var
    idx: Integer;
  begin
    GlobalCustomHelp.LastHelpCallKeyword := AHelpString;
    for idx := 0 to GlobalCustomHelp.EnabledhxSessions.Count - 1 do
    begin
      try
        GlobalCustomHelp.PerformInHxSession(AHelpString, idx, HelpStrings);
      except on e: Exception do
        errmsgs.Add(GlobalCustomHelp.GetNamespaceName(GlobalCustomHelp.EnabledhxSession[idx]) + ': '+ e.Message);
      end;
    end;
    Result := HelpStrings.Count > 0;
  end;

begin
  Result := NIL;
  GlobalCustomHelp.LastHelpCallKeyword := HelpString;

  //Weil wir bei UnderstandsKeyword gesagt haben, das wir das Keyword verstehen (Result = 1)
  //werden wir jetzt gefragt, welche Hilfethemen wir zu diesem Keyword liefern können
  //Die StringList wird vom Hilfesystem wieder freigegeben

  HelpStrings := TStringList.Create;
  sl := TStringList.Create;
  try
    HelpStrings.Duplicates:=dupIgnore;

    errmsgs := TStringList.Create;
    try
      errmsgs.Sorted := True;
      errmsgs.Duplicates:=dupIgnore;
      // Und jetzt noch die eigentlichen Hilfe-Namespaces
      for idx := 0 to GlobalCustomHelp.EnabledhxSessions.Count - 1 do
      begin
        try
          if not GlobalCustomHelp.PerformInHxSession(HelpString, idx, HelpStrings) then
            if GlobalCustomHelp.TrimNamespacesUntilResultFound <> nstoNoTrim then
            begin
              ShortHelpString:=HelpString;
              while Pos('.', ShortHelpString) > 0 do
              begin
                Delete(ShortHelpString, 1, Pos('.', ShortHelpString));
                if ((ShortHelpString<>'') and (ShortHelpString<>HelpString)) then
                begin // Und jetzt noch das verkürzte Suchwort in den eigentlichen Hilfe-Namespaces suchen
                  if GlobalCustomHelp.PerformInHxSession(ShortHelpString, idx, HelpStrings) then
                    Break;
                end;
                if GlobalCustomHelp.TrimNamespacesUntilResultFound = nstoTrimFirst then
                  Break;
              end;
            end;
        except
          on e: Exception do
            errmsgs.Add(GlobalCustomHelp.GetNamespaceName(GlobalCustomHelp.EnabledhxSession[idx]) + ': '+ e.Message);
        end;
      end;

      for idx := 0 to GlobalCustomHelp.ProviderList.Count - 1 do
      begin
        if not TCustomHelp.DecodeURL(GlobalCustomHelp.ProviderList.Strings[idx], c, d, u, g, TrimOption) then
          Continue;

        ShortHelpString:=HelpString;
        TCustomHelp.TrimNamespace(ShortHelpString, TrimOption);

        if Pos('://', u)>0 then
        begin
          if PosText(ENVVAR_NAME_FORMATTED_KEYWORD,u)=0 then
            u:=u+ENVVAR_NAME_FORMATTED_KEYWORD;

          sl.Values[ENVVAR_NAME_KEYWORD]:=EncodedHelpString(ShortHelpString);
          ExpandEnvVars(u,ENVVAR_TOKEN_START,ENVVAR_TOKEN_END, sl);

          HelpStrings.Add(TCustomHelp.EncodeURL(c,d,u, g, TrimOption))
        end
        else
        if AnsiSameText(ExtractFileExt(u),'.hlp') then
        begin
          ShortHelpString:=AnsiReplaceStr(ShortHelpString, '.', ',');

          if (not GlobalCustomHelp.CheckWinHelpGid) or
             TCustomHelp.CheckGidFile(u) then
          begin
            if (not GlobalCustomHelp.CheckWinHelpGid) or
               FileContainsText(u, ShortHelpString) then
              HelpStrings.Add(TCustomHelp.EncodeURL(c,d,PROTPREFIX_WINHELP+'-k '+ShortHelpString+' '+u, g, TrimOption));
          end;
        end
        else
        if AnsiSameText(ExtractFileExt(u),'.chm') then
        begin
          HelpStrings.Add(TCustomHelp.EncodeURL(c,d,PROTPREFIX_HTMLHELP+ShortHelpString+URL_SPLITTER+u, g, TrimOption));
        end
        else
        begin
          //we got something we don't know. So let's try our best to execute that stuff
          sl.Values[ENVVAR_NAME_KEYWORD]:=ShortHelpString;
          ExpandEnvVars(u, ENVVAR_TOKEN_START, ENVVAR_TOKEN_END, sl);

          HelpStrings.Add(TCustomHelp.EncodeURL(c,d,PROTPREFIX_UNKNOWNHELP+u, g, TrimOption));
        end;
      end;

      Result := HelpStrings;
      GlobalCustomHelp.LastHelpErrors := errmsgs.Text;
    finally
      errmsgs.Free;
    end;
  finally
    sl.Free;
    if not Assigned(Result) then
      FreeAndNil(HelpStrings);
  end;
end;

class function TCustomHelp.GetNamespaceTitle(Session: IHxSession): String;
var
  NamespaceName : String;
  idx : integer;
  nsList: IHxRegNamespaceList;
begin
  NamespaceName:=GetNamespaceName(Session);
  nsList := GetNamespaces;
  for idx := 1 to nsList.Count do
  begin
    if nsList.Item(idx).Name=NamespaceName then
    begin
      Result:=nsList.Item(idx).GetProperty(HxRegNamespaceDescription);
      break;
    end;
  end;
end;

function TCustomHelp.GetRedirectSchemes: string;
begin
  Result := FHandledSchemes.DelimitedText;
end;

procedure TCustomHelp.InitHelpSelector(const HelpString: string);
var
  hs: IHelpSystem;
begin
  LastHelpCallKeyword:=HelpString;
  //the selector has to be constructed every time
  //if not the second help call will crash under BDS2006
  if GetHelpSystem(hs) then
    hs.AssignHelpSelector(THelpSelector.Create);
end;

function TCustomHelp.IsHandledByDefaultViewer(const AKeyword: string): boolean;
var
  idx: Integer;
begin
  Result := true;
  for idx := 0 to FHandledSchemes.Count - 1 do
    if AnsiStartsText(FHandledSchemes[idx], AKeyword) then
    begin
      Result := false;
      Break;
    end;
end;

function TCustomHelpViewer.GetViewerName: String;
begin
  Result := 'Custom Help Viewer (TUO)';
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
  if TFormHelpSelector.Execute(GlobalCustomHelp.LastHelpCallKeyword, sl, i, u) then
    Result:=u;
end;

procedure TCustomHelpViewer.ShowHelp(const HelpString: String);
var
  u: String;
  alternativeNavigate : boolean;
  sl : TStringList;
  command, params : String;
begin
  if HelpString<>'' then
  begin
    //Hier gehts dann wirklich um die Wurst
    //Wir bekommen den Hilfestring übergeben, den der
    //Nutzer aus der Liste, die wir bei GetHelpStrings gebaut haben,
    //gewählt hat. Natürlich bekommen wir hier nur die, die wir auch definiert haben

    if TCustomHelp.DecodeURL(HelpString, u) then
    begin
      if Pos(PROTPREFIX_UNKNOWNHELP, u)=1 then
      begin
        Delete(u,1,Length(PROTPREFIX_UNKNOWNHELP));
        // we got some unknown help format... just try to execute it ;-)
        sl:=TStringList.Create;
        try

          sl.Delimiter:=' ';
          sl.QuoteChar:='"';
          sl.DelimitedText:=u;
          if sl.Count>0 then
          begin
            command:='"'+sl[0]+'"';
            sl.Delete(0);
            params:=sl.DelimitedText;
            
            ShellExecute(Application.Handle,
                     'open',
                     PChar(Command),
                     PChar(params),
                     PChar(ExtractFileDir(Application.ExeName)),
                     SW_SHOWNORMAL);
          end;
        finally
          sl.Free;
        end;
      end
      else
      if Pos(PROTPREFIX_WINHELP, u)=1 then
      begin
        Delete(u,1,Length(PROTPREFIX_WINHELP));
        ShellExecute(Application.Handle,
                     'open',
                     'winhlp32',
                     PChar(u),
                     PChar(ExtractFileDir(Application.ExeName)),
                     SW_SHOWNORMAL);
      end
      else
      if Pos(PROTPREFIX_HTMLHELP, u)=1 then
      begin
        Delete(u,1,Length(PROTPREFIX_HTMLHELP));
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
  begin
    HelpManager.Release(FViewerID);
    HelpManager := nil;
  end;
end;

class procedure TCustomHelp.TrimNamespace(var s: string; ATrimOption: TNamespaceTrimOption);
var
  idx: integer;
begin
  case ATrimOption of
    nstoNoTrim: idx := 0;
    nstoTrimFirst: idx := PosStr('.', s, 1, MaxInt);
    nstoTrimAll: idx := PosStr('.', s, MaxInt, 1);
  else
    Assert(false, 'ATrimOption #'+IntToStr(Ord(ATrimOption)));
    idx := -1;
  end;

  if idx > 0 then
    Delete(s, 1, idx);
end;

function TCustomHelp.SearchInHxSession(hxSession: IHxSession;
  const HelpString: string; AResult: TStringList;
  hxIndex: IHxIndex): Boolean;
var
  Topics: IHxTopicList;
  idx: Integer;
  slot: Integer;
  s : String;
  g: string;
begin
  Result:=False;
  FSessionLock.Acquire;
  try
    slot := hxIndex.GetSlotFromString(HelpString);
    if AnsiContainsText(hxIndex.GetStringFromSlot(slot), HelpString) then
    begin
      Topics := hxIndex.GetTopicsFromSlot(slot);
      g := GetNamespaceTitle(hxSession);
      for idx := 1 to Topics.Count do
      begin
        with Topics.Item(idx) do
          s:=TCustomHelp.EncodeURL(Title[HxTopicGetRLTitle, 0], Location, URL, g, nstoNoTrim);
        AResult.Add(s);
        Result:=True;
      end;
    end;
  finally
    FSessionLock.Release;
  end;
end;

function TCustomHelp.QueryInHxSession(hxSession: IHxSession;
  const HelpString: string; const AResult: TStringList): Boolean;
var
  Topics: IHxTopicList;
  idx: Integer;
  s : String;
  g: string;
begin
  Result := false;
  FSessionLock.Acquire;
  try
    Topics:=hxSession.Query(HelpString, '!DefaultFullTextSearch', HxQuery_No_Option, '');

    g := GetNamespaceTitle(hxSession);
    for idx := 1 to Topics.Count do
    begin
      with Topics.Item(idx) do
        s:=TCustomHelp.EncodeURL(Title[HxTopicGetRLTitle, 0], Location, URL, g, nstoNoTrim);
      AResult.Add(s);
      Result:=True;
    end;
  finally
    FSessionLock.Release;
  end;
end;

procedure TCustomHelpViewer.SoftShutDown;
var
  hs : IHelpSystem;
begin
  if GetHelpSystem(hs) then
  begin
    hs.AssignHelpSelector(nil);
  end;
end;

function TCustomHelpViewer.UnderstandsKeyword(const HelpString: String): Integer;
begin
  //Das Hilfesystem fragt uns: Verstehst du dieses Keyword (der Begriff unter dem Cursor)?
  Result := 0;
  if AnsiContainsText(HelpString, 'erroneous type') then
    Exit;

  with GetHelpStrings(HelpString) do
  begin
    Result := Count;
    Free;
  end;

  GlobalCustomHelp.InitHelpSelector(HelpString);
end;

{ TCustomHelp }

class function TCustomHelp.CheckIndexInHxSession(hxSession: IHxSession;
  var hxIndex: IHxIndex): boolean;
begin
  Result := Supports(hxSession.GetNavigationObject('!DefaultKeywordIndex', ''), IID_IHxIndex, hxIndex);
end;

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
begin
  FEnabledhxSessions:=TInterfaceList.Create;
  FSessionLock := TCriticalSection.Create;
  FProvider := TStringList.Create;
  FHandledSchemes := TStringList.Create;
  FHandledSchemes.QuoteChar:='"';
  FHandledSchemes.Delimiter:=';';
  FHandledSchemes.StrictDelimiter:=True;

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
                            'http://ref.dp200x.de/dp_reference.php?securitytoken=guest&tabbed=1&sbutton=Search&query='+ENVVAR_NAME_FORMATTED_KEYWORD,
                            nstoTrimFirst);

    WriteProviderToRegistry('2',
                            'Koders.com',
                            'Search at koders.com',
                            'http://www.koders.com/default.aspx?submit=Search&la=Delphi&li=*&s='+ENVVAR_NAME_FORMATTED_KEYWORD,
                            nstoTrimFirst);

    WriteProviderToRegistry('3',
                            'Google Codesearch',
                            'Search using Google Codesearch',
                            'http://www.google.com/codesearch?btnG=Code+suchen&hl=de&as_lang=pascal&as_license_restrict=i&as_license=&as_package=&as_filename=&as_case=&as_q='+ENVVAR_NAME_FORMATTED_KEYWORD,
                            nstoTrimFirst);

    WriteProviderToRegistry('3',
                            'MSDN Online',
                            'Search using MSDN Online',
                            'http://search.msdn.microsoft.com/Default.aspx?locale=en-US&Query='+ENVVAR_NAME_FORMATTED_KEYWORD,
                            nstoTrimFirst);

    LoadProviderFromRegistry;
  end;
end;

class function TCustomHelp.GetTopicFromURL(hxHierarchy: IHxHierarchy; const URL: string): IHxTopic;
var
  si: PSafeArray;
  ari: TIntegerDynArray;
  hNode: integer;
begin
  try
    si := hxHierarchy.GetSyncInfo(URL);
    ari := SafeArrayToIntArray(si);
    hNode := ari[Length(ari)-1];
    Result := hxHierarchy.GetTopic(hNode);
  except
    try
      hNode := hxHierarchy.GetNextFromNode(hxHierarchy.GetPrevFromUrl(URL));
      Result := hxHierarchy.GetTopic(hNode);
    except
      hNode := hxHierarchy.GetPrevFromNode(hxHierarchy.GetNextFromUrl(URL));
      Result := hxHierarchy.GetTopic(hNode);
    end;
  end;
end;

class function TCustomHelp.GetTopicFromURL(const URL: string; var Group: string): IHxTopic;
var
  idx: Integer;
  hxSession: IHxSession;
  hxHierarchy: IHxHierarchy;
begin
  Result := nil;
  for idx := 0 to GlobalCustomHelp.FEnabledhxSessions.Count - 1 do
  begin
    hxSession := GlobalCustomHelp.EnabledhxSession[idx];
    if hxSession <> nil then
    begin
      if not AnsiStartsText(PROTPREFIX_MSHELP+GlobalCustomHelp.GetNamespaceName(hxSession), URL) then
        Continue;
      try
        if Supports(hxSession.GetNavigationObject('!DefaultToc', ''), IHxHierarchy, hxHierarchy) then
        begin
          Result := GetTopicFromURL(hxHierarchy, URL);
          Group := GlobalCustomHelp.GetNamespaceTitle(hxSession);
        end;
      except
      end;
    end;
  end;
end;

class function TCustomHelp.GetTopicInfo(const URL: string;
  out Caption, Description, Link, Group: string;
  out TrimOption: TNamespaceTrimOption): boolean;
var
  tp: IHxTopic;
begin
  tp := GetTopicFromURL(URL, Group);
  Result := tp <> nil;
  if Result then
  begin
    Caption := tp.Title[HxTopicGetHTMTitle, 0];
    Description := tp.Location;
    Link := URL;
    TrimOption := nstoNoTrim;
  end else
  begin
    Caption := URL;
    Description := '';
    Link := URL;
    TrimOption := nstoNoTrim;
  end;
end;

class function TCustomHelp.DecodeURL(const URL: String; out Caption,
  Description, Link: String; out Group: String;
  out TrimOption: TNamespaceTrimOption): boolean;
var
  sl : TStringList;
begin
  Result:=False;
  if AnsiStartsText(PROTPREFIX_CUSTOMHELP, URL) then
  begin
    sl:=TStringList.Create;
    try
      sl.QuoteChar:=#0;
      sl.Delimiter:='|';
      sl.StrictDelimiter:=True;
      sl.DelimitedText:=Copy(URL, Length(PROTPREFIX_CUSTOMHELP)+1, Length(URL));
      Caption:=Sl[0];
      Description:=sl[1];
      Link:=sl[2];
      Group:=sl[3];
      TrimOption:=TNamespaceTrimOption(StrToIntDef(sl[4],0));
    finally
      sl.Free;
    end;
    Result:=True;
  end
  else if AnsiStartsText(PROTPREFIX_MSHELP, URL) then
  begin
    Result := GetTopicInfo(URL, Caption, Description, Link, Group, TrimOption);
    if not Result then
    begin
      TrimOption:=nstoNoTrim;
      sl:=TStringList.Create;
      try
        sl.QuoteChar:=#0;
        sl.Delimiter:='/';
        sl.StrictDelimiter:=True;
        sl.DelimitedText:=Copy(URL, Length(PROTPREFIX_MSHELP)+1, Length(URL));
        if sl.Count >= 2 then
        begin
          Description:=sl[0];
          Caption:=sl[sl.Count - 1];
          Link:=URL;
          Group := GROUP_LABEL_STANDARD;
          Result := True;
        end else
        begin
          Description:='';
          Caption:=URL;
          Link:=URL;
          Group := GROUP_LABEL_STANDARD;
          Result := True;
        end;
      finally
        sl.Free;
      end;
    end;
  end;
end;

class function TCustomHelp.DecodeURL(const URL: String; out Link: String): boolean;
var
  Caption, Description, Group: string;
  TrimOption: TNamespaceTrimOption;
begin
  Result:=False;
  if AnsiStartsText(PROTPREFIX_CUSTOMHELP, URL) then
  begin
    Result := DecodeURL(URL, Caption, Description, Link, Group, TrimOption);
  end
  else if AnsiStartsText(PROTPREFIX_MSHELP, URL) then
  begin
    Link := URL;
    Result := True;
  end;
end;

destructor TCustomHelp.Destroy;
begin
  DisconnectFromIDE;

  FEnabledhxSessions.Free;
  FProvider.Free;
  FSessionLock.Free;

  FCustomHelpViewer := nil;

  if Assigned(HelpViewer.FHelpManager) then
    HelpViewer.ShutDown;
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
  Result:=PROTPREFIX_CUSTOMHELP+Caption+'|'+Description+'|'+Link+'|'+Group+'|'+IntToStr(Integer(TrimOption));
end;

function TCustomHelp.GetEnabledhxSession(Index: Integer): IHxSession;
begin
  if not Supports(EnabledhxSessions[Index], IHxSession, Result) then
    Result := nil;
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

class function TCustomHelp.GetNamespaceName(Session: IHxSession): string;
begin
  Result:=Session.Collection.GetProperty(HxCollectionProp_NamespaceName);
end;

class function TCustomHelp.GetNamespaces: IHxRegNamespaceList;
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
  hxIndex: IHxIndex;
  errmsg: string;
  nsList: IHxRegNamespaceList;
begin
  FEnabledhxSessions.Clear;
  sl:=TStringList.Create;
  errmsg := '';
  try
    ReadEnabledNamespacesFromRegistry(sl);

    nsList := Namespaces;
    for idx := 1 to nsList.Count do
    begin
      if sl.IndexOf(nsList.Item(idx).Name)>=0 then
      begin
        hxSession:=CoHxSession.Create;
        hxSession.Initialize(PROTPREFIX_MSHELP+nsList.Item(idx).Name,0);
        try
          CheckIndexInHxSession(hxSession, hxIndex);
        except on e: Exception do
          begin
            errmsg := errmsg + nsList.Item(idx).Name + ': '+ e.Message + #13#10;
            // WriteNamespacesToRegistry(nsList.Item(idx).Name, false);
          end;
        end;
        FEnabledhxSessions.Add(hxSession);
      end;
    end;
    if errmsg <> '' then
      ShowMessage('The following namespaces had errors:'#13#10#13#10 + errmsg);
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
    FShowOHSAtTop:=sl.Values[SETTINGS_OHSATTOP]='1';
    FFullTextSearch:=sl.Values[SETTINGS_FULLTEXTSEARCH]='1';
    FTrimNamespaces:=TNamespaceTrimOption(StrToIntDef(sl.Values[SETTINGS_TRIMNAMESPACES], 0));
    FHandledSchemes.DelimitedText:=sl.Values[SETTINGS_HANDLEDSCHEMES];
    FReplaceDefaultViewer:=sl.Values[SETTINGS_REPLACEDEFAULT]='1';
  finally
    sl.Free;
  end;
end;

procedure TCustomHelp.OnMenuItemClick(Sender: TObject);
begin
  if Tform_Config.Execute then
    LoadProviderFromRegistry;
end;

function TCustomHelp.PerformInHxSession(HelpString: string;
  SessionIndex: integer; const AResult: TStringList): Boolean;
var
  hxSession: IHxSession;
  hxIndex: IHxIndex;
begin
  Result:=False;
  hxSession := EnabledhxSession[SessionIndex];
  if hxSession <> nil then
  begin
    //Soll nach dem kompletten Text gesucht werden?
    if PerformFullTextSearch then
    begin
      Result:=QueryInHxSession(hxSession, HelpString, AResult);
    end //oder nur im Index der Hilfe
    else if CheckIndexInHxSession(hxSession, hxIndex) then
    begin
      Result:=SearchInHxSession(hxSession, HelpString, AResult, hxIndex);
    end;
  end;
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

procedure TCustomHelp.SetCheckWinHelpGid(const Value: Boolean);
begin
  FCheckWinHelpGid := Value;
  TCustomHelp.WriteSettingToRegistry(SETTING_WINHELPGIDCHECK, IntToStr(byte(Value)));
end;

procedure TCustomHelp.SetFullTextSearch(const Value: Boolean);
begin
  FFullTextSearch := Value;
  TCustomHelp.WriteSettingToRegistry(SETTINGS_FULLTEXTSEARCH, IntToStr(byte(Value)));
end;

procedure TCustomHelp.SetRedirectSchemes(const Value: string);
begin
  FHandledSchemes.DelimitedText := Value;
  TCustomHelp.WriteSettingToRegistry(SETTINGS_HANDLEDSCHEMES, Value);
end;

procedure TCustomHelp.SetReplaceDefaultViewer(const Value: boolean);
begin
  FReplaceDefaultViewer := Value;
  TCustomHelp.WriteSettingToRegistry(SETTINGS_REPLACEDEFAULT, IntToStr(byte(Value)));
end;

procedure TCustomHelp.SetShowCustomHelpOnWP(const Value: Boolean);
begin
  FShowCustHelpOnWP := Value;
  TCustomHelp.WriteSettingToRegistry(SETTINGS_CUSTHELPWP, IntToStr(byte(Value)));
end;

procedure TCustomHelp.SetShowOHSAtTop(const Value: boolean);
begin
  FShowOHSAtTop := Value;
  TCustomHelp.WriteSettingToRegistry(SETTINGS_OHSATTOP, IntToStr(byte(Value)));
end;

procedure TCustomHelp.SetTrimNamespaces(const Value: TNamespaceTrimOption);
begin
  FTrimNamespaces := Value;
  TCustomHelp.WriteSettingToRegistry(SETTINGS_TRIMNAMESPACES, IntToStr(byte(Value)));
end;

class function TCustomHelp.CheckGidFile(AWinHelpFile: String): Boolean;
var
  GIDFile : String;
begin
  GIDFile:=ChangeFileExt(AWinHelpFile, '.gid');

  Result:=FileExists(GIDFile);
  if not Result then
    ShowMessage('No *.gid file available for '+AWinHelpFile+' no help query possible.'+
                #13#10'Please open the help file using winhlp32.exe and generate indes manually');
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
