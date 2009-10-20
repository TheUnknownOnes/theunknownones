{-----------------------------------------------------------------------------
 Purpose: The main unit of the custom help expert

 (c) by TheUnknownOnes
 see http://www.TheUnknownOnes.net
-----------------------------------------------------------------------------}
unit uCustomHelpMain;

interface

uses
  Classes, Forms, Dialogs, ToolsAPI, Menus, Registry, HelpIntfs, Windows,
  uMSHelpServices, uHtmlHelp, SyncObjs, uCustomHelpSelector;

type
  TNamespaceTrimOption = (nstoNoTrim=0, nstoTrimFirst, nstoTrimAll);

  //Main object
  TCustomHelp = class
  private
    FLastKeyword: String;
    FLastHelpErrors: string;
    FSessionLock: TCriticalSection;
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
    class function GetSessionFromURL(const URL: string; var Group: string): IHxSession;
    class function GetTopicFromURL(hxHierarchy: IHxHierarchy; const URL: string): IHxTopic; overload;
    class function GetTopicInfo(const URL: string; out Caption, Description, Link, Group: string;
      out TrimOption: TNamespaceTrimOption): boolean;
    class function DecodeURL(const URL: String; out Link: String): boolean; overload;
    class function CheckGidFile(AWinHelpFile: String; const ARaiseError: Boolean): Boolean; static;
    procedure ShowHelp(const HelpString: String; const SelectedKeyword: string); overload;
    function GetViewerID: integer;
  end;

const
    REG_ROOT_BASE = '\TheUnknownOnes\Delphi';
    REG_ROOT_PROJECT = '\CustomHelp';

  {$ifDef VER170}
     REG_ROOT_KEY = '\Software'+REG_ROOT_BASE+'\VER170' + REG_ROOT_PROJECT;
  {$Endif}
  {$ifDef VER180}
    {$ifDef VER185}
      REG_ROOT_KEY = '\Software'+REG_ROOT_BASE+'\VER185' + REG_ROOT_PROJECT;
    {$Else}
      REG_ROOT_KEY = '\Software'+REG_ROOT_BASE+'\VER180' + REG_ROOT_PROJECT;
    {$Endif}
  {$Endif}
  {$ifDef VER200}
    REG_ROOT_KEY = '\Software'+REG_ROOT_BASE+'\VER200' + REG_ROOT_PROJECT;
  {$Endif}
  {$ifDef VER210}
    REG_ROOT_KEY = '\Software'+REG_ROOT_BASE+'\VER210' + REG_ROOT_PROJECT;
  {$Endif}

const
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
  SETTINGS_MIGRATION_COMPLETE = 'SettingsMigrated';

  GROUP_LABEL_DEFAULT = 'Available Search engines';
  GROUP_LABEL_STANDARD = 'Other Help Providers';

  ENVVAR_NAME_KEYWORD = 'HelpString';
  ENVVAR_NAME_KEYWORD_URL = 'HelpStringURLEncoded';
  KIBITZ_IGNORED_HELPSTRING = 'erroneous type';

var
  GlobalCustomHelp : TCustomHelp;

  //We keep these global ... as it is done in WinHelpViewer.pas
  //and keep the reference to the object in the implementation section.
  HelpViewerIntf: ICustomHelpViewer;

implementation

uses
  SysUtils, StrUtils, ShellAPI, uFormConfigCustomHelp,
  Graphics, ActiveX, Variants, Types, uUtils,
  uCustomHelpKeywordRecorder, uCustomHelpIDEIntegration;

type
  TCustomHelpViewer = class(TInterfacedObject,
    ICustomHelpViewer)
  private
    FHelpManager: IHelpManager;
    FViewerID: Integer;
    procedure ShowHTMLHelp(AURL: String);
    procedure ForceSelector(const HelpString: String);
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

    function  InternalGetHelpStrings(const HelpString: String; const AddProviders: Boolean): TStringList;
  private
    FEnabled: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ShowHelp(const HelpString: String); overload;
    procedure ShowHelp(const HelpString: String; const SelectedKeyword: string); overload;
    property HelpManager : IHelpManager read FHelpManager write FHelpManager;
    property ViewerID: Integer read FViewerID;
    property Name: String read GetViewerName;
    function CheckUnderstandsKeyword(const HelpString: String): Boolean;
    property Enabled: boolean read FEnabled;
  end;

var
  HelpViewer: TCustomHelpViewer;

{$WARN SYMBOL_PLATFORM OFF}

function EncodedHelpString(AHelpString: String): String;
var
  i: integer;
begin
  Result:='';
  for i := 1 to Length(AHelpString)do
    Result:=Result + '%'+Format('%.2x', [Ord(AHelpString[i])]);
end;

procedure AddEnvVar(sl: TStringList; const AName, AValue: string);
begin
  sl.Values[AName] := AValue;
end;

procedure ExpandEnvVars(var s: string; const HelpString: string);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    AddEnvVar(sl, ENVVAR_NAME_KEYWORD, HelpString);
    AddEnvVar(sl, ENVVAR_NAME_KEYWORD_URL, EncodedHelpString(HelpString));
    uUtils.ExpandEnvVars(s, sl);
  finally
    sl.Free;
  end;
end;

{ TCustomHelpViewer }

function TCustomHelpViewer.CanShowTableOfContents: Boolean;
begin
  Result := false;
end;

function TCustomHelpViewer.GetHelpStrings(
  const HelpString: String): TStringList;
var
  idx: Integer;
begin
  Result := InternalGetHelpStrings(HelpString, true);
end;

function TCustomHelpViewer.InternalGetHelpStrings(const HelpString: String; const AddProviders: Boolean): TStringList;
var
  idx : Integer;
  c, d, u, g : String;
  ShortHelpString : String;
  HelpStrings, errmsgs: TStringList;
  TrimOption: TNamespaceTrimOption;
  sl : TStringList;
begin
  Result := nil;
  GlobalCustomHelp.LastHelpCallKeyword := HelpString;

  //Weil wir bei UnderstandsKeyword gesagt haben, das wir das Keyword verstehen (Result = 1)
  //werden wir jetzt gefragt, welche Hilfethemen wir zu diesem Keyword liefern können
  //Die StringList wird vom Hilfesystem wieder freigegeben

  HelpStrings := TStringList.Create;
  sl := TStringList.Create;
  try
    HelpStrings.Duplicates:=dupIgnore;
    sl.CaseSensitive := False;

    errmsgs := TStringList.Create;
    try
      errmsgs.Sorted := True;
      errmsgs.Duplicates:=dupIgnore;
      // Und jetzt noch die eigentlichen Hilfe-Namespaces
      for idx := 0 to GlobalCustomHelp.EnabledhxSessions.Count - 1 do
      begin
        try
          ShortHelpString := HelpString;
          if CustomHelpKeywordRecorderIntf.GetEnabled then
            CustomHelpKeywordRecorderIntf.AddKeyword(ShortHelpString, true);
          if not GlobalCustomHelp.PerformInHxSession(ShortHelpString, idx, HelpStrings) then
            if GlobalCustomHelp.TrimNamespacesUntilResultFound <> nstoNoTrim then
            begin
              repeat
                LeftToken(ShortHelpString, '.', true);
                if ShortHelpString = '' then
                  Break;
                if CustomHelpKeywordRecorderIntf.GetEnabled then
                  CustomHelpKeywordRecorderIntf.AddKeyword(ShortHelpString, true);
                if GlobalCustomHelp.PerformInHxSession(ShortHelpString, idx, HelpStrings) then
                  Break;
                if GlobalCustomHelp.TrimNamespacesUntilResultFound = nstoTrimFirst then
                  Break;
              until ShortHelpString = '';
            end;
        except
          on e: Exception do
            errmsgs.Add(GlobalCustomHelp.GetNamespaceName(GlobalCustomHelp.EnabledhxSession[idx]) + ': '+ e.Message);
        end;
      end;

      if AddProviders then
      begin
        for idx := 0 to GlobalCustomHelp.ProviderList.Count - 1 do
        begin
          if not TCustomHelp.DecodeURL(GlobalCustomHelp.ProviderList.Strings[idx], c, d, u, g, TrimOption) then
            Continue;

          ShortHelpString:=HelpString;
          TCustomHelp.TrimNamespace(ShortHelpString, TrimOption);

          try
            if Pos('://', u)>0 then
            begin
              if PosText(EnvVarToken(ENVVAR_NAME_KEYWORD),u)=0 then
                u:=u+EnvVarToken(ENVVAR_NAME_KEYWORD_URL)
              else
                u:=ReplaceText(u,
                               EnvVarToken(ENVVAR_NAME_KEYWORD),
                               EnvVarToken(ENVVAR_NAME_KEYWORD_URL));

              HelpStrings.Add(TCustomHelp.EncodeURL(c,d,u, g, TrimOption))
            end
            else
            if AnsiSameText(ExtractFileExt(u),'.hlp') then
            begin
              ShortHelpString:=AnsiReplaceStr(ShortHelpString, '.', ',');

              if (not GlobalCustomHelp.CheckWinHelpGid) or
                 TCustomHelp.CheckGidFile(u, True) then
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
              HelpStrings.Add(TCustomHelp.EncodeURL(c,d,PROTPREFIX_UNKNOWNHELP+u, g, TrimOption));
            end;
          except
            on e: Exception do
              errmsgs.Add(u + ': '+ e.Message);
          end;
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
    if StrUtils.AnsiStartsText(FHandledSchemes[idx], AKeyword) then
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

function TCustomHelpViewer.CheckUnderstandsKeyword(
  const HelpString: String): Boolean;
var
  hs: IHelpSystem2;
begin
  FEnabled := False;
  try
    // try to figure out, if someone else has results for this keyword ...
    if GetHelpSystem(hs) then
      Result := hs.UnderstandsKeyword(HelpString, FHelpManager.GetHelpFile)
    else
      Result := True;
  finally
    FEnabled := True;
  end;
end;


constructor TCustomHelpViewer.Create;
begin
  inherited Create;
  HelpViewerIntf := Self;
  FEnabled := True;
end;

destructor TCustomHelpViewer.Destroy;
begin
  if HelpViewer = self then
    HelpViewer := nil;

  inherited;
end;

procedure TCustomHelpViewer.ForceSelector(const HelpString: String);
var
  sl : TStringList;
  i : integer;
  u : String;
  sk: string;
begin
  sl:=InternalGetHelpStrings(HelpString, true);
  if TFormHelpSelector.Execute(GlobalCustomHelp.LastHelpCallKeyword, sl, i, u, sk) then
    ShowHelp(u, sk);
end;

procedure TCustomHelpViewer.ShowHelp(const HelpString: String);
begin
  ShowHelp(HelpString, '');
end;

procedure TCustomHelpViewer.ShowHelp(const HelpString, SelectedKeyword: string);
var
  u: String;
  alternativeNavigate : boolean;
  sl : TStringList;
  command, params : String;
  hs: IHelpSystem;
begin
  if HelpString<>'' then
  begin
//    GetHelpSystem(hs);
    //Hier gehts dann wirklich um die Wurst
    //Wir bekommen den Hilfestring übergeben, den der
    //Nutzer aus der Liste, die wir bei GetHelpStrings gebaut haben,
    //gewählt hat. Natürlich bekommen wir hier nur die, die wir auch definiert haben

    if TCustomHelp.DecodeURL(HelpString, u) then
    begin
      // Late expand environment variables contained in the url ...
      ExpandEnvVars(u, SelectedKeyword);

      if StrUtils.AnsiStartsText(PROTPREFIX_UNKNOWNHELP, u) then
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
      if StrUtils.AnsiStartsText(PROTPREFIX_WINHELP, u) then
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
      if StrUtils.AnsiStartsText(PROTPREFIX_HTMLHELP, u) then
      begin
        Delete(u,1,Length(PROTPREFIX_HTMLHELP));
        ShowHTMLHelp(u);
      end
      else
      if not GlobalCustomHelp.ReplaceDefaultViewer and
        ( GlobalCustomHelp.IsHandledByDefaultViewer(u) or
          AnsiStartsText([PROTPREFIX_MSHELP, 'http://', 'https://', 'file://', 'bds://'], u)) then
      begin
        if GetHelpSystem(hs) then
          hs.ShowTopicHelp(u, '');
      end
      else
      begin
        alternativeNavigate := True;
        if GlobalCustomHelp.ShowCustomHelpOnWelcomePage then
          if WelcomePageNavigate(u) then
            alternativeNavigate:=False;

        if alternativeNavigate then
          ShellExecute(Application.Handle, 'open', PChar(u), '', '', SW_SHOWNORMAL);
      end;
    end
    else
    begin
      //oops das haben wir jetzt nicht verstanden....
      //das heißt wir sind die einzige Instanz, die Hilfe angemeldet hat.
      //Wir erzwingen einen Selektor!
      if AnsiSameText(HelpString, KIBITZ_IGNORED_HELPSTRING) or (SelectedKeyword <> '') then
        ForceSelector(SelectedKeyword)
      else
        ForceSelector(HelpString);
    end;
  end;
end;

procedure TCustomHelpViewer.ShowTableOfContents;
begin
end;

procedure TCustomHelpViewer.ShutDown;
var
  hm: IHelpManager;
begin
  SoftShutDown;
  if Assigned(FHelpManager) then
  begin
    hm := FHelpManager;
    FHelpManager := nil;
    hm.Release(FViewerID);
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
var
  dontCheck: Boolean;
  dontAddDefault: Boolean;
  AHelpString: string;
begin
  Result := 0;
  AHelpString := HelpString;

  if not Enabled then
    Exit;

  if AnsiSameText(AHelpString, KIBITZ_IGNORED_HELPSTRING) then
    Exit;

  dontCheck := ShiftDown;

  // Das Hilfesystem fragt uns: Verstehst du dieses Keyword (der Begriff unter dem Cursor)?
  // Die Abfrage auf 'erroneous type' ist nur eine Teillösung, weil das Delphi-Hilfesystem
  // mehrere Abfragen mit unterschiedlichen HelpStrings, allerdings nur dann
  // wenn bis dahin keine Ergebnisse gefunden wurden.
  // PROBLEM: Wie erzwingt man die Anzeige des Selectors, auch wenn "keine"
  // Ergebnisse vorliegen, damit man ggf. alle Varianten prüfen kann?
  // ----- START Delphi 2009 ------
  // Im Formulareditor (Pseudo-Code):
  //    class := typeof(comp);
  //    while class <> nil do
  //    begin
  //      if UnderStandsKeyword(unitofcomp.class.prop) > 0 then Break;
  //      if UnderStandsKeyword(class.prop) > 0 then Break;
  //      class := class.parent;
  //    end;
  //
  // Im Objektinspektor (Pseudo-Code):
  //    class := typeof(comp);
  //    while class <> nil do
  //    begin
  //      if UnderStandsKeyword(unitofcomp.class.prop) > 0 then Break;
  //      if UnderStandsKeyword(class.prop) > 0 then Break;
  //      class := class.parent;
  //    end;
  // Im Codeeditor:
  //    0. Wenn Selektion, dann gehe zu 2.
  //    1. Abfrage mit durch Kibitz-Compiler
  //    1a. Abfrage mit ermittelten Symbol analog zu oben.
  //       (bei Compilierfehler gibt es das untenstehende 'erroneous type'
  //    1b. Bei Properties: Analog zu oben mit Klasse des Symbols.
  //    2. Abfrage des aktuellen Wortes / der Selektion
  //    3. Schleife: Abfrage des am Anfang gekürzten Wortes / der Selektion
  //    4. Schleife: Abfrage des am Ende gekürzten Wortes / der Selektion
  // ----- ENDE Delphi 2009 ------
  dontAddDefault := False;

  if not dontCheck then
    dontAddDefault := not CheckUnderstandsKeyword(AHelpString);

  // add default entries, if we are in a second call to the help system
  // with the same keyword and the first call did not yield any result
  // (otherwise Keywords.Count is empty)
  with CustomHelpKeywordRecorderIntf, GetKeywordList do
    if dontAddDefault and (Count > 0) and (Strings[0] = AHelpString) then
    begin
      dontAddDefault := False;
      AddKeyword(AHelpString);
      Delete(Count - 1);
      if Count > 0 then
        AHelpString := Strings[Count - 1];
    end
    else
    begin
      // add "missing" keyword.
      AddKeyword(AHelpString);
    end;
  // Start recording of keywords and add "missing" keyword.
  CustomHelpKeywordRecorderIntf.SetEnabled(True);

  with InternalGetHelpStrings(AHelpString, not dontAddDefault) do
  begin
    Result := Count;
    Free;
  end;

  result:=1;

  GlobalCustomHelp.InitHelpSelector(AHelpString);
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
  // clear keyword history ...
  CustomHelpKeywordRecorderIntf.SetEnabled(False);

  ConnectToIDE;
  LoadProviderFromRegistry;

  //Falls es keine Provider gibt, schreiben wir mal die Standards rein
  if FProvider.Count = 0 then
  begin
    WriteProviderToRegistry('1',
                            'DP DelphiReference',
                            'Search with Daniels Cool Tool',
                            'http://ref.dp200x.de/dp_reference.php?query='+EnvVarToken(ENVVAR_NAME_KEYWORD),
                            nstoTrimFirst);

    WriteProviderToRegistry('2',
                            'Koders.com',
                            'Search at koders.com',
                            'http://www.koders.com/default.aspx?submit=Search&la=Delphi&li=*&s='+EnvVarToken(ENVVAR_NAME_KEYWORD),
                            nstoTrimFirst);

    WriteProviderToRegistry('3',
                            'Google Codesearch',
                            'Search using Google Codesearch',
                            'http://www.google.com/codesearch?btnG=Code+suchen&hl=de&as_lang=pascal&as_license_restrict=i&as_license=&as_package=&as_filename=&as_case=&as_q='+EnvVarToken(ENVVAR_NAME_KEYWORD),
                            nstoTrimFirst);

    WriteProviderToRegistry('4',
                            'MSDN Online',
                            'Search using MSDN Online',
                            'http://search.msdn.microsoft.com/Default.aspx?locale=en-US&Query='+EnvVarToken(ENVVAR_NAME_KEYWORD),
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
      try
        hNode := hxHierarchy.GetPrevFromNode(hxHierarchy.GetNextFromUrl(URL));
        Result := hxHierarchy.GetTopic(hNode);
      except
        Result := nil;
      end;
    end;
  end;
end;

class function TCustomHelp.GetSessionFromURL(const URL: string; var Group: string): IHxSession;
var
  idx: integer;
  nsName: string;
begin
  Result := nil;
  if not StrUtils.AnsiStartsText(PROTPREFIX_MSHELP, URL) then
    Exit;
  idx := PosEx('/', URL, Length(PROTPREFIX_MSHELP)+1) - 1;
  if idx < 0 then
    nsName := URL
  else
    nsName := Copy(URL, 1, idx);

  Result := CoHxSession.Create;
  Result.Initialize(nsName, 0);
  Group := GlobalCustomHelp.GetNamespaceTitle(Result);
end;

class function TCustomHelp.GetTopicInfo(const URL: string;
  out Caption, Description, Link, Group: string;
  out TrimOption: TNamespaceTrimOption): boolean;
var
  hxSession: IHxSession;
  hxHierarchy: IHxHierarchy;
  tp: IHxTopic;
begin
  tp := nil;
  hxSession := GetSessionFromURL(URL, Group);
  if hxSession <> nil then
    try
      if Supports(hxSession.GetNavigationObject('!DefaultToc', ''), IHxHierarchy, hxHierarchy) then
        tp := GetTopicFromURL(hxHierarchy, URL);
    except
    end;
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

function TCustomHelp.GetViewerID: integer;
begin
  Result := -1;
  if Assigned(HelpViewerIntf) then
    Result := HelpViewer.FViewerID;
end;

class function TCustomHelp.DecodeURL(const URL: String; out Caption,
  Description, Link: String; out Group: String;
  out TrimOption: TNamespaceTrimOption): boolean;
var
  sl : TStringList;
begin
  Result:=False;
  if StrUtils.AnsiStartsText(PROTPREFIX_CUSTOMHELP, URL) then
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
  else if StrUtils.AnsiStartsText(PROTPREFIX_MSHELP, URL) then
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
  if StrUtils.AnsiStartsText(PROTPREFIX_CUSTOMHELP, URL) then
  begin
    Result := DecodeURL(URL, Caption, Description, Link, Group, TrimOption);
  end
  else if StrUtils.AnsiStartsText(PROTPREFIX_MSHELP, URL) then
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

  HelpViewerIntf := nil; // This will automatically clear HelpViewer if object is destroyed.
  if HelpViewer <> nil then // This will unregister the viewer from the help system.
    HelpViewer.ShutDown;

  if GlobalCustomHelp = self then
    GlobalCustomHelp := nil;
    
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
        try
          hxSession:=CoHxSession.Create;
          hxSession.Initialize(PROTPREFIX_MSHELP+nsList.Item(idx).Name,0);
        
          CheckIndexInHxSession(hxSession, hxIndex);
        except
          on e: Exception do
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

procedure TCustomHelp.ShowHelp(const HelpString, SelectedKeyword: string);
begin
  if Assigned(HelpViewerIntf) then
    HelpViewer.ShowHelp(HelpString, SelectedKeyword);
end;

class function TCustomHelp.CheckGidFile(AWinHelpFile: String; const ARaiseError: Boolean): Boolean;
const
  msg = 'No gid file%1:s available. No help query possible.%0:s'+
        'Please open the help file using winhlp32.exe and generate index manually.';
var
  GIDFile : String;
begin
  GIDFile:=ChangeFileExt(AWinHelpFile, '.gid');

  Result := FileExists(GIDFile);
  if not Result then
  begin
    if ARaiseError then
      raise EHelpSystemException.CreateFmt(msg, [ ' ', '' ]);
    ShowMessageFmt(msg, [ sLineBreak, 'for "'+AWinHelpFile+'"' ]);
  end;
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

procedure AddSplashBitmap;
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
  AddSplashBitmap;

finalization
  if Assigned(GlobalCustomHelp) then
    FreeAndNil(GlobalCustomHelp);
  if Assigned(bmp) then
    bmp.Free;

end.
