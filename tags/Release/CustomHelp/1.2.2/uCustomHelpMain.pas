{-----------------------------------------------------------------------------
 Purpose: The main unit of the custom help expert

 (c) by TheUnknownOnes under Apache License 2.0
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
  uMSHelpServices,
  uHtmlHelp,
  SyncObjs,
  uCustomHelpSelector,
  uCustomHelpConsts,
  Graphics,
  msxml;

type
  TProviderType = (ptStandard, ptRSS);

  //Main object
  TCustomHelp = class
  private
    FLastKeyword:     string;
    FLastHelpErrors:  string;
    FSessionLock:     TCriticalSection;
    FCheckWinHelpGid: Boolean;

    procedure SetCheckWinHelpGid(const Value: Boolean);
    class function GetNamespaces: IHxRegNamespaceList;
    function GetEnabledhxSession(Index: Integer): IHxSession;
    procedure SetFullTextSearch(const Value: Boolean);
    procedure SetTrimNamespaces(const Value: TNamespaceTrimOption);
    procedure SetDisplayLocation(const Value: TDisplayLocationOption);
    function GetResultOrderOfIndex(AIndex: Integer): string;
    function GetResultOrderOfGroupName(AGroup: string): Integer;
    procedure InternalLoadProviderFromRegistry(ATarget: TStrings; ARootKey: string);
    function GetColor(AProvider: string): TColor;
    procedure SetColor(AProvider: string; const Value: TColor);
  protected
    FMenuItem: TMenuItem;

    F3rdPartyViewers, FProvider, FRSSProvider: TStringList;
    FShowCustHelpOnWP:  Boolean;
    FFullTextSearch:    Boolean;
    FEnabledhxSessions: TInterfaceList;
    FTrimNamespaces:    TNamespaceTrimOption;
    FDisplayLocation:   TDisplayLocationOption;
    FResultOrder:       TStringList;

    //hack into the IDE (Menu-entry, ...)
    procedure ConnectToIDE;
    procedure DisconnectFromIDE;
    function GetHelpMenu: TMenuItem;

    procedure LoadProviderFromRegistry;
    procedure LoadRSSProviderFromRegistry;
    procedure LoadSettingsFromRegistry;
    procedure LoadResultOrderFromRegistry;
    procedure LoadEnabledNamespacesFromRegistry;

    procedure OnMenuItemClick(Sender: TObject);

    class function GetNamespaceTitle(Session: IHxSession): string;
    class function GetNamespaceName(Session: IHxSession): string;
    class procedure TrimNamespace(var s: string; ATrimOption: TNamespaceTrimOption);
    class function CheckIndexInHxSession(hxSession: IHxSession;
      var hxIndex: IHxIndex): Boolean;
    function SearchInHxSession(hxSession: IHxSession; const HelpString: string;
      AResult: TStringList; hxIndex: IHxIndex): Boolean;
    function QueryInHxSession(hxSession: IHxSession; const HelpString: string;
      const AResult: TStringList): Boolean;
    function PerformInHxSession(HelpString: string; SessionIndex: Integer;
      const AResult: TStringList): Boolean;
  public
    constructor Create();
    destructor Destroy(); override;

    property _3rdPartyViewers: TStringList read F3rdPartyViewers; //self learning list of installed 3rd Party help ICustomHelpViewers

    property ProviderList: TStringList read FProvider;
    property RSSProviderList: TStringList read FRSSProvider;
    property Namespaces: IHxRegNamespaceList read GetNamespaces;
    property EnabledhxSessions: TInterfaceList read FEnabledhxSessions;

    property DisplayLocation: TDisplayLocationOption
      read FDisplayLocation write SetDisplayLocation;
    property ResultOrderFromIndex[AIndex: Integer]: string read GetResultOrderOfIndex;
    property ResultOrderFromString[AGroup: string]: Integer
      read GetResultOrderOfGroupName;
    property PerformFullTextSearch: Boolean read FFullTextSearch write SetFullTextSearch;
    property LastHelpCallKeyword: string read FLastKeyword write FLastKeyword;
    property TrimNamespacesUntilResultFound: TNamespaceTrimOption
      read FTrimNamespaces write SetTrimNamespaces;

    property Color[AProvider: string]: TColor read GetColor write SetColor;

    property EnabledhxSession[Index: Integer]: IHxSession read GetEnabledhxSession;
    property LastHelpErrors: string read FLastHelpErrors write FLastHelpErrors;

    procedure InitHelpSelector(const HelpString: string);
    property CheckWinHelpGid: Boolean read FCheckWinHelpGid write SetCheckWinHelpGid;

    class function DecodeURL(const URL: string; out Caption: string;
      out Description: string; out Link: string; out Group: string;
      out TrimOption: TNamespaceTrimOption; out Enabled: Boolean): Boolean;
      overload;
    class function EncodeURL(Caption, Description, Link, Group: string;
      TrimOption: TNamespaceTrimOption; AEnabled: Boolean): string;
    class procedure WriteProviderToRegistry(AKeyName, AName, ADesc, AURL: string;
      ATrimNamespaces: TNamespaceTrimOption; AType: TProviderType; AEnabled: Boolean);
    class procedure WriteNamespacesToRegistry(ANamespace: string; AEnabled: Boolean);
    class procedure WriteResultOrderToRegistry(AOrder: TStrings);
    class procedure ReadEnabledNamespacesFromRegistry(const ANamesList: TStrings);
    class procedure WriteSettingToRegistry(AName, AValue: string); overload;
    class procedure WriteSettingToRegistry(ASubKey, AName, AValue: string); overload;
    class procedure ReadSettingsFromRegistry(const ANameValueList: TStrings);
    class function GetSessionFromURL(const URL: string; var Group: string): IHxSession;
    class function GetTopicFromURL(hxHierarchy: IHxHierarchy;
      const URL: string): IHxTopic; overload;
    class function GetTopicInfo(const URL: string;
      out Caption, Description, Link, Group: string;
      out TrimOption: TNamespaceTrimOption): Boolean;
    class function DecodeURL(const URL: string; out Link: string): Boolean; overload;
    class function CheckGidFile(AWinHelpFile: string;
      const ARaiseError: Boolean): Boolean; static;
    procedure ShowHelp(const HelpString: string); overload;
    function GetViewerID: Integer;
  end;

function REG_ROOT_KEY: string;

var
  GlobalCustomHelp: TCustomHelp;

  //We keep these global ... as it is done in WinHelpViewer.pas
  //and keep the reference to the object in the implementation section.
  HelpViewerIntf:   ICustomHelpViewer;

implementation

uses
  SysUtils,
  StrUtils,
  ShellAPI,
  uFormConfigCustomHelp,
  ActiveX,
  Variants,
  Types,
  uUtils,
  uCustomHelpKeywordRecorder,
  uCustomHelpIDEIntegration,
  UrlMon;

type
  TCustomHelpViewer = class(TInterfacedObject,
    ICustomHelpViewer)
  private
    FHelpManager: IHelpManager;
    FViewerID:    Integer;
    procedure ShowHTMLHelp(AURL: string);
    procedure ForceSelector(const HelpString: string);
  protected
    {$REGION 'ICustomHelpViewer'}
    function GetViewerName: string;
    function UnderstandsKeyword(const HelpString: string): Integer;
    function GetHelpStrings(const HelpString: string): TStringList;
    function CanShowTableOfContents: Boolean;
    procedure ShowTableOfContents;
    procedure NotifyID(const ViewerID: Integer);
    procedure SoftShutDown;
    procedure ShutDown;
    {$ENDREGION}

    function InternalGetHelpStrings(const HelpString: string;
      const AddProviders: Boolean; const AddRSSResults: Boolean): TStringList;
  private
    FEnabled: Boolean;
    procedure InternalGetRSSResults(const AURL, AHelpString: string;
      AList, AErrors: TStrings);
    procedure EnsureValidHelpString(var HelpString: String);
  public
    constructor Create;
    destructor Destroy; override;
    procedure ShowHelp(const HelpString: string);
    property HelpManager: IHelpManager read FHelpManager write FHelpManager;
    property ViewerID: Integer read FViewerID;
    property Name: string read GetViewerName;
    function CheckUnderstandsKeyword(const HelpString: string): Boolean;
    property Enabled: Boolean read FEnabled;
  end;

var
  HelpViewer: TCustomHelpViewer;

{$WARN SYMBOL_PLATFORM OFF}

function EncodedHelpString(AHelpString: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(AHelpString) do
    Result := Result + '%' + Format('%.2x', [Ord(AHelpString[i])]);
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
  Result := False;
end;

function TCustomHelpViewer.GetHelpStrings(const HelpString: string): TStringList;
begin
  Result := InternalGetHelpStrings(HelpString, True, True);
end;

procedure TCustomHelpViewer.InternalGetRSSResults(const AURL, AHelpString: string;
  AList, AErrors: TStrings);
var
  idx, channelidx: Integer;
  Caption, Description, Url, Group:  string;
  provEnabled: Boolean;
  oldc:        string;
  TrimOption:  TNamespaceTrimOption;
  HelpString:  string;
  xmldocument: IXMLDomDocument;
  node:        IXMLDOMNode;
  channels, nodes: IXMLDOMNodeList;
  FileName:    string;
begin
  SetLength(FileName, MAX_PATH + 1);

  try
    if not TCustomHelp.DecodeURL(AURL, Caption, Description, Url, Group, TrimOption, provEnabled) then
      Exit;

    if not provEnabled then
      Exit;

    oldc := Caption;

    HelpString := AHelpString;
    TCustomHelp.TrimNamespace(HelpString, TrimOption);

    if PosText(EnvVarToken(ENVVAR_NAME_KEYWORD), Url) = 0 then
      Url := Url + EnvVarToken(ENVVAR_NAME_KEYWORD_URL)
    else
      Url := ReplaceText(Url, EnvVarToken(ENVVAR_NAME_KEYWORD),
        EnvVarToken(ENVVAR_NAME_KEYWORD_URL));

    ExpandEnvVars(Url, HelpString);

    if URLDownloadToCacheFile(nil, PChar(Url), PChar(FileName), MAX_PATH,
      0, nil) = s_OK then
    begin
      xmldocument       := CoDOMDocument.Create;
      xmldocument.async := False;
      xmldocument.validateOnParse := False;
      xmldocument.load(FileName);

      if xmldocument.parseError.errorCode <> 0 then
        raise Exception.Create('Could not parse RSS data for ' + Caption +
          #13#10 + 'Reason: ' + xmldocument.parseError.reason +
          //reason has a linebreak for itself
          'File saved to: ' + FileName);

      channels := xmldocument.selectNodes('/rss/channel');

      for channelidx := 0 to channels.length - 1 do
      begin
        node := channels[channelidx].selectSingleNode('title');
        if Assigned(node) then
          Group := GROUP_PREFIX_RSS + node.Text
        else
          Group := GROUP_PREFIX_RSS + oldc;

        nodes := channels[channelidx].selectNodes('item');

        for idx := 0 to nodes.length - 1 do
        begin
          Caption := EmptyStr;
          Description := EmptyStr;
          Url := EmptyStr;

          node := nodes[idx].selectSingleNode('title');
          if Assigned(node) then
            Caption := node.Text;
          node := nodes[idx].selectSingleNode('description');
          if Assigned(node) then
            Description := node.Text;
          node := nodes[idx].selectSingleNode('link');
          if Assigned(node) then
            Url := node.Text;

          AList.Add(TCustomHelp.EncodeURL(Caption, Description, Url, Group, TrimOption, provEnabled));
        end;

        if nodes.length > 0 then
        begin
          node := channels[channelidx].selectSingleNode('title');
          if Assigned(node) then
            Caption := node.Text;

          node := channels[channelidx].selectSingleNode('link');
          if Assigned(node) then
            AList.Add(TCustomHelp.EncodeURL(' -=all results=-', 'for ' + Caption,
              node.Text, Group, TrimOption, provEnabled));
        end;
      end;
    end
    else
      raise Exception.Create('Could not download RSS data for ' + Caption);
  except
    on E: Exception do
      AErrors.Add(E.Message)
  end;
end;

function TCustomHelpViewer.InternalGetHelpStrings(const HelpString: string;
  const AddProviders: Boolean; const AddRSSResults: Boolean): TStringList;
var
  idx:         Integer;
  AHelpString : String;
  Caption,
  Description,
  Url,
  Group           : string;
  ShortHelpString : string;
  ProvEnabled     : Boolean;
  HelpStrings,
  errmsgs         : TStringList;
  TrimOption      : TNamespaceTrimOption;
  sl              : TStringList;
begin
  Result := nil;
  AHelpString:=HelpString;
  EnsureValidHelpString(AHelpString);
  GlobalCustomHelp.LastHelpCallKeyword := AHelpString;


  //Weil wir bei UnderstandsKeyword gesagt haben, das wir das Keyword verstehen (Result = 1)
  //werden wir jetzt gefragt, welche Hilfethemen wir zu diesem Keyword liefern können
  //Die StringList wird vom Hilfesystem wieder freigegeben

  HelpStrings := TStringList.Create;
  sl          := TStringList.Create;
  try
    HelpStrings.Duplicates := dupIgnore;
    sl.CaseSensitive := False;

    errmsgs := TStringList.Create;
    try
      errmsgs.Sorted := True;
      errmsgs.Duplicates := dupIgnore;
      // Und jetzt noch die eigentlichen Hilfe-Namespaces
      for idx := 0 to GlobalCustomHelp.EnabledhxSessions.Count - 1 do
      begin
        try
          ShortHelpString := AHelpString;
          if CustomHelpKeywordRecorderIntf.GetEnabled then
            CustomHelpKeywordRecorderIntf.AddKeyword(ShortHelpString, True);
          if not GlobalCustomHelp.PerformInHxSession(ShortHelpString,
            idx, HelpStrings) then
            if GlobalCustomHelp.TrimNamespacesUntilResultFound <> nstoNoTrim then
            begin
              repeat
                LeftToken(ShortHelpString, '.', True);
                if ShortHelpString = '' then
                  Break;
                if CustomHelpKeywordRecorderIntf.GetEnabled then
                  CustomHelpKeywordRecorderIntf.AddKeyword(ShortHelpString, True);
                if GlobalCustomHelp.PerformInHxSession(ShortHelpString,
                  idx, HelpStrings) then
                  Break;
                if GlobalCustomHelp.TrimNamespacesUntilResultFound = nstoTrimFirst then
                  Break;
              until ShortHelpString = '';
            end;
        except
          on e: Exception do
            errmsgs.Add(GlobalCustomHelp.GetNamespaceName(
              GlobalCustomHelp.EnabledhxSession[idx]) + ': ' + e.Message);
        end;
      end;

      if AddProviders then
      begin
        for idx := 0 to GlobalCustomHelp.ProviderList.Count - 1 do
        begin
          if not TCustomHelp.DecodeURL(GlobalCustomHelp.ProviderList.Strings[idx],
            Caption, Description, Url, Group, TrimOption, ProvEnabled) then
            Continue;

          if not ProvEnabled then
            Continue;

          ShortHelpString := AHelpString;
          TCustomHelp.TrimNamespace(ShortHelpString, TrimOption);
          Caption := Caption + #9 + ' search "' + ShortHelpString + '"';

          try
            if Pos('://', Url) > 0 then
            begin
              if PosText(EnvVarToken(ENVVAR_NAME_KEYWORD), Url) = 0 then
                Url := Url + EnvVarToken(ENVVAR_NAME_KEYWORD_URL)
              else
                Url := ReplaceText(Url, EnvVarToken(
                  ENVVAR_NAME_KEYWORD), EnvVarToken(
                  ENVVAR_NAME_KEYWORD_URL));

              ExpandEnvVars(Url, ShortHelpString);

              HelpStrings.Add(TCustomHelp.EncodeURL(Caption, Description, Url, Group, TrimOption, ProvEnabled));
            end
            else if AnsiSameText(ExtractFileExt(Url), '.hlp') then
            begin
              ShortHelpString := AnsiReplaceStr(ShortHelpString, '.', ',');

              if (not GlobalCustomHelp.CheckWinHelpGid) or
                TCustomHelp.CheckGidFile(Url, True) then
              begin
                if (not GlobalCustomHelp.CheckWinHelpGid) or
                  FileContainsText(Url, ShortHelpString) then
                  HelpStrings.Add(TCustomHelp.EncodeURL(
                    Caption, Description, PROTPREFIX_WINHELP + '-k ' + ShortHelpString + ' ' + Url, Group, TrimOption, ProvEnabled));
              end;
            end
            else if AnsiSameText(ExtractFileExt(Url), '.chm') then
            begin
              HelpStrings.Add(TCustomHelp.EncodeURL(
                Caption, Description, PROTPREFIX_HTMLHELP + ShortHelpString + URL_SPLITTER + Url, Group, TrimOption, ProvEnabled));
            end
            else
            begin
              ExpandEnvVars(Url, ShortHelpString);

              HelpStrings.Add(TCustomHelp.EncodeURL(Caption, Description, PROTPREFIX_UNKNOWNHELP + Url,
                Group, TrimOption, ProvEnabled));
            end;
          except
            on e: Exception do
              errmsgs.Add(Url + ': ' + e.Message);
          end;
        end;
      end;

      if AddRSSResults then
        for idx := 0 to GlobalCustomHelp.RSSProviderList.Count - 1 do
        begin
          InternalGetRSSResults(GlobalCustomHelp.RSSProviderList.Strings[idx],
            AHelpString, HelpStrings, errmsgs);
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



class function TCustomHelp.GetNamespaceTitle(Session: IHxSession): string;
var
  NamespaceName: string;
  idx:           Integer;
  nsList:        IHxRegNamespaceList;
begin
  NamespaceName := GetNamespaceName(Session);
  nsList        := GetNamespaces;
  for idx := 1 to nsList.Count do
  begin
    if nsList.Item(idx).Name = NamespaceName then
    begin
      Result := nsList.Item(idx).GetProperty(HxRegNamespaceDescription);
      break;
    end;
  end;
end;

function TCustomHelp.GetResultOrderOfGroupName(AGroup: string): Integer;
var
  idx: Integer;
begin
  Result := 0;
  for idx := 0 to FResultOrder.Count - 1 do
  begin
    if SameText(FResultOrder.ValueFromIndex[idx], AGroup) then
    begin
      Result := StrToInt(FResultOrder.Names[idx]);
      break;
    end;
  end;
end;

function TCustomHelp.GetResultOrderOfIndex(AIndex: Integer): string;
begin
  Result := FResultOrder.Values[Format('%.4d', [AIndex])];
end;

procedure TCustomHelp.InitHelpSelector(const HelpString: string);
var
  hs: IHelpSystem;
begin
  LastHelpCallKeyword := HelpString;
  //the selector has to be constructed every time
  //if not the second help call will crash under BDS2006
  if GetHelpSystem(hs) then
    hs.AssignHelpSelector(THelpSelector.Create);
end;

function TCustomHelpViewer.GetViewerName: string;
begin
  Result := 'Custom Help Viewer (TUO)';
end;

procedure TCustomHelpViewer.NotifyID(const ViewerID: Integer);
begin
  //Das Hilfesystem sagt uns, welche ID wir bekommen haben
  //Die brauchen wir am Ende zum freigeben
  FViewerID := ViewerID;
end;

procedure TCustomHelpViewer.ShowHTMLHelp(AURL: string);
var
  SearchRecord: tagHH_AKLINK;
  sl:           TStringList;
begin
  sl := TStringList.Create;
  try
    sl.Delimiter := URL_SPLITTER;
    sl.QuoteChar       := #0;
    sl.StrictDelimiter := True;
    sl.DelimitedText   := AURL;
    sl.Add('');
    sl.Add('');

    SearchRecord.cbStruct     := sizeof(SearchRecord);
    SearchRecord.fReserved    := False;
    SearchRecord.pszKeywords  := PChar(sl[0]);
    SearchRecord.pszUrl       := nil;
    SearchRecord.pszMsgText   := nil;
    SearchRecord.pszMsgTitle  := nil;
    SearchRecord.pszWindow    := nil;
    SearchRecord.fIndexOnFail := True;

    HtmlHelp(Application.Handle, PChar(sl[1]), HH_DISPLAY_INDEX, 0);
    HtmlHelp(Application.Handle, PChar(sl[1]), HH_KEYWORD_LOOKUP, Cardinal(@SearchRecord));
  finally
    sl.Free;
  end;
end;

function TCustomHelpViewer.CheckUnderstandsKeyword(const HelpString: string): Boolean;
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
  FEnabled       := True;
end;

destructor TCustomHelpViewer.Destroy;
begin
  if HelpViewer = self then
    HelpViewer := nil;

  inherited;
end;

procedure TCustomHelpViewer.ForceSelector(const HelpString: string);
var
  sl: TStringList;
  i:  Integer;
  u:  string;
begin
  sl := InternalGetHelpStrings(HelpString, True, True);
  if TFormHelpSelector.Execute(GlobalCustomHelp.LastHelpCallKeyword, sl, i, u) then
    ShowHelp(u);
end;

procedure TCustomHelpViewer.ShowHelp(const HelpString: string);
var
  u:  string;
  sl: TStringList;
  command, params: string;
  hs: IHelpSystem;
begin
  if HelpString <> '' then
  begin
    //    GetHelpSystem(hs);
    //Hier gehts dann wirklich um die Wurst
    //Wir bekommen den Hilfestring übergeben, den der
    //Nutzer aus der Liste, die wir bei GetHelpStrings gebaut haben,
    //gewählt hat. Natürlich bekommen wir hier nur die, die wir auch definiert haben

    if TCustomHelp.DecodeURL(HelpString, u) then
    begin
      if StrUtils.AnsiStartsText(PROTPREFIX_UNKNOWNHELP, u) then
      begin
        Delete(u, 1, Length(PROTPREFIX_UNKNOWNHELP));
        // we got some unknown help format... just try to execute it ;-)
        sl := TStringList.Create;
        try

          sl.Delimiter     := ' ';
          sl.QuoteChar     := '"';
          sl.DelimitedText := u;
          if sl.Count > 0 then
          begin
            command := '"' + sl[0] + '"';
            sl.Delete(0);
            params  := sl.DelimitedText;

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
      else if StrUtils.AnsiStartsText(PROTPREFIX_WINHELP, u) then
      begin
        Delete(u, 1, Length(PROTPREFIX_WINHELP));
        ShellExecute(Application.Handle,
          'open',
          'winhlp32',
          PChar(u),
          PChar(ExtractFileDir(Application.ExeName)),
          SW_SHOWNORMAL);
      end
      else if StrUtils.AnsiStartsText(PROTPREFIX_HTMLHELP, u) then
      begin
        Delete(u, 1, Length(PROTPREFIX_HTMLHELP));
        ShowHTMLHelp(u);
      end
      else
        case GlobalCustomHelp.DisplayLocation of
          dloWelcomePage: WelcomePageNavigate(u);
          dloDefaultBrowser: ShellExecute(Application.Handle, 'open',
              PChar(u), '', '', SW_SHOWNORMAL);
          dloMSDocumentExplorer: if GetHelpSystem(hs) then
              hs.ShowTopicHelp(u, '');
        end;
    end
    else
    begin
      //oops das haben wir jetzt nicht verstanden....
      //das heißt wir sind die einzige Instanz, die Hilfe angemeldet hat.
      //Wir erzwingen einen Selektor!

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
    hm           := FHelpManager;
    FHelpManager := nil;
    hm.Release(FViewerID);
  end;
end;

class procedure TCustomHelp.TrimNamespace(var s: string;
  ATrimOption: TNamespaceTrimOption);
var
  idx: Integer;
begin
  case ATrimOption of
    nstoNoTrim: idx    := 0;
    nstoTrimFirst: idx := PosStr('.', s, 1, MaxInt);
    nstoTrimAll: idx   := PosStr('.', s, MaxInt, 1);
    else
      Assert(False, 'ATrimOption #' + IntToStr(Ord(ATrimOption)));
      idx := -1;
  end;

  if idx > 0 then
    Delete(s, 1, idx);
end;

function TCustomHelp.SearchInHxSession(hxSession: IHxSession;
  const HelpString: string; AResult: TStringList; hxIndex: IHxIndex): Boolean;
var
  Topics: IHxTopicList;
  idx:    Integer;
  slot:   Integer;
  s:      string;
  g:      string;
begin
  Result := False;
  FSessionLock.Acquire;
  try
    slot := hxIndex.GetSlotFromString(HelpString);
    if AnsiContainsText(hxIndex.GetStringFromSlot(slot), HelpString) then
    begin
      Topics := hxIndex.GetTopicsFromSlot(slot);
      g      := GetNamespaceTitle(hxSession);
      for idx := 1 to Topics.Count do
      begin
        with Topics.Item(idx) do
          s := TCustomHelp.EncodeURL(Title[HxTopicGetRLTitle, 0], Location,
            URL, g, nstoNoTrim, True);
        AResult.Add(s);
        Result := True;
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
  idx:    Integer;
  s:      string;
  g:      string;
begin
  Result := False;
  FSessionLock.Acquire;
  try
    Topics := hxSession.Query(HelpString, '!DefaultFullTextSearch', HxQuery_No_Option, '');

    g := GetNamespaceTitle(hxSession);
    for idx := 1 to Topics.Count do
    begin
      with Topics.Item(idx) do
        s := TCustomHelp.EncodeURL(Title[HxTopicGetRLTitle, 0], Location,
          URL, g, nstoNoTrim, True);
      AResult.Add(s);
      Result := True;
    end;
  finally
    FSessionLock.Release;
  end;
end;

procedure TCustomHelpViewer.SoftShutDown;
var
  hs: IHelpSystem;
begin
  if GetHelpSystem(hs) then
  begin
    hs.AssignHelpSelector(nil);
  end;
end;

procedure TCustomHelpViewer.EnsureValidHelpString(var HelpString: String);
var
  Editor            : IOTAEditor;
  SourceEditor      : IOTASourceEditor70;
  EditView          : IOTAEditView;
  LookedUpWord      : String;
begin
  if AnsiContainsText(HelpString, KIBITZ_IGNORED_HELPSTRING) then
  begin
    Editor:=(BorlandIDEServices as IOTAModuleServices).CurrentModule.CurrentEditor;

    if Supports(Editor, IOTASourceEditor70, SourceEditor) then
    begin
      EditView:=SourceEditor.EditViews[0];
      LookedUpWord:=EditView.Position.RipText(['_','.'],rfBackward or rfIncludeAlphaChars or rfIncludeNumericChars)+
              EditView.Position.RipText(['_','.'],rfIncludeAlphaChars or rfIncludeNumericChars);

      HelpString:=ReplaceText(HelpString, KIBITZ_IGNORED_HELPSTRING, LookedUpWord);
    end;
  end;
end;

function TCustomHelpViewer.UnderstandsKeyword(const HelpString: string): Integer;
var
  doCheck:      Boolean;
  doAddDefault: Boolean;
  AHelpString:  string;
begin
  Result      := 0;
  AHelpString := HelpString;

  if not Enabled then
    Exit;

  EnsureValidHelpString(AHelpString);

  if AnsiContainsText(AHelpString, KIBITZ_IGNORED_HELPSTRING) then
    Exit;

  doCheck := CtrlDown;

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
  doAddDefault := True;

  if doCheck then
    doAddDefault := CheckUnderstandsKeyword(AHelpString);

  // add default entries, if we are in a second call to the help system
  // with the same keyword and the first call did not yield any result
  // (otherwise Keywords.Count is empty)
  with CustomHelpKeywordRecorderIntf, GetKeywordList do
    if (not doAddDefault) and (Count > 0) and (Strings[0] = AHelpString) then
    begin
      doAddDefault := True;
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

  with InternalGetHelpStrings(AHelpString, doAddDefault, False) do
  begin
    Result := Count;
    Free;
  end;

  if Result = 0 then
    with InternalGetHelpStrings(AHelpString, False, doAddDefault) do
    begin
      Result := Count;
      Free;
    end;

  GlobalCustomHelp.InitHelpSelector(AHelpString);
end;

{ TCustomHelp }

class function TCustomHelp.CheckIndexInHxSession(hxSession: IHxSession;
  var hxIndex: IHxIndex): Boolean;
begin
  Result := Supports(hxSession.GetNavigationObject('!DefaultKeywordIndex', ''),
    IID_IHxIndex, hxIndex);
end;

procedure TCustomHelp.ConnectToIDE;
var
  HelpMenu: TMenuItem;
begin
  HelpMenu := GetHelpMenu;

  if Assigned(HelpMenu) then
  begin
    FMenuItem         := TMenuItem.Create(HelpMenu);
    FMenuItem.Caption := 'Configure Custom Help ...';
    FMenuItem.OnClick := OnMenuItemClick;
    HelpMenu.Insert(1, FMenuItem);
  end;

end;

constructor TCustomHelp.Create;
begin
  FEnabledhxSessions := TInterfaceList.Create;
  FSessionLock       := TCriticalSection.Create;
  FProvider          := TStringList.Create;
  FRSSProvider       := TStringList.Create;

  F3rdPartyViewers        := TStringList.Create;
  F3rdPartyViewers.Sorted := True;

  FResultOrder        := TStringList.Create;
  FResultOrder.Sorted := True;

  HelpViewer := TCustomHelpViewer.Create;
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
      'http://ref.dp200x.de/dp_reference.php?query=' +
      EnvVarToken(ENVVAR_NAME_KEYWORD),
      nstoTrimFirst,
      ptStandard,
      True);

    WriteProviderToRegistry('2',
      'Koders.com',
      'Search at koders.com',
      'http://www.koders.com/default.aspx?submit=Search&la=Delphi&li=*&s='
      +
      EnvVarToken(ENVVAR_NAME_KEYWORD),
      nstoTrimFirst,
      ptStandard,
      True);

    WriteProviderToRegistry('3',
      'Google Codesearch',
      'Search using Google Codesearch',
      'http://www.google.com/codesearch?btnG=Code+suchen&hl=de&as_lang=pascal&as_license_restrict=i&as_license=&as_package=&as_filename=&as_case=&as_q=' + EnvVarToken(ENVVAR_NAME_KEYWORD),
      nstoTrimFirst,
      ptStandard,
      True);

    WriteProviderToRegistry('4',
      'MSDN Online',
      'Search using MSDN Online',
      'http://search.msdn.microsoft.com/Default.aspx?locale=en-US&Query='
      +
      EnvVarToken(ENVVAR_NAME_KEYWORD),
      nstoTrimFirst,
      ptStandard,
      True);

    LoadProviderFromRegistry;
  end;


  LoadRSSProviderFromRegistry;
end;

class function TCustomHelp.GetTopicFromURL(hxHierarchy: IHxHierarchy;
  const URL: string): IHxTopic;
var
  si:    PSafeArray;
  ari:   TIntegerDynArray;
  hNode: Integer;
begin
  try
    si     := hxHierarchy.GetSyncInfo(URL);
    ari    := SafeArrayToIntArray(si);
    hNode  := ari[Length(ari) - 1];
    Result := hxHierarchy.GetTopic(hNode);
  except
    try
      hNode  := hxHierarchy.GetNextFromNode(hxHierarchy.GetPrevFromUrl(URL));
      Result := hxHierarchy.GetTopic(hNode);
    except
      try
        hNode  := hxHierarchy.GetPrevFromNode(hxHierarchy.GetNextFromUrl(URL));
        Result := hxHierarchy.GetTopic(hNode);
      except
        Result := nil;
      end;
    end;
  end;
end;

class function TCustomHelp.GetSessionFromURL(const URL: string;
  var Group: string): IHxSession;
var
  idx:    Integer;
  nsName: string;
begin
  Result := nil;
  if not StrUtils.AnsiStartsText(PROTPREFIX_MSHELP, URL) then
    Exit;
  idx := PosEx('/', URL, Length(PROTPREFIX_MSHELP) + 1) - 1;
  if idx < 0 then
    nsName := URL
  else
    nsName := Copy(URL, 1, idx);

  Result := CoHxSession.Create;
  Result.Initialize(nsName, 0);
  Group  := GlobalCustomHelp.GetNamespaceTitle(Result);
end;

class function TCustomHelp.GetTopicInfo(const URL: string;
  out Caption, Description, Link, Group: string;
  out TrimOption: TNamespaceTrimOption): Boolean;
var
  hxSession:   IHxSession;
  hxHierarchy: IHxHierarchy;
  tp:          IHxTopic;
begin
  tp        := nil;
  hxSession := GetSessionFromURL(URL, Group);
  if hxSession <> nil then
    try
      if Supports(hxSession.GetNavigationObject('!DefaultToc', ''),
        IHxHierarchy, hxHierarchy) then
        tp := GetTopicFromURL(hxHierarchy, URL);
    except
    end;
  Result := tp <> nil;
  if Result then
  begin
    Caption     := tp.Title[HxTopicGetHTMTitle, 0];
    Description := tp.Location;
    Link        := URL;
    TrimOption  := nstoNoTrim;
  end
  else
  begin
    Caption     := URL;
    Description := '';
    Link        := URL;
    TrimOption  := nstoNoTrim;
  end;
end;

function TCustomHelp.GetViewerID: Integer;
begin
  Result := -1;
  if Assigned(HelpViewerIntf) then
    Result := HelpViewer.FViewerID;
end;

class function TCustomHelp.DecodeURL(const URL: string;
  out Caption, Description, Link: string; out Group: string;
  out TrimOption: TNamespaceTrimOption; out Enabled: Boolean): Boolean;
var
  sl: TStringList;
begin
  Result := False;
  if StrUtils.AnsiStartsText(PROTPREFIX_CUSTOMHELP, URL) then
  begin
    sl := TStringList.Create;
    try
      sl.QuoteChar := #0;
      sl.Delimiter := URL_SEPERATOR;
      sl.StrictDelimiter := True;
      sl.DelimitedText := Copy(URL, Length(PROTPREFIX_CUSTOMHELP) + 1, Length(URL));
      Caption     := Sl[0];
      Description := sl[1];
      Link        := sl[2];
      Group       := sl[3];
      TrimOption  := TNamespaceTrimOption(StrToIntDef(sl[4], 0));
      Enabled     := StrToIntDef(sl[5], 1) = 1;
    finally
      sl.Free;
    end;
    Result := True;
  end
  else if StrUtils.AnsiStartsText(PROTPREFIX_MSHELP, URL) then
  begin
    Result := GetTopicInfo(URL, Caption, Description, Link, Group, TrimOption);
    if not Result then
    begin
      TrimOption := nstoNoTrim;
      sl         := TStringList.Create;
      try
        sl.QuoteChar := #0;
        sl.Delimiter       := '/';
        sl.StrictDelimiter := True;
        sl.DelimitedText   := Copy(URL, Length(PROTPREFIX_MSHELP) + 1, Length(URL));
        if sl.Count >= 2 then
        begin
          Description := sl[0];
          Caption     := sl[sl.Count - 1];
          Link        := URL;
          Group       := GROUP_LABEL_STANDARD;
          Result      := True;
        end
        else
        begin
          Description := '';
          Caption     := URL;
          Link        := URL;
          Group       := GROUP_LABEL_STANDARD;
          Result      := True;
        end;
      finally
        sl.Free;
      end;
    end;
  end;
end;

class function TCustomHelp.DecodeURL(const URL: string; out Link: string): Boolean;
var
  Caption, Description, Group: string;
  TrimOption:  TNamespaceTrimOption;
  ProvEnabled: Boolean;
begin
  Result := False;
  if StrUtils.AnsiStartsText(PROTPREFIX_CUSTOMHELP, URL) then
  begin
    Result := DecodeURL(URL, Caption, Description, Link, Group, TrimOption, ProvEnabled);
  end
  else if StrUtils.AnsiStartsText(PROTPREFIX_MSHELP, URL) then
  begin
    Link   := URL;
    Result := True;
  end;
end;

destructor TCustomHelp.Destroy;
begin
  DisconnectFromIDE;

  FEnabledhxSessions.Free;
  F3rdPartyViewers.Free;
  FProvider.Free;
  FRSSProvider.Free;
  FSessionLock.Free;

  HelpViewerIntf := nil;
  // This will automatically clear HelpViewer if object is destroyed.
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

class function TCustomHelp.EncodeURL(Caption, Description, Link, Group: string;
  TrimOption: TNamespaceTrimOption; AEnabled: Boolean): string;
begin
  Result := PROTPREFIX_CUSTOMHELP + Caption + URL_SEPERATOR + Description +
    URL_SEPERATOR + Link + URL_SEPERATOR + Group + URL_SEPERATOR + IntToStr(
    Integer(TrimOption)) + URL_SEPERATOR + IfThen(AEnabled, '1', '0');
end;

function TCustomHelp.GetColor(AProvider: string): TColor;
var
  Reg: TRegistry;
begin
  Result := 0;

  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;

    if Reg.OpenKey(REG_ROOT_KEY + COLORS_SUB_KEY, True) then
    begin
      Result := StrToIntDef(Reg.ReadString(AProvider), clActiveCaption);

      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

function TCustomHelp.GetEnabledhxSession(Index: Integer): IHxSession;
begin
  if not Supports(EnabledhxSessions[Index], IHxSession, Result) then
    Result := nil;
end;

function TCustomHelp.GetHelpMenu: TMenuItem;
var
  NTA: INTAServices;
  idx: Integer;
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
  Result := Session.Collection.GetProperty(HxCollectionProp_NamespaceName);
end;

class function TCustomHelp.GetNamespaces: IHxRegNamespaceList;
begin
  Result := CoHxRegistryWalker.Create.RegisteredNamespaceList[''];
end;

procedure TCustomHelp.LoadProviderFromRegistry;
begin
  LoadResultOrderFromRegistry;
  LoadSettingsFromRegistry;
  LoadEnabledNamespacesFromRegistry;

  InternalLoadProviderFromRegistry(FProvider, PROVIDER_SUB_KEY);
end;

procedure TCustomHelp.LoadEnabledNamespacesFromRegistry;
var
  sl:        TStringList;
  idx:       Integer;
  hxSession: IHxSession;
  hxIndex:   IHxIndex;
  errmsg:    string;
  nsList:    IHxRegNamespaceList;
begin
  FEnabledhxSessions.Clear;
  sl     := TStringList.Create;
  errmsg := '';
  try
    ReadEnabledNamespacesFromRegistry(sl);

    nsList := Namespaces;
    for idx := 1 to nsList.Count do
    begin
      if sl.IndexOf(nsList.Item(idx).Name) >= 0 then
      begin
        try
          hxSession := CoHxSession.Create;
          hxSession.Initialize(PROTPREFIX_MSHELP + nsList.Item(idx).Name, 0);

          CheckIndexInHxSession(hxSession, hxIndex);
        except
          on e: Exception do
          begin
            errmsg := errmsg + nsList.Item(idx).Name + ': ' + e.Message + #13#10;
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
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    ReadSettingsFromRegistry(sl);
    FFullTextSearch  := sl.Values[SETTINGS_FULLTEXTSEARCH] = '1';
    F3rdPartyViewers.CommaText := sl.Values[SETTINGS_3RD_PARTY_VIEWERS];

    FTrimNamespaces  := TNamespaceTrimOption(
      StrToIntDef(sl.Values[SETTINGS_TRIMNAMESPACES], 0));
    FDisplayLocation := TDisplayLocationOption(
      StrToIntDef(sl.Values[SETTINGS_DISPLAY_LOCATION], 0));
  finally
    sl.Free;
  end;
end;

procedure TCustomHelp.LoadResultOrderFromRegistry;
var
  Reg: TRegistry;
  idx: Integer;
begin
  Reg := TRegistry.Create;
  FResultOrder.BeginUpdate;
  FResultOrder.Sorted := False;
  try
    FResultOrder.Clear;

    Reg.RootKey := HKEY_CURRENT_USER;

    if Reg.OpenKey(REG_ROOT_KEY + RESULT_ORDER_SUB_KEY, True) then
    begin
      Reg.GetValueNames(FResultOrder);

      for idx := 0 to FResultOrder.Count - 1 do
        FResultOrder[idx] := Format('%.4d', [Reg.ReadInteger(FResultOrder[idx])]) +
          '=' + FResultOrder[idx];

      Reg.CloseKey;
    end;
  finally
    FResultOrder.EndUpdate;
    FResultOrder.Sorted := True;
    Reg.Free;
  end;
end;

procedure TCustomHelp.InternalLoadProviderFromRegistry(ATarget: TStrings;
  ARootKey: string);
var
  Reg:        TRegistry;
  sl:         TStringList;
  s:          string;
  GroupLabel: string;
begin
  ATarget.Clear;

  sl := TStringList.Create;

  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;

    if Reg.OpenKey(REG_ROOT_KEY + ARootKey, True) then
    begin
      Reg.GetKeyNames(sl);
      Reg.CloseKey;
    end;

    for s in sl do
    begin
      if Reg.OpenKey(REG_ROOT_KEY + ARootKey + '\' + s, False) then
      begin
        if trim(reg.ReadString(VALUE_NAME)) <> EmptyStr then
        begin
          if AnsiContainsStr(Reg.ReadString(VALUE_URL), '://') then
            GroupLabel := GROUP_LABEL_WEB_BASED
          else
            GroupLabel := GROUP_LABEL_FILE_BASED;

          ATarget.Add(TCustomHelp.EncodeURL(
            Reg.ReadString(VALUE_NAME),
            Reg.ReadString(VALUE_DESCR),
            Reg.ReadString(VALUE_URL),
            GroupLabel,
            TNamespaceTrimOption(
            StrToIntDef(Reg.ReadString(VALUE_TRIMNAMESPACE), 0)),
            Reg.ReadString(VALUE_ENABLED) <> '0'));
        end;

        Reg.CloseKey;
      end;
    end;

  finally
    Reg.Free;
    sl.Free;
  end;
end;

procedure TCustomHelp.LoadRSSProviderFromRegistry;
begin
  InternalLoadProviderFromRegistry(FRSSProvider, RSS_PROVIDER_SUB_KEY);
end;

procedure TCustomHelp.OnMenuItemClick(Sender: TObject);
begin
  if Tform_Config.Execute then
  begin
    LoadProviderFromRegistry;
    LoadRSSProviderFromRegistry;
  end;
end;

function TCustomHelp.PerformInHxSession(HelpString: string;
  SessionIndex: Integer; const AResult: TStringList): Boolean;
var
  hxSession: IHxSession;
  hxIndex:   IHxIndex;
begin
  Result    := False;
  hxSession := EnabledhxSession[SessionIndex];
  if hxSession <> nil then
  begin
    //Soll nach dem kompletten Text gesucht werden?
    if PerformFullTextSearch then
    begin
      Result := QueryInHxSession(hxSession, HelpString, AResult);
    end //oder nur im Index der Hilfe
    else if CheckIndexInHxSession(hxSession, hxIndex) then
    begin
      Result := SearchInHxSession(hxSession, HelpString, AResult, hxIndex);
    end;
  end;
end;

class procedure TCustomHelp.ReadEnabledNamespacesFromRegistry(
  const ANamesList: TStrings);
var
  Reg: TRegistry;
  idx: Integer;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;

    if Reg.OpenKey(REG_ROOT_KEY + NAMESPACES_SUB_KEY, True) then
    begin
      Reg.GetValueNames(ANamesList);

      for idx := ANamesList.Count - 1 downto 0 do
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
  Reg: TRegistry;
  idx: Integer;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;

    if Reg.OpenKey(REG_ROOT_KEY + SETTINGS_SUB_KEY, True) then
    begin
      Reg.GetValueNames(ANameValueList);

      for idx := 0 to ANameValueList.Count - 1 do
        ANameValueList[idx] := ANameValueList[idx] + '=' + Reg.ReadString(ANameValueList[idx]);

      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

procedure TCustomHelp.SetCheckWinHelpGid(const Value: Boolean);
begin
  FCheckWinHelpGid := Value;
  TCustomHelp.WriteSettingToRegistry(SETTING_WINHELPGIDCHECK, IntToStr(Byte(Value)));
end;

procedure TCustomHelp.SetColor(AProvider: string; const Value: TColor);
begin
  TCustomHelp.WriteSettingToRegistry(COLORS_SUB_KEY, AProvider, IntToStr(Value));
end;

procedure TCustomHelp.SetDisplayLocation(const Value: TDisplayLocationOption);
begin
  FDisplayLocation := Value;
  TCustomHelp.WriteSettingToRegistry(SETTINGS_DISPLAY_LOCATION, IntToStr(Byte(Value)));
end;

procedure TCustomHelp.SetFullTextSearch(const Value: Boolean);
begin
  FFullTextSearch := Value;
  TCustomHelp.WriteSettingToRegistry(SETTINGS_FULLTEXTSEARCH, IntToStr(Byte(Value)));
end;

procedure TCustomHelp.SetTrimNamespaces(const Value: TNamespaceTrimOption);
begin
  FTrimNamespaces := Value;
  TCustomHelp.WriteSettingToRegistry(SETTINGS_TRIMNAMESPACES, IntToStr(Byte(Value)));
end;

procedure TCustomHelp.ShowHelp(const HelpString: string);
begin
  if Assigned(HelpViewerIntf) then
    HelpViewer.ShowHelp(HelpString);
end;

class function TCustomHelp.CheckGidFile(AWinHelpFile: string;
  const ARaiseError: Boolean): Boolean;
const
  msg = 'No gid file%1:s available. No help query possible.%0:s' +
    'Please open the help file using winhlp32.exe and generate index manually.';
var
  GIDFile: string;
begin
  GIDFile := ChangeFileExt(AWinHelpFile, '.gid');

  Result := FileExists(GIDFile);
  if not Result then
  begin
    if ARaiseError then
      raise EHelpSystemException.CreateFmt(msg, [' ', '']);
    ShowMessageFmt(msg, [sLineBreak, 'for "' + AWinHelpFile + '"']);
  end;
end;

class procedure TCustomHelp.WriteSettingToRegistry(AName, AValue: string);
begin
  WriteSettingToRegistry(SETTINGS_SUB_KEY, AName, AValue);
end;

class procedure TCustomHelp.WriteSettingToRegistry(ASubKey, AName, AValue: string);
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;

    if Reg.OpenKey(REG_ROOT_KEY + ASubKey, True) then
    begin
      Reg.WriteString(AName, AValue);

      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

class procedure TCustomHelp.WriteNamespacesToRegistry(ANamespace: string;
  AEnabled: Boolean);
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;

    if Reg.OpenKey(REG_ROOT_KEY + NAMESPACES_SUB_KEY, True) then
    begin
      Reg.WriteBool(ANamespace, AEnabled);

      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

class procedure TCustomHelp.WriteProviderToRegistry(AKeyName, AName,
  ADesc, AURL: string;
  ATrimNamespaces: TNamespaceTrimOption; AType: TProviderType; AEnabled: Boolean);
var
  Reg:    TRegistry;
  SubKey: string;
begin
  case AType of
    ptStandard: SubKey := PROVIDER_SUB_KEY;
    ptRSS: SubKey      := RSS_PROVIDER_SUB_KEY;
  end;

  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;

    if Reg.OpenKey(REG_ROOT_KEY + SubKey + '\' + AKeyName, True) then
    begin
      Reg.WriteString(VALUE_NAME, AName);
      Reg.WriteString(VALUE_DESCR, ADesc);
      Reg.WriteString(VALUE_URL, AURL);
      Reg.WriteString(VALUE_TRIMNAMESPACE, IntToStr(Integer(ATrimNamespaces)));
      Reg.WriteString(VALUE_ENABLED, IfThen(AEnabled, '1', '0'));

      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

class procedure TCustomHelp.WriteResultOrderToRegistry(AOrder: TStrings);
var
  Reg: TRegistry;
  idx: Integer;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;

    Reg.DeleteKey(REG_ROOT_KEY + RESULT_ORDER_SUB_KEY);
    if Reg.OpenKey(REG_ROOT_KEY + RESULT_ORDER_SUB_KEY, True) then
    begin
      for idx := 0 to AOrder.Count - 1 do
        Reg.WriteInteger(AOrder[idx], idx);
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
  if Assigned(GlobalCustomHelp) then
    GlobalCustomHelp.LoadResultOrderFromRegistry;
end;

{$R Images.res}

var
  bmp: TBitmap;

procedure AddSplashBitmap;
begin
  bmp := TBitmap.Create;
  bmp.LoadFromResourceName(hinstance, 'Splash');
  SplashScreenServices.AddPluginBitmap(
    'Custom Help',
    bmp.Handle,
    False,
    'Free and OpenSource',
    'by TheUnknownOnes');

end;

var
  F_REG_ROOT_KEY: string;

function REG_ROOT_KEY: string;
begin
  Result := F_REG_ROOT_KEY;
end;

procedure InitializeRegRootKey;
const
  {$ifDef VER170}
     OLD_REG_ROOT_KEY = '\Software'+REG_ROOT_BASE+'\VER170' + REG_ROOT_PROJECT;
  {$Endif}
  {$ifDef VER180}
    {$ifDef VER185}
      OLD_REG_ROOT_KEY = '\Software'+REG_ROOT_BASE+'\VER185' + REG_ROOT_PROJECT;
    {$Else}
      OLD_REG_ROOT_KEY = '\Software'+REG_ROOT_BASE+'\VER180' + REG_ROOT_PROJECT;
    {$Endif}
  {$Endif}
  {$ifDef VER200}
    OLD_REG_ROOT_KEY = '\Software'+REG_ROOT_BASE+'\VER200' + REG_ROOT_PROJECT;
  {$Endif}
  {$ifDef VER210}
    OLD_REG_ROOT_KEY = '\Software'+REG_ROOT_BASE+'\VER210' + REG_ROOT_PROJECT;
  {$Endif}
var
  reg: TRegistry;
begin
  F_REG_ROOT_KEY := GetIdeBaseRegistryKey + REG_ROOT_BASE + REG_ROOT_PROJECT;

  if OLD_REG_ROOT_KEY = F_REG_ROOT_KEY then
    Exit;

  reg := TRegistry.Create(KEY_READ);

  try
    reg.RootKey := HKEY_CURRENT_USER;

    if reg.KeyExists(F_REG_ROOT_KEY) then
      Exit;

    if not reg.KeyExists(OLD_REG_ROOT_KEY) then
      Exit;

    // migrate old settings ...
    reg.Access := KEY_READ or KEY_WRITE;
    reg.MoveKey(OLD_REG_ROOT_KEY, F_REG_ROOT_KEY, True);

  finally
    reg.Free;
  end;
end;

initialization
  InitializeRegRootKey;

  GlobalCustomHelp := TCustomHelp.Create;
  AddSplashBitmap;

finalization
  if Assigned(GlobalCustomHelp) then
    FreeAndNil(GlobalCustomHelp);
  if Assigned(bmp) then
    bmp.Free;

end.
