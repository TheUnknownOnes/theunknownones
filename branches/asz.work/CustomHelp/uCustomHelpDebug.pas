unit uCustomHelpDebug;

{(*}
// Remove the . to enable debugging ...
{.$DEFINE DEBUG_CUSTOMHELP}
{*)}

interface

uses
  Classes,
  HelpIntfs;

const
  DEBUG_CUSTOMHELP: Boolean = {$IFDEF DEBUG_CUSTOMHELP}True{$ELSE}False{$ENDIF};

var
  //We keep these global ... as it is done in WinHelpViewer.pas
  //and keep the reference to the object in the implementation section.
  CustomHelpDebugViewerIntf: ICustomHelpViewer;

implementation

uses
  SysUtils,
  ToolsApi,
  Windows;

type
  {$TYPEINFO ON}
  TCustomHelpDebugViewer = class(TInterfacedObject,
    IExtendedHelpViewer,
    IHelpSystemFlags,
    ICustomHelpViewer)
  public
    {$REGION 'ICustomHelpViewer'}
    function GetViewerName: string;
    function UnderstandsKeyword(const HelpString: string): Integer;
    function GetHelpStrings(const HelpString: string): TStringList;
    function CanShowTableOfContents: Boolean;
    procedure ShowTableOfContents;
    procedure ShowHelp(const HelpString: string);
    procedure NotifyID(const ViewerID: Integer);
    procedure SoftShutDown;
    procedure ShutDown;
    {$ENDREGION}

    {$region 'IExtendedHelpViewer'}
    function UnderstandsTopic(const Topic: string): Boolean;
    procedure DisplayTopic(const Topic: string);
    function UnderstandsContext(const ContextID: Integer;
      const HelpFileName: string): Boolean;
    procedure DisplayHelpByContext(const ContextID: Integer;
      const HelpFileName: string);
    {$endregion}

    {$region 'IHelpSystemFlags'}
    function GetUseDefaultTopic: Boolean;
    procedure SetUseDefaultTopic(AValue: Boolean);
    {$endregion}

    destructor Destroy; override;
  private
    FUseDefaultTopic: Boolean;
    FViewerID:        Integer;
    FHelpManager:     IHelpManager;
    FKeywords:        TStringList;
    FContents:        TStringList;
    FViewers:         TStringList;
    FHelpStrings:     TStringList;
    FUnderstandsKeywords: TStringList;
    FShowHelpStrings: TStringList;
    constructor Create;
    procedure AssignHelpSelector(AClear: Boolean);
    procedure DoUnregister;
    procedure DoRegister;
    procedure SetContents(const Value: TStringList);
    procedure SetHelpStrings(const Value: TStringList);
    procedure SetKeywords(const Value: TStringList);
    procedure SetShowHelpStrings(const Value: TStringList);
    procedure SetUnderstandsKeywords(const Value: TStringList);
    procedure SetViewers(const Value: TStringList);
  protected
    procedure DebugLog(method, msg: string; LogDefaultTopic: Boolean); overload;
    procedure DebugLog(method, msg: string); overload;
  published
    property Keywords: TStringList read FKeywords write SetKeywords;
    property Contents: TStringList read FContents write SetContents;
    property Viewers: TStringList read FViewers write SetViewers;
    property HelpStrings: TStringList read FHelpStrings write SetHelpStrings;
    property ShowHelpStrings: TStringList read FShowHelpStrings write SetShowHelpStrings;
    property UnderstandsKeywords: TStringList
      read FUnderstandsKeywords write SetUnderstandsKeywords;
  end;

  //This class must not contain any local fields
  //applies only to BDS 2006
  {$TYPEINFO ON}
  TCustomHelpDebugSelector = class(TInterfacedObject,
    IHelpSelector, IHelpSelector2)
  protected
    function SelectKeyword(Keywords: TStrings): Integer;
    function TableOfContents(Contents: TStrings): Integer;
    function SelectContext(Viewers: TStrings): Integer;
  end;

var
  CustomHelpDebugViewer: TCustomHelpDebugViewer;

{ TKeywordsHelpSelector }

function TCustomHelpDebugSelector.SelectContext(Viewers: TStrings): Integer;
begin
  CustomHelpDebugViewer.Viewers.Text := Viewers.Text;
  Result := 0;
  CustomHelpDebugViewer.DebugLog('Selector.SelectContext',
    '(' + Viewers.CommaText + ')=' + IntToStr(Result), False);
end;

function TCustomHelpDebugSelector.SelectKeyword(Keywords: TStrings): Integer;
begin
  CustomHelpDebugViewer.Keywords.Text := Keywords.Text;
  // Abort help call ...
  Result := -1;
  CustomHelpDebugViewer.DebugLog('Selector.SelectKeyword',
    '(' + Keywords.CommaText + ')=' + IntToStr(Result), False);
end;

function TCustomHelpDebugSelector.TableOfContents(Contents: TStrings): Integer;
begin
  CustomHelpDebugViewer.Contents.Text := Contents.Text;
  Result := -1;
  CustomHelpDebugViewer.DebugLog('Selector.TableOfContents',
    '(' + Contents.CommaText + ')=' + IntToStr(Result), False);
end;

{ TKeywordsHelpViewer }

function TCustomHelpDebugViewer.CanShowTableOfContents: Boolean;
begin
  Result := False;
  DebugLog('ShowTableOfContents', '=' + BoolToStr(Result, True));
end;

constructor TCustomHelpDebugViewer.Create;

  procedure CreateStringList(var sl: TStringList);
  begin
    sl := TStringList.Create;
    sl.Duplicates := dupAccept;
  end;

begin
  inherited;
  DebugLog('Create', '', False);
  FHelpManager     := NIL;
  FUseDefaultTopic := False;
  CreateStringList(self.FKeywords);
  CreateStringList(self.FContents);
  CreateStringList(self.FViewers);
  CreateStringList(self.FHelpStrings);
  CreateStringList(self.FUnderstandsKeywords);
  CreateStringList(self.FShowHelpStrings);
  DoRegister;
  CustomHelpDebugViewer := self;
end;

procedure TCustomHelpDebugViewer.DebugLog(method, msg: string);
begin
  DebugLog(method, msg, True);
end;

procedure TCustomHelpDebugViewer.DebugLog(method, msg: string;
  LogDefaultTopic: Boolean);
var
  s: string;
begin
  if LogDefaultTopic then
    method := method + '{' + BoolToStr(FUseDefaultTopic, True) + '}';
  s := FormatDateTime('', Now) + ': ' + ClassName + '[' + method + ']: ' + msg;
  OutputDebugString(PChar(s));
end;

destructor TCustomHelpDebugViewer.Destroy;
begin
  DebugLog('Destroy', '', False);

  FreeAndNil(self.FKeywords);
  FreeAndNil(self.FContents);
  FreeAndNil(self.FViewers);
  FreeAndNil(self.FHelpStrings);
  FreeAndNil(self.FUnderstandsKeywords);
  FreeAndNil(self.FShowHelpStrings);

  if CustomHelpDebugViewer = Self then
    CustomHelpDebugViewer := NIL;

  inherited;
end;

function TCustomHelpDebugViewer.GetHelpStrings(const HelpString: string): TStringList;
begin
  Self.HelpStrings.Add(HelpString);
  Result := TStringList.Create;
  Result.Values[GetViewerName] := HelpString;
  DebugLog('GetHelpStrings', '(' + HelpString + ')=' + Result.CommaText);
end;

function TCustomHelpDebugViewer.GetViewerName: string;
begin
  Result := ClassName;
  DebugLog('GetViewerName', '=' + Result);
end;

procedure TCustomHelpDebugViewer.AssignHelpSelector(AClear: Boolean);
var
  hs: IHelpSystem;
begin
  if GetHelpSystem(hs) then
    if AClear then
      hs.AssignHelpSelector(NIL)
    else
      hs.AssignHelpSelector(TCustomHelpDebugSelector.Create);
end;

procedure TCustomHelpDebugViewer.DoRegister;
begin
  if not Assigned(FHelpManager) then
  begin

    RegisterViewer(Self, FHelpManager);
  end;
end;

procedure TCustomHelpDebugViewer.DoUnregister;
var
  hm: IHelpManager;
begin
  if Assigned(FHelpManager) then
  begin
    hm           := FHelpManager;
    FHelpManager := NIL;
    hm.Release(FViewerID);
  end;
end;

procedure TCustomHelpDebugViewer.NotifyID(const ViewerID: Integer);
begin
  FViewerID := ViewerID;
end;

procedure TCustomHelpDebugViewer.ShowHelp(const HelpString: string);
begin
  DebugLog('ShowHelp', '(' + HelpString + ')');
  Self.ShowHelpStrings.Add(HelpString);
end;

procedure TCustomHelpDebugViewer.ShowTableOfContents;
begin
  DebugLog('ShowTableOfContents', '');
  // do nothing.
end;

procedure TCustomHelpDebugViewer.ShutDown;
begin
  DebugLog('ShutDown', '');
  SoftShutDown;
  DoUnregister;
end;

procedure TCustomHelpDebugViewer.SoftShutDown;
begin
  DebugLog('SoftShutDown', '');
  // do nothing
end;

function TCustomHelpDebugViewer.UnderstandsKeyword(const HelpString: string): Integer;
begin
  Self.UnderstandsKeywords.Add(HelpString);
  Result := 0;
  DebugLog('UnderstandsKeyword', '(' + HelpString + ')=' + IntToStr(Result));
end;

{$region 'IHelpSystemFlags'}

function TCustomHelpDebugViewer.GetUseDefaultTopic: Boolean;
begin
  Result := FUseDefaultTopic;
end;

function TCustomHelpDebugViewer.UnderstandsTopic(const Topic: string): Boolean;
begin
  Result := False;
  DebugLog('UnderstandsTopic', '(' + Topic + ')=' + BoolToStr(Result, True));
end;

procedure TCustomHelpDebugViewer.DisplayTopic(const Topic: string);
begin
  DebugLog('DisplayTopic', Topic);
end;

procedure TCustomHelpDebugViewer.SetContents(const Value: TStringList);
begin
  FContents.Assign(Value);
end;

procedure TCustomHelpDebugViewer.SetHelpStrings(const Value: TStringList);
begin
  FHelpStrings.Assign(Value);
end;

procedure TCustomHelpDebugViewer.SetKeywords(const Value: TStringList);
begin
  FKeywords.Assign(Value);
end;

procedure TCustomHelpDebugViewer.SetShowHelpStrings(const Value: TStringList);
begin
  FShowHelpStrings.Assign(Value);
end;

procedure TCustomHelpDebugViewer.SetUnderstandsKeywords(const Value: TStringList);
begin
  FUnderstandsKeywords.Assign(Value);
end;

procedure TCustomHelpDebugViewer.SetUseDefaultTopic(AValue: Boolean);
begin
  FUseDefaultTopic := AValue;
end;

procedure TCustomHelpDebugViewer.SetViewers(const Value: TStringList);
begin
  FViewers.Assign(Value);
end;

function TCustomHelpDebugViewer.UnderstandsContext(const ContextID: Integer;
  const HelpFileName: string): Boolean;
begin
  Result := False;
  DebugLog('UnderstandsContext', '(' + IntToStr(ContextID) + ', ' +
    HelpFileName + ')=' + BoolToStr(Result));
end;

procedure TCustomHelpDebugViewer.DisplayHelpByContext(const ContextID: Integer;
  const HelpFileName: string);
begin
  DebugLog('DisplayHelpByContext', '(' + IntToStr(ContextID) + ', ' + HelpFileName);
end;

{$endregion}

initialization
  if DEBUG_CUSTOMHELP then
    CustomHelpDebugViewerIntf := TCustomHelpDebugViewer.Create
  else
    CustomHelpDebugViewerIntf := NIL;

finalization
  CustomHelpDebugViewerIntf := NIL;
  // This will automatically clear HelpViewer if object is destroyed.
  if CustomHelpDebugViewer <> NIL then
    // This will unregister the viewer from the help system
    CustomHelpDebugViewer.ShutDown;

end.

