UNIT uCustomHelpKeywordRecorder;

interface

uses
  Classes,
  HelpIntfs,
  uCustomHelpIntfs;

const
  hcIDEStructureView   = $49E;
  hcIDEProjectExplorer = $550;
  hcIDEFormDesigner    = $4E2;
  hcIDEToolPalette     = $90E;
  hcIDECodeEditor      = $564;

var
  //We keep these global ... as it is done in WinHelpViewer.pas
  //and keep the reference to the object in the implementation section.
  CustomHelpKeywordRecorderIntf: ICustomHelpKeywordRecorder;

implementation

uses
  SysUtils,
  ToolsApi,
  uCustomHelpMain;

type
  {$TYPEINFO ON}
  TCustomHelpKeywordRecorder = class(TInterfacedObject,
    ICustomHelpKeywordRecorder,
    IExtendedHelpViewer,
    IHelpSystemFlags,
    ICustomHelpViewer)
  public
    {$REGION 'ICustomHelpViewer'}
    function GetViewerName: string;
    function UnderstandsKeyword(const HelpString: string): integer;
    function GetHelpStrings(const HelpString: string): TStringList;
    function CanShowTableOfContents: boolean;
    procedure ShowTableOfContents;
    procedure ShowHelp(const HelpString: string);
    procedure NotifyID(const ViewerID: integer);
    procedure SoftShutDown;
    procedure ShutDown;
    {$ENDREGION}

    {$region 'IExtendedHelpViewer'}
    function UnderstandsTopic(const Topic: string): boolean;
    procedure DisplayTopic(const Topic: string);
    function UnderstandsContext(const ContextID: integer;
      const HelpFileName: string): boolean;
    procedure DisplayHelpByContext(const ContextID: integer;
      const HelpFileName: string);
    {$endregion}

    {$region 'IHelpSystemFlags'}
    function GetUseDefaultTopic: boolean;
    procedure SetUseDefaultTopic(AValue: boolean);
    {$endregion}

    {$REGION 'ICustomHelpKeywordRecorder'}
    function GetKeywordList: TStringList;
    procedure SetKeywordList(const Value: TStringList);
    function GetHelpStringList: TStringList;
    procedure SetHelpStringList(const Value: TStringList);
    function GetShowHelpStringList: TStringList;
    procedure SetShowHelpStringList(const Value: TStringList);
    function GetEnabled: boolean;
    procedure SetEnabled(const Value: boolean);
    procedure Reset;
    procedure AddKeyword(HelpString: string; AIgnoreDuplicate: boolean = False);
    {$ENDREGION}

    constructor Create;
    destructor Destroy; override;
  private
    FViewerID: integer;
    FHelpManager: IHelpManager;
    FKeywords: TStringList;
    FHelpStrings: TStringList;
    FShowHelpStrings: TStringList;
    FEnabled:  boolean;
    FUseDefaultTopic: boolean;
    procedure DoUnregister;
    procedure DoRegister;
  published
    property Keywords: TStringList Read GetKeywordList Write SetKeywordList;
    property HelpStrings: TStringList Read GetHelpStringList Write SetHelpStringList;
    property ShowHelpStrings: TStringList Read GetShowHelpStringList
      Write SetShowHelpStringList;
    property Enabled: boolean Read GetEnabled Write SetEnabled;
  end;

var
  CustomHelpKeywordRecorder: TCustomHelpKeywordRecorder;

{ TKeywordsHelpViewer }

procedure TCustomHelpKeywordRecorder.AddKeyword(HelpString: string;
  AIgnoreDuplicate: boolean);
begin
  if AnsiSameText(HelpString, KIBITZ_IGNORED_HELPSTRING) then
    Exit;
  with FKeywords do
    if AIgnoreDuplicate and (IndexOf(HelpString) > -1) then
      Exit
    else if (Count = 0) or (Strings[Count - 1] <> HelpString) then
      Add(HelpString);
end;

function TCustomHelpKeywordRecorder.CanShowTableOfContents: boolean;
begin
  Result := False;
end;

constructor TCustomHelpKeywordRecorder.Create;

  procedure Action(var sl: TStringList);
  begin
    sl := TStringList.Create;
    sl.Duplicates := dupAccept;
  end;

begin
  inherited;
  FHelpManager := nil;
  FEnabled := False;
  FUseDefaultTopic := True;
  Action(self.FKeywords);
  Action(self.FHelpStrings);
  Action(self.FShowHelpStrings);
  DoRegister;
  CustomHelpKeywordRecorder := Self;
end;

destructor TCustomHelpKeywordRecorder.Destroy;

  procedure Action(var sl: TStringList);
  begin
    FreeAndNil(sl);
  end;

begin
  Action(self.FKeywords);
  Action(self.FHelpStrings);
  Action(self.FShowHelpStrings);

  if CustomHelpKeywordRecorder = Self then
    CustomHelpKeywordRecorder := nil;

  inherited;
end;

function TCustomHelpKeywordRecorder.GetEnabled: boolean;
begin
  Result := FEnabled;
end;

function TCustomHelpKeywordRecorder.GetHelpStringList: TStringList;
begin
  Result := FHelpStrings;
end;

function TCustomHelpKeywordRecorder.GetHelpStrings(
  const HelpString: string): TStringList;
begin
  Result := TStringList.Create;
  Result.Values[GetViewerName] := HelpString;
  if NOT Enabled then
    Exit;
  FHelpStrings.Add(HelpString);
end;

function TCustomHelpKeywordRecorder.GetKeywordList: TStringList;
begin
  Result := FKeywords;
end;

function TCustomHelpKeywordRecorder.GetShowHelpStringList: TStringList;
begin
  Result := FShowHelpStrings;
end;

function TCustomHelpKeywordRecorder.GetViewerName: string;
begin
  Result := ClassName;
end;

procedure TCustomHelpKeywordRecorder.DoRegister;
begin
  if NOT Assigned(FHelpManager) then
    RegisterViewer(Self, FHelpManager);
end;

procedure TCustomHelpKeywordRecorder.DoUnregister;
var
  hm: IHelpManager;
begin
  if Assigned(FHelpManager) then
  begin
    hm := FHelpManager;
    FHelpManager := nil;
    hm.Release(FViewerID);
  end;
end;

procedure TCustomHelpKeywordRecorder.NotifyID(const ViewerID: integer);
begin
  FViewerID := ViewerID;
end;

procedure TCustomHelpKeywordRecorder.Reset;

  procedure Action(var sl: TStringList);
  begin
    sl.Clear;
  end;

begin
  Action(self.FKeywords);
  Action(self.FHelpStrings);
  Action(self.FShowHelpStrings);
end;

procedure TCustomHelpKeywordRecorder.ShowHelp(const HelpString: string);
begin
  if NOT Enabled then
    Exit;

  // do nothing
  FShowHelpStrings.Add(HelpString);
end;

procedure TCustomHelpKeywordRecorder.ShowTableOfContents;
begin
  // do nothing.
end;

procedure TCustomHelpKeywordRecorder.ShutDown;
begin
  SoftShutDown;
  DoUnregister;
end;

procedure TCustomHelpKeywordRecorder.SoftShutDown;
begin
  // do nothing
end;

function TCustomHelpKeywordRecorder.UnderstandsKeyword(
  const HelpString: string): integer;
begin
  Result := 0;
  if NOT Enabled then
    Exit;
  AddKeyword(HelpString);
end;

procedure TCustomHelpKeywordRecorder.SetEnabled(const Value: boolean);
begin
  FEnabled := Value;
  if NOT FEnabled then
    Reset;
end;

procedure TCustomHelpKeywordRecorder.SetHelpStringList(const Value: TStringList);
begin
  FHelpStrings := Value;
end;

procedure TCustomHelpKeywordRecorder.SetKeywordList(const Value: TStringList);
begin
  FKeywords.Assign(Value);
end;

procedure TCustomHelpKeywordRecorder.SetShowHelpStringList(const Value: TStringList);
begin
  FShowHelpStrings := Value;
end;

{$region 'IExtendedHelpViewer'}

procedure TCustomHelpKeywordRecorder.DisplayHelpByContext(const ContextID: integer;
  const HelpFileName: string);
begin
  if Keywords.Count > 0 then
    GlobalCustomHelp.ShowHelp(Keywords[0], GlobalCustomHelp.LastHelpCallKeyword);
end;

procedure TCustomHelpKeywordRecorder.DisplayTopic(const Topic: string);
begin

end;

function TCustomHelpKeywordRecorder.UnderstandsTopic(const Topic: string): boolean;
begin
  Result := False;
end;

function TCustomHelpKeywordRecorder.UnderstandsContext(const ContextID: integer;
  const HelpFileName: string): boolean;
begin
  Result := False;
  if NOT Enabled then
    Exit;
  CASE ContextID OF
    hcIDEFormDesigner, hcIDECodeEditor:
      Result := Keywords.Count > 0;
  end;
end;

{$endregion}

{$region 'IHelpSystemFlags'}
function TCustomHelpKeywordRecorder.GetUseDefaultTopic: boolean;
begin
  Result := FUseDefaultTopic;
end;

procedure TCustomHelpKeywordRecorder.SetUseDefaultTopic(AValue: boolean);
begin
  FUseDefaultTopic := AValue;
end;

{$endregion}

initialization
  CustomHelpKeywordRecorderIntf := TCustomHelpKeywordRecorder.Create;

finalization
  CustomHelpKeywordRecorderIntf := nil;
  // This will automatically clear HelpViewer if object is destroyed.
  if CustomHelpKeywordRecorder <> nil then
    // This will unregister the viewer from the help system
    CustomHelpKeywordRecorder.ShutDown;

end.

