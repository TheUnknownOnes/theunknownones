unit uCustomHelpKeywordRecorder;

interface

uses
  Classes, HelpIntfs;

const
  hcIDEStructureView = $49E;
  hcIDEProjectExplorer = $550;
  hcIDEFormDesigner = $4E2;
  hcIDEToolPalette = $90E;
  hcIDECodeEditor = $564;

type
  {$TYPEINFO ON}
  TCustomHelpKeywordRecorder = class(TInterfacedObject,
    IExtendedHelpViewer,
    IHelpSystemFlags,
    ICustomHelpViewer)
  public
    {$REGION 'ICustomHelpViewer'}
    function  GetViewerName: string;
    function  UnderstandsKeyword(const HelpString: string): Integer;
    function  GetHelpStrings(const HelpString: string): TStringList;
    function  CanShowTableOfContents : Boolean;
    procedure ShowTableOfContents;
    procedure ShowHelp(const HelpString: string);
    procedure NotifyID(const ViewerID: Integer);
    procedure SoftShutDown;
    procedure ShutDown;
    {$ENDREGION}

    {$region 'IExtendedHelpViewer'}
    FUNCTION UnderstandsTopic(CONST Topic: string): boolean;
    PROCEDURE DisplayTopic(CONST Topic: string);
    FUNCTION UnderstandsContext(CONST ContextID: integer;
      CONST HelpFileName: string): boolean;
    PROCEDURE DisplayHelpByContext(CONST ContextID: integer;
      CONST HelpFileName: string);
    {$endregion}

    {$region 'IHelpSystemFlags'}
    FUNCTION GetUseDefaultTopic: boolean;
    PROCEDURE SetUseDefaultTopic(AValue: boolean);
    {$endregion}

    constructor Create;
    destructor Destroy; override;
    procedure Reset;
    procedure AddKeyword(HelpString: string; AIgnoreDuplicate: boolean = false);
  private
    FViewer: ICustomHelpViewer;
    FViewerID: Integer;
    FHelpManager: IHelpManager;
    FKeywords: TStringList;
    FHelpStrings: TStringList;
    FShowHelpStrings: TStringList;
    FEnabled: Boolean;
    FUseDefaultTopic: Boolean;
    procedure DoUnregister;
    procedure DoRegister;
    procedure SetKeywords(const Value: TStringList);
    procedure SetHelpStrings(const Value: TStringList);
    procedure SetShowHelpStrings(const Value: TStringList);
    procedure SetEnabled(const Value: Boolean);
  published
    property Keywords: TStringList read FKeywords write SetKeywords;
    property HelpStrings: TStringList read FHelpStrings write SetHelpStrings;
    property ShowHelpStrings: TStringList read FShowHelpStrings write SetShowHelpStrings;
    property Enabled: Boolean read FEnabled write SetEnabled;
  end;

var
  CustomHelpKeywordRecorder: TCustomHelpKeywordRecorder;

implementation

uses
  SysUtils, ToolsApi, uCustomHelpMain;

{ TKeywordsHelpViewer }

procedure TCustomHelpKeywordRecorder.AddKeyword(HelpString: string; AIgnoreDuplicate: boolean);
begin
  if AnsiSameText(HelpString, KIBITZ_IGNORED_HELPSTRING) then
    Exit;
  with FKeywords do
    if AIgnoreDuplicate and (IndexOf(HelpString) > -1) then
      Exit
    else if (Count = 0) or (Strings[Count-1] <> HelpString) then
      Add(HelpString);
end;

function TCustomHelpKeywordRecorder.CanShowTableOfContents: Boolean;
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
  FViewer := Self;
  FEnabled := False;
  FUseDefaultTopic := True;
  Action(self.FKeywords);
  Action(self.FHelpStrings);
  Action(self.FShowHelpStrings);
  DoRegister;
end;

destructor TCustomHelpKeywordRecorder.Destroy;
  procedure Action(var sl: TStringList);
  begin
    FreeAndNil(sl);
  end;
begin
  ShutDown;
  Action(self.FKeywords);
  Action(self.FHelpStrings);
  Action(self.FShowHelpStrings);
  if CustomHelpKeywordRecorder = Self then
    CustomHelpKeywordRecorder := nil;
  inherited;
end;

function TCustomHelpKeywordRecorder.GetHelpStrings(
  const HelpString: string): TStringList;
begin
  Result := TStringList.Create;
  Result.Values[GetViewerName] := HelpString;
  if not Enabled then
    Exit;
  FHelpStrings.Add(HelpString);
end;

function TCustomHelpKeywordRecorder.GetViewerName: string;
begin
  Result := ClassName;
end;

procedure TCustomHelpKeywordRecorder.DoRegister;
begin
  if not Assigned(FHelpManager) then
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
    FViewer := NIL;
    hm.Release(FViewerID);
  end;
end;

procedure TCustomHelpKeywordRecorder.NotifyID(const ViewerID: Integer);
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
  if not Enabled then
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
  const HelpString: string): Integer;
begin
  Result := 0;
  if not Enabled then
    Exit;
  AddKeyword(HelpString);
end;

procedure TCustomHelpKeywordRecorder.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
  if not FEnabled then
    Reset;
end;

procedure TCustomHelpKeywordRecorder.SetHelpStrings(const Value: TStringList);
begin
  FHelpStrings := Value;
end;

procedure TCustomHelpKeywordRecorder.SetKeywords(const Value: TStringList);
begin
  FKeywords.Assign(Value);
end;

procedure TCustomHelpKeywordRecorder.SetShowHelpStrings(
  const Value: TStringList);
begin
  FShowHelpStrings := Value;
end;

{$region 'IExtendedHelpViewer'}

procedure TCustomHelpKeywordRecorder.DisplayHelpByContext(
  const ContextID: integer; const HelpFileName: string);
begin
  if Keywords.Count > 0 then
    HelpViewer.ShowHelp(Keywords[0], GlobalCustomHelp.LastHelpCallKeyword);
end;

procedure TCustomHelpKeywordRecorder.DisplayTopic(const Topic: string);
begin
  //
end;

function TCustomHelpKeywordRecorder.UnderstandsTopic(
  const Topic: string): boolean;
begin
  Result := False;
end;

function TCustomHelpKeywordRecorder.UnderstandsContext(const ContextID: integer;
  const HelpFileName: string): boolean;
begin
  Result := False;
  if not Enabled then
    Exit;
  case ContextID of
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
  CustomHelpKeywordRecorder := TCustomHelpKeywordRecorder.Create;

finalization
  if Assigned(CustomHelpKeywordRecorder) then
  begin
    if Assigned(CustomHelpKeywordRecorder.FHelpManager) then
      CustomHelpKeywordRecorder.ShutDown
    else
      FreeAndNil(CustomHelpKeywordRecorder);
  end;

end.
