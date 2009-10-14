unit uCustomHelpKeywordRecorder;

interface

uses
  Classes, HelpIntfs;

type
  {$TYPEINFO ON}
  TCustomHelpKeywordRecorder = class(TInterfacedObject,
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
    constructor Create;
    destructor Destroy; override;
    procedure Reset;
  private
    FViewer: ICustomHelpViewer;
    FViewerID: Integer;
    FHelpManager: IHelpManager;
    FKeywords: TStringList;
    FHelpStrings: TStringList;
    FShowHelpStrings: TStringList;
    procedure DoUnregister;
    procedure DoRegister;
    procedure SetKeywords(const Value: TStringList);
    procedure SetHelpStrings(const Value: TStringList);
    procedure SetShowHelpStrings(const Value: TStringList);
  published
    property Keywords: TStringList read FKeywords write SetKeywords;
    property HelpStrings: TStringList read FHelpStrings write SetHelpStrings;
    property ShowHelpStrings: TStringList read FShowHelpStrings write SetShowHelpStrings;
  end;

var
  CustomHelpKeywordRecorder: TCustomHelpKeywordRecorder;

implementation

uses
  SysUtils, ToolsApi;

{ TKeywordsHelpViewer }

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
  with FKeywords do
    if (Count = 0) or (Strings[Count-1] <> HelpString) then
      Add(HelpString);
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

initialization
  CustomHelpKeywordRecorder := TCustomHelpKeywordRecorder.Create;

finalization
  if Assigned(CustomHelpKeywordRecorder) then
    FreeAndNil(CustomHelpKeywordRecorder);

end.
