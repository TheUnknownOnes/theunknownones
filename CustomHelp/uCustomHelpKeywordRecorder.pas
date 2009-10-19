UNIT uCustomHelpKeywordRecorder;

INTERFACE

USES
  Classes,
  HelpIntfs,
  uCustomHelpIntfs;

CONST
  hcIDEStructureView   = $49E;
  hcIDEProjectExplorer = $550;
  hcIDEFormDesigner    = $4E2;
  hcIDEToolPalette     = $90E;
  hcIDECodeEditor      = $564;

VAR
  //We keep these global ... as it is done in WinHelpViewer.pas
  //and keep the reference to the object in the implementation section.
  CustomHelpKeywordRecorderIntf: ICustomHelpKeywordRecorder;

IMPLEMENTATION

USES
  SysUtils,
  ToolsApi,
  uCustomHelpMain;

TYPE
  {$TYPEINFO ON}
  TCustomHelpKeywordRecorder = CLASS(TInterfacedObject,
    ICustomHelpKeywordRecorder,
    IExtendedHelpViewer,
    IHelpSystemFlags,
    ICustomHelpViewer)
  PUBLIC
    {$REGION 'ICustomHelpViewer'}
    FUNCTION GetViewerName: string;
    FUNCTION UnderstandsKeyword(CONST HelpString: string): integer;
    FUNCTION GetHelpStrings(CONST HelpString: string): TStringList;
    FUNCTION CanShowTableOfContents: boolean;
    PROCEDURE ShowTableOfContents;
    PROCEDURE ShowHelp(CONST HelpString: string);
    PROCEDURE NotifyID(CONST ViewerID: integer);
    PROCEDURE SoftShutDown;
    PROCEDURE ShutDown;
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

    {$REGION 'ICustomHelpKeywordRecorder'}
    FUNCTION GetKeywordList: TStringList;
    PROCEDURE SetKeywordList(CONST Value: TStringList);
    FUNCTION GetHelpStringList: TStringList;
    PROCEDURE SetHelpStringList(CONST Value: TStringList);
    FUNCTION GetShowHelpStringList: TStringList;
    PROCEDURE SetShowHelpStringList(CONST Value: TStringList);
    FUNCTION GetEnabled: boolean;
    PROCEDURE SetEnabled(CONST Value: boolean);
    PROCEDURE Reset;
    PROCEDURE AddKeyword(HelpString: string; AIgnoreDuplicate: boolean = False);
    {$ENDREGION}

    CONSTRUCTOR Create;
    DESTRUCTOR Destroy; OVERRIDE;
  PRIVATE
    FViewerID: integer;
    FHelpManager: IHelpManager;
    FKeywords: TStringList;
    FHelpStrings: TStringList;
    FShowHelpStrings: TStringList;
    FEnabled:  boolean;
    FUseDefaultTopic: boolean;
    PROCEDURE DoUnregister;
    PROCEDURE DoRegister;
  PUBLISHED
    PROPERTY Keywords: TStringList Read GetKeywordList Write SetKeywordList;
    PROPERTY HelpStrings: TStringList Read GetHelpStringList Write SetHelpStringList;
    PROPERTY ShowHelpStrings: TStringList Read GetShowHelpStringList
      Write SetShowHelpStringList;
    PROPERTY Enabled: boolean Read GetEnabled Write SetEnabled;
  END;

VAR
  CustomHelpKeywordRecorder: TCustomHelpKeywordRecorder;

{ TKeywordsHelpViewer }

PROCEDURE TCustomHelpKeywordRecorder.AddKeyword(HelpString: string;
  AIgnoreDuplicate: boolean);
BEGIN
  IF AnsiSameText(HelpString, KIBITZ_IGNORED_HELPSTRING) THEN
    Exit;
  WITH FKeywords DO
    IF AIgnoreDuplicate AND (IndexOf(HelpString) > -1) THEN
      Exit
    ELSE IF (Count = 0) OR (Strings[Count - 1] <> HelpString) THEN
      Add(HelpString);
END;

FUNCTION TCustomHelpKeywordRecorder.CanShowTableOfContents: boolean;
BEGIN
  Result := False;
END;

CONSTRUCTOR TCustomHelpKeywordRecorder.Create;

  PROCEDURE Action(VAR sl: TStringList);
  BEGIN
    sl := TStringList.Create;
    sl.Duplicates := dupAccept;
  END;

BEGIN
  INHERITED;
  FHelpManager := NIL;
  FEnabled := False;
  FUseDefaultTopic := True;
  Action(self.FKeywords);
  Action(self.FHelpStrings);
  Action(self.FShowHelpStrings);
  DoRegister;
  CustomHelpKeywordRecorder := Self;
END;

DESTRUCTOR TCustomHelpKeywordRecorder.Destroy;

  PROCEDURE Action(VAR sl: TStringList);
  BEGIN
    FreeAndNil(sl);
  END;

BEGIN
  Action(self.FKeywords);
  Action(self.FHelpStrings);
  Action(self.FShowHelpStrings);

  IF CustomHelpKeywordRecorder = Self THEN
    CustomHelpKeywordRecorder := NIL;

  INHERITED;
END;

FUNCTION TCustomHelpKeywordRecorder.GetEnabled: boolean;
BEGIN
  Result := FEnabled;
END;

FUNCTION TCustomHelpKeywordRecorder.GetHelpStringList: TStringList;
BEGIN
  Result := FHelpStrings;
END;

FUNCTION TCustomHelpKeywordRecorder.GetHelpStrings(
  CONST HelpString: string): TStringList;
BEGIN
  Result := TStringList.Create;
  Result.Values[GetViewerName] := HelpString;
  IF NOT Enabled THEN
    Exit;
  FHelpStrings.Add(HelpString);
END;

FUNCTION TCustomHelpKeywordRecorder.GetKeywordList: TStringList;
BEGIN
  Result := FKeywords;
END;

FUNCTION TCustomHelpKeywordRecorder.GetShowHelpStringList: TStringList;
BEGIN
  Result := FShowHelpStrings;
END;

FUNCTION TCustomHelpKeywordRecorder.GetViewerName: string;
BEGIN
  Result := ClassName;
END;

PROCEDURE TCustomHelpKeywordRecorder.DoRegister;
BEGIN
  IF NOT Assigned(FHelpManager) THEN
    RegisterViewer(Self, FHelpManager);
END;

PROCEDURE TCustomHelpKeywordRecorder.DoUnregister;
VAR
  hm: IHelpManager;
BEGIN
  IF Assigned(FHelpManager) THEN
  BEGIN
    hm := FHelpManager;
    FHelpManager := NIL;
    hm.Release(FViewerID);
  END;
END;

PROCEDURE TCustomHelpKeywordRecorder.NotifyID(CONST ViewerID: integer);
BEGIN
  FViewerID := ViewerID;
END;

PROCEDURE TCustomHelpKeywordRecorder.Reset;

  PROCEDURE Action(VAR sl: TStringList);
  BEGIN
    sl.Clear;
  END;

BEGIN
  Action(self.FKeywords);
  Action(self.FHelpStrings);
  Action(self.FShowHelpStrings);
END;

PROCEDURE TCustomHelpKeywordRecorder.ShowHelp(CONST HelpString: string);
BEGIN
  IF NOT Enabled THEN
    Exit;

  // do nothing
  FShowHelpStrings.Add(HelpString);
END;

PROCEDURE TCustomHelpKeywordRecorder.ShowTableOfContents;
BEGIN
  // do nothing.
END;

PROCEDURE TCustomHelpKeywordRecorder.ShutDown;
BEGIN
  SoftShutDown;
  DoUnregister;
END;

PROCEDURE TCustomHelpKeywordRecorder.SoftShutDown;
BEGIN
  // do nothing
END;

FUNCTION TCustomHelpKeywordRecorder.UnderstandsKeyword(
  CONST HelpString: string): integer;
BEGIN
  Result := 0;
  IF NOT Enabled THEN
    Exit;
  AddKeyword(HelpString);
END;

PROCEDURE TCustomHelpKeywordRecorder.SetEnabled(CONST Value: boolean);
BEGIN
  FEnabled := Value;
  IF NOT FEnabled THEN
    Reset;
END;

PROCEDURE TCustomHelpKeywordRecorder.SetHelpStringList(CONST Value: TStringList);
BEGIN
  FHelpStrings := Value;
END;

PROCEDURE TCustomHelpKeywordRecorder.SetKeywordList(CONST Value: TStringList);
BEGIN
  FKeywords.Assign(Value);
END;

PROCEDURE TCustomHelpKeywordRecorder.SetShowHelpStringList(CONST Value: TStringList);
BEGIN
  FShowHelpStrings := Value;
END;

{$region 'IExtendedHelpViewer'}

PROCEDURE TCustomHelpKeywordRecorder.DisplayHelpByContext(CONST ContextID: integer;
  CONST HelpFileName: string);
BEGIN
  IF Keywords.Count > 0 THEN
    GlobalCustomHelp.ShowHelp(Keywords[0], GlobalCustomHelp.LastHelpCallKeyword);
END;

PROCEDURE TCustomHelpKeywordRecorder.DisplayTopic(CONST Topic: string);
BEGIN

END;

FUNCTION TCustomHelpKeywordRecorder.UnderstandsTopic(CONST Topic: string): boolean;
BEGIN
  Result := False;
END;

FUNCTION TCustomHelpKeywordRecorder.UnderstandsContext(CONST ContextID: integer;
  CONST HelpFileName: string): boolean;
BEGIN
  Result := False;
  IF NOT Enabled THEN
    Exit;
  CASE ContextID OF
    hcIDEFormDesigner, hcIDECodeEditor:
      Result := Keywords.Count > 0;
  END;
END;

{$endregion}

{$region 'IHelpSystemFlags'}
FUNCTION TCustomHelpKeywordRecorder.GetUseDefaultTopic: boolean;
BEGIN
  Result := FUseDefaultTopic;
END;

PROCEDURE TCustomHelpKeywordRecorder.SetUseDefaultTopic(AValue: boolean);
BEGIN
  FUseDefaultTopic := AValue;
END;

{$endregion}

INITIALIZATION
  CustomHelpKeywordRecorderIntf := TCustomHelpKeywordRecorder.Create;

FINALIZATION
  CustomHelpKeywordRecorderIntf := NIL;
  // This will automatically clear HelpViewer if object is destroyed.
  IF CustomHelpKeywordRecorder <> NIL THEN
    // This will unregister the viewer from the help system
    CustomHelpKeywordRecorder.ShutDown;

END.

