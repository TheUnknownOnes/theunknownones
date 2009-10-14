UNIT uCustomHelpDebug;

{$DEFINE DEBUG_CUSTOMHELP}

INTERFACE

USES
  Classes,
  HelpIntfs;

CONST
  DEBUG_CUSTOMHELP: boolean = {$IFDEF DEBUG_CUSTOMHELP}True{$ELSE}False{$ENDIF};

TYPE
  {$TYPEINFO ON}
  TCustomHelpDebugViewer = CLASS(TInterfacedObject,
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
    DESTRUCTOR Destroy; OVERRIDE;
    PROCEDURE AssignHelpSelector(AClear: boolean);
  PRIVATE
    FUseDefaultTopic: boolean;
    FViewer:          ICustomHelpViewer;
    FViewerID:        integer;
    FHelpManager:     IHelpManager;
    FKeywords:        TStringList;
    FContents:        TStringList;
    FViewers:         TStringList;
    FHelpStrings:     TStringList;
    FUnderstandsKeywords: TStringList;
    FShowHelpStrings: TStringList;
    CONSTRUCTOR Create;
    PROCEDURE DoUnregister;
    PROCEDURE DoRegister;
    PROCEDURE SetContents(CONST Value: TStringList);
    PROCEDURE SetHelpStrings(CONST Value: TStringList);
    PROCEDURE SetKeywords(CONST Value: TStringList);
    PROCEDURE SetShowHelpStrings(CONST Value: TStringList);
    PROCEDURE SetUnderstandsKeywords(CONST Value: TStringList);
    PROCEDURE SetViewers(CONST Value: TStringList);
  PROTECTED
    PROCEDURE DebugLog(method, msg: string; LogDefaultTopic: boolean); OVERLOAD;
    PROCEDURE DebugLog(method, msg: string); OVERLOAD;
  PUBLISHED
    PROPERTY Keywords: TStringList Read FKeywords Write SetKeywords;
    PROPERTY Contents: TStringList Read FContents Write SetContents;
    PROPERTY Viewers: TStringList Read FViewers Write SetViewers;
    PROPERTY HelpStrings: TStringList Read FHelpStrings Write SetHelpStrings;
    PROPERTY ShowHelpStrings: TStringList Read FShowHelpStrings Write SetShowHelpStrings;
    PROPERTY UnderstandsKeywords: TStringList
      Read FUnderstandsKeywords Write SetUnderstandsKeywords;
  END;

  //This class must not contain any local fields
  //applies only to BDS 2006
  {$TYPEINFO ON}
  TCustomHelpDebugSelector = CLASS(TInterfacedObject,
    IHelpSelector, IHelpSelector2)
  PROTECTED
    FUNCTION SelectKeyword(Keywords: TStrings): integer;
    FUNCTION TableOfContents(Contents: TStrings): integer;
    FUNCTION SelectContext(Viewers: TStrings): integer;
  END;

VAR
  //We keep these global ... as it is done in WinHelpViewer.pas
  CustomHelpDebugViewer: TCustomHelpDebugViewer;

IMPLEMENTATION

USES
  SysUtils,
  ToolsApi,
  Windows;

{ TKeywordsHelpSelector }

FUNCTION TCustomHelpDebugSelector.SelectContext(Viewers: TStrings): integer;
BEGIN
  CustomHelpDebugViewer.Viewers.Text := Viewers.Text;
  Result := 0;
  CustomHelpDebugViewer.DebugLog('Selector.SelectContext',
    '(' + Viewers.CommaText + ')=' + IntToStr(Result), False);
END;

FUNCTION TCustomHelpDebugSelector.SelectKeyword(Keywords: TStrings): integer;
BEGIN
  CustomHelpDebugViewer.Keywords.Text := Keywords.Text;
  // Abort help call ...
  Result := -1;
  CustomHelpDebugViewer.DebugLog('Selector.SelectKeyword',
    '(' + Keywords.CommaText + ')=' + IntToStr(Result), False);
END;

FUNCTION TCustomHelpDebugSelector.TableOfContents(Contents: TStrings): integer;
BEGIN
  CustomHelpDebugViewer.Contents.Text := Contents.Text;
  Result := -1;
  CustomHelpDebugViewer.DebugLog('Selector.TableOfContents',
    '(' + Contents.CommaText + ')=' + IntToStr(Result), False);
END;

{ TKeywordsHelpViewer }

FUNCTION TCustomHelpDebugViewer.CanShowTableOfContents: boolean;
BEGIN
  Result := False;
  DebugLog('ShowTableOfContents', '=' + BoolToStr(Result, True));
END;

CONSTRUCTOR TCustomHelpDebugViewer.Create;

  PROCEDURE CreateStringList(VAR sl: TStringList);
  BEGIN
    sl := TStringList.Create;
    sl.Duplicates := dupAccept;
  END;

BEGIN
  INHERITED;
  DebugLog('Create', '', False);
  FHelpManager := NIL;
  FUseDefaultTopic := True;
  FViewer := Self;
  CreateStringList(self.FKeywords);
  CreateStringList(self.FContents);
  CreateStringList(self.FViewers);
  CreateStringList(self.FHelpStrings);
  CreateStringList(self.FUnderstandsKeywords);
  CreateStringList(self.FShowHelpStrings);
  DoRegister;
END;

PROCEDURE TCustomHelpDebugViewer.DebugLog(method, msg: string);
BEGIN
  DebugLog(method, msg, True);
END;

PROCEDURE TCustomHelpDebugViewer.DebugLog(method, msg: string;
  LogDefaultTopic: boolean);
VAR
  s: string;
BEGIN
  IF LogDefaultTopic THEN
    method := method + '{' + BoolToStr(FUseDefaultTopic, True) + '}';
  s := FormatDateTime('', Now) + ': ' + ClassName + '[' + method + ']: ' + msg;
  OutputDebugString(PChar(s));
END;

DESTRUCTOR TCustomHelpDebugViewer.Destroy;
BEGIN
  DebugLog('Destroy', '', False);
  ShutDown;
  FreeAndNil(self.FKeywords);
  FreeAndNil(self.FContents);
  FreeAndNil(self.FViewers);
  FreeAndNil(self.FHelpStrings);
  FreeAndNil(self.FUnderstandsKeywords);
  FreeAndNil(self.FShowHelpStrings);
  IF CustomHelpDebugViewer = Self THEN
    CustomHelpDebugViewer := NIL;
  INHERITED;
END;

FUNCTION TCustomHelpDebugViewer.GetHelpStrings(CONST HelpString: string): TStringList;
BEGIN
  Self.HelpStrings.Add(HelpString);
  Result := TStringList.Create;
  Result.Values[GetViewerName] := HelpString;
  DebugLog('GetHelpStrings', '(' + HelpString + ')=' + Result.CommaText);
END;

FUNCTION TCustomHelpDebugViewer.GetViewerName: string;
BEGIN
  Result := ClassName;
  DebugLog('GetViewerName', '=' + Result);
END;

PROCEDURE TCustomHelpDebugViewer.AssignHelpSelector(AClear: boolean);
VAR
  hs: IHelpSystem;
BEGIN
  IF GetHelpSystem(hs) THEN
    IF AClear THEN
      hs.AssignHelpSelector(NIL)
    ELSE
      hs.AssignHelpSelector(TCustomHelpDebugSelector.Create);
END;

PROCEDURE TCustomHelpDebugViewer.DoRegister;
BEGIN
  IF NOT Assigned(FHelpManager) THEN
    RegisterViewer(Self, FHelpManager);
END;

PROCEDURE TCustomHelpDebugViewer.DoUnregister;
VAR
  hm: IHelpManager;
BEGIN
  IF Assigned(FHelpManager) THEN
  BEGIN
    hm := FHelpManager;
    FHelpManager := NIL;
    FViewer := NIL;
    hm.Release(FViewerID);
  END;
END;

PROCEDURE TCustomHelpDebugViewer.NotifyID(CONST ViewerID: integer);
BEGIN
  FViewerID := ViewerID;
END;

PROCEDURE TCustomHelpDebugViewer.ShowHelp(CONST HelpString: string);
BEGIN
  DebugLog('ShowHelp', '(' + HelpString + ')');
  Self.ShowHelpStrings.Add(HelpString);
END;

PROCEDURE TCustomHelpDebugViewer.ShowTableOfContents;
BEGIN
  DebugLog('ShowTableOfContents', '');
  // do nothing.
END;

PROCEDURE TCustomHelpDebugViewer.ShutDown;
BEGIN
  DebugLog('ShutDown', '');
  SoftShutDown;
  DoUnregister;
END;

PROCEDURE TCustomHelpDebugViewer.SoftShutDown;
BEGIN
  DebugLog('SoftShutDown', '');
  // do nothing
END;

FUNCTION TCustomHelpDebugViewer.UnderstandsKeyword(CONST HelpString: string): integer;
BEGIN
  Self.UnderstandsKeywords.Add(HelpString);
  Result := 0;
  DebugLog('UnderstandsKeyword', '(' + HelpString + ')=' + IntToStr(Result));
END;

{$region 'IHelpSystemFlags'}

FUNCTION TCustomHelpDebugViewer.GetUseDefaultTopic: boolean;
BEGIN
  Result := FUseDefaultTopic;
END;

FUNCTION TCustomHelpDebugViewer.UnderstandsTopic(CONST Topic: string): boolean;
BEGIN
  Result := False;
  DebugLog('UnderstandsTopic', '(' + Topic + ')=' + BoolToStr(Result, True));
END;

PROCEDURE TCustomHelpDebugViewer.DisplayTopic(CONST Topic: string);
BEGIN
  DebugLog('DisplayTopic', Topic);
END;

PROCEDURE TCustomHelpDebugViewer.SetContents(CONST Value: TStringList);
BEGIN
  FContents.Assign(Value);
END;

PROCEDURE TCustomHelpDebugViewer.SetHelpStrings(CONST Value: TStringList);
BEGIN
  FHelpStrings.Assign(Value);
END;

PROCEDURE TCustomHelpDebugViewer.SetKeywords(CONST Value: TStringList);
BEGIN
  FKeywords.Assign(Value);
END;

PROCEDURE TCustomHelpDebugViewer.SetShowHelpStrings(CONST Value: TStringList);
BEGIN
  FShowHelpStrings.Assign(Value);
END;

PROCEDURE TCustomHelpDebugViewer.SetUnderstandsKeywords(CONST Value: TStringList);
BEGIN
  FUnderstandsKeywords.Assign(Value);
END;

PROCEDURE TCustomHelpDebugViewer.SetUseDefaultTopic(AValue: boolean);
BEGIN
  FUseDefaultTopic := AValue;
END;

PROCEDURE TCustomHelpDebugViewer.SetViewers(CONST Value: TStringList);
BEGIN
  FViewers.Assign(Value);
END;

FUNCTION TCustomHelpDebugViewer.UnderstandsContext(CONST ContextID: integer;
  CONST HelpFileName: string): boolean;
BEGIN
  Result := False;
  DebugLog('UnderstandsContext', '(' + IntToStr(ContextID) + ', ' +
    HelpFileName + ')=' + BoolToStr(Result));
END;

PROCEDURE TCustomHelpDebugViewer.DisplayHelpByContext(CONST ContextID: integer;
  CONST HelpFileName: string);
BEGIN
  DebugLog('DisplayHelpByContext', '(' + IntToStr(ContextID) + ', ' + HelpFileName);
END;

{$endregion}

INITIALIZATION
  IF DEBUG_CUSTOMHELP THEN
    CustomHelpDebugViewer := TCustomHelpDebugViewer.Create
  ELSE
    CustomHelpDebugViewer := NIL;

FINALIZATION
  IF CustomHelpDebugViewer <> NIL THEN
  begin
    CustomHelpDebugViewer.FViewer := nil;
    CustomHelpDebugViewer := nil;
  end;

END.

