{-----------------------------------------------------------------------------
 Project: Settings
 Purpose: Contains the base classes for working with Settings
 Created: 21.05.2008 14:40:48

 (c) by TheUnknownOnes under dwywbdbu license - see http://theunknownones.googlecode.com/svn/ for the license
 see http://www.TheUnknownOnes.net
-----------------------------------------------------------------------------}
unit uSettingsBase;

interface

{$I JEDI.inc}

uses
  Classes,
  Contnrs,
  Sysutils,
  Variants,
  RegExpr,
  Windows,
  uSettingsRTTI,
  uRTTIHelper,
  TypInfo;

type

  TSetting = class;

  TCustomSettingsLink = class;
  TCustomSettingsLinkList = class;

  { - use the following types whenever possible}
  TSettingName = String;

  TSettingValue = Variant;

  TSettingValues = array of TSettingValue;

  TSettingNames = array of TSettingName;

  TSettingNameValue = record
    Name : TSettingName;
    Value : TSettingValue;
  end;

  TSettingNameValues = array of TSettingNameValue;


//==============================================================================


  TSettingList = class(TList)
  { - just a list of TSetting-Objects
    - for internal use only}
  protected
    function GetItem(Index: Integer): TSetting;
    procedure SetItem(Index: Integer; const Value: TSetting);

  public
    function Add(ASetting: TSetting): Integer;
    function Extract(Item: TSetting): TSetting;
    function Remove(ASetting: TSetting): Integer;
    function IndexOf(ASetting: TSetting): Integer; overload;
    function IndexOfPath(APath: TSettingName): Integer; overload;
    function IndexOfName(AName: TSettingName): Integer; overload;
    procedure Insert(Index: Integer; ASetting: TSetting);
    function First: TSetting;
    function Last: TSetting;
    function ContainsPath(APath : TSettingName) : Boolean;
    procedure AddSettings(ASettings : TSettingList);
    property Items[Index: Integer]: TSetting read GetItem write SetItem; default;
  end;


//==============================================================================


  TSetting = class
  { - represents one setting
    - cares about its parent and children
    - for internal use only}
  protected
    FParent : TSetting;
    FName : TSettingName;
    FValue : TSettingValue;

    FChildren : TSettingList;

    FIndex : TStringList;

    function CalculatePath : TSettingName;

    procedure CreateIndex();
    procedure InitIndex();
    procedure AddSettingToIndex(ASetting : TSetting);
    procedure RemoveSettingFromIndex(ASetting : TSetting);
    procedure DestroyIndex();

    procedure SetValue(const Value: TSettingValue);
    procedure SetParent(const Value: TSetting);
    procedure SetName(const Value: TSettingName);

    procedure CreateFromPath(ARoot : TSetting; APath : TSettingName);
  public
    constructor Create(); overload;
    constructor Create(AParent : TSetting); overload;
    constructor Create(AParent : TSetting; AName : TSettingName; ANameIsPath : Boolean = false); overload;
    constructor Create(AParent : TSetting; AName : TSettingName; AValue : TSettingValue); overload;


    destructor Destroy(); override;

    procedure RegisterChild(const AChild : TSetting);
    procedure UnregisterChild(const AChild : TSetting);

    procedure Clear;

    procedure Assign(ASetting : TSetting);

    function NameMatches(APattern : TSettingName; AIsRegEx : Boolean = false) : Boolean;

    function GetPath : TSettingName;

    property Name : TSettingName read FName write SetName;
    property Value : TSettingValue read FValue write SetValue;
    property Parent : TSetting read FParent write SetParent;
    property Children : TSettingList read FChildren;
    property Index : TStringList read FIndex;
  end;


//==============================================================================


  TParentMode = (pmDontUse,
                 pmAddsMissing,
                 pmOverridesAll);


//==============================================================================


  TCustomSettings = class(TComponent)
  { - the mother of all all TSetting-Components
    - provides routines for setting/getting/deleting settings
    - should be the base for implementing new features}
  protected
    FRootSetting : TSetting;
    FParentSettings: TCustomSettings;
    FParentMode: TParentMode;
    FAutoSaveOnDestroy : Boolean;

    FComponentLinks : TCustomSettingsLinkList;

    procedure SetParentSettings(const Value: TCustomSettings);

    procedure QuerySettings(const APath : TSettingName;
                            const AIsRegEx : Boolean;
                            const AList : TSettingList;
                            const AUseParent : Boolean);

    function DoLoad() : Boolean; virtual; abstract;
    function DoSave() : Boolean; virtual; abstract;

    procedure InformComponentLinksAboutLoad;
    procedure InformComponentLinksAboutSave;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Assign(ASource : TPersistent); override;

    procedure SetValue(APath : TSettingName;
                       AValue : Variant;
                       ACreateIfMissing : Boolean = true;
                       AIsRegExPath : Boolean = false);

    procedure WriteObject(APath : TSettingName;
                        const AObject : TObject;
                        ARecursive : Boolean = true;
                        ACreateIfMssing : Boolean = true;
                        AIsRegExPath : Boolean = false);

    function GetValues(APath : TSettingName;
                       ADefault : Variant;
                       AIsRegExPath : Boolean = false) : TSettingValues;

    function GetValue(APath : TSettingName;
                      ADefault : Variant;
                      AIsRegExPath : Boolean = false) : TSettingValue;

    procedure ReadObject(APath : TSettingName;
                        const AObject : TObject;
                        ARecursive : Boolean = true;
                        ACreateIfMssing : Boolean = true;
                        AIsRegExPath : Boolean = false);

    function GetNames(APath : TSettingName;
                      AIsRegExPath : Boolean = true;
                      AFullPathNames : Boolean = false) : TSettingNames;

    function GetNameValues(APath : TSettingName;
                           ADefault : Variant;
                           AIsRegExPath : Boolean;
                           AGetNames : Boolean = true;
                           AFullPathNames : Boolean = false;
                           AGetValues : Boolean = true) : TSettingNameValues;

    function GetNameValue(APath : TSettingName;
                          ADefault : Variant;
                          AIsRegEx : Boolean= false;
                          AGetName : Boolean = true;
                          AFullPathName : Boolean = false;
                          AGetValues : Boolean = true) : TSettingNameValue;

    function GetSubNames(APath : TSettingName;
                         AIsRegExPath : Boolean = false;
                         AFullPathNames : Boolean = false) : TSettingNames;

    function GetSubNameValues(APath : TSettingName;
                              AIsRegExPath : Boolean = false;
                              AGetNames : Boolean = true;
                              AFullPathNames : Boolean = false;
                              AGetValues : Boolean = true) : TSettingNameValues;

    function Exists(APath : TSettingName; AIsRegEx : Boolean = false) : Boolean;
    procedure Delete(APath : TSettingName; AIsRegEx : Boolean = false);
    procedure Clear;
    function Move(ANewParent : TSettingName;
                  APath : TSettingName;
                  AIsRegExPath : Boolean = false;
                  AFullPathNames : Boolean = false) : TSettingNames;

    procedure RegisterComponentLink(ALink : TCustomSettingsLink);
    procedure UnRegisterComponentLink(ALink : TCustomSettingsLink);

    property ParentSettings : TCustomSettings read FParentSettings write SetParentSettings;
    property ParentMode : TParentMode read FParentMode write FParentMode default pmAddsMissing;
    property AutoSaveOnDestroy : Boolean read FAutoSaveOnDestroy write FAutoSaveOnDestroy default false;

    function Load() : Boolean;
    function Save() : Boolean;
  end;


//==============================================================================


  TCustomSettingsLinkList = class(TList)
  { - a list for Settings-Links
    - for internal use only}
  protected
    function GetItem(Index: Integer): TCustomSettingsLink;
    procedure SetItem(Index: Integer; const Value: TCustomSettingsLink);
  public
    function Add(ALink: TCustomSettingsLink): Integer;
    function Extract(Item: TCustomSettingsLink): TCustomSettingsLink;
    function Remove(ALink: TCustomSettingsLink): Integer;
    function IndexOf(ALink: TCustomSettingsLink): Integer; overload;
    procedure Insert(Index: Integer; ALink: TCustomSettingsLink);
    function First: TCustomSettingsLink;
    function Last: TCustomSettingsLink;
    property Items[Index: Integer]: TCustomSettingsLink read GetItem write SetItem; default;
  end;


//==============================================================================

  TNeedRootSettingProc = procedure(const ALink : TCustomSettingsLink; out ARootSetting : TSettingName) of object;


  TCustomSettingsLink = class(TComponent)
  { - a Settings-Link connects something (a component, ...) with a
      Settings-Cbject in order to store/restore Settings of the "something"}
  protected
    FSettings: TCustomSettings;
    FDefaultRootSetting: TSettingName;
    FActive: Boolean;

    FOnNeedRootSetting: TNeedRootSettingProc;
    FOnBeforeSaveSettings: TNotifyEvent;
    FOnBeforeApplySettings: TNotifyEvent;
    FOnAfterSaveSettings: TNotifyEvent;
    FOnAfterApplySettings: TNotifyEvent;

    procedure SetSettings(const Value: TCustomSettings);
    procedure SetRootSetting(const Value: TSettingName);
    procedure SetActive(const Value: Boolean);

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure DoApplySettings(const ARootSetting : TSettingName); virtual; abstract;
    procedure DoSaveSettings(const ARootSetting : TSettingName); virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ApplySettings;
    procedure SaveSettings;

    property Active : Boolean read FActive write SetActive default true;

    property Settings : TCustomSettings read FSettings write SetSettings;
    property DefaultRootSetting : TSettingName read FDefaultRootSetting write SetRootSetting;

    property OnNeedRootSetting : TNeedRootSettingProc read FOnNeedRootSetting write FOnNeedRootSetting;

    property OnBeforeSaveSettings : TNotifyEvent read FOnBeforeSaveSettings write FOnBeforeSaveSettings;
    property OnAfterSaveSettings : TNotifyEvent read FOnAfterSaveSettings write FOnAfterSaveSettings;
    property OnBeforeApplySettings : TNotifyEvent read FOnBeforeApplySettings write FOnBeforeApplySettings;
    property OnAfterApplySettings : TNotifyEvent read FOnAfterApplySettings write FOnAfterApplySettings;


  end;


//==============================================================================


  TCustomSettingsComponentLink = class(TCustomSettingsLink)
  protected
    FComponent: TComponent;

    FSaveProperties: TsrPropertyList;

    procedure SetComponent(const Value: TComponent);

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    function GenerateRootSettingName(AComponent : TComponent) : TSettingName; virtual;

    procedure DoApplySettings(const ARootSetting : TSettingName); override;
    procedure DoSaveSettings(const ARootSetting : TSettingName); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Component : TComponent read FComponent write SetComponent;
    property SaveProperties : TsrPropertyList read FSaveProperties write FSaveProperties;
  end;


//==============================================================================


const
  SettingsPathDelimiter = '/';

  {$IFDEF DELPHI12_UP}
  SettingsValidValueTypes : array[0..15] of TVarType = (varEmpty,
                                                        varNull,
                                                        varSmallint,
                                                        varInteger,
                                                        varSingle,
                                                        varDouble,
                                                        varDate,
                                                        varOleStr,
                                                        varBoolean,
                                                        varShortInt,
                                                        varByte,
                                                        varWord,
                                                        varLongWord,
                                                        varInt64,
                                                        varString,
                                                        varUString);
  {$else}
  SettingsValidValueTypes : array[0..14] of TVarType = (varEmpty,
                                                        varNull,
                                                        varSmallint,
                                                        varInteger,
                                                        varSingle,
                                                        varDouble,
                                                        varDate,
                                                        varOleStr,
                                                        varBoolean,
                                                        varShortInt,
                                                        varByte,
                                                        varWord,
                                                        varLongWord,
                                                        varInt64,
                                                        varString);
  {$endif}
  SettingsComponentGroup = 'Settings';

  SettingsExcludeObjectPropertyTypeKinds = [tkUnknown,
                                            tkClass,
                                            tkMethod,
                                            tkInterface];

var
  SettingsRegExCaseinsensitive : Boolean = true;
  SettingsRegExRussianSupport : Boolean = false;
  SettingsRegExDotMatchesLineSeperators : Boolean = true;
  SettingsRegExAllOperatorsGreedy : Boolean = true;


//==============================================================================

function SettingsExcludeTrailingPathDelimiter(APath : TSettingName) : TSettingName;
function SettingsIncludeLeadingPathDelimiter(APath : TSettingName) : TSettingName;
procedure SettingsSplitPath(APath : String; AList : TStrings);
procedure SettingsInitRegEx(const ARegEx : TRegExpr);
function SettingsNameStringMatches(AName, APattern : TSettingName; AIsRegEx : Boolean) : Boolean;
function SettingsCheckValueType(AVariant : Variant; ARaiseException : Boolean = true) : Boolean;

procedure SettingsApplyAllLinks(AParentComponent: TComponent);
procedure SettingsSaveAllLinks(AParentComponent: TComponent);

implementation

{ Helper }

procedure SettingsApplyAllLinks(AParentComponent: TComponent);
var
  idx : Integer;
  compo : TComponent;
begin
  for idx := 0 to AParentComponent.ComponentCount - 1 do
  begin
    compo := AParentComponent.Components[idx];

    if compo is TCustomSettingsLink then
      TCustomSettingsLink(compo).ApplySettings;

    SettingsApplyAllLinks(compo);
  end;
end;

procedure SettingsSaveAllLinks(AParentComponent: TComponent);
var
  idx : Integer;
  compo : TComponent;
begin
  for idx := 0 to AParentComponent.ComponentCount - 1 do
  begin
    compo := AParentComponent.Components[idx];

    if compo is TCustomSettingsLink then
      TCustomSettingsLink(compo).SaveSettings;

    SettingsSaveAllLinks(compo);
  end;
end;

function SettingsExcludeTrailingPathDelimiter(APath : TSettingName) : TSettingName;
begin
  if Copy(APath, Length(APath), 1) = SettingsPathDelimiter then
    Result := Copy(APath, 1, Length(APath) - 1)
  else
    Result := APath;
end;

function SettingsIncludeLeadingPathDelimiter(APath : TSettingName) : TSettingName;
begin
  if Copy(APath, 1, 1) <> SettingsPathDelimiter then
    Result := SettingsPathDelimiter + APath
  else
    Result := APath;
end;

procedure SettingsSplitPath(APath : String; AList : TStrings);
begin
  AList.Delimiter := SettingsPathDelimiter;
  AList.StrictDelimiter := true;
  AList.QuoteChar := #0;
  AList.DelimitedText := APath;
end;

procedure SettingsInitRegEx(const ARegEx : TRegExpr);
begin
  ARegEx.ModifierI := SettingsRegExCaseinsensitive;
  ARegEx.ModifierR := SettingsRegExRussianSupport;
  ARegEx.ModifierS := SettingsRegExDotMatchesLineSeperators;
  ARegEx.ModifierG := SettingsRegExAllOperatorsGreedy;
end;

function SettingsNameStringMatches(AName, APattern : TSettingName; AIsRegEx : Boolean) : Boolean;
var
  Reg : TRegExpr;
begin
  if AIsRegEx then
  begin
    if APattern = EmptyStr then
      raise Exception.Create('Empty pattern not allowed');

    Reg := TRegExpr.Create();
    try
      SettingsInitRegEx(Reg);
      Reg.InputString := AName;
      Reg.Expression := APattern;
      Result := Reg.Exec;
    finally
      Reg.Free;
    end;
  end
  else
    Result := SameText(AName, APattern);
end;

function SettingsCheckValueType(AVariant : Variant; ARaiseException : Boolean = true) : Boolean; 
begin
  Result:=VarIsType(AVariant, SettingsValidValueTypes);

  if (not Result) and ARaiseException then
    raise Exception.Create('Invalid value type "'+VarTypeAsText(VarType(AVariant))+'"');
end;


//==============================================================================


{ TSetting }

procedure TSetting.AddSettingToIndex(ASetting: TSetting);
var
  idx : Integer;
begin
  FIndex.AddObject(ASetting.CalculatePath, ASetting);

  for idx := 0 to ASetting.Children.Count - 1 do
    AddSettingToIndex(ASetting.Children[idx]);
end;

procedure TSetting.Assign(ASetting: TSetting);
var
  idx : Integer;
begin
  Clear;

  Name := ASetting.Name;
  Value := ASetting.Value;

  for idx := 0 to ASetting.Children.Count - 1 do
    TSetting.Create(Self).Assign(ASetting.Children[idx]);
end;

function TSetting.CalculatePath: TSettingName;
var
  Setting : TSetting;
  Path : TStringList;
begin
  Result := EmptyStr;

  Path := TStringList.Create;
  try
    SettingsSplitPath(EmptyStr, Path); //init list
    Path.Clear;

    Setting := Self;

    while Assigned(Setting) do
    begin
      Path.Insert(0, Setting.Name);

      Setting := Setting.Parent;
    end;

    Result := Path.DelimitedText; 
  finally
    Path.Free;
  end;
end;

procedure TSetting.Clear;
begin
  while FChildren.Count > 0 do
    FChildren.First.Free;

  VarClear(FValue);
end;

constructor TSetting.Create();
begin
  FParent := nil;
  FName := EmptyStr;
  VarClear(FValue);

  FChildren := TSettingList.Create;

  CreateIndex;
  InitIndex; 
end;

constructor TSetting.Create(AParent: TSetting);
begin
  Create;
  Parent := AParent;
end;

constructor TSetting.Create(AParent: TSetting; AName: TSettingName; ANameIsPath : Boolean);
begin
  if ANameIsPath then
    CreateFromPath(AParent, AName)
  else
  begin
    Create;
    Name := AName;
    Parent := AParent;
  end;
end;

constructor TSetting.Create(AParent: TSetting; AName: TSettingName;
  AValue: TSettingValue);
begin
  Create(AParent, AName);

  Value := AValue; 
end;

procedure TSetting.CreateFromPath(ARoot: TSetting; APath: TSettingName);
var
  Path : TStringList;
  Setting : TSetting;
  ChildIndex,
  PathIndex : Integer;
  SettingName : TSettingName;
begin
  Assert(Assigned(ARoot), 'CreateFromPath needs a valid root setting');

  Setting := ARoot;

  Path := TStringList.Create;
  try
    SettingsSplitPath(APath, Path);

    if not SettingsNameStringMatches(Path[0], ARoot.Name, false) then
      raise Exception.Create('Path doesnt match the root setting. A path usally starts with "/".');

    PathIndex := 1;

    SettingName := Path[PathIndex];

    while (PathIndex < Path.Count - 1) and (Assigned(Setting)) do
    begin
      ChildIndex := Setting.Children.IndexOfName(SettingName);

      if ChildIndex > -1 then
        Setting := Setting.Children[ChildIndex]
      else
        Setting := TSetting.Create(Setting, SettingName);

      Inc(PathIndex);
      SettingName := Path[PathIndex];
    end;
  finally
    Path.Free;
  end;

  Create(Setting, SettingName);
end;

procedure TSetting.CreateIndex;
begin
  FIndex := TStringList.Create;
  FIndex.Sorted := true;
  FIndex.Duplicates := dupIgnore;
end;

destructor TSetting.Destroy;
begin
  if Assigned(FParent) then
    FParent.UnregisterChild(Self);


  if Assigned(FChildren) then
  begin
    while FChildren.Count > 0 do
    begin
      FChildren.First.Free;
    end;

    FChildren.Free;
  end;


  if not Assigned(FParent) then //only destroy our own index
    DestroyIndex;

  inherited;
end;

procedure TSetting.DestroyIndex;
begin
  FIndex.Free;
  FIndex := nil;
end;

function TSetting.GetPath: TSettingName;
begin
  Result := Index[Index.IndexOfObject(Self)];
end;

procedure TSetting.InitIndex;
begin
  FIndex.Clear;
  AddSettingToIndex(Self);
end;

function TSetting.NameMatches(APattern: TSettingName;
  AIsRegEx: Boolean): Boolean;
begin
  Result := SettingsNameStringMatches(Name, APattern, AIsRegEx);
end;

procedure TSetting.RegisterChild(const AChild: TSetting);
begin
  if FChildren.IndexOf(AChild) = -1 then
    FChildren.Add(AChild);

  AddSettingToIndex(AChild);
end;

procedure TSetting.RemoveSettingFromIndex(ASetting: TSetting);
var
  idx : Integer;
begin
  idx := FIndex.IndexOfObject(ASetting);

  if idx > -1 then
    FIndex.Delete(idx);
end;

procedure TSetting.SetName(const Value: TSettingName);
var
  idx : Integer;
begin
  FName := Value;

  idx := FIndex.IndexOfObject(Self);
  if idx > -1 then
    FIndex.Delete(idx);
    
  FIndex.AddObject(CalculatePath, Self);
end;

procedure TSetting.SetParent(const Value: TSetting);
var
  OldParent, NewParent : TSetting;
begin
  if Value <> FParent then
  begin
    OldParent := FParent;
    NewParent := Value;

    FParent := Value;

    if Assigned(OldParent) then
      OldParent.UnregisterChild(Self);

    if Assigned(NewParent) then
      NewParent.RegisterChild(Self);

    if Assigned(OldParent) and (not Assigned(NewParent)) then
    begin
      CreateIndex;
      InitIndex;
    end
    else
    if (not Assigned(OldParent)) and Assigned(NewParent) then
    begin
      DestroyIndex;
      FIndex := NewParent.Index;
    end
    else 
    begin
      FIndex := NewParent.Index;
    end;
  end;
  
end;

procedure TSetting.SetValue(const Value: TSettingValue);
begin
  SettingsCheckValueType(Value);
  
  FValue := Value;
end;

procedure TSetting.UnregisterChild(const AChild: TSetting);
var
  Index : Integer;
begin
  RemoveSettingFromIndex(AChild);

  Index := FChildren.IndexOf(AChild);

  if Index > -1 then
    FChildren.Delete(Index);
end;


//==============================================================================


{ TSettingList }

function TSettingList.Add(ASetting: TSetting): Integer;
begin
  Result := inherited Add(ASetting);
end;

procedure TSettingList.AddSettings(ASettings: TSettingList);
var
  idx : Integer;
begin
  for idx := 0 to ASettings.Count - 1 do
  begin
    Add(ASettings[idx]);                                                    
  end;
end;

function TSettingList.ContainsPath(APath: TSettingName): Boolean;
begin
  Result := IndexOfPath(APath) > -1;
end;

function TSettingList.Extract(Item: TSetting): TSetting;
begin
  Result := TSetting(inherited Extract(Item));
end;

function TSettingList.First: TSetting;
begin
  Result := TSetting(inherited First);
end;

function TSettingList.GetItem(Index: Integer): TSetting;
begin
  Result := TSetting(inherited Get(Index));
end;

function TSettingList.IndexOf(ASetting: TSetting): Integer;
begin
  Result := inherited IndexOf(ASetting);
end;

function TSettingList.IndexOfName(AName: TSettingName): Integer;
var
  idx : Integer;
begin
  Result := -1;

  for idx := 0 to Count - 1 do
  begin
    if SettingsNameStringMatches(Items[idx].Name, AName, false) then
    begin
      Result := idx;
      break;
    end;
  end;
end;

function TSettingList.IndexOfPath(APath: TSettingName): Integer;
var
  idx : Integer;
begin
  Result := -1;

  for idx := 0 to Count - 1 do
  begin
    if SettingsNameStringMatches(Items[idx].GetPath, APath, false) then
    begin
      Result := idx;
      break;
    end;
  end;
end;

procedure TSettingList.Insert(Index: Integer; ASetting: TSetting);
begin
  inherited Insert(Index, ASetting);
end;

function TSettingList.Last: TSetting;
begin
  Result := TSetting(inherited Last);
end;

function TSettingList.Remove(ASetting: TSetting): Integer;
begin
  Result := inherited Remove(ASetting);
end;

procedure TSettingList.SetItem(Index: Integer; const Value: TSetting);
begin
  inherited Put(Index, Value);
end;


//==============================================================================


{ TCustomSettings }

procedure TCustomSettings.Assign(ASource: TPersistent);
begin
  if ASource is TCustomSettings then
    FRootSetting.Assign(TCustomSettings(ASource).FRootSetting)
  else
    inherited;
end;

procedure TCustomSettings.Clear;
begin
  FRootSetting.Clear;
end;

constructor TCustomSettings.Create(AOwner: TComponent);
begin
  inherited;

  FComponentLinks := TCustomSettingsLinkList.Create;

  FParentMode := pmAddsMissing;
  FParentSettings := nil;
  FAutoSaveOnDestroy := false;

  FRootSetting := TSetting.Create();
end;

procedure TCustomSettings.Delete(APath: TSettingName; AIsRegEx: Boolean);
var
  Setts : TSettingList;
  idx : Integer;
begin
  Setts := TSettingList.Create;
  try
    QuerySettings(APath, AIsRegEx, Setts, false);

    for idx := 0 to Setts.Count - 1 do
    begin
      if Setts[idx] <> FRootSetting then
        Setts[idx].Free;
    end;
  finally
    Setts.Free;
  end;
end;

destructor TCustomSettings.Destroy;
begin
  try
    if FAutoSaveOnDestroy then
      Save;
  finally
    FComponentLinks.Free;

    FRootSetting.Free;

    inherited;
  end;
end;

function TCustomSettings.Exists(APath: TSettingName;
  AIsRegEx: Boolean): Boolean;
var
  Setts : TSettingList;
begin
  Setts := TSettingList.Create;
  try
    QuerySettings(APath, AIsRegEx, Setts, true);
    Result := Setts.Count > 0;
  finally
    Setts.Free;
  end;
end;

function TCustomSettings.GetSubNames(APath: TSettingName; AIsRegExPath,
  AFullPathNames: Boolean): TSettingNames;
var
  Setts : TSettingList;
  idx : Integer;
begin
  Setts := TSettingList.Create;
  try
    QuerySettings(APath, AIsRegExPath, Setts, true);

    for idx := Setts.Count - 1 downto 0 do
    begin
      Setts.AddSettings(Setts[idx].Children);
      Setts.Delete(idx);
    end;

    SetLength(Result, Setts.Count);

    for idx := 0 to Setts.Count - 1 do
    begin
      if AFullPathNames then
        Result[idx] := Setts[idx].GetPath
      else
        Result[idx] := Setts[idx].Name
    end;

  finally
    Setts.Free;
  end;
end;

function TCustomSettings.GetSubNameValues(APath: TSettingName; AIsRegExPath,
  AGetNames, AFullPathNames, AGetValues: Boolean): TSettingNameValues;
var
  Setts : TSettingList;
  idx : Integer;
begin
  Setts := TSettingList.Create;
  try
    QuerySettings(APath, AIsRegExPath, Setts, true);

    for idx := Setts.Count - 1 downto 0 do
    begin
      Setts.AddSettings(Setts[idx].Children);
      Setts.Delete(idx);
    end;

    SetLength(Result, Setts.Count);

    for idx := 0 to Setts.Count - 1 do
    begin
      if AGetValues then
        Result[idx].Value := Setts[idx].Value
      else
        VarClear(Result[idx].Value);

      if AGetNames then
      begin
        if AFullPathNames then
          Result[idx].Name := Setts[idx].GetPath
        else
          Result[idx].Name := Setts[idx].Name
      end
      else
        Result[idx].Name := EmptyStr;
    end;

  finally
    Setts.Free;
  end;
end;

function TCustomSettings.GetNames(APath: TSettingName;
  AIsRegExPath: Boolean; AFullPathNames : Boolean): TSettingNames;
var
  Setts : TSettingList;
  idx : Integer;
begin
  Setts := TSettingList.Create;
  try
    QuerySettings(APath, AIsRegExPath, Setts, true);

    SetLength(Result, Setts.Count);

    for idx := 0 to Setts.Count - 1 do
    begin
      if AFullPathNames then
        Result[idx] := Setts[idx].GetPath
      else
        Result[idx] := Setts[idx].Name;
    end;
  finally
    Setts.Free;
  end;
end;

function TCustomSettings.GetNameValue(APath: TSettingName; ADefault: Variant;
  AIsRegEx, AGetName, AFullPathName, AGetValues: Boolean): TSettingNameValue;
var
  NameValues : TSettingNameValues;
begin
  NameValues := GetNameValues(APath, ADefault, AIsRegEx, AGetName, AFullPathName, AGetValues);

  if Length(NameValues) = 0 then
  begin
    Result.Name := EmptyStr;
    Result.Value := ADefault;
  end
  else
    Result := NameValues[0]; 
end;

function TCustomSettings.GetNameValues(APath: TSettingName; ADefault: Variant;
  AIsRegExPath, AGetNames, AFullPathNames,
  AGetValues: Boolean): TSettingNameValues;
var
  Setts : TSettingList;
  idx : Integer;
begin
  Setts := TSettingList.Create;
  try
    QuerySettings(APath, AIsRegExPath, Setts, true);

    SetLength(Result, Setts.Count);

    for idx := 0 to Setts.Count - 1 do
    begin
      if AGetValues then
      begin
        if VarIsEmpty(Setts[idx].Value) then
          Result[idx].Value := ADefault
        else
          Result[idx].Value := Setts[idx].Value
      end
      else
        VarClear(Result[idx].Value);

      if AGetNames then
      begin
        if AFullPathNames then
          Result[idx].Name := Setts[idx].GetPath
        else
          Result[idx].Name := Setts[idx].Name
      end
      else
        Result[idx].Name := EmptyStr;
    end;

  finally
    Setts.Free;
  end;
end;

procedure TCustomSettings.ReadObject(APath: TSettingName; const AObject: TObject;
  ARecursive, ACreateIfMssing, AIsRegExPath: Boolean);
var
  APropertyNames : TStringList;
  idx : Integer;
  v : Variant;
  Setts : TSettingList;
begin
  APropertyNames := TStringList.Create;
  Setts := TSettingList.Create;
  try
    rttihGetPropertiesList(AObject,
                           APropertyNames,
                           ARecursive,
                           [],
                           SettingsExcludeObjectPropertyTypeKinds,
                           true,
                           '',
                           SettingsPathDelimiter);

    for idx := 0 to APropertyNames.Count - 1 do
    begin
      Setts.Clear;

      QuerySettings(APath + SettingsPathDelimiter + APropertyNames[idx], AIsRegExPath, Setts, true);

      if (Setts.Count > 0) then
      begin
        v := Setts.First.Value;

        if not VarIsEmpty(v) then
          rttihSetPropertyValue(AObject,
                                APropertyNames[idx],
                                v,
                                SettingsPathDelimiter);
      end;
    end;
  finally
    APropertyNames.Free;
    Setts.Free;
  end;
end;

function TCustomSettings.GetValue(APath: TSettingName; ADefault: Variant;
  AIsRegExPath: Boolean): TSettingValue;
var
  vals : TSettingValues;
begin
  vals := GetValues(APath, ADefault, AIsRegExPath);

  if Length(vals) > 0 then
    Result := vals[0]
  else
    Result := ADefault;
end;

function TCustomSettings.GetValues(APath: TSettingName; ADefault: Variant;
  AIsRegExPath: Boolean): TSettingValues;
var
  Setts : TSettingList;
  idx : Integer;
begin
  Setts := TSettingList.Create;
  try
    QuerySettings(APath, AIsRegExPath, Setts, true);

    SetLength(Result, Setts.Count);
    for idx := 0 to Setts.Count - 1 do
    begin
      if VarIsEmpty(Setts[idx].Value) then
        Result[idx] := ADefault
      else
        Result[idx] := Setts[idx].Value;
    end;

  finally
    Setts.Free;
  end;
end;

procedure TCustomSettings.InformComponentLinksAboutLoad;
var
  idx : Integer;
begin
  for idx := 0 to FComponentLinks.Count - 1 do
    FComponentLinks[idx].ApplySettings;

end;

procedure TCustomSettings.InformComponentLinksAboutSave;
var
  idx : Integer;
begin
  for idx := 0 to FComponentLinks.Count - 1 do
    FComponentLinks[idx].SaveSettings;
end;

function TCustomSettings.Load: Boolean;
begin
  FRootSetting.Clear;
  
  Result := DoLoad;

  if Result then
    InformComponentLinksAboutLoad;
end;

function TCustomSettings.Move(ANewParent : TSettingName;
                                APath : TSettingName;
                                AIsRegExPath : Boolean = false;
                                AFullPathNames : Boolean = false) : TSettingNames;
var
  Setts,
  NewParent : TSettingList;
  idx : Integer;
begin
  Setts := TSettingList.Create;
  NewParent := TSettingList.Create;
  try
    QuerySettings(ANewParent, false, NewParent, true);
    
    if NewParent.Count = 0 then
      NewParent.Add(TSetting.Create(FRootSetting, ANewParent, true));

    QuerySettings(APath, AIsRegExPath, Setts, true);

    SetLength(Result, Setts.Count);

    for idx := 0 to Setts.Count - 1 do
    begin
      Setts[idx].Parent := NewParent.First;

      if AFullPathNames then
        Result[idx] := Setts[idx].GetPath
      else
        Result[idx] := Setts[idx].Name;
    end;
  finally
    Setts.Free;
    NewParent.Free;
  end;
end;

procedure TCustomSettings.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if Operation = opRemove then
  begin
    if AComponent = FParentSettings then
      FParentSettings := nil;
  end;
end;

procedure TCustomSettings.QuerySettings(const APath: TSettingName;
  const AIsRegEx: Boolean; const AList: TSettingList; const AUseParent : Boolean);
var
  idx : Integer;
  Setting : TSetting;
begin
  if AUseParent and
     (FParentMode = pmOverridesAll) and
     Assigned(FParentSettings) then
    FParentSettings.QuerySettings(APath, AIsRegEx, AList, AUseParent);

  if AIsRegEx then
  begin
    for idx := 0 to FRootSetting.Index.Count - 1 do
    begin
      Setting := TSetting(FRootSetting.Index.Objects[idx]);

      if SettingsNameStringMatches(FRootSetting.Index[idx], APath, AIsRegEx) and
         (not AList.ContainsPath(Setting.GetPath))then
        AList.Add(Setting);
    end;
  end
  else
  begin
    idx := FRootSetting.Index.IndexOf(APath);
    if idx > -1 then
      AList.Add(TSetting(FRootSetting.Index.Objects[idx]));
  end;

  if AUseParent and
     (FParentMode = pmAddsMissing) and
     Assigned(FParentSettings) then
    FParentSettings.QuerySettings(APath, AIsRegEx, AList, AUseParent);

end;

procedure TCustomSettings.RegisterComponentLink(
  ALink: TCustomSettingsLink);
begin
  if FComponentLinks.IndexOf(ALink) = -1 then
    FComponentLinks.Add(ALink);
end;

function TCustomSettings.Save: Boolean;
begin
  InformComponentLinksAboutSave;

  Result := DoSave;
end;

procedure TCustomSettings.WriteObject(APath: TSettingName; const AObject: TObject;
  ARecursive, ACreateIfMssing, AIsRegExPath: Boolean);
var
  APropertyNames : TStringList;
  idx : Integer;
  v : Variant;
begin
  APropertyNames := TStringList.Create;
  try
    rttihGetPropertiesList(AObject,
                           APropertyNames,
                           ARecursive,
                           [],
                           SettingsExcludeObjectPropertyTypeKinds,
                           true,
                           '',
                           SettingsPathDelimiter);

    for idx := 0 to APropertyNames.Count - 1 do
    begin
      v := rttihGetPropertyValue(AObject, APropertyNames[idx], SettingsPathDelimiter);

      SetValue(APath + SettingsPathDelimiter + APropertyNames[idx],
               v,
               ACreateIfMssing,
               AIsRegExPath);
    end;
  finally
    APropertyNames.Free;
  end;
end;

procedure TCustomSettings.SetParentSettings(const Value: TCustomSettings);
begin
  if (Value <> Self) and (Value <> FParentSettings) then
  begin
    if Assigned(FParentSettings) then
      FParentSettings.RemoveFreeNotification(Self);

    FParentSettings := Value;

    if Assigned(FParentSettings) then
      FParentSettings.FreeNotification(Self);
  end;
end;

procedure TCustomSettings.SetValue(APath: TSettingName; AValue: Variant;
  ACreateIfMissing : Boolean; AIsRegExPath: Boolean);
var
  Setts : TSettingList;
  idx : Integer;
begin
  Assert(not (ACreateIfMissing and AIsRegExPath), 'Can not create non-existing settings if regular expression is supplied');

  Setts := TSettingList.Create;
  try
    QuerySettings(APath, AIsRegExPath, Setts, false);

    if (Setts.Count = 0) and ACreateIfMissing then
      Setts.Add(TSetting.Create(FRootSetting, APath, true));

    for idx := 0 to Setts.Count - 1 do
      Setts[idx].Value := AValue;
  finally
    Setts.Free;
  end;
end;


procedure TCustomSettings.UnRegisterComponentLink(
  ALink: TCustomSettingsLink);
var
  idx : Integer;
begin
  idx := FComponentLinks.IndexOf(ALink);

  if idx > - 1 then
    FComponentLinks.Delete(idx);
end;

//==============================================================================


{ TCustomSettingsLink }

procedure TCustomSettingsLink.ApplySettings;
var
  RootSetting : TSettingName;
begin
  if (not (csDesigning in Self.ComponentState)) and Active then
  begin
    if Assigned(FOnBeforeApplySettings) then
      FOnBeforeApplySettings(Self);

    if Assigned(FOnNeedRootSetting) then
      FOnNeedRootSetting(Self, RootSetting)
    else
      RootSetting := DefaultRootSetting;

    DoApplySettings(SettingsExcludeTrailingPathDelimiter(SettingsIncludeLeadingPathDelimiter(RootSetting)));

    if Assigned(FOnAfterApplySettings) then
      FOnAfterApplySettings(Self);
  end;
end;

constructor TCustomSettingsLink.Create(AOwner: TComponent);
begin
  inherited;

  FSettings := nil;
  FActive := true;
end;

destructor TCustomSettingsLink.Destroy;
begin
  Settings := nil;

  inherited;
end;

procedure TCustomSettingsLink.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if Operation = opRemove then
  begin
    if AComponent = FSettings then
      FSettings := nil;
  end;
end;

procedure TCustomSettingsLink.SaveSettings;
var
  RootSetting : TSettingName;
begin
  if (not (csDesigning in Self.ComponentState)) and Active then
  begin
    if Assigned(FOnBeforeSaveSettings) then
      FOnBeforeSaveSettings(Self);

    if Assigned(FOnNeedRootSetting) then
      FOnNeedRootSetting(Self, RootSetting)
    else
      RootSetting := DefaultRootSetting;

    DoSaveSettings(SettingsExcludeTrailingPathDelimiter(SettingsIncludeLeadingPathDelimiter(RootSetting)));

    if Assigned(FOnAfterSaveSettings) then
      FOnAfterSaveSettings(Self)
  end;
end;

procedure TCustomSettingsLink.SetActive(const Value: Boolean);
begin
  if Value <> FActive then
  begin
    FActive := Value;

    ApplySettings;
  end;
end;

procedure TCustomSettingsLink.SetRootSetting(
  const Value: TSettingName);
begin
  if Value <> EmptyStr then
    FDefaultRootSetting := SettingsIncludeLeadingPathDelimiter(Value)
  else
    FDefaultRootSetting := Value;

  FDefaultRootSetting := SettingsExcludeTrailingPathDelimiter(FDefaultRootSetting);
end;

procedure TCustomSettingsLink.SetSettings(
  const Value: TCustomSettings);
begin
  if FSettings <> Value then
  begin
    if Assigned(FSettings) then
    begin
      FSettings.UnRegisterComponentLink(Self);
      FSettings.RemoveFreeNotification(Self);
    end;

    FSettings := Value;

    if Assigned(FSettings) then
    begin
      FSettings.RegisterComponentLink(Self);
      FSettings.FreeNotification(Self);
    end;

    ApplySettings;
  end;
end;


//==============================================================================


{ TCustomSettingsLinkList }

function TCustomSettingsLinkList.Add(
  ALink: TCustomSettingsLink): Integer;
begin
  Result := inherited Add(ALink);
end;

function TCustomSettingsLinkList.Extract(
  Item: TCustomSettingsLink): TCustomSettingsLink;
begin
  Result := inherited Extract(Item);
end;

function TCustomSettingsLinkList.First: TCustomSettingsLink;
begin
  Result := inherited First;
end;

function TCustomSettingsLinkList.GetItem(
  Index: Integer): TCustomSettingsLink;
begin
  Result := inherited Get(Index);
end;

function TCustomSettingsLinkList.IndexOf(
  ALink: TCustomSettingsLink): Integer;
begin
  Result := inherited IndexOf(ALink);
end;

procedure TCustomSettingsLinkList.Insert(Index: Integer;
  ALink: TCustomSettingsLink);
begin
  inherited Insert(Index, ALink);
end;

function TCustomSettingsLinkList.Last: TCustomSettingsLink;
begin
  Result := inherited Last;
end;

function TCustomSettingsLinkList.Remove(
  ALink: TCustomSettingsLink): Integer;
begin
  Result := inherited Remove(ALink);
end;

procedure TCustomSettingsLinkList.SetItem(Index: Integer;
  const Value: TCustomSettingsLink);
begin
  inherited Put(Index, Value);
end;

{ TCustomSettingsComponentLink }

constructor TCustomSettingsComponentLink.Create(AOwner: TComponent);
begin
  inherited;

  FSaveProperties := TsrPropertyList.Create(Self);

  FComponent := nil;
end;

destructor TCustomSettingsComponentLink.Destroy;
begin
  SetComponent(nil);

  FSaveProperties.Free;

  inherited;
end;

procedure TCustomSettingsComponentLink.DoApplySettings(
  const ARootSetting: TSettingName);
var
  idx : Integer;
  PropValue : Variant;
begin
  if Assigned(FComponent) and Assigned(FSettings) then
  begin
    for idx := 0 to FSaveProperties.Count - 1 do
    begin
      PropValue := FSettings.GetValue(ARootSetting +
                                      SettingsPathDelimiter +
                                      FSaveProperties[idx],
                                      rttihGetPropertyValue(FComponent, FSaveProperties[idx], SettingsPathDelimiter));
      rttihSetPropertyValue(FComponent, FSaveProperties[idx], PropValue, SettingsPathDelimiter);
    end;
  end;

end;

procedure TCustomSettingsComponentLink.DoSaveSettings(
  const ARootSetting: TSettingName);
var
  idx : Integer;
begin
  if Assigned(FComponent) and Assigned(FSettings) then
  begin
    for idx := 0 to FSaveProperties.Count - 1 do
    begin
        FSettings.SetValue(ARootSetting +
                           SettingsPathDelimiter +
                           FSaveProperties[idx], rttihGetPropertyValue(FComponent, FSaveProperties[idx], SettingsPathDelimiter));
    end;
  end;

end;

function TCustomSettingsComponentLink.GenerateRootSettingName(
  AComponent: TComponent): TSettingName;
var
  Path : TStringList;
  Compo : TComponent;
begin
  Compo := AComponent;

  Path := TStringList.Create;
  try
    SettingsSplitPath(EmptyStr, Path); //Init the list;

    while Assigned(Compo) do
    begin
      Path.Insert(0, Compo.Name);

      if Compo = AComponent.Owner then
        break;

      Compo := Compo.GetParentComponent;
    end;

    Path.Insert(0, 'GUI');
    Path.Insert(0, EmptyStr);

    Result := Path.DelimitedText;
  finally
    Path.Free;
  end;
end;

procedure TCustomSettingsComponentLink.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;

  if Operation = opRemove then
  begin
    if AComponent = FComponent then
      FComponent := nil
  end;
end;

procedure TCustomSettingsComponentLink.SetComponent(const Value: TComponent);
begin
  if (FComponent <> Value) and (Value <> Self) then
  begin
    if Assigned(FComponent) then
      FComponent.RemoveFreeNotification(Self);

    FComponent := Value;

    if Assigned(FComponent) then
      FComponent.FreeNotification(Self);

    if SameText(FDefaultRootSetting, EmptyStr) then
      DefaultRootSetting := GenerateRootSettingName(FComponent);

    if Assigned(FComponent) then
      FSaveProperties.ReadPropertiesFromObject(FComponent, True);

    ApplySettings;
  end;
end;



end.
