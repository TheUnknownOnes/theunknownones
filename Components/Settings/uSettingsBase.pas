{-----------------------------------------------------------------------------
 Project: Settings
 Purpose: Contains the base classes for working with Settings 
 Created: 21.05.2008 14:40:48
 
 (c) by TheUnknownOnes
 see http://www.TheUnknownOnes.net
-----------------------------------------------------------------------------}
unit uSettingsBase;

interface

{$DEFINE UniCode}

uses
  Classes,
  Contnrs,
  Sysutils,
  Variants,
  WideStrings,
  RegExpr,
  TypInfo,
  Windows, Dialogs;

type

  TSetting = class;

  TCustomSettingsLink = class;
  TCustomSettingsLinkList = class;

  TSettingName = WideString;

  TSettingValue = Variant;

  TSettingValues = array of Variant;

  TSettingNameValue = record
    Name : TSettingName;
    Value : TSettingValue;
  end;

  TSettingNameValues = array of TSettingNameValue;


//==============================================================================


  TSettingsComponentProperty = record
    Name : TSettingName;
    Save : Boolean;
  end;
  PSettingsComponentProperty = ^TSettingsComponentProperty;


//==============================================================================


  TSettingsComponentPropertyList = class(TList)
  private
    function Get(Index: Integer): PSettingsComponentProperty;
    procedure Put(Index: Integer; const Value: PSettingsComponentProperty);

  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;

    procedure AddPropertiesFromObject(AObject : TObject;
                                      AParentPropertyPath : TSettingName);

    function GetPropertyFromObject(AObject : TObject; AIndex : Integer) : PPropInfo;
  public
    function Add(Item: PSettingsComponentProperty): Integer;
    function Extract(Item: PSettingsComponentProperty): PSettingsComponentProperty;
    function First: PSettingsComponentProperty;
    function IndexOf(Item: PSettingsComponentProperty): Integer; overload;
    function IndexOf(AName : TSettingName): Integer; overload;
    procedure Insert(Index: Integer; Item: PSettingsComponentProperty);
    function Last: PSettingsComponentProperty;
    function Remove(Item: PSettingsComponentProperty): Integer;
    property Items[Index: Integer]: PSettingsComponentProperty read Get write Put; default;

    procedure ReadFromComponent(AComponent : TComponent);

    procedure CopyFrom(AList : TSettingsComponentPropertyList);

    function GetPropertiesToSaveCount : Integer;

    procedure Load(AReader: TReader);
    procedure Save(AWriter: TWriter);
  end;


//==============================================================================


  //Just a list for holding settings (for internal use)
  TSettingList = class(TList)
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
  protected
    FParent : TSetting;
    FName : TSettingName;
    FValue : TSettingValue;

    FChildren : TSettingList;

    FIndex : TWideStringList;

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
    property Index : TWideStringList read FIndex;
  end;


//==============================================================================


  TParentMode = (pmDontUse,
                 pmAddsMissing,
                 pmOverridesAll);


//==============================================================================


  TCustomSettings = class(TComponent)
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

    function GetValues(APath : TSettingName;
                       AIsRegExPath : Boolean = true) : TSettingValues; overload;

    function GetValues(APath : TSettingName;
                       ADefault : Variant;
                       AIsRegExPath : Boolean = true) : TSettingValues; overload;

    function GetValue(APath : TSettingName;
                      AIsRegExPath : Boolean = false) : TSettingValue; overload;

    function GetValue(APath : TSettingName;
                      ADefault : Variant;
                      AIsRegExPath : Boolean = false) : TSettingValue; overload;

    function GetNameValues(APath : TSettingName;
                           AIsRegExPath : Boolean = true;
                           AGetNames : Boolean = true;
                           AFullPathNames : Boolean = false;
                           AGetValues : Boolean = true) : TSettingNameValues; overload;

    function GetNameValues(APath : TSettingName;
                           ADefault : Variant;
                           AIsRegExPath : Boolean = true;
                           AGetNames : Boolean = true;
                           AFullPathNames : Boolean = false;
                           AGetValues : Boolean = true) : TSettingNameValues; overload;

    function GetNameValue(APath : TSettingName;
                          AIsRegEx : Boolean = false;
                          AGetName : Boolean = true;
                          AFullPathName : Boolean = false;
                          AGetValues : Boolean = true) : TSettingNameValue; overload;

    function GetNameValue(APath : TSettingName;
                          ADefault : Variant;
                          AIsRegEx : Boolean = false;
                          AGetName : Boolean = true;
                          AFullPathName : Boolean = false;
                          AGetValues : Boolean = true) : TSettingNameValue; overload;

    function GetSubNameValues(APath : TSettingName;
                              AIsRegExPath : Boolean = false;
                              AGetNames : Boolean = true;
                              AFullPathNames : Boolean = false;
                              AGetValues : Boolean = true) : TSettingNameValues;

    function Exists(APath : TSettingName; AIsRegEx : Boolean = false) : Boolean;
    procedure Delete(APath : TSettingName; AIsRegEx : Boolean = false);

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
  protected
    FSettings: TCustomSettings;
    FDefaultRootSetting: TSettingName;

    FOnNeedRootSetting: TNeedRootSettingProc;

    procedure SetSettings(const Value: TCustomSettings);
    procedure SetRootSetting(const Value: TSettingName);

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure DoApplySettings(const ARootSetting : TSettingName); virtual; abstract;
    procedure DoSaveSettings(const ARootSetting : TSettingName); virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ApplySettings;
    procedure SaveSettings;

    property Settings : TCustomSettings read FSettings write SetSettings;
    property DefaultRootSetting : TSettingName read FDefaultRootSetting write SetRootSetting;

    property OnNeedRootSetting : TNeedRootSettingProc read FOnNeedRootSetting write FOnNeedRootSetting;
  end;


//==============================================================================


  TCustomSettingsComponentLink = class(TCustomSettingsLink)
  protected
    FComponent: TComponent;

    FSaveProperties: TSettingsComponentPropertyList;

    procedure SetComponent(const Value: TComponent);

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    function GenerateRootSettingName(AComponent : TComponent) : TSettingName; virtual;

    procedure DefineProperties(Filer: TFiler); override;

    procedure DoApplySettings(const ARootSetting : TSettingName); override; 
    procedure DoSaveSettings(const ARootSetting : TSettingName); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Component : TComponent read FComponent write SetComponent;
    property SaveProperties : TSettingsComponentPropertyList read FSaveProperties write FSaveProperties;
  end;


//==============================================================================


const
  SettingsPathDelimiter = '/';
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
  SettingsComponentGroup = 'Settings';                                                        

var
  SettingsRegExCaseinsensitive : Boolean = true;
  SettingsRegExRussianSupport : Boolean = false;
  SettingsRegExDotMatchesLineSeperators : Boolean = true;
  SettingsRegExAllOperatorsGreedy : Boolean = true;

implementation


{ Helper }

function ExcludeTrailingSettingsPathDelimiter(APath : TSettingName) : TSettingName;
begin
  if Copy(APath, Length(APath), 1) = SettingsPathDelimiter then
    Result := Copy(APath, 1, Length(APath) - 1)
  else
    Result := APath;
end;

function IncludeLeadingSettingsPathDelimiter(APath : TSettingName) : TSettingName;
begin
  if Copy(APath, 1, 1) <> SettingsPathDelimiter then
    Result := SettingsPathDelimiter + APath
  else
    Result := APath;
end;

procedure SplitPath(APath : WideString; AList : TWideStrings);
begin
  AList.Delimiter := SettingsPathDelimiter;
  AList.StrictDelimiter := true;
  AList.QuoteChar := #0;
  AList.DelimitedText := APath;
end;

procedure InitRegEx(const ARegEx : TRegExpr);
begin
  ARegEx.ModifierI := SettingsRegExCaseinsensitive;
  ARegEx.ModifierR := SettingsRegExRussianSupport;
  ARegEx.ModifierS := SettingsRegExDotMatchesLineSeperators;
  ARegEx.ModifierG := SettingsRegExAllOperatorsGreedy;
end;

function NameStringMatches(AName, APattern : TSettingName; AIsRegEx : Boolean) : Boolean;
var
  Reg : TRegExpr;
begin
  if AIsRegEx then
  begin
    if APattern = EmptyWideStr then
      raise Exception.Create('Empty pattern not allowed');

    Reg := TRegExpr.Create();
    try
      InitRegEx(Reg);
      Reg.InputString := AName;
      Reg.Expression := APattern;
      Result := Reg.Exec;
    finally
      Reg.Free;
    end;
  end
  else
    Result := WideSameText(AName, APattern);
end;

function CheckSettingsValueType(AVariant : Variant; ARaiseException : Boolean = true) : Boolean; 
begin
  Result:=VarIsType(AVariant, SettingsValidValueTypes);

  if (not Result) and ARaiseException then
    raise Exception.Create('Invalid value type');
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
  Path : TWideStringList;
begin
  Result := EmptyWideStr;

  Path := TWideStringList.Create;
  try
    SplitPath(EmptyWideStr, Path); //init list
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

constructor TSetting.Create;
begin
  FParent := nil;
  FName := EmptyWideStr;
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
    Create(AParent);

    Name := AName;
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
  Path : TWideStringList;
  Setting : TSetting;
  ChildIndex,
  PathIndex : Integer;
  SettingName : TSettingName;
begin
  Assert(Assigned(ARoot), 'CreateFromPath needs a valid root setting');

  Setting := ARoot;

  Path := TWideStringList.Create;
  try
    SplitPath(APath, Path);

    if not NameStringMatches(Path[0], ARoot.Name, false) then
      raise Exception.Create('Path doesnt match the root setting');

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
  FIndex := TWideStringList.Create;
  FIndex.Sorted := true;
  FIndex.Duplicates := dupError;
end;

destructor TSetting.Destroy;
begin
  if Assigned(FParent) then
    FParent.UnregisterChild(Self);

  while FChildren.Count > 0 do
  begin
    FChildren.First.Free;
  end;

  FChildren.Free;

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
  Result := NameStringMatches(Name, APattern, AIsRegEx);
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
begin
  FName := Value;

  FIndex.Delete(FIndex.IndexOfObject(Self));
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
  CheckSettingsValueType(Value);
  
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
    if NameStringMatches(Items[idx].Name, AName, false) then
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
    if NameStringMatches(Items[idx].GetPath, APath, false) then
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
      Setts[idx].Free;
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
        Result[idx].Name := EmptyWideStr;
    end;

  finally
    Setts.Free;
  end;
end;

function TCustomSettings.GetNameValue(APath: TSettingName; AIsRegEx, AGetName,
  AFullPathName, AGetValues: Boolean): TSettingNameValue;
var
  NameValues : TSettingNameValues;
begin
  NameValues := GetNameValues(APath, AIsRegEx, AGetName, AFullPathName, AGetValues);

  if Length(NameValues) > 0 then
    Result := NameValues[0];
end;

function TCustomSettings.GetNameValue(APath: TSettingName; ADefault: Variant;
  AIsRegEx, AGetName, AFullPathName, AGetValues: Boolean): TSettingNameValue;
begin
  Result := GetNameValue(APath, AIsRegEx, AGetName, AFullPathName, AGetValues);

  if AGetValues and VarIsEmpty(Result.Value) then
    Result.Value := ADefault;
end;

function TCustomSettings.GetNameValues(APath: TSettingName; ADefault: Variant;
  AIsRegExPath, AGetNames, AFullPathNames,
  AGetValues: Boolean): TSettingNameValues;
var
  idx : Integer;
begin
  Result := GetNameValues(APath, AIsRegExPath, AGetNames, AFullPathNames, AGetValues);

  if AGetValues then
  begin
    for idx := Low(Result) to High(Result) do
    begin
      if VarIsEmpty(Result[idx].Value) then
        Result[idx].Value := ADefault;
    end;
  end;
end;

function TCustomSettings.GetNameValues(APath : TSettingName;
                                       AIsRegExPath : Boolean;
                                       AGetNames : Boolean;
                                       AFullPathNames : Boolean;
                                       AGetValues : Boolean): TSettingNameValues;
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
        Result[idx].Name := EmptyWideStr;
    end;

  finally
    Setts.Free;
  end;
end;

  
function TCustomSettings.GetValue(APath: TSettingName;
  AIsRegExPath: Boolean): TSettingValue;
var
  vals : TSettingValues;
begin
  vals := GetValues(APath, AIsRegExPath);
  if Length(vals) > 0 then
    Result := vals[0]
  else
    VarClear(Result);
end;

function TCustomSettings.GetValue(APath: TSettingName; ADefault: Variant;
  AIsRegExPath: Boolean): TSettingValue;
begin
  Result := GetValue(APath, AIsRegExPath);

  if VarIsEmpty(Result) then
    Result := ADefault;
end;

function TCustomSettings.GetValues(APath: TSettingName; ADefault: Variant;
  AIsRegExPath: Boolean): TSettingValues;
var
  idx : Integer;
begin
  Result := GetValues(APath, AIsRegExPath);

  for idx := Low(Result) to High(Result) do
  begin
    if VarIsEmpty(Result[idx]) then
      Result[idx] := ADefault;
  end;

end;

function TCustomSettings.GetValues(APath: TSettingName;
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
      Result[idx] := Setts[idx].Value;

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

      if NameStringMatches(FRootSetting.Index[idx], APath, AIsRegEx) and
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
  if not (csDesigning in Self.ComponentState) then
  begin
    if Assigned(FOnNeedRootSetting) then
      FOnNeedRootSetting(Self, RootSetting)
    else
      RootSetting := DefaultRootSetting;

    DoApplySettings(ExcludeTrailingSettingsPathDelimiter(IncludeLeadingSettingsPathDelimiter(RootSetting)));
  end;
end;

constructor TCustomSettingsLink.Create(AOwner: TComponent);
begin
  inherited;

  FSettings := nil;
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
  if not (csDesigning in Self.ComponentState) then
  begin
    if Assigned(FOnNeedRootSetting) then
      FOnNeedRootSetting(Self, RootSetting)
    else
      RootSetting := DefaultRootSetting;

    DoSaveSettings(ExcludeTrailingSettingsPathDelimiter(IncludeLeadingSettingsPathDelimiter(RootSetting)));
  end;
end;

procedure TCustomSettingsLink.SetRootSetting(
  const Value: TSettingName);
begin
  if Value <> EmptyStr then
    FDefaultRootSetting := IncludeLeadingSettingsPathDelimiter(Value)
  else
    FDefaultRootSetting := Value;

  FDefaultRootSetting := ExcludeTrailingSettingsPathDelimiter(FDefaultRootSetting);
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

  FSaveProperties := TSettingsComponentPropertyList.Create;

  FComponent := nil;
end;

procedure TCustomSettingsComponentLink.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);

  Filer.DefineProperty('FSaveProperties', FSaveProperties.Load, FSaveProperties.Save, FSaveProperties.Count > 0);
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
  SCP : TSettingsComponentProperty;
  PropValue : Variant;
  Prop : PPropInfo;
begin
  if Assigned(FComponent) and Assigned(FSettings) then
  begin
    for idx := 0 to FSaveProperties.Count - 1 do
    begin
      SCP := FSaveProperties[idx]^;

      if SCP.Save then
      begin
        Prop := FSaveProperties.GetPropertyFromObject(FComponent, idx);

        if Assigned(Prop) then
        begin
          PropValue := FSettings.GetValue(ARootSetting + SCP.Name);
          if not VarIsEmpty(PropValue) then
            SetPropValue(FComponent, Prop, PropValue);
        end;
      end;
    end;
  end;

end;

procedure TCustomSettingsComponentLink.DoSaveSettings(
  const ARootSetting: TSettingName);
var
  idx : Integer;
  SCP : TSettingsComponentProperty;
  PropValue : Variant;
  Prop : PPropInfo;
begin
  if Assigned(FComponent) and Assigned(FSettings) then
  begin
    for idx := 0 to FSaveProperties.Count - 1 do
    begin
      SCP := FSaveProperties[idx]^;

      if SCP.Save then
      begin
        Prop := FSaveProperties.GetPropertyFromObject(FComponent, idx);

        if Assigned(Prop) then
        begin
          PropValue := GetPropValue(FComponent, Prop, false);

          FSettings.SetValue(ARootSetting + SCP.Name, PropValue);
        end;
      end;
    end;
  end;

end;

function TCustomSettingsComponentLink.GenerateRootSettingName(
  AComponent: TComponent): TSettingName;
var
  Path : TWideStringList;
  Compo : TComponent;
begin
  Compo := AComponent;

  Path := TWideStringList.Create;
  try
    SplitPath(EmptyWideStr, Path); //Init the list;

    while Assigned(Compo) do
    begin
      Path.Insert(0, Compo.Name);

      if Compo = AComponent.Owner then
      begin
        Path.Insert(0, EmptyWideStr);
        break;
      end;

      Compo := Compo.GetParentComponent;
    end;

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

    if WideSameText(FDefaultRootSetting, EmptyWideStr) then
      DefaultRootSetting := GenerateRootSettingName(FComponent);

    if Assigned(FComponent) then
      FSaveProperties.ReadFromComponent(FComponent);

    ApplySettings;
  end;
end;

{ TSettingsComponentPropertyList }

function TSettingsComponentPropertyList.Add(
  Item: PSettingsComponentProperty): Integer;
begin
  Result := inherited Add(Item);
end;

procedure TSettingsComponentPropertyList.AddPropertiesFromObject
  (AObject : TObject; AParentPropertyPath : TSettingName);
var
  ObjectClassInfo : PTypeInfo;
  TypeData : PTypeData;
  PropertyCount : Smallint;
  PropertyList : PPropList;
  PropValue : Variant;
  idx : Integer;
  SCP : PSettingsComponentProperty;
  Name : TSettingName;
begin
  if not Assigned(AObject) then
    exit;

  ObjectClassInfo := AObject.ClassInfo;

  if not Assigned(ObjectClassInfo)  then
    exit;

  TypeData := GetTypeData(ObjectClassInfo);
  PropertyCount := TypeData^.PropCount;

  if PropertyCount > 0 then
  begin
    GetMem(PropertyList, SizeOf(PPropInfo) * PropertyCount);
    try
      GetPropInfos(ObjectClassInfo, PropertyList);

      for idx := 0 to PropertyCount - 1 do
      begin

        VarClear(PropValue);
        try
          PropValue := GetPropValue(AObject, PropertyList[idx]);
        except
          //not the fine way ... but who knows what the components are doing? :)
        end;

        if VarIsEmpty(PropValue) or
           ((PropertyList^[idx].PropType^.Kind = tkClass) and
            (VarIsNull(PropValue))) or
           (PropertyList^[idx].PropType^.Kind = tkMethod) then
           //Todo: Check for readonly properties
        begin
          Continue;
        end;

        if PropertyList[idx]^.PropType^.Kind = tkClass then
        begin
          AddPropertiesFromObject(TObject(Integer(PropValue)),
                                  AParentPropertyPath + SettingsPathDelimiter + PropertyList^[idx].Name);
        end
        else
        if CheckSettingsValueType(PropValue, false) then
        begin
          Name := AParentPropertyPath + SettingsPathDelimiter + PropertyList^[idx].Name;

          if IndexOf(Name) = -1 then
          begin
            New(SCP);
            Add(SCP);
            scp^.Name := Name;
            scp^.Save := false;
          end;

        end;
      end;

    finally
      FreeMem(PropertyList, SizeOf(PPropInfo) * PropertyCount);
    end;
  end;
end;

procedure TSettingsComponentPropertyList.CopyFrom(
  AList: TSettingsComponentPropertyList);
var
  idx : Integer;
  SCP : PSettingsComponentProperty;
begin
  Clear;

  for idx := 0 to AList.Count - 1 do
  begin
    New(SCP);
    Add(SCP);
    
    scp^.Name := AList[idx]^.Name;
    scp^.Save := AList[idx]^.Save; 
  end;
end;

function TSettingsComponentPropertyList.Extract(
  Item: PSettingsComponentProperty): PSettingsComponentProperty;
begin
  Result := inherited Extract(Item);
end;

function TSettingsComponentPropertyList.First: PSettingsComponentProperty;
begin
  Result := inherited First;
end;

function TSettingsComponentPropertyList.Get(
  Index: Integer): PSettingsComponentProperty;
begin
  Result := inherited Get(Index);
end;

function TSettingsComponentPropertyList.GetPropertiesToSaveCount: Integer;
var
  idx : Integer;
begin
  Result := 0;

  for idx := 0 to Count - 1 do
  begin
    if Items[idx]^.Save then
      Inc(Result);
  end;
end;

function TSettingsComponentPropertyList.GetPropertyFromObject(AObject: TObject;
  AIndex: Integer): PPropInfo;
var
  SCP : PSettingsComponentProperty;
  Path : TWideStringList;
  idx : Integer;
  tempInfo : PPropInfo;
  PropValue : Variant;
begin
  Result := nil;

  SCP := Items[AIndex];

  Path := TWideStringList.Create;
  try
    SplitPath(SCP.Name, Path);
    if Path.Count > 0 then
    begin
      path.Delete(0); //the empty string before the first '/'

      for idx := 0 to Path.Count - 1 do
      begin
        tempInfo := GetPropInfo(AObject, Path[idx]);

        try
          PropValue := GetPropValue(AObject, tempInfo);
        except
          //not fine ... but who knows?
        end;

        if Assigned(tempInfo) then
        begin
          if (tempInfo^.PropType^.Kind = tkClass) and
             (PropValue <> 0) then
          begin
            AObject := TObject(Integer(PropValue));
          end
          else
            Result := tempInfo; //last property found
        end
        else
          break;

      end;
    end;
  finally
    Path.Free;
  end;
end;

function TSettingsComponentPropertyList.IndexOf(
  Item: PSettingsComponentProperty): Integer;
begin
  Result := inherited IndexOf(Item);
end;

function TSettingsComponentPropertyList.IndexOf(AName: TSettingName): Integer;
var
  idx : Integer;
begin
  Result := -1;

  for idx := 0 to Count - 1 do
  begin
    if WideSameText(AName, Items[idx]^.Name) then
    begin
      Result := idx;
      break;
    end;
  end;
end;

procedure TSettingsComponentPropertyList.Insert(Index: Integer;
  Item: PSettingsComponentProperty);
begin
  inherited Insert(Index, Item);
end;

function TSettingsComponentPropertyList.Last: PSettingsComponentProperty;
begin
  Result := inherited Last;
end;

procedure TSettingsComponentPropertyList.Load(AReader: TReader);
var
  SCP : PSettingsComponentProperty;
  Name : WideString;
  idx : Integer;
begin
  AReader.ReadListBegin;
  try
    while not AReader.EndOfList do
    begin
      Name := AReader.ReadWideString;

      idx := IndexOf(Name);

      if idx = -1 then
      begin
        New(SCP);
        Add(SCP);
        SCP^.Name := Name;
      end
      else
        SCP := Items[idx];

      SCP.Save := true;
    end;                           
  finally
    AReader.ReadListEnd;
  end;
end;

procedure TSettingsComponentPropertyList.Notify(Ptr: Pointer;
  Action: TListNotification);
begin
  inherited;

  if Action = lnDeleted then
    Dispose(PSettingsComponentProperty(Ptr));
end;

procedure TSettingsComponentPropertyList.Put(Index: Integer;
  const Value: PSettingsComponentProperty);
begin
  inherited Put(Index, Value);
end;

procedure TSettingsComponentPropertyList.ReadFromComponent(
  AComponent: TComponent);
var
  idx : Integer;
begin
  AddPropertiesFromObject(AComponent, EmptyWideStr);

  //delete all which doesnt match this component
  for idx := Count - 1 downto 0 do
  begin
    if not Assigned(GetPropertyFromObject(AComponent, idx)) then
      Delete(idx);
  end;
end;

function TSettingsComponentPropertyList.Remove(
  Item: PSettingsComponentProperty): Integer;
begin
  Result := inherited Remove(Item);
end;

procedure TSettingsComponentPropertyList.Save(AWriter: TWriter);
var
  idx : Integer;
  SCP : TSettingsComponentProperty;
begin
  AWriter.WriteListBegin;
  try
    for idx := 0 to Count - 1 do
    begin
      SCP := Items[idx]^;
      if SCP.Save then
        AWriter.WriteWideString(SCP.Name);
    end;
  finally
    AWriter.WriteListEnd;
  end;
end;

end.
