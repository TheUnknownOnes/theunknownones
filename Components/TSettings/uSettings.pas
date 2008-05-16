unit uSettings;

interface

{$DEFINE UniCode}

uses
  Classes,
  Contnrs,
  Sysutils,
  Variants,
  WideStrings,
  RegExpr;

type

  TSetting = class;

  TCustomSettings = class;

  TSettingName = WideString;

  TSettingValue = Variant;

  TSettingValues = array of Variant;

  TSettingNameValue = record
    Name : TSettingName;
    Value : TSettingValue;
  end;

  TSettingNameValues = array of TSettingNameValue;

  
//==============================================================================


  TSettingList = class(TObjectList)
  protected
    function GetItem(Index: Integer): TSetting;
    procedure SetItem(Index: Integer; const Value: TSetting);

    property OwnsObjects;
  public
    constructor Create;
    
    function Add(ASetting: TSetting; ADoNotIfDuplicatePath : Boolean = false): Integer;
    function Extract(Item: TSetting): TSetting;
    function Remove(ASetting: TSetting): Integer;
    function IndexOf(ASetting: TSetting): Integer; overload;
    function IndexOf(APath: TSettingName): Integer; overload;
    procedure Insert(Index: Integer; ASetting: TSetting);
    function First: TSetting;
    function Last: TSetting;
    property Items[Index: Integer]: TSetting read GetItem write SetItem; default;

  end;


//==============================================================================


  TSetting = class
  protected
    FParent : TSetting;
    FName : TSettingName;
    FValue : TSettingValue;

    FChildren : TSettingList;

  public
    constructor Create(AParent : TSetting; AName : TSettingName);
    destructor Destroy(); override;

    procedure RegisterChild(const AChild : TSetting);
    procedure UnregisterChild(const AChild : TSetting);

    procedure Clear;

    function NameMatches(APattern : TSettingName; AIsRegEx : Boolean = false) : Boolean;

    function GetPath : TSettingName;

    property Name : TSettingName read FName;
    property Value : TSettingValue read FValue write FValue;
    property Parent : TSetting read FParent;
    property Children : TSettingList read FChildren;
  end;


//==============================================================================


  TCustomSettingsLoaderSaver = class(TComponent)
  protected
    function DoLoad(const ASettings : TCustomSettings;
                    const ARootSetting : TSetting) : Boolean; virtual; abstract;
    function DoSave(const ASettings : TCustomSettings;
                    const ARootSetting : TSetting) : Boolean; virtual; abstract;
  public
    function Load(ASettings : TCustomSettings;
                  ARootSetting : TSetting) : Boolean; virtual;
    function Save(ASettings : TCustomSettings;
                  ARootSetting : TSetting) : Boolean; virtual;
  end;


//==============================================================================


  TCustomSettingsLSStream = class(TCustomSettingsLoaderSaver)
  protected
    procedure WriteValue(const AStream : TStream; AValue : Integer); overload;
    procedure WriteValue(const AStream : TStream; AValue : WideString); overload;
    procedure WriteValue(const AStream : TStream; AValue : Variant); overload;

    procedure SaveSetting(ASetting : TSetting; AStream : TStream);

    function DoLoad(const ASettings : TCustomSettings;
                    const ARootSetting : TSetting) : Boolean; override;
    function DoSave(const ASettings : TCustomSettings;
                    const ARootSetting : TSetting) : Boolean; override;

    function DoCreateStream(out AStream : TStream) : Boolean; virtual; abstract;
    function DoLoadStreamContent(const AStream : TStream) : Boolean; virtual; abstract;
    function DoSaveStreamContent(const AStream : TStream) : Boolean; virtual; abstract;
  end;


//==============================================================================


  TSettingsLSFile = class(TCustomSettingsLSStream)
  private
    FFilename: String;
  protected
    function DoCreateStream(out AStream : TStream) : Boolean; override;
    function DoLoadStreamContent(const AStream : TStream) : Boolean; override;
    function DoSaveStreamContent(const AStream : TStream) : Boolean; override;

  published
    property FileName : String read FFilename write FFilename;
  end;


//==============================================================================


  TCustomSettings = class(TComponent)
  protected
    FRootSetting : TSetting;
    FLoader,
    FSaver : TCustomSettingsLoaderSaver;
    FSupplier : TCustomSettings;
    
    procedure GetSettings(const APath : TSettingName;
                          const AIsRegEx : Boolean;
                          const AList : TSettingList;
                          const ACreateIfNotExisting : Boolean;
                          const AUseSupplier : Boolean);

    procedure GetMatchingSettings(const APath : TWideStringList;
                                  const APathIndex : Integer;
                                  const AList : TSettingList;
                                  const AIsRegEx : Boolean;
                                  const ACreateIfNotExisting : Boolean);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function ItemsExists(APath : TSettingName;
                         AIsRegEx : Boolean = false) : Boolean;

    procedure SetValue(AValue : TSettingValue;
                       APath : TSettingName;
                       AIsRegEx : Boolean = false;
                       ACreateIfNotExisting : Boolean = true);

    function GetItems(APath : TSettingName;
                      AIsRegEx : Boolean = false;
                      AGetName : Boolean = true;
                      AFullPath : Boolean = false;
                      AGetValue : Boolean = true) : TSettingNameValues;

    function GetItem(APath : TSettingName;
                     out AResult : TSettingNameValue;
                     AIsRegEx : Boolean = false;
                     AGetName : Boolean = true;
                     AFullPath : Boolean = false;
                     AGetValue : Boolean = true) : Boolean;

    procedure Delete(APath : TSettingName; AIsRegEx : Boolean);

    function Load : Boolean;
    function Save : Boolean;

    property Loader : TCustomSettingsLoaderSaver read FLoader write FLoader;
    property Saver : TCustomSettingsLoaderSaver read FSaver write FSaver;

    property Supplier : TCustomSettings read FSupplier write FSupplier;
  end;

  TSettings = class(TCustomSettings)
  published
    property Loader;
    property Saver;

    property Supplier;
  end;


//==============================================================================

const
  SettingsPathDelimiter = '/';

var
  SettingsRegExCaseinsensitive : Boolean = true;
  SettingsRegExRussianSupport : Boolean = false;
  SettingsRegExDotMatchesLineSeperators : Boolean = true;
  SettingsRegExAllOperatorsGreedy : Boolean = true;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TUO', [TSettings, TSettingsLSFile]);
end;


//==============================================================================

{ Helper }

procedure SplitPath(APath : WideString; AList : TWideStrings);
begin
  AList.Delimiter := SettingsPathDelimiter;
  AList.StrictDelimiter := true;
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


//==============================================================================

{ TSetting }

procedure TSetting.Clear;
begin
  FChildren.Clear;
  
  Value := null;
end;

constructor TSetting.Create(AParent: TSetting; AName : WideString);
begin
  FParent := AParent;
  FName := AName;
  FValue := null;

  FChildren := TSettingList.Create;
  FChildren.OwnsObjects := true;

  if Assigned(FParent) then
    FParent.RegisterChild(Self);
end;

destructor TSetting.Destroy;
begin
  FChildren.Free;

  if Assigned(FParent) then
    FParent.UnregisterChild(Self);

  inherited;
end;

function TSetting.GetPath: TSettingName;
var
  Path : TWideStringList;
  Setting : TSetting;
begin
  Setting := Self;

  Path := TWideStringList.Create;
  try
    SplitPath(EmptyWideStr, Path); //init the list

    while Assigned(Setting) do
    begin
      Path.Insert(0,Setting.Name);
      Setting := Setting.Parent;
    end;

    Result := Path.DelimitedText;
  finally
    Path.Free;
  end;
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
end;

procedure TSetting.UnregisterChild(const AChild: TSetting);
var
  Index : Integer;
begin
  Index := FChildren.IndexOf(AChild);
  
  if Index > -1 then
    FChildren.Delete(Index);
end;

//==============================================================================

{ TSettingList }

function TSettingList.Add(ASetting: TSetting;
  ADoNotIfDuplicatePath : Boolean): Integer;
begin
  Result := -1;
  
  if (not ADoNotIfDuplicatePath) or
     (ADoNotIfDuplicatePath and (IndexOf(ASetting.GetPath) = -1)) then  
    Result := inherited Add(ASetting);
end;

constructor TSettingList.Create;
begin
  inherited Create(false);
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

function TSettingList.IndexOf(APath: TSettingName): Integer;
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
  inherited SetItem(Index, Value);
end;

{ TCustomSettings }

constructor TCustomSettings.Create(AOwner: TComponent);
begin
  inherited;

  FLoader := nil;
  FSaver := nil;

  FRootSetting := TSetting.Create(nil, EmptyWideStr);
end;

procedure TCustomSettings.Delete(APath: TSettingName; AIsRegEx: Boolean);
var
  Sett : TSettingList;
  idx : Integer;
begin
  Sett := TSettingList.Create;
  try
    GetSettings(APath, AIsRegEx, Sett, false, false);

    for idx := 0 to Sett.Count - 1 do
    begin
      if Assigned(Sett[idx].Parent) then
        Sett[idx].Parent.UnregisterChild(Sett[idx])
      else
        Sett[idx].Free;
    end;
  finally
    Sett.Free;
  end;
end;

destructor TCustomSettings.Destroy;
begin
  FRootSetting.Free;

  inherited;
end;

function TCustomSettings.GetItem(APath: TSettingName; out AResult : TSettingNameValue;
  AIsRegEx, AGetName, AFullPath, AGetValue: Boolean): Boolean;
var
  Values : TSettingNameValues;
begin
  Values := GetItems(APath, AIsRegEx, AGetName, AFullPath, AGetValue);

  Result := Length(Values) > 0;

  if Result then
    AResult := Values[0];
end;

function TCustomSettings.GetItems(APath: TSettingName; AIsRegEx, AGetName,
  AFullPath, AGetValue: Boolean): TSettingNameValues;
var
  Sett : TSettingList;
  idx : Integer;
begin
  Sett := TSettingList.Create;
  try
    GetSettings(APath, AIsRegEx, Sett, false, true);

    SetLength(Result, Sett.Count);

    for idx := 0 to Sett.Count - 1 do
    begin

      if AGetName then
      begin
        if AFullPath then
          Result[idx].Name := Sett[idx].GetPath
        else
          Result[idx].Name := Sett[idx].Name;
      end
      else
        Result[idx].Name := EmptyWideStr;

      if AGetValue then
        Result[idx].Value := Sett[idx].Value
      else
        Result[idx].Value := null;
    end;
  finally
    Sett.Free;
  end;
end;

procedure TCustomSettings.GetMatchingSettings(const APath: TWideStringList;
  const APathIndex: Integer; const AList: TSettingList; const AIsRegEx : Boolean;
  const ACreateIfNotExisting : Boolean);
var
  idxParent,
  idxChild : Integer;
  Setting : TSetting;
  ListCount : Integer;
  FoundMatch : Boolean;
begin
  if AIsRegEx and ACreateIfNotExisting then
    raise Exception.Create('Can not create non-existing settings if regular expression is supplied');

  if APathIndex = APath.Count then
    exit; //all parts of the path as been processed

  ListCount := AList.Count;

  for idxParent := ListCount - 1 downto 0 do
  begin
    Setting := AList[idxParent];

    FoundMatch := false;

    for idxChild := 0 to Setting.Children.Count - 1 do
    begin
      //add each child which matches the part of the path
      if Setting.Children[idxChild].NameMatches(APath[APathIndex], AIsRegEx) then
      begin
        AList.Add(Setting.Children[idxChild], true);
        FoundMatch := true;
      end;
    end;

    if (not FoundMatch) and ACreateIfNotExisting then
      AList.Add(TSetting.Create(Setting, APath[APathIndex]));

    AList.Remove(Setting);
  end;

  GetMatchingSettings(APath, APathIndex + 1, AList, AIsRegEx, ACreateIfNotExisting);
end;

procedure TCustomSettings.GetSettings(const APath: WideString; const AIsRegEx: Boolean;
  const AList: TSettingList; const ACreateIfNotExisting : Boolean;
  const AUseSupplier : Boolean);
var
  Path : TWideStringList;
begin
  Path := TWideStringList.Create;
  try
    SplitPath(APath, Path);

    AList.Add(FRootSetting);

    GetMatchingSettings(Path, 1, AList, AIsRegEx, ACreateIfNotExisting);
  finally
    Path.Free;
  end;

  if AUseSupplier and Assigned(FSupplier) then
    FSupplier.GetSettings(APath, AIsRegEx, AList, ACreateIfNotExisting, AUseSupplier);
end;

function TCustomSettings.Save: Boolean;
begin
  if Assigned(FSaver) then
    Result := FSaver.Save(Self, FRootSetting)
  else
    Result := false;
end;

procedure TCustomSettings.SetValue(AValue: TSettingValue; APath: TSettingName;
  AIsRegEx: Boolean; ACreateIfNotExisting : Boolean);
var
  Setts : TSettingList;
  idx : Integer;
begin
  Setts := TSettingList.Create;
  try
    GetSettings(APath, AIsRegEx, Setts, true, false);

    for idx := 0 to Setts.Count - 1 do
      Setts[idx].Value := AValue;
  finally
    Setts.Free;
  end;
end;

function TCustomSettings.ItemsExists(APath: TSettingName;
  AIsRegEx: Boolean): Boolean;
var
  Setts : TSettingList;
begin
  Setts := TSettingList.Create;
  try
    GetSettings(APath, AIsRegEx, Setts, false, true);
    Result := Setts.Count > 0;
  finally
    Setts.Free;
  end;
end;

function TCustomSettings.Load: Boolean;
begin
  if Assigned(FLoader) then
  begin
    FRootSetting.Clear;
    Result := FLoader.Load(Self, FRootSetting)
  end
  else
    Result := false;
end;

{ TCustomSettingsLoaderSaver }

function TCustomSettingsLoaderSaver.Load(ASettings: TCustomSettings;
  ARootSetting: TSetting): Boolean;
begin
  Result := DoLoad(ASettings, ARootSetting);
end;

function TCustomSettingsLoaderSaver.Save(ASettings: TCustomSettings;
  ARootSetting: TSetting): Boolean;
begin
  Result := DoSave(ASettings, ARootSetting);
end;

{ TCustomSettingsLSStream }

function TCustomSettingsLSStream.DoLoad(const ASettings: TCustomSettings;
  const ARootSetting: TSetting): Boolean;
var
  Stream : TStream;
begin
  Result := DoCreateStream(Stream);

  if not Result then
    exit;

  try
    DoLoadStreamContent(Stream);

  finally
    Stream.Free;
  end;

end;

function TCustomSettingsLSStream.DoSave(const ASettings: TCustomSettings;
  const ARootSetting: TSetting): Boolean;
var
  Stream  : TStream;
begin
  Result := DoCreateStream(Stream);

  if not Result then
    exit;

  try
    SaveSetting(ARootSetting, Stream);

    DoSaveStreamContent(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TCustomSettingsLSStream.SaveSetting(ASetting: TSetting;
  AStream: TStream);
var
  idx : Integer;
begin
  WriteValue(AStream, ASetting.Name);
  WriteValue(AStream, ASetting.Value);
  WriteValue(AStream, ASetting.Children.Count);

  for idx := 0 to ASetting.Children.Count - 1 do
    SaveSetting(ASetting.Children[idx], AStream);
end;

procedure TCustomSettingsLSStream.WriteValue(const AStream: TStream;
  AValue: Variant);
var
  Data : Pointer;
  Size : Integer;
  vArray : Variant;
begin
  vArray := VarArrayOf([AValue]);
  Size := SizeOf(AValue);

  WriteValue(AStream, Size);

  if Size > 0 then
  begin
    Data := VarArrayLock(vArray);
    try
      AStream.Write(Data^, Size);
    finally
      VarArrayUnlock(vArray);
    end;
  end;
end;

procedure TCustomSettingsLSStream.WriteValue(const AStream: TStream;
  AValue: WideString);
begin
  WriteValue(AStream, Length(AValue) * SizeOf(WideChar));
  AStream.Write(PWideChar(AValue)^, Length(AValue) * SizeOf(WideChar));
end;

procedure TCustomSettingsLSStream.WriteValue(const AStream: TStream;
  AValue: Integer);
begin
  AStream.Write(AValue, SizeOf(AValue));
end;

{ TSettingsLSFile }

function TSettingsLSFile.DoCreateStream(out AStream: TStream): Boolean;
begin
  Result := Trim(FFilename) <> EmptyStr;

  if Result then
    AStream := TFileStream.Create(FFilename, fmOpenWrite or fmCreate);
end;

function TSettingsLSFile.DoLoadStreamContent(const AStream: TStream): Boolean;
begin
  Result := true;
end;

function TSettingsLSFile.DoSaveStreamContent(const AStream: TStream): Boolean;
begin
  Result := true;
end;

end.
