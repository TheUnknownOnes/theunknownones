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

    procedure SetValue(const Value: TSettingValue);

  public
    constructor Create(AParent : TSetting; AName : TSettingName);
    destructor Destroy(); override;

    procedure RegisterChild(const AChild : TSetting);
    procedure UnregisterChild(const AChild : TSetting);

    procedure Clear;

    function NameMatches(APattern : TSettingName; AIsRegEx : Boolean = false) : Boolean;

    function GetPath : TSettingName;

    property Name : TSettingName read FName;
    property Value : TSettingValue read FValue write SetValue;
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
    procedure WriteValue(const AStream : TStream; AValue : Smallint); overload;
    procedure WriteValue(const AStream : TStream; AValue : Integer); overload;
    procedure WriteValue(const AStream : TStream; AValue : Single); overload;
    procedure WriteValue(const AStream : TStream; AValue : Double); overload;
    procedure WriteValue(const AStream : TStream; AValue : WideString); overload;
    procedure WriteValue(const AStream : TStream; AValue : Boolean); overload;
    procedure WriteValue(const AStream : TStream; AValue : ShortInt); overload;
    procedure WriteValue(const AStream : TStream; AValue : Byte); overload;
    procedure WriteValue(const AStream : TStream; AValue : Word); overload;
    procedure WriteValue(const AStream : TStream; AValue : LongWord); overload;
    procedure WriteValue(const AStream : TStream; AValue : Int64); overload;
    procedure WriteValue(const AStream : TStream; AValue : String); overload;
    procedure WriteValue(const AStream : TStream; AValue : Variant); overload;

    procedure ReadValue(const AStream : TStream; out AValue : Smallint); overload;
    procedure ReadValue(const AStream : TStream; out AValue : Integer); overload;
    procedure ReadValue(const AStream : TStream; out AValue : Single); overload;
    procedure ReadValue(const AStream : TStream; out AValue : Double); overload;
    procedure ReadValue(const AStream : TStream; out AValue : WideString); overload;
    procedure ReadValue(const AStream : TStream; out AValue : Boolean); overload;
    procedure ReadValue(const AStream : TStream; out AValue : ShortInt); overload;
    procedure ReadValue(const AStream : TStream; out AValue : Byte); overload;
    procedure ReadValue(const AStream : TStream; out AValue : Word); overload;
    procedure ReadValue(const AStream : TStream; out AValue : LongWord); overload;
    procedure ReadValue(const AStream : TStream; out AValue : Int64); overload;
    procedure ReadValue(const AStream : TStream; out AValue : String); overload;
    procedure ReadValue(const AStream : TStream; out AValue : Variant); overload;

    procedure SaveSetting(ASetting : TSetting; AStream : TStream);
    procedure LoadSetting(ASetting : TSetting; AStream : TStream);

    function DoLoad(const ASettings : TCustomSettings;
                    const ARootSetting : TSetting) : Boolean; override;
    function DoSave(const ASettings : TCustomSettings;
                    const ARootSetting : TSetting) : Boolean; override;

    function DoCreateStream(out AStream : TStream; ARead : Boolean) : Boolean; virtual; abstract;
    function DoLoadStreamContent(const AStream : TStream) : Boolean; virtual; abstract;
    function DoSaveStreamContent(const AStream : TStream) : Boolean; virtual; abstract;
  end;


//==============================================================================


  TSettingsLSFile = class(TCustomSettingsLSStream)
  private
    FFilename: String;
  protected
    function DoCreateStream(out AStream : TStream; ARead : Boolean) : Boolean; override;
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

procedure CheckSettingsValueType(AVariant : Variant);
begin
  if not VarIsType(AVariant, SettingsValidValueTypes) then
    raise Exception.Create('Invalid value type');
end;

//==============================================================================

{ TSetting }

procedure TSetting.Clear;
begin
  FChildren.Clear;
  
  VarClear(FValue);
end;

constructor TSetting.Create(AParent: TSetting; AName : WideString);
begin
  FParent := AParent;
  FName := AName;
  VarClear(FValue);

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

procedure TSetting.SetValue(const Value: TSettingValue);
begin
  CheckSettingsValueType(Value);
  
  FValue := Value;
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
        VarClear(Result[idx].Value);
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
  Result := DoCreateStream(Stream, true);

  if not Result then
    exit;

  try
    Result := DoLoadStreamContent(Stream);
    
    if Result then
      LoadSetting(ARootSetting, Stream);
  finally
    Stream.Free;
  end;

end;

function TCustomSettingsLSStream.DoSave(const ASettings: TCustomSettings;
  const ARootSetting: TSetting): Boolean;
var
  Stream  : TStream;
begin
  Result := DoCreateStream(Stream, False);

  if not Result then
    exit;

  try
    SaveSetting(ARootSetting, Stream);

    Result := DoSaveStreamContent(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TCustomSettingsLSStream.LoadSetting(ASetting: TSetting;
  AStream: TStream);
var
  idx, ChildCount : Integer;
  Child : TSetting;
begin
  ReadValue(AStream, ASetting.FName);
  ReadValue(AStream, ASetting.FValue);

  ReadValue(AStream, ChildCount);

  for idx := 0 to ChildCount - 1 do
  begin
    Child := TSetting.Create(ASetting, EmptyWideStr);

    LoadSetting(Child, AStream);
  end;
end;

procedure TCustomSettingsLSStream.ReadValue(const AStream: TStream;
  out AValue: Double);
begin
  AStream.ReadBuffer(AValue, SizeOf(AValue));
end;

procedure TCustomSettingsLSStream.ReadValue(const AStream: TStream;
  out AValue: WideString);
var
  Buffer : array of Byte;
  Len : Integer;
begin
  ReadValue(AStream, Len);
  Inc(Len, SizeOf(WideChar));
  SetLength(Buffer, Len);
  FillChar(Buffer[0], Len, 0);
  AStream.ReadBuffer(Buffer[0], Len - SizeOf(WideChar));
  AValue := WideString(PWideChar(@Buffer[0]));
end;

procedure TCustomSettingsLSStream.ReadValue(const AStream: TStream;
  out AValue: Boolean);
begin
  AStream.ReadBuffer(AValue, SizeOf(AValue));
end;

procedure TCustomSettingsLSStream.ReadValue(const AStream: TStream;
  out AValue: Smallint);
begin
  AStream.ReadBuffer(AValue, SizeOf(AValue));
end;

procedure TCustomSettingsLSStream.ReadValue(const AStream: TStream;
  out AValue: Integer);
begin
  AStream.ReadBuffer(AValue, SizeOf(AValue));
end;

procedure TCustomSettingsLSStream.ReadValue(const AStream: TStream;
  out AValue: Single);
begin
  AStream.ReadBuffer(AValue, SizeOf(AValue));
end;

procedure TCustomSettingsLSStream.ReadValue(const AStream: TStream;
  out AValue: ShortInt);
begin
  AStream.ReadBuffer(AValue, SizeOf(AValue));
end;

procedure TCustomSettingsLSStream.ReadValue(const AStream: TStream;
  out AValue: Int64);
begin
  AStream.ReadBuffer(AValue, SizeOf(AValue));
end;

procedure TCustomSettingsLSStream.ReadValue(const AStream: TStream;
  out AValue: String);
var
  Buffer : array of Char;
  Len : Integer;
begin
  ReadValue(AStream, Len);
  Inc(Len);
  SetLength(Buffer, Len);
  FillChar(Buffer, Len, 0);
  AStream.ReadBuffer(Buffer, Len - 1);
  AValue := StrPas(@Buffer[0]);
end;

procedure TCustomSettingsLSStream.ReadValue(const AStream: TStream;
  out AValue: Variant);
var
  ValueType : TVarType;
  tempSmallInt : Smallint;
  tempInteger : Integer;
  tempSingle : Single;
  tempDouble : Double;
  tempDate : Double;
  tempOleStr : WideString;
  tempBoolean : Boolean;
  tempShortInt : Shortint;
  tempByte : Byte;
  tempWord : Word;
  tempLongWord : LongWord;
  tempInt64 : Int64;
  tempString : String;
begin
  ReadValue(AStream, ValueType);

  case ValueType of
    varEmpty    : VarClear(AValue);
    varNull     : AValue := null;
    varSmallint :
    begin
      ReadValue(AStream, tempSmallInt);
      AValue := tempSmallInt;
    end;
    varInteger  :
    begin
      ReadValue(AStream, tempInteger);
      AValue := tempInteger;
    end;
    varSingle   :
    begin
      ReadValue(AStream, tempSingle);
      AValue := tempSingle;
    end;
    varDouble   :
    begin
      ReadValue(AStream, tempDouble);
      AValue := tempDouble;
    end;
    varDate     :
    begin
      ReadValue(AStream, tempDate);
      AValue := tempDate;
    end;
    varOleStr   :
    begin
      ReadValue(AStream, tempOleStr);
      AValue := tempOleStr;
    end;
    varBoolean  :
    begin
      ReadValue(AStream, tempBoolean);
      AValue := tempBoolean;
    end;
    varShortInt :
    begin
      ReadValue(AStream, tempShortInt);
      AValue := tempShortInt;
    end;
    varByte     :
    begin
      ReadValue(AStream, tempByte);
      AValue := tempByte;
    end;
    varWord     :
    begin
      ReadValue(AStream, tempWord);
      AValue := tempWord;
    end;
    varLongWord :
    begin
      ReadValue(AStream, tempLongWord);
      AValue := tempLongWord;
    end;
    varInt64    :
    begin
      ReadValue(AStream, tempInt64);
      AValue := tempInt64;
    end;
    varString   :
    begin
      ReadValue(AStream, tempString);
      AValue := tempString;
    end;
  end;
end;

procedure TCustomSettingsLSStream.ReadValue(const AStream: TStream;
  out AValue: Byte);
begin
  AStream.ReadBuffer(AValue, SizeOf(AValue));
end;

procedure TCustomSettingsLSStream.ReadValue(const AStream: TStream;
  out AValue: Word);
begin
  AStream.ReadBuffer(AValue, SizeOf(AValue));
end;

procedure TCustomSettingsLSStream.ReadValue(const AStream: TStream;
  out AValue: LongWord);
begin
  AStream.ReadBuffer(AValue, SizeOf(AValue));
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
  AValue: Boolean);
begin
  Astream.WriteBuffer(AValue, SizeOf(AValue));
end;

procedure TCustomSettingsLSStream.WriteValue(const AStream: TStream;
  AValue: Double);
begin
  Astream.WriteBuffer(AValue, SizeOf(AValue));
end;

procedure TCustomSettingsLSStream.WriteValue(const AStream: TStream;
  AValue: Smallint);
begin
  Astream.WriteBuffer(AValue, SizeOf(AValue));
end;

procedure TCustomSettingsLSStream.WriteValue(const AStream: TStream;
  AValue: Single);
begin
  Astream.WriteBuffer(AValue, SizeOf(AValue));
end;

procedure TCustomSettingsLSStream.WriteValue(const AStream: TStream;
  AValue: Int64);
begin
  Astream.WriteBuffer(AValue, SizeOf(AValue));
end;

procedure TCustomSettingsLSStream.WriteValue(const AStream: TStream;
  AValue: String);
begin
  WriteValue(AStream, Length(AValue));
  Astream.WriteBuffer(PChar(AValue)^, Length(AValue));
end;

procedure TCustomSettingsLSStream.WriteValue(const AStream: TStream;
  AValue: LongWord);
begin
  Astream.WriteBuffer(AValue, SizeOf(AValue));
end;

procedure TCustomSettingsLSStream.WriteValue(const AStream: TStream;
  AValue: ShortInt);
begin
  Astream.WriteBuffer(AValue, SizeOf(AValue));
end;

procedure TCustomSettingsLSStream.WriteValue(const AStream: TStream;
  AValue: Byte);
begin
  Astream.WriteBuffer(AValue, SizeOf(AValue));
end;

procedure TCustomSettingsLSStream.WriteValue(const AStream: TStream;
  AValue: Variant);
var
  ValueType : Word;
  Data : TVarData;
begin
  ValueType := VarType(AValue);

  WriteValue(AStream, ValueType);

  Data := TVarData(AValue);

  case ValueType of
    varSmallint : WriteValue(AStream, Data.VSmallInt);
    varInteger  : WriteValue(AStream, Data.VInteger);
    varSingle   : WriteValue(AStream, Data.VSingle);
    varDouble   : WriteValue(AStream, Data.VDouble);
    varDate     : WriteValue(AStream, Data.VDate);
    varOleStr   : WriteValue(AStream, WideString(Data.VOleStr));
    varBoolean  : WriteValue(AStream, Data.VBoolean);
    varShortInt : WriteValue(AStream, Data.VShortInt);
    varByte     : WriteValue(AStream, Data.VByte);
    varWord     : WriteValue(AStream, Data.VWord);
    varLongWord : WriteValue(AStream, Data.VLongWord);
    varInt64    : WriteValue(AStream, Data.VInt64);
    varString   : WriteValue(AStream, String(Data.VString));
  end;
end;

procedure TCustomSettingsLSStream.WriteValue(const AStream: TStream;
  AValue: WideString);
begin
  WriteValue(AStream, Length(AValue) * SizeOf(WideChar));
  Astream.WriteBuffer(PWideChar(AValue)^, Length(AValue) * SizeOf(WideChar));
end;

procedure TCustomSettingsLSStream.WriteValue(const AStream: TStream;
  AValue: Integer);
begin
  Astream.WriteBuffer(AValue, SizeOf(AValue));
end;

procedure TCustomSettingsLSStream.WriteValue(const AStream: TStream;
  AValue: Word);
begin
  Astream.WriteBuffer(AValue, SizeOf(AValue));
end;

{ TSettingsLSFile }

function TSettingsLSFile.DoCreateStream(out AStream: TStream; ARead : Boolean): Boolean;
var
  Flags : Word;
begin
  Result := Trim(FFilename) <> EmptyStr;

  if ARead then
    Flags := fmOpenRead
  else
    Flags := fmOpenWrite or fmCreate or fmShareDenyWrite;

  if Result then
    AStream := TFileStream.Create(FFilename, Flags);
end;

function TSettingsLSFile.DoLoadStreamContent(const AStream: TStream): Boolean;
begin
  AStream.Seek(0, soFromBeginning);
  
  Result := true;
end;

function TSettingsLSFile.DoSaveStreamContent(const AStream: TStream): Boolean;
begin
  Result := true;
end;

end.
