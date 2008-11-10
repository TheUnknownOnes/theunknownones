//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit uCNA2Profiles;

interface

uses
  Classes,
  SysUtils,
  uRTTIHelper,
  WideStrings,
  WideStrUtils,
  uCNA2Settings,
  uSettingsBase,
  StdCtrls,
  Dialogs;

type
  Tcna2Profiles = class;
  Tcna2Profile = class;
  Tcna2Group = class;

  Tcna2Component = class
  private
    FClass: TClass;
    FGroup : Tcna2Group;
    procedure SetGroup(const Value: Tcna2Group);
  public
    constructor Create(AGroup : Tcna2Group);
    destructor Destroy; override;

    procedure LoadFromSettings(APath : TSettingName);
    procedure SaveToSettings(APath : TSettingName);

    property Group : Tcna2Group read FGroup write SetGroup;
    property ComponentClass : TClass read FClass write FClass;
  end;

  Tcna2ComponentList = class(TList)
  private
    function Get(Index: Integer): Tcna2Component;
    procedure Put(Index: Integer; const Value: Tcna2Component);
  public
    function Add(Item: Tcna2Component): Integer;
    function Extract(Item: Tcna2Component): Tcna2Component;
    function First: Tcna2Component;
    function IndexOf(Item: Tcna2Component): Integer;
    procedure Insert(Index: Integer; Item: Tcna2Component);
    function Last: Tcna2Component;
    function Remove(Item: Tcna2Component): Integer;
    property Items[Index: Integer]: Tcna2Component read Get write Put; default;
  end;

  Tcna2Group = class
  private
    FName: String;
    FComponents: Tcna2ComponentList;
    FProfile : Tcna2Profile;
    FProperties: TWideStringList;
    procedure SetProfile(const Value: Tcna2Profile);
  public
    constructor Create(AProfile : Tcna2Profile);
    destructor Destroy; override;

    procedure RegisterComponent(AComponent : Tcna2Component);
    procedure UnregisterComponent(AComponent : Tcna2Component);

    procedure Clear;
    procedure LoadFromSettings(APath : TSettingName);
    procedure SaveToSettings(APath : TSettingName);

    procedure CopyContent(ASource : Tcna2Group);

    function FindComponent(AClass : TClass; out AComponent : Tcna2Component) : Boolean;

    function AddComponent(AClass : TClass) : Tcna2Component;

    property Name : String read FName write FName;
    property Components : Tcna2ComponentList read FComponents;
    property Profile : Tcna2Profile read FProfile write SetProfile;
    property Properties : TWideStringList read FProperties;
  end;

  Tcna2GroupList = class(Tlist)
  private
    function Get(Index: Integer): Tcna2Group;
    procedure Put(Index: Integer; const Value: Tcna2Group);
  public
    function Add(Item: Tcna2Group): Integer;
    function Extract(Item: Tcna2Group): Tcna2Group;
    function First: Tcna2Group;
    function IndexOf(Item: Tcna2Group): Integer;
    procedure Insert(Index: Integer; Item: Tcna2Group);
    function Last: Tcna2Group;
    function Remove(Item: Tcna2Group): Integer;
    property Items[Index: Integer]: Tcna2Group read Get write Put; default;
  end;

  Tcna2Profile = class
  private
    FName: String;
    FGroups: Tcna2GroupList;
    FProfiles : Tcna2Profiles;
    procedure SetProfiles(const Value: Tcna2Profiles);
  public
    constructor Create(AProfiles : Tcna2Profiles);
    destructor Destroy; override;

    procedure RegisterGroup(AGroup : Tcna2Group);
    procedure UnregisterGroup(AGroup : Tcna2Group);

    procedure Clear;
    procedure LoadFromSettings(APath : TSettingName);
    procedure SaveToSettings(APath : TSettingName);

    function AddGroup(AName : String) : Tcna2Group;

    procedure CopyContent(ASource : Tcna2Profile);
    
    property Name : String read FName write FName;
    property Groups : Tcna2GroupList read FGroups;
    property Profiles : Tcna2Profiles read FProfiles write SetProfiles;
  end;

  Tcna2ProfileList = class(TList)
  private
    function Get(Index: Integer): Tcna2Profile;
    procedure Put(Index: Integer; const Value: Tcna2Profile);
  public
    function Add(Item: Tcna2Profile): Integer;
    function Extract(Item: Tcna2Profile): Tcna2Profile;
    function First: Tcna2Profile;
    function IndexOf(Item: Tcna2Profile): Integer;
    procedure Insert(Index: Integer; Item: Tcna2Profile);
    function Last: Tcna2Profile;
    function Remove(Item: Tcna2Profile): Integer;
    property Items[Index: Integer]: Tcna2Profile read Get write Put; default;
  end;

  Tcna2Profiles = class
  private
    FProfiles: Tcna2ProfileList;
    FCurrentProfile: Tcna2Profile;

    procedure EnsureMinimalContent;
  public
    constructor Create;
    destructor Destroy(); override;

    procedure Clear;

    procedure RegisterProfile(AProfile : Tcna2Profile);
    procedure UnregisterProfile(AProfile : Tcna2Profile);

    function AddProfile(AName : WideString) : Tcna2Profile;

    procedure Load;
    procedure Save;

    property Profiles : Tcna2ProfileList read FProfiles;
    property CurrentProfile : Tcna2Profile read FCurrentProfile write FCurrentProfile;
  end;

var
  cna2Profiles : Tcna2Profiles;

implementation

{ Tcna2Profiles }

function Tcna2Profiles.AddProfile(AName: WideString): Tcna2Profile;
begin
  Result := Tcna2Profile.Create(Self);
  Result.Name := AName;

  if not Assigned(CurrentProfile) then
    CurrentProfile := Result; 
end;

procedure Tcna2Profiles.Clear;
begin
  while FProfiles.Count > 0 do
    FProfiles.First.Free;
end;

constructor Tcna2Profiles.Create;
begin
  FProfiles := Tcna2ProfileList.Create;
  FCurrentProfile := nil;

  Load;

  EnsureMinimalContent;
end;

destructor Tcna2Profiles.Destroy;
begin
  Save;

  Clear;

  FProfiles.Free;

  inherited;
end;

procedure Tcna2Profiles.EnsureMinimalContent;
var
  P : Tcna2Profile;
  G : Tcna2Group;
  C : Tcna2Component;
begin
  if Profiles.Count = 0 then
  begin
    P := AddProfile('Default Profile');
    G := P.AddGroup('Buttons');
    C := G.AddComponent(TButton);
  end;
end;

procedure Tcna2Profiles.Load;
var
  P : Tcna2Profile;
  Setts : TSettingNames;
  idx : Integer;
begin
  Setts := cna2Settings.GetSubNames('/Profiles', false, true);
  for idx := Low(Setts) to High(Setts) do
  begin
    P := Tcna2Profile.Create(Self);
    P.LoadFromSettings(Setts[idx]);

    if WideSameText(Setts[idx], cna2Settings.GetValue('/CurrentProfile', '')) then
      CurrentProfile := P;
  end;
end;

procedure Tcna2Profiles.RegisterProfile(AProfile: Tcna2Profile);
begin
  if Profiles.IndexOf(AProfile) = -1 then  
    Profiles.Add(AProfile);
end;

procedure Tcna2Profiles.Save;
var
  Path : TSettingName;
  idx : Integer;
begin
  cna2Settings.Delete('/Profiles');
  cna2Settings.Delete('/CurrentProfile');

  for idx := 0 to Profiles.Count - 1 do
  begin
    Path := '/Profiles/Profile' + IntToStr(idx);
    Profiles[idx].SaveToSettings(Path);

    if Profiles[idx] = CurrentProfile then
      cna2Settings.SetValue('/CurrentProfile', Path);
  end;
end;

procedure Tcna2Profiles.UnregisterProfile(AProfile: Tcna2Profile);
begin
  Profiles.Extract(AProfile);

  if AProfile = CurrentProfile then
  begin
    if Profiles.Count > 0 then
      CurrentProfile := Profiles.First
    else
      CurrentProfile := nil;
  end;
end;

{ Tcna2ProfileList }

function Tcna2ProfileList.Add(Item: Tcna2Profile): Integer;
begin
  Result := inherited Add(Item);
end;

function Tcna2ProfileList.Extract(Item: Tcna2Profile): Tcna2Profile;
begin
  Result := inherited Extract(Item);
end;

function Tcna2ProfileList.First: Tcna2Profile;
begin
  Result := inherited First;
end;

function Tcna2ProfileList.Get(Index: Integer): Tcna2Profile;
begin
  Result := inherited Get(Index);
end;

function Tcna2ProfileList.IndexOf(Item: Tcna2Profile): Integer;
begin
  Result := inherited IndexOf(Item);
end;

procedure Tcna2ProfileList.Insert(Index: Integer; Item: Tcna2Profile);
begin
  inherited Insert(Index, Item);
end;

function Tcna2ProfileList.Last: Tcna2Profile;
begin
  Result := inherited Last;
end;

procedure Tcna2ProfileList.Put(Index: Integer; const Value: Tcna2Profile);
begin
  inherited Put(Index, Value);
end;

function Tcna2ProfileList.Remove(Item: Tcna2Profile): Integer;
begin
  Result := inherited Remove(Item);
end;

{ Tcna2GroupList }

function Tcna2GroupList.Add(Item: Tcna2Group): Integer;
begin
  Result := inherited Add(Item);
end;

function Tcna2GroupList.Extract(Item: Tcna2Group): Tcna2Group;
begin
  Result := inherited Extract(Item);
end;

function Tcna2GroupList.First: Tcna2Group;
begin
  Result := inherited First;
end;

function Tcna2GroupList.Get(Index: Integer): Tcna2Group;
begin
  Result := inherited Get(Index);
end;

function Tcna2GroupList.IndexOf(Item: Tcna2Group): Integer;
begin
  Result := inherited IndexOf(Item);
end;

procedure Tcna2GroupList.Insert(Index: Integer; Item: Tcna2Group);
begin
  inherited Insert(Index, Item);
end;

function Tcna2GroupList.Last: Tcna2Group;
begin
  Result := inherited Last;
end;

procedure Tcna2GroupList.Put(Index: Integer; const Value: Tcna2Group);
begin
  inherited Put(Index, Value);
end;

function Tcna2GroupList.Remove(Item: Tcna2Group): Integer;
begin
  Result := inherited Remove(Item);
end;

{ Tcna2ComponentList }

function Tcna2ComponentList.Add(Item: Tcna2Component): Integer;
begin
  Result := inherited Add(Item);
end;

function Tcna2ComponentList.Extract(Item: Tcna2Component): Tcna2Component;
begin
  Result := inherited Extract(Item);
end;

function Tcna2ComponentList.First: Tcna2Component;
begin
  Result := inherited First;
end;

function Tcna2ComponentList.Get(Index: Integer): Tcna2Component;
begin
  Result := inherited Get(Index);
end;

function Tcna2ComponentList.IndexOf(Item: Tcna2Component): Integer;
begin
  Result := inherited IndexOf(Item);
end;

procedure Tcna2ComponentList.Insert(Index: Integer; Item: Tcna2Component);
begin
  inherited Insert(Index, Item);
end;

function Tcna2ComponentList.Last: Tcna2Component;
begin
  Result := inherited Last;
end;

procedure Tcna2ComponentList.Put(Index: Integer; const Value: Tcna2Component);
begin
  inherited Put(Index, Value);
end;

function Tcna2ComponentList.Remove(Item: Tcna2Component): Integer;
begin
  Result := inherited Remove(Item);
end;

{ Tcna2Group }

function Tcna2Group.AddComponent(AClass: TClass): Tcna2Component;
begin
  if not FindComponent(AClass, Result) then
  begin
    Result := Tcna2Component.Create(Self);
    Result.ComponentClass := AClass;
  end;
end;

procedure Tcna2Group.Clear;
begin
  while FComponents.Count > 0 do
    FComponents.First.Free;
end;

procedure Tcna2Group.CopyContent(ASource: Tcna2Group);
var
  idx : Integer;
begin
  if ASource <> Self then
  begin
    Clear;

    for idx := 0 to ASource.Components.Count - 1 do
    begin
      AddComponent(ASource.Components[idx].ComponentClass);
    end;
  end;
end;

constructor Tcna2Group.Create(AProfile: Tcna2Profile);
begin
  FProperties := TWideStringList.Create;
  FProfile := nil;
  Profile := AProfile;
  FComponents := Tcna2ComponentList.Create;
end;

destructor Tcna2Group.Destroy;
begin
  Clear;

  FComponents.Free;

  Profile := nil;
  
  FProperties.Free;

  inherited;
end;

function Tcna2Group.FindComponent(AClass: TClass;
  out AComponent: Tcna2Component): Boolean;
var
  idx : Integer;
begin
  Result := false;

  for idx := 0 to Components.Count - 1 do
  begin
    if Components[idx].ComponentClass = AClass then
    begin
      AComponent := Components[idx];
      Result := true;
      break;
    end;
  end;
end;

procedure Tcna2Group.LoadFromSettings(APath: TSettingName);
var
  Setts : TSettingNames;
  idx : Integer;
begin
  Name := cna2Settings.GetValue(APath, Name);

  Clear;

  Setts := cna2Settings.GetSubNames(APath + '/Components', false, true);

  for idx := Low(Setts) to High(Setts) do
  begin
    Tcna2Component.Create(Self).LoadFromSettings(Setts[idx]);
  end;

end;

procedure Tcna2Group.RegisterComponent(AComponent: Tcna2Component);
var
  Compo : Tcna2Component;
begin
  if not FindComponent(AComponent.ComponentClass, Compo) then
    Components.Add(AComponent);

end;

procedure Tcna2Group.SaveToSettings(APath: TSettingName);
var
  idx : Integer;
begin
  cna2Settings.SetValue(APath, Name);

  for idx := 0 to Components.Count - 1 do
  begin
    if Assigned(Components[idx].ComponentClass) then
      Components[idx].SaveToSettings(APath + '/Components/' + Components[idx].ComponentClass.ClassName);
  end;
end;

procedure Tcna2Group.SetProfile(const Value: Tcna2Profile);
begin
  if Value <> FProfile then
  begin
    if Assigned(FProfile) then
      FProfile.UnregisterGroup(Self);

    if Assigned(Value) then
      Value.RegisterGroup(Self);

    FProfile := Value;
  end;
end;

procedure Tcna2Group.UnregisterComponent(AComponent: Tcna2Component);
begin
  if FindComponent(AComponent.ComponentClass, AComponent) then
    Components.Extract(AComponent);
end;

{ Tcna2Profile }

function Tcna2Profile.AddGroup(AName: String): Tcna2Group;
begin
  Result := Tcna2Group.Create(Self);
  Result.Name := AName;
end;

procedure Tcna2Profile.Clear;
begin
  while FGroups.Count > 0 do
    FGroups.First.Free;
end;

procedure Tcna2Profile.CopyContent(ASource: Tcna2Profile);
var
  idx : Integer;
begin
  if ASource <> Self then
  begin
    Clear;

    for idx := 0 to ASource.Groups.Count - 1 do
    begin
      AddGroup(ASource.Groups[idx].Name).CopyContent(ASource.Groups[idx]);
    end;
  end;
end;

constructor Tcna2Profile.Create(AProfiles : Tcna2Profiles);
begin
  FProfiles := nil;
  Profiles := AProfiles;
  FGroups := Tcna2GroupList.Create;
end;

destructor Tcna2Profile.Destroy;
begin
  Clear;
  
  if Assigned(FProfiles) then
    FProfiles.Profiles.Extract(Self);

  FGroups.Free;

  Profiles := nil;

  inherited;
end;

procedure Tcna2Profile.LoadFromSettings(APath: TSettingName);
var
  Setts : TSettingNames;
  idx : Integer;
begin
  Name := cna2Settings.GetValue(APath, Name);

  Clear;
  
  Setts := cna2Settings.GetSubNames(APath, false, true);

  for idx := Low(Setts) to High(Setts) do
  begin
    Tcna2Group.Create(self).LoadFromSettings(Setts[idx]);
  end;
end;

procedure Tcna2Profile.RegisterGroup(AGroup: Tcna2Group);
begin
  if Groups.IndexOf(AGroup) = -1 then
    Groups.Add(AGroup);
end;

procedure Tcna2Profile.SaveToSettings(APath: TSettingName);
var
  idx : Integer;
begin
  cna2Settings.SetValue(APath, Name);

  for idx := 0 to Groups.Count - 1 do
  begin
    Groups[idx].SaveToSettings(APath + '/Group' + IntToStr(idx));
  end;
end;

procedure Tcna2Profile.SetProfiles(const Value: Tcna2Profiles);
begin
  if Value <> FProfiles then
  begin
    if Assigned(FProfiles) then
      FProfiles.UnregisterProfile(Self);

    if Assigned(Value) then
      Value.RegisterProfile(Self);
      
    FProfiles := Value;
  end;
end;

procedure Tcna2Profile.UnregisterGroup(AGroup: Tcna2Group);
begin
  Groups.Extract(AGroup);
end;

{ Tcna2Component }

constructor Tcna2Component.Create(AGroup: Tcna2Group);
begin
  FGroup := nil;
  Group := AGroup;
end;

destructor Tcna2Component.Destroy;
begin
  if Assigned(FGroup) then
    FGroup.Components.Extract(Self);

  Group := nil;
    
  inherited;
end;

procedure Tcna2Component.LoadFromSettings(APath: TSettingName);
begin
  FClass := GetClass(cna2Settings.GetValue(APath, ''));
  if not Assigned(FClass) then
    Free;
end;

procedure Tcna2Component.SaveToSettings(APath: TSettingName);
begin
  cna2Settings.SetValue(APath, FClass.ClassName);
end;

procedure Tcna2Component.SetGroup(const Value: Tcna2Group);
begin
  if FGroup <> Value then
  begin
    if Assigned(FGroup) then
      FGroup.UnregisterComponent(Self);

    if Assigned(Value) then
      Value.RegisterComponent(Self);
      
    FGroup := Value;
  end;
end;

end.
