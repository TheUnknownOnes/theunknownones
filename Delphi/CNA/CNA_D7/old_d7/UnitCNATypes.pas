//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit UnitCNATypes;

interface
uses
  Classes, SysUtils, StrUtils, Dialogs;

type

  TCNAComponent = class(TObject)
  private
    FClassName    : String; //the real classname; example: TButton
    FObjectName   : String; //the Name of a Object; example: Button
  public
    constructor Create(ClassName: String);
    destructor Destroy(); override;
  published
    property CNAClassName  : String read FClassName;
    property ObjectName : String read FObjectName;
  end;

  TCNAComponentGroup = class(TObject)
  private
    FName       : String; //The Name of the group
    FComponents,
    FPropertiesAvail,
    FPropertiesSet    : TStrings;
    FPrefix       : String;
    FSuffix       : String;

    function GeneratePropertyList(Force : Boolean=false) : Boolean;
    {Results: False=>will reduce properties}
  public
    constructor Create(GroupName : String);
    destructor Destroy(); override;

    function GetComponent(AClassName : String) : TCNAComponent; overload;
    function GetComponent(Index : Integer) : TCNAComponent; overload;
  published
    property GroupName : String read FName;
    property PropertiesAvailable : TStrings read FPropertiesAvail;
    property PropertiesSet       : TStrings read FPropertiesSet write FPropertiesSet;
    property Prefix : String read FPrefix write FPrefix;
    property Suffix : String read FSuffix write FSuffix;

    function AddComponent(AClassName : String; Force : Boolean=false) : Boolean;
    {Results: False=>will reduce properties}
    procedure DeleteComponent(_AClassName : String);
    function GetComponentCount() : Integer;
  end;

  TCNAProfile = class(TObject)
  private
    FName : String;
    FGroups : TStrings;
  public
    constructor Create(ProfileName : String);
    destructor Destroy(); override;

    function GetGroup(GroupName : String) : TCNAComponentGroup; overload;
    function GetGroup(Index : Integer) : TCNAComponentGroup; overload;
  published
    property ProfileName : String read FName;

    function AddGroup(GroupName : String) : Boolean;
    function GetGroupCount() : Integer;
    procedure DeleteGroup(GroupName : String);
    procedure RenameGroup(OldName : String; NewName : String);
  end;

  TCNAProfiles = class(TObject)
  private
    FProfiles : TStrings;
  public
    constructor Create();
    destructor Destroy(); override;

    function GetProfile(ProfileName : String) : TCNAProfile; overload;
    function GetProfile(Index : Integer) : TCNAProfile; overload;
  published
    
    procedure AddProfile(ProfileName : String);
    function GetProfileCount() : Integer;
    procedure DeleteProfile(ProfileName : String);
    procedure RenameProfile(OldName : String; NewName : String);
  end;

  TCNASettings = record
    ExpertActive : Boolean;
    UseNA : Boolean;
    NAPopupIfAssigned : Boolean;
    NACreateName : Boolean;
    NADelimiter : String;
    UsePA : Boolean;
    PAShowDLG : Boolean;
    Profiles : TCNAProfiles;
    curProfile : TCNAProfile;
  end;

  PCNASettings = ^TCNASettings;

implementation

uses UnitCNAMain;

{ TCNAComponent }

constructor TCNAComponent.Create(ClassName: String);
begin
  FClassName:=ClassName;

  if (AnsiStartsText('T',ClassName)) then
    FObjectName:=RightStr(ClassName,Length(ClassName)-1)
  else
    FObjectName:=ClassName;
end;

destructor TCNAComponent.Destroy;
begin

  inherited;
end;

{ TCNAComponentGroup }

function TCNAComponentGroup.AddComponent(AClassName: String; Force: Boolean): Boolean;
var
  NewComponent : TCNAComponent;
begin
  Result:=True;
  if (FComponents.IndexOfName(AClassName)>-1) then
    exit;

  NewComponent:=TCNAComponent.Create(AClassName);
  FComponents.AddObject(AClassName+'=',NewComponent);

  if (not GeneratePropertyList(Force)) then
  begin
    Result:=false;
    DeleteComponent(AClassName);
  end;
end;

constructor TCNAComponentGroup.Create(GroupName: String);
begin
  FName:=GroupName;

  FComponents:=TStringList.Create;
  FPropertiesAvail:=TStringList.Create;
  FPropertiesSet:=TStringList.Create;
end;

destructor TCNAComponentGroup.Destroy;
var
  i : Integer;
begin

  for i:=FComponents.Count-1 downto 0 do
    TCNAComponent(FComponents.Objects[i]).Free;
  FComponents.Free;

  FPropertiesAvail.Free;
  FPropertiesSet.Free;
  inherited;
end;

function TCNAComponentGroup.GetComponentCount: Integer;
begin
  Result:=FComponents.Count;
end;

function TCNAComponentGroup.GetComponent(AClassName: String): TCNAComponent;
var
  Index : Integer;
begin
  Index:=FComponents.IndexOfName(AClassName);
  if (Index>-1) then
    Result:=TCNAComponent(FComponents.Objects[Index])
  else
    Result:=nil;
end;

procedure TCNAComponentGroup.DeleteComponent(_AClassName: String);
var
  i : Integer;
begin
  i:=FComponents.IndexOfName(_AClassName);
  if (i>-1) then
  begin
    FComponents.Objects[i].Free;
    FComponents.Delete(i);
    GeneratePropertyList(true);
  end;
end;

function TCNAComponentGroup.GeneratePropertyList(Force: Boolean): Boolean;
var
  CompI : Integer;
  PropI : Integer;
  tmpProps : Tstrings;
  CompProps : TStrings;
begin
  Result:=true;

  if (FComponents.Count=0) then
  begin
    FPropertiesAvail.Clear;
    exit;
  end;

  tmpProps:=TStringList.Create;
  CompProps:=TStringList.Create;
  FindProperties(TCNAComponent(FComponents.Objects[0]).CNAClassName,CompProps);

  tmpProps.Assign(CompProps);

  for Compi:=1 to FComponents.Count-1 do
	begin
    CompProps.Clear;
    FindProperties(TCNAComponent(FComponents.Objects[CompI]).CNAClassName,CompProps);
    for PropI:=tmpProps.Count-1 downto 0 do
    begin
      if (CompProps.IndexOfName(tmpProps.Names[PropI])=-1) then
      begin
        if (not Force) then
        begin
          Result:=false;
          break;
        end
        else
          tmpProps.Delete(PropI);
      end;
    end;
    if (not Result) then break;
	end;

  if (Result) then
    FPropertiesAvail.Assign(tmpProps);
    
  tmpProps.Free;
  CompProps.Free;
end;

function TCNAComponentGroup.GetComponent(Index: Integer): TCNAComponent;
begin
  if (Index>-1) and (Index<FComponents.Count) then
    Result:=TCNAComponent(FComponents.Objects[Index])
  else
    Result:=nil;
end;

{ TCNAProfile }

function TCNAProfile.AddGroup(GroupName: String): Boolean;
var
  NewGroup : TCNAComponentGroup;
begin
  Result:=false;
  if (FGroups.IndexOfName(GroupName)>-1) then
    exit;
  NewGroup:=TCNAComponentGroup.Create(GroupName);
  FGroups.AddObject(GroupName+'=',NewGroup);
  Result:=true;
end;

constructor TCNAProfile.Create(ProfileName: String);
begin
  FGroups:=TStringList.Create;
  FName:=ProfileName;
end;

procedure TCNAProfile.DeleteGroup(GroupName: String);
var
  i :Integer;
begin
  i:=FGroups.IndexOfName(GroupName);
  if (i>-1) then
  begin
    TCNAComponentGroup(FGroups.Objects[i]).Free;
    FGroups.Delete(i);
  end;
end;

destructor TCNAProfile.Destroy;
var
  i : Integer;
begin
  for i:=FGroups.Count-1 downto 0 do
    TCNAComponentGroup(FGroups.Objects[i]).Free;
  FGroups.Free;
  inherited;
end;

function TCNAProfile.GetGroup(GroupName: String): TCNAComponentGroup;
var
  i : Integer;
begin
  Result:=nil;
  i:=FGroups.IndexOfName(GroupName);
  if (i>-1) then
    Result:=TCNAComponentGroup(FGroups.Objects[i]);
end;

function TCNAProfile.GetGroup(Index: Integer): TCNAComponentGroup;
begin
  if (Index>-1) and (Index<FGroups.Count) then
    Result:=TCNAComponentGroup(FGroups.Objects[Index])
  else
    Result:=nil;
end;

function TCNAProfile.GetGroupCount: Integer;
begin
  Result:=FGroups.Count;
end;

procedure TCNAProfile.RenameGroup(OldName, NewName: String);
var
  i : Integer;
begin
  i:=FGroups.IndexOfName(OldName);
  if (i>-1) then
  begin
    FGroups[i]:=NewName+'=';
    TCNAComponentGroup(FGroups.Objects[i]).FName:=NewName;
  end;
end;

{ TCNAProfiles }

constructor TCNAProfiles.Create;
begin
  FProfiles:=TStringList.Create;
end;

function TCNAProfiles.GetProfile(ProfileName: String): TCNAProfile;
var
  i : Integer;
begin
  i:=FProfiles.IndexOf(ProfileName);
  if (i>-1) then
    Result:=TCNAProfile(FProfiles.Objects[i])
  else
    Result:=nil;
end;

procedure TCNAProfiles.DeleteProfile(ProfileName: String);
var
  i : Integer;
begin
  i:=FProfiles.IndexOf(ProfileName);
  if (i>-1) then
  begin
    TCNAProfile(FProfiles.Objects[i]).Free;
    FProfiles.Delete(i);
  end;
end;

function TCNAProfiles.GetProfileCount: Integer;
begin
  Result:=FProfiles.Count;
end;

procedure TCNAProfiles.AddProfile(ProfileName: String);
begin
  FProfiles.AddObject(ProfileName,TCNAProfile.Create(ProfileName));
end;

destructor TCNAProfiles.Destroy;
var
  i : Integer;
begin
  for i:=FProfiles.Count-1 downto 0 do
  begin
    TCNAProfile(FProfiles.Objects[i]).Free;
    FProfiles.Delete(i);
  end;
  FProfiles.Free;
  inherited;
end;

function TCNAProfiles.GetProfile(Index: Integer): TCNAProfile;
begin
  if (Index>-1) and (Index<FProfiles.Count) then
    Result:=TCNAProfile(FProfiles.Objects[Index])
  else
    Result:=nil;
end;

procedure TCNAProfiles.RenameProfile(OldName, NewName: String);
var
  i : Integer;
begin
  i:=FProfiles.IndexOf(OldName);
  if (i>-1) then
  begin
    FProfiles[i]:=NewName;
    TCNAProfile(FProfiles.Objects[i]).FName:=NewName;
  end;
end;

end.
