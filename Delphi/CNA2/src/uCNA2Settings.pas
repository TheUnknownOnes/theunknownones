unit uCNA2Settings;

interface

uses
  uSettingsBase,
  uSettingsXML,
  ToolsAPI,
  SysUtils,
  Dialogs,
  Classes,
  ShlObj,
  uSysTools,
  ImageHlp,
  uRTTIHelper,
  Variants,
  StdCtrls;

type
  Tcna2Settings = class(TSettingsXMLFile)
  private
    procedure EnsureMinimalContent;
    function GetCurrentProfile: TSettingName;
    procedure SetCurrentProfile(const Value: TSettingName);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetProfiles : TSettingNames;
    function GetGroups(AProfilePath : TSettingName) : TSettingNames;
    function GetComponents(AGroupPath : TSettingName) : TSettingNames;

    function AddProfile(AName : String) : TSettingName;
    function AddGroup(AProfilePath : TSettingName; AName : String) : TSettingName;
    function AddComponent(AGroupPath : TSettingName; AComponent : TClass) : TSettingName;

    property CurrentProfile : TSettingName read GetCurrentProfile write SetCurrentProfile;
  end;

var
  cna2Settings : Tcna2Settings;

implementation

const
  SettingsFileName = 'CNA2.xml';


{ Tcna2Settings }

function Tcna2Settings.AddComponent(AGroupPath: TSettingName;
  AComponent: TClass): TSettingName;
begin
  Result := AGroupPath + '/Components/' + rttihGetUnit(AComponent) + '.' + AComponent.ClassName;
  if not Exists(Result) then
    SetValue(Result, AComponent.ClassName + ' (' + rttihGetUnit(AComponent) + ')');
end;

function Tcna2Settings.AddGroup(AProfilePath: TSettingName;
  AName: String): TSettingName;
var
  idx : Integer;
begin
  idx := 0;

  while idx < MaxInt do
  begin
    Result := AProfilePath + '/Group' + IntToStr(idx);
    if Exists(Result) then
      Inc(idx)
    else
      break;
  end;

  SetValue(Result, AName);
end;

function Tcna2Settings.AddProfile(AName: String): TSettingName;
var
  idx : Integer;
begin
  idx := 0;

  while idx < MaxInt do
  begin
    Result := '/Profiles/Profile' + IntToStr(idx);
    if Exists(Result) then
      Inc(idx)
    else
      break;
  end;

  SetValue(Result, AName);
end;

constructor Tcna2Settings.Create(AOwner: TComponent);
var
  CNA2Dir : String;
begin
  inherited;

  CNA2Dir := GetEnvironmentVariable('CNA2Dir');

  if DirectoryExists(CNA2Dir) then
    FileName := IncludeTrailingPathDelimiter(CNA2Dir) + SettingsFileName
  else
    FileName := IncludeTrailingPathDelimiter(GetShellFolder(CSIDL_APPDATA)) + 'CNA2\' + SettingsFileName;

  if FileExists(FileName) then
    Load;

  EnsureMinimalContent;
end;

destructor Tcna2Settings.Destroy;
begin
  try
    MakeSureDirectoryPathExists(PChar(ExtractFilePath(FileName)));
    if not Save then
      raise Exception.Create(EmptyStr); 
  except
    MessageDlg('CNA2-Settings could not be saved!', mtWarning, [mbOK], 0);
  end;
  

  inherited;
end;

procedure Tcna2Settings.EnsureMinimalContent;
var
  Group,
  Profile : TSettingName;
begin
  if Length(GetProfiles) = 0 then
  begin
    Profile := AddProfile('DefaultProfile');
    Group := AddGroup(Profile, 'Buttons');
    AddComponent(Group, TButton);
    CurrentProfile := Profile;
  end;
end;

function Tcna2Settings.GetComponents(AGroupPath: TSettingName): TSettingNames;
begin
  Result := GetSubNames(AGroupPath + '/Components', true, true);
end;

function Tcna2Settings.GetCurrentProfile: TSettingName;
begin
  Result := GetValue('/CurrentProfile', EmptyWideStr);
end;

function Tcna2Settings.GetGroups(AProfilePath: TSettingName): TSettingNames;
begin
  Result := GetSubNames(AProfilePath, false, true);
end;

function Tcna2Settings.GetProfiles: TSettingNames;
begin
  Result := GetSubNames('/Profiles', false, true);
end;

procedure Tcna2Settings.SetCurrentProfile(const Value: TSettingName);
begin
  SetValue('/CurrentProfile', Value);
end;

end.

{
Profiles
  Profile1 = 'Default'
  Profile2 = 'MyProfile'
    Group1
    Group2
      Components
        UnitName1
          ClassName1
CurrentProfile = '/Profiles/Profile2'      
      
}
