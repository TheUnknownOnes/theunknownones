//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
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
  StdCtrls,
  StrUtils,
  WideStrUtils;

type
  Tcna2Settings = class(TSettingsXMLFile)
  private
    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);
  published
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SaveNow;

    property ExpertActive : Boolean read GetActive write SetActive;
  end;

var
  cna2Settings : Tcna2Settings;

implementation

const
  SettingsFileName = 'CNA2.xml';


{ Tcna2Settings }

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

end;

destructor Tcna2Settings.Destroy;
begin
  SaveNow;

  inherited;
end;


function Tcna2Settings.GetActive: Boolean;
begin
  Result := GetValue('/ExpertActive', true);
end;

procedure Tcna2Settings.SaveNow;
begin
  try
    MakeSureDirectoryPathExists(PAnsiChar(ExtractFilePath(FileName)));
    if not Save then
      raise Exception.Create(EmptyStr);
  except
    MessageDlg('CNA2-Settings could not be saved!', mtError, [mbOK], 0);
  end;
end;

procedure Tcna2Settings.SetActive(const Value: Boolean);
begin
  SetValue('/ExpertActive', Value);
end;

initialization
  cna2Settings := nil;

end.

(*
Profiles
  Profile1 = 'Default'
  Profile2 = 'MyProfile'
    Group1 = 'Group1'
    Group2 = 'Group2'
      Components
        ClassName1 = 'Displayname'
      Actions
        Property1 = 'Font.Color'
          Class = 'Tcna2ActSetValue'
          ActionData
            ActionData1 = 'foobar'
            ActionDataX = '...'
CurrentProfile = '/Profiles/Profile2'
ExpertActive = 'true'
*)
