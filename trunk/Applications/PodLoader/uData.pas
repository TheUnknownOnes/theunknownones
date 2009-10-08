unit uData;

interface

uses
  SysUtils, Classes, uSettingsBase, uSettingsStream,
  uSysTools, ShlObj, ImageHlp, Dialogs, uSettingsXML,
  ComObj, ActiveX, JwaBits;

type
  TData = class(TDataModule)
    Settings: TSettingsFile;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    Bits : IBackgroundCopyManager;
  end;

var
  Data: TData;

implementation

{$R *.dfm}

procedure TData.DataModuleCreate(Sender: TObject);
var
  SettingsPath : String;
begin
  if not Succeeded(CoCreateInstance(CLSID_BackgroundCopyManager,
                                    nil,
                                    CLSCTX_LOCAL_SERVER,
                                    IID_IBackgroundCopyManager,
                                    Bits)) then
    raise Exception.Create('Can not connect to BITS. Is the service running?');


  SettingsPath := IncludeTrailingPathDelimiter(GetShellFolder(CSIDL_APPDATA)) + 'TUO\PodLoader\';

  if not MakeSureDirectoryPathExists(PChar(SettingsPath)) then
    raise Exception.Create('Can create settings path');

  Settings.FileName := SettingsPath + 'settings.bin';

  if FileExists(Settings.FileName) then
    if not Settings.Load then
      raise Exception.Create('Can not load settings');
end;

procedure TData.DataModuleDestroy(Sender: TObject);
begin
  if not Settings.Save then
    MessageDlg('Can not save settings', mtError, [mbOK], 0);

  Bits := nil;
end;

initialization
  CoInitFlags := COINIT_APARTMENTTHREADED;

end.
