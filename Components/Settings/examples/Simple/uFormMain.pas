unit uFormMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uSettingsBase, uSettingsStream;

type
  TForm1 = class(TForm)
    SettingsFile1: TSettingsFile;
    procedure FormCreate(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  AppDir : String;
begin
  AppDir := ExtractFilePath(Application.ExeName);

  SettingsFile1.FileName := IncludeTrailingPathDelimiter(AppDir) + 'settings.dat';

  if FileExists(SettingsFile1.FileName) and SettingsFile1.Load then
  begin
    MessageDlg(vartostr(SettingsFile1.GetValue('/Application/LastStart', 'unknown')), mtWarning, [mbOK], 0);
  end;

  SettingsFile1.SetValue('/Application/LastStart', Now);

  if not SettingsFile1.Save then
    raise Exception.Create('Could not save settings');
end;

end.
