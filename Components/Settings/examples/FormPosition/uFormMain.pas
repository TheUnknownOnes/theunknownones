unit uFormMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uSettingsBase, uSettingsXML, uSettingsLinksDefault;

type
  TForm1 = class(TForm)
    SettingsLinkForm1: TSettingsLinkForm;
    SettingsXMLFile1: TSettingsXMLFile;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SettingsXMLFile1.Save;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  SettingsXMLFile1.FileName := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) +
                               'Settings.xml';

  if FileExists(SettingsXMLFile1.FileName) then
    SettingsXMLFile1.Load;
    
end;

end.
