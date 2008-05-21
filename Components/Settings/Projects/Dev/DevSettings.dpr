program DevSettings;

uses
  Forms,
  uFormMain in 'uFormMain.pas' {Form1},
  uSettingsBase in '..\..\uSettingsBase.pas',
  uSettingsStream in '..\..\uSettingsStream.pas',
  uSettingsCompLinksDefault in '..\..\uSettingsCompLinksDefault.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
