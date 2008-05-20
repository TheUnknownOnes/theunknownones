program DevSettings;

uses
  Forms,
  uFormMain in 'uFormMain.pas' {Form1},
  uSettings in '..\..\uSettings.pas',
  uSettingsStream in '..\..\uSettingsStream.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
