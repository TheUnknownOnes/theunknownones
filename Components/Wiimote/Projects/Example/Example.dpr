program Example;

uses
  Forms,
  uFormMain in 'uFormMain.pas' {Form1},
  uWiimote in '..\..\uWiimote.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
