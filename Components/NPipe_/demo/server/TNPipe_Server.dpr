program TNPipe_Server;

uses
  Forms,
  unit_Main in 'unit_Main.pas' {Form4};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm4, Form4);
  Application.Run;
end.
