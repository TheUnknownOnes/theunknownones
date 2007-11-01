program Project1;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  DWMApi in '..\DWMApi.pas';

{$R *.res}
{$R winxp.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
