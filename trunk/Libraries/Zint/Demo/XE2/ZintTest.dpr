program ZintTest;

uses
  Vcl.Forms,
  uFormZintTest in 'uFormZintTest.pas' {Form46};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm46, Form46);
  Application.Run;
end.
