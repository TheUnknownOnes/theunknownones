program ZintTest;

uses
  Forms,
  uFormZintTest in 'uFormZintTest.pas' {Form46};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown:=True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm46, Form46);
  Application.Run;
end.
