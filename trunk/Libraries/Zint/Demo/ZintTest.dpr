program ZintTest;

uses
  Vcl.Forms,
  uFormZintTest in 'uFormZintTest.pas' {Form46},
  zint._library in '..\zint._library.pas',
  zint.bmp in '..\zint.bmp.pas',
  zint.zint in '..\zint.zint.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm46, Form46);
  Application.Run;
end.
