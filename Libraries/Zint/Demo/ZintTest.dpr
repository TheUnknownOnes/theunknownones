program ZintTest;

uses
  Vcl.Forms,
  uFormZintTest in 'uFormZintTest.pas' {Form46},
  zint._2of5 in '..\zint._2of5.pas',
  zint.auspost in '..\zint.auspost.pas',
  zint.aztec in '..\zint.aztec.pas',
  zint.code in '..\zint.code.pas',
  zint.code16k in '..\zint.code16k.pas',
  zint.code49 in '..\zint.code49.pas',
  zint.code128 in '..\zint.code128.pas',
  zint.common in '..\zint.common.pas',
  zint.dmatrix in '..\zint.dmatrix.pas',
  zint.gs1 in '..\zint.gs1.pas',
  zint.maxicode in '..\zint.maxicode.pas',
  zint.medical in '..\zint.medical.pas',
  zint.reedsol in '..\zint.reedsol.pas',
  zint.metafile in '..\zint.metafile.pas',
  zint.zint in '..\zint.zint.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm46, Form46);
  Application.Run;
end.
