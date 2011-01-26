program PattEx;

uses
  Forms,
  uFormMain in 'uFormMain.pas' {formMain},
  uFrameTest in 'uFrameTest.pas' {frameTest: TFrame},
  uFormCode in 'uFormCode.pas' {formCode},
  uPattExCommon in 'uPattExCommon.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TformMain, formMain);
  Application.Run;
end.
