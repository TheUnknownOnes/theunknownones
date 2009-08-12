program SimpleRecorder;

uses
  Forms,
  uFormMain in 'uFormMain.pas' {form_Main},
  uFormSettings in 'uFormSettings.pas' {form_Settings};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(Tform_Main, form_Main);
  Application.Run;


end.
