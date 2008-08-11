program Partshot;

uses
  Forms,
  uFormMain in 'uFormMain.pas' {form_Main},
  uFormShooter in 'uFormShooter.pas' {form_Shooter};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(Tform_Main, form_Main);
  Application.Run;
end.
