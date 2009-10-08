program Image2DateTime;

uses
  Forms,
  uFormMain in 'uFormMain.pas' {form_Main};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(Tform_Main, form_Main);
  Application.Run;
end.
