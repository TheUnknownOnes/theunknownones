program APITranslationHelper;

uses
  Forms,
  uFormMain in 'uFormMain.pas' {form_Main},
  uFormEditExpression in 'uFormEditExpression.pas' {form_EditExpression};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(Tform_Main, form_Main);
  Application.Run;
end.
