program PodLoader;

uses
  Forms,
  uFormMain in 'uFormMain.pas' {form_Main},
  uData in 'uData.pas' {Data: TDataModule},
  uFormEditCast in 'uFormEditCast.pas' {form_EditCast};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TData, Data);
  Application.CreateForm(Tform_Main, form_Main);
  Application.Run;
end.
