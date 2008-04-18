program rss2disk;

uses
  Forms,
  uFormMain in 'uFormMain.pas' {form_Main},
  uData in 'uData.pas' {Data: TDataModule},
  uEditFeed in 'uEditFeed.pas' {form_EditFeed};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TData, Data);
  Application.CreateForm(Tform_Main, form_Main);
  Application.Run;
end.
