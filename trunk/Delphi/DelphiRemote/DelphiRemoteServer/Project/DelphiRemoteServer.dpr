program DelphiRemoteServer;



{%File 'DelphiRemoteServer.tlb'}

uses
  Forms,
  uMain in '..\uMain.pas' {FormMain},
  uGlobal in '..\uGlobal.pas' {Global: TDataModule},
  uService in '..\uService.pas' {Service: CoClass},
  DelphiRemoteServer_TLB in 'DelphiRemoteServer_TLB.pas';

{$R *.TLB}

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TGlobal, Global);
  Application.ShowMainForm:=False;
  Application.Run;
end.
