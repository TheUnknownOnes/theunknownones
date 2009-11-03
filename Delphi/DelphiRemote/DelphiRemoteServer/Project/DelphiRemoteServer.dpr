program DelphiRemoteServer;



{%File 'DelphiRemoteServer.tlb'}

uses
  Forms,
  uMain in '..\uMain.pas' {Form9},
  uGlobal in '..\uGlobal.pas' {Global: TDataModule},
  uService in '..\uService.pas' {Service: CoClass},
  DelphiRemoteServer_TLB in 'DelphiRemoteServer_TLB.pas';

{$R *.TLB}

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm9, Form9);
  Application.CreateForm(TGlobal, Global);
  Application.Run;
end.
