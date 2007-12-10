library QRPreview;

uses
  ComServ {: CoClass},
  uMain in 'uMain.pas' {QRPreview: CoClass},
  ShObjIdlQuot in 'ShObjIdlQuot.pas',
  QRPreview_TLB in 'QRPreview_TLB.pas',
  uResample in 'uResample.pas';

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.TLB}

{$R *.RES}

begin
end.
