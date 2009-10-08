library IMAPMailCheck;

uses
  ComServ,
  IMAPMailCheck_TLB in 'IMAPMailCheck_TLB.pas',
  uIMAPMailChecker in 'uIMAPMailChecker.pas' {IMAPMailChecker: CoClass},
  uDlgConfig in 'uDlgConfig.pas' {form_Config},
  uData in 'uData.pas' {Data: TDataModule};

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.TLB}

{$R *.RES}

begin
end.
