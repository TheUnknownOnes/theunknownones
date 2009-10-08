library IBTransactionCheck;

uses
  ComServ,
  IBTransactionCheck_TLB in 'IBTransactionCheck_TLB.pas',
  uIBTransactionCheck in 'uIBTransactionCheck.pas' {TIBTransactionCheck: CoClass},
  uData in 'uData.pas' {Data: TDataModule},
  uFormSettings in 'uFormSettings.pas' {form_Settings};

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.TLB}

{$R *.RES}

begin
end.
