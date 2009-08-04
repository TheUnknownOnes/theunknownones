unit uIBTransactionCheck;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, IBTransactionCheck_TLB, StdVcl, Dialogs;

type
  TTIBTransactionCheck = class(TAutoObject, ITIBTransactionCheck)
  protected
    function Get_Transactions: WideString; safecall;
    procedure Shutdown(Attachment: Integer); safecall;
    procedure ShowOptionsDialog; safecall;
    function Get_RefreshInterval: Integer; safecall;

  end;

implementation

uses ComServ, uData;

function TTIBTransactionCheck.Get_Transactions: WideString;
begin
  Result := Data.GetTransactions;
end;

procedure TTIBTransactionCheck.Shutdown(Attachment: Integer);
begin
  Data.Shutdown(Attachment);
end;

procedure TTIBTransactionCheck.ShowOptionsDialog;
begin
  Data.ShowOptionsDialog;
end;

function TTIBTransactionCheck.Get_RefreshInterval: Integer;
begin
  Result := Data.RefreshInterval;
end;

initialization
  TAutoObjectFactory.Create(ComServer, TTIBTransactionCheck, Class_TIBTransactionCheck,
    ciMultiInstance, tmApartment);
end.
