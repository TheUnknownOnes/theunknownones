unit uDelphiRemoteIDEManager;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, DelphiRemoteServer_TLB, StdVcl;

type
  TIDEManager = class(TAutoObject, IIDEManager)
  protected
    function Get_IDECount: Integer; safecall;
    function Get_IDE(Idx: Integer): IDispatch; safecall;
    procedure RegisterIDE(const IDE: IDispatch); safecall;
    procedure UnregisterIDE(const IDE: IDispatch); safecall;

  end;

implementation

uses ComServ, uGlobal, uMain, SysUtils, Dialogs;

function TIDEManager.Get_IDECount: Integer;
begin
  Result:=Global.DelphiIDEs.Count;
end;

function TIDEManager.Get_IDE(Idx: Integer): IDispatch;
begin
  Result:=Global.DelphiIDEs[idx] as IDispatch;
end;

procedure TIDEManager.RegisterIDE(const IDE: IDispatch);
begin
  Global.DelphiIDEs.Add(IDE);
end;

procedure TIDEManager.UnregisterIDE(const IDE: IDispatch);
begin
  Global.DelphiIDEs.Remove(IDE);
end;

initialization
  TAutoObjectFactory.Create(ComServer, TIDEManager, Class_IDEManager,
    ciMultiInstance, tmApartment);
end.
