unit uDelphiRemoteIDEClient;

interface

uses
  Classes, ComObj, Forms, DelphiRemoteServer_TLB, ComObjWrapper;

implementation

uses
  Dialogs, ObjComAuto, Windows;


type
  {$TYPEINFO ON}
  {$METHODINFO ON}
  TDelphiRemoteIDEClient = class(TInterfacedObject)
  public

  published

  end;
  {$TYPEINFO OFF}
  {$METHODINFO OFF}

var
  DelphiRemoteServerService : Service;
  DelphiRemoteIDEClientIntf : IDispatch;

{ TDelphiRemoteIDEClient }

initialization
  DelphiRemoteServerService:=CreateOleObject('DelphiRemoteServer.Service') as Service;
  DelphiRemoteIDEClientIntf:=TObjectDispatch.Create(TDelphiRemoteIDEClient.Create);
  DelphiRemoteServerService.RegisterIDE(DelphiRemoteIDEClientIntf);

finalization
  try
    DelphiRemoteServerService.UnregisterIDE(DelphiRemoteIDEClientIntf);
    DelphiRemoteServerService:=nil;
    DelphiRemoteIDEClientIntf:=nil;
  except
  end;
end.
