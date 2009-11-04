unit uDelphiRemoteIDEClient;

interface

uses
  Classes, ComObj, Forms, DelphiRemoteServer_TLB, ComObjWrapper,
  uDelphiRemoteIDEClientPlugin;

function GlobalDelphiRemoteIDEClient: TDelphiRemoteIDEClientPlugin;

implementation

uses
  Dialogs, ObjComAuto, Windows;
     
type
   {$TYPEINFO ON}
  {$METHODINFO ON}
  TDelphiRemoteIDEClient = class(TDelphiRemoteIDEClientPlugin)
  public
    procedure DoSomething;
  end;
   {$TYPEINFO OFF}
  {$METHODINFO OFF}
var  
  DelphiRemoteIDEClient : TDelphiRemoteIDEClient = nil;

function GlobalDelphiRemoteIDEClient: TDelphiRemoteIDEClientPlugin;
begin
  if not Assigned(DelphiRemoteIDEClient) then
    DelphiRemoteIDEClient:=TDelphiRemoteIDEClient.Create;

  Result:=DelphiRemoteIDEClient;
end;

{ TDelphiRemoteIDEClient }

procedure TDelphiRemoteIDEClient.DoSomething;
begin
  showmessage('test');
end;

var
  DelphiRemoteServerService : Service;
  DelphiRemoteIDEClientIntf : IDispatch;

initialization
  DelphiRemoteServerService:=CreateOleObject('DelphiRemoteServer.Service') as Service;
  DelphiRemoteIDEClientIntf:=GlobalDelphiRemoteIDEClient.GetDispatchInterface; // TObjectDispatch.Create(TDelphiRemoteIDEClient.Create);
  DelphiRemoteServerService.RegisterIDE(DelphiRemoteIDEClientIntf);

finalization
  try
    DelphiRemoteServerService.UnregisterIDE(DelphiRemoteIDEClientIntf);
    DelphiRemoteServerService:=nil;
    DelphiRemoteIDEClientIntf:=nil;
    GlobalDelphiRemoteIDEClient.Free;
  except
  end;
end.
