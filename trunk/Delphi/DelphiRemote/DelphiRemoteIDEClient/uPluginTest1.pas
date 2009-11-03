unit uPluginTest1;

interface

uses uDelphiRemoteIDEClient, uDelphiRemoteIDEClientPlugin;

type
  TTestPlugin = class(TDelphiRemoteIDEClientPlugin)
  public
    procedure Abc;
  end;

implementation

uses
  Dialogs;

{ TTestPlugin }

procedure TTestPlugin.Abc;
begin
  showmessage('abc');
end;

var
  myPlugin: TTestPlugin;

initialization
  myPlugin:=TTestPlugin.Create;
  GlobalDelphiRemoteIDEClient.RegisterChild('testPlugin', myPlugin);

finalization
  GlobalDelphiRemoteIDEClient.UnregisterChild(myPlugin);
  myPlugin.Free;

end.
