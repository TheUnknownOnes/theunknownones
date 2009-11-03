unit uPluginTest1;

interface

uses uDelphiRemoteIDEClient, uDelphiRemoteIDEClientPlugin;

type
  ttestrecord = record
    a : string;
    b : integer;
  end;
  TTestPlugin = class(TDelphiRemoteIDEClientPlugin)
  public
    function Abc: ttestrecord;
  end;

implementation

uses
  Dialogs;

{ TTestPlugin }

function TTestPlugin.Abc: ttestrecord;
begin
  result.a:='abc';
  result.b:=55;
end;

initialization
  GlobalDelphiRemoteIDEClient.RegisterChild('testPlugin', TTestPlugin.Create);

end.
