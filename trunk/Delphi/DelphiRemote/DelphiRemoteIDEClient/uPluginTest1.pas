unit uPluginTest1;

interface

uses uDelphiRemoteIDEClient, uDelphiRemoteIDEClientPlugin, Forms;

type
  ttestrecord = record
    a : string;
    b : integer;
  end;

  {$TYPEINFO ON}
  {$METHODINFO ON}
  TExposedApp = class(TApplication)

  end;
  {$METHODINFO OFF}


  TTestPlugin = class(TDelphiRemoteIDEClientPlugin)
  public
    function Abc: ttestrecord;
    function GetApp: TExposedApp;
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

function TTestPlugin.GetApp: TExposedApp;
begin
  Result:=TExposedApp(Application);
end;

initialization
  GlobalDelphiRemoteIDEClient.RegisterChild('testPlugin', TTestPlugin.Create);

end.
