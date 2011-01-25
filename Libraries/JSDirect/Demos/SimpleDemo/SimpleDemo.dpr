program SimpleDemo;

uses
  uMainApp in 'uMainApp.pas',
  uJSDirect, Windows;

{$R *.res}

var
  jsdServer : TjsdServer;

begin
  RegisterJSDApplication('TWSDemoSimple', TmyApp);

  jsdServer:=TjsdServer.Create();
  jsdServer.DefaultPort:=2680;   // our default port... you may change this
  jsdServer.Active:=True;

  writeln('TWSDemoSimple is now running');
  writeln('Kindly provided by TheUnkownOnes.net ;-)');

  while true do
  begin
    // We will not do anything in the main loop
    // everything will be handled in the TjsApplication instance
    // this is only a quick and dirty solution :-)
    // for production you should use a service here
    sleep(10000);
  end;
end.
