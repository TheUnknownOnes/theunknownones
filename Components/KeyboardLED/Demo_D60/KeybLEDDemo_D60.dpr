program KeybLEDDemo_D60;

uses
  madExcept,
  madLinkDisAsm,
  madListProcesses,
  madListModules,
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  KeyboardLED_D60 in '..\KeyboardLED_D60.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
