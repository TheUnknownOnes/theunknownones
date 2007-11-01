program KeybLEDDemo_D70;

uses
  madExcept,
  madLinkDisAsm,
  madListProcesses,
  madListModules,
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  KeyboardLED_D70 in '..\KeyboardLED_D70.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
