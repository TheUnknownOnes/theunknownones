program KeybLEDDemo_D90;



uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  KeyboardLED_D90 in '..\KeyboardLED_D90.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
