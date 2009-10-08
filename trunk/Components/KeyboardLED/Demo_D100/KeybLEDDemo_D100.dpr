program KeybLEDDemo_D100;



uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  KeyboardLED_D100 in '..\KeyboardLED_D100.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
