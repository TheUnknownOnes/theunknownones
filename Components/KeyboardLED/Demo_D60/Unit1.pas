//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Math, KeyboardLED_D60;

type
  TForm1 = class(TForm)
    btn_Button2: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    Reset: TButton;
    procedure ResetClick(Sender: TObject);
    procedure btn_Button2Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}


procedure TForm1.btn_Button2Click(Sender: TObject);
var
  LEDState: TKeyboardLEDStates;
begin
  LEDState:=[];

  if CheckBox1.Checked then
    LEDState:=LEDState + [klsScrollLock];

  if CheckBox2.Checked then
    LEDState:=LEDState + [klsCAPSLock];

  if CheckBox3.Checked then
    LEDState:=LEDState + [klsNUMLock];

  SetKeyboardLED(LEDState);
end;


procedure TForm1.ResetClick(Sender: TObject);
begin
  ResetKeyboardLED;
end;

end.
