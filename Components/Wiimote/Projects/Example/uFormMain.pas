unit uFormMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uWiimote, StdCtrls, ExtCtrls, pngimage, uEffectPNGImage, uEffectPNG,
  ComCtrls, TeEngine, Series, TeeProcs, Chart;

type
  TForm1 = class(TForm)
    ListBox1: TListBox;
    btn_Connnect: TButton;
    Wiimote1: TWiimote;
    pb_Battery: TProgressBar;
    tm_Battery: TTimer;
    tm_Leds: TTimer;
    GroupBox1: TGroupBox;
    Image1: TImage;
    img_ButtonA: TEffectPNGImage;
    img_ButtonMinus: TEffectPNGImage;
    img_ButtonPlus: TEffectPNGImage;
    img_ButtonHome: TEffectPNGImage;
    img_Button1: TEffectPNGImage;
    img_Button2: TEffectPNGImage;
    img_CrossDown: TEffectPNGImage;
    img_CrossLeft: TEffectPNGImage;
    img_CrossUp: TEffectPNGImage;
    img_CrossRight: TEffectPNGImage;
    img_CrossCenter: TEffectPNGImage;
    pan_Led1: TPanel;
    pan_Led2: TPanel;
    pan_Led3: TPanel;
    pan_Led4: TPanel;
    pb_IRPoints: TProgressBar;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure btn_ConnnectClick(Sender: TObject);
    procedure Wiimote1Connected(Sender: TObject);
    procedure Wiimote1Disconnected(Sender: TObject);
    procedure Wiimote1ButtonDown(AButton: TwmButton);
    procedure Wiimote1ButtonUp(AButton: TwmButton);
    procedure tm_BatteryTimer(Sender: TObject);
    procedure Wiimote1Status(Sender: TObject);
    procedure tm_LedsTimer(Sender: TObject);
    procedure FormClick(Sender: TObject);
    procedure Wiimote1NewReport(const AReport: TwmReport);
  private
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btn_ConnnectClick(Sender: TObject);
begin
  if Wiimote1.Connected then
    Wiimote1.Disconnect
  else
  begin
    Wiimote1.Connect(ListBox1.Items[ListBox1.ItemIndex]);
    Wiimote1.IRSensitivity := wmiSense5;
    Wiimote1.IRMode := wmiExtended;
  end;

end;

procedure TForm1.FormClick(Sender: TObject);
begin
  Wiimote1.IRMode := wmiOff;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Randomize;

  ReportMemoryLeaksOnShutdown := true;

  TwmDeviceConnection.ListDevices(ListBox1.Items);

  ListBox1.ItemIndex := 0;

  
end;

procedure TForm1.tm_BatteryTimer(Sender: TObject);
begin
  Wiimote1.RequestStatus;
end;

procedure TForm1.tm_LedsTimer(Sender: TObject);
begin
  if Random(3) = 1 then
    Wiimote1.LedsOn := Wiimote1.LedsOn + [wmLED1]
  else
    Wiimote1.LedsOn := Wiimote1.LedsOn - [wmLED1];

  if Random(3) = 1 then
    Wiimote1.LedsOn := Wiimote1.LedsOn + [wmLED2]
  else
    Wiimote1.LedsOn := Wiimote1.LedsOn - [wmLED2];

  if Random(3) = 1 then
    Wiimote1.LedsOn := Wiimote1.LedsOn + [wmLED3]
  else
    Wiimote1.LedsOn := Wiimote1.LedsOn - [wmLED3];

  if Random(3) = 1 then
    Wiimote1.LedsOn := Wiimote1.LedsOn + [wmLED4]
  else
    Wiimote1.LedsOn := Wiimote1.LedsOn - [wmLED4];
end;

procedure TForm1.Wiimote1ButtonDown(AButton: TwmButton);
var
  img : TEffectPNGImage;
begin
  case AButton of
    wmbA: img := img_ButtonA;
    wmbB: img := nil;
    wmbPlus: img := img_ButtonPlus;
    wmbMinus: img := img_ButtonMinus;
    wmbHome: img := img_ButtonHome;
    wmbOne: img := img_Button1;
    wmbTwo: img := img_Button2;
    wmbUp: img := img_CrossUp;
    wmbDown: img := img_CrossDown;
    wmbLeft: img := img_CrossLeft;
    wmbRight: img := img_CrossRight;
  end;

  if Assigned(img) then
    img.Effects.Effect := peInvert;

  if AButton = wmbB then
    Wiimote1.Rumble := true;
end;

procedure TForm1.Wiimote1ButtonUp(AButton: TwmButton);
var
  img : TEffectPNGImage;
begin
  case AButton of
    wmbA: img := img_ButtonA;
    wmbB: img := nil;
    wmbPlus: img := img_ButtonPlus;
    wmbMinus: img := img_ButtonMinus;
    wmbHome: img := img_ButtonHome;
    wmbOne: img := img_Button1;
    wmbTwo: img := img_Button2;
    wmbUp: img := img_CrossUp;
    wmbDown: img := img_CrossDown;
    wmbLeft: img := img_CrossLeft;
    wmbRight: img := img_CrossRight;
  end;

  if Assigned(img) then
    img.Effects.Effect := peNothing;

  if AButton = wmbB then
    Wiimote1.Rumble := false;
end;

procedure TForm1.Wiimote1Connected(Sender: TObject);
begin
  btn_Connnect.Caption := 'Disconnect';
end;

procedure TForm1.Wiimote1Disconnected(Sender: TObject);
begin
  btn_Connnect.Caption := 'Connect';
end;

procedure TForm1.Wiimote1NewReport(const AReport: TwmReport);
var
  CP : Tpoint;
begin
  pb_IRPoints.Position := Wiimote1.IRPointCount;

  {GetCursorPos(CP);
  if Abs(Wiimote1.AccelX) > 0.3 then
    cp.X := cp.X + Round(Wiimote1.AccelX * 10);
  if Abs(Wiimote1.AccelZ) > 0.3 then
  cp.Y := cp.Y + Round(Wiimote1.AccelZ * 10);
  SetCursorPos(CP.X, cp.Y);}

  Edit1.Text := FloatToStr(Wiimote1.AccelX);
  Edit2.Text := FloatToStr(Wiimote1.AccelY);
  Edit3.Text := FloatToStr(Wiimote1.AccelZ);

  if StrToFloat(Edit1.Text)> StrToFloatDef(Edit4.Text, 0) then
    Edit4.Text := Edit1.Text;
  if StrToFloat(Edit2.Text)> StrToFloatDef(Edit5.Text, 0) then
    Edit5.Text := Edit2.Text;
  if StrToFloat(Edit3.Text)> StrToFloatDef(Edit6.Text, 0) then
    Edit6.Text := Edit3.Text;
end;

procedure TForm1.Wiimote1Status(Sender: TObject);
begin
  pb_Battery.Position := Wiimote1.BatteryPercent;

  if wmLED1 in Wiimote1.LedsOn then
    pan_Led1.Color := $00FFC140
  else
    pan_Led1.Color := clBlack;

  if wmLED2 in Wiimote1.LedsOn then
    pan_Led2.Color := $00FFC140
  else
    pan_Led2.Color := clBlack;

  if wmLED3 in Wiimote1.LedsOn then
    pan_Led3.Color := $00FFC140
  else
    pan_Led3.Color := clBlack;

  if wmLED4 in Wiimote1.LedsOn then
    pan_Led4.Color := $00FFC140
  else
    pan_Led4.Color := clBlack;
end;

end.
