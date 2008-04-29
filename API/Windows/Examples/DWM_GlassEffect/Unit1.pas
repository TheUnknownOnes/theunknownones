//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Spin, uDWMHelper, JwaDwmapi, xpman;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    SpinEdit3: TSpinEdit;
    SpinEdit4: TSpinEdit;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    procedure Button1Click(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

uses Unit2;

{$R *.dfm}


procedure TForm1.Button1Click(Sender: TObject);
begin
  SpinEdit1.Value:=0;
  SpinEdit2.Value:=0;
  SpinEdit3.Value:=0;
  SpinEdit4.Value:=0;

  DWM_EnableBlurBehind(form2.Handle, true);
  DWM_ExtendFrameIntoClientArea(form2.Handle, 0,0,0,0);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  SpinEdit1.Value:=10;
  SpinEdit2.Value:=10;
  SpinEdit3.Value:=10;
  SpinEdit4.Value:=10;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  DWM_EnableBlurBehind(form2.Handle, False);


  //SetLayeredWindowAttributes(form2.Handle, ColorToRGB(form2.Color), 0, LWA_COLORKEY);
  DWM_SheetOfGlass(form2.Handle);
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  col : Cardinal;
  bl: Bool;
begin
  DWMGetColorizationColor(col, bl);
  MessageDlg(IntToHex(col,8), mtWarning, [mbOK], 0);
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  rgn : HRGN;
begin
  rgn:=CreateEllipticRgn(0,0,form2.clientwidth,form2.clientheight);
  DWM_EnableBlurBehind(form2.Handle, true, rgn, false, DWM_BB_ENABLE or DWM_BB_BLURREGION);
end;

procedure TForm1.SpinEdit1Change(Sender: TObject);
begin
  DWM_EnableBlurBehind(form2.Handle, true);
  DWM_ExtendFrameIntoClientArea(form2.Handle, SpinEdit1.Value, SpinEdit3.Value, SpinEdit2.Value, SpinEdit4.Value);
end;

end.
