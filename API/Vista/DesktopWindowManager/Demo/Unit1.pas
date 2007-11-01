unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Spin, DWMAPI;



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
    procedure Button1Click(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  SpinEdit1.Value:=0;
  SpinEdit2.Value:=0;
  SpinEdit3.Value:=0;
  SpinEdit4.Value:=0;
  DWM_EnableBlurBehind(self.Handle, true);
  DWM_ExtendFrameIntoClientArea(self.Handle, 0,0,0,0);
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
  DWM_EnableBlurBehind(self.Handle, False);
  DWM_ExtendFrameIntoAll(Self.Handle);
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  col : Cardinal;
  bl: Bool;
begin
  DWMGetColorizationColor(col, bl);
  MessageDlg(IntToHex(col,8), mtWarning, [mbOK], 0);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Button1.Click;
end;

procedure TForm1.SpinEdit1Change(Sender: TObject);
begin
  DWM_EnableBlurBehind(self.Handle, False);
  DWM_ExtendFrameIntoClientArea(self.Handle, SpinEdit1.Value, SpinEdit3.Value, SpinEdit2.Value, SpinEdit4.Value);
end;

end.
