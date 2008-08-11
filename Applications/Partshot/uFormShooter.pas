unit uFormShooter;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Math, StdCtrls, ExtCtrls, Menus;

type
  Tform_Shooter = class(TForm)
    lbl_Size: TLabel;
    lbl_Bottom: TLabel;
    lbl_Client: TLabel;
    pum_Shooter: TPopupMenu;
    mi_EndShooting: TMenuItem;
    mi_Transp: TMenuItem;
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormResize(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure mi_EndShootingClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormMouseLeave(Sender: TObject);
  private
    procedure AlignWindowUnderMouse;

    procedure OnClickTransp(Sender : TObject);
  public
    { Public-Deklarationen }
  end;

implementation

uses uFormMain;

{$R *.dfm}

{ Tform_Shooter }

procedure Tform_Shooter.AlignWindowUnderMouse;
var
  WindowMiddle,
  CP : TPoint;
begin
  GetCursorPos(CP);

  WindowMiddle.X := Left + Round(Width / 2);
  WindowMiddle.Y := Top + Round(Height / 2);

  Left := Left - (WindowMiddle.X - CP.X);
  Top := Top - (WindowMiddle.Y - CP.Y);
end;

procedure Tform_Shooter.FormCreate(Sender: TObject);
var
  mi : TMenuItem;
  Percent : INteger;
begin
  Percent := 5;
  while Percent <= 100 do
  begin
    mi := TMenuItem.Create(mi_Transp);
    mi_Transp.Add(mi);
    mi.Caption := IntToStr(Percent) + '%';
    mi.Tag := Byte(Round(255 * (Percent / 100)));
    mi.OnClick := OnClickTransp;

    Inc(Percent, 5);
  end;
end;

procedure Tform_Shooter.FormMouseLeave(Sender: TObject);
begin
  AlignWindowUnderMouse;
end;

procedure Tform_Shooter.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  AlignWindowUnderMouse;
end;

procedure Tform_Shooter.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  case Button of
    mbLeft: form_Main.DoShot;
  end;
end;

procedure Tform_Shooter.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  form_Main.PerformMouseWheel(WheelDelta, Shift);
end;

procedure Tform_Shooter.FormResize(Sender: TObject);
begin
  lbl_Size.Caption := Format('%d x %d', [Width, Height]) +
                      ' | Next file: ' + form_Main.GenNextFileName;
end;

procedure Tform_Shooter.FormShow(Sender: TObject);
begin
  AlignWindowUnderMouse;
end;

procedure Tform_Shooter.mi_EndShootingClick(Sender: TObject);
begin
  Hide;
end;

procedure Tform_Shooter.OnClickTransp(Sender: TObject);
begin
  AlphaBlendValue := Byte(TMenuItem(Sender).Tag);
end;

end.
