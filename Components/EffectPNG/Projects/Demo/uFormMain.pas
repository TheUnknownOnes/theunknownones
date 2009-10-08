unit uFormMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uEffectPNGImage, StdCtrls, ComCtrls, ExtCtrls, uEffectPNG,
  JvExStdCtrls, JvEdit, JvValidateEdit;

type
  TForm1 = class(TForm)
    GroupBox1: TGroupBox;
    EffectPNGImage1: TEffectPNGImage;
    Label1: TLabel;
    tb_Alpha: TTrackBar;
    rg_EffectFrom: TRadioGroup;
    rg_Mirror: TRadioGroup;
    tb_EffectPercent: TTrackBar;
    Label2: TLabel;
    com_Effect: TComboBox;
    Label3: TLabel;
    Label4: TLabel;
    cb_Color: TColorBox;
    Label5: TLabel;
    ed_Gamma: TJvValidateEdit;
    tb_Sepia: TTrackBar;
    Label6: TLabel;
    procedure tb_AlphaChange(Sender: TObject);
    procedure rg_MirrorClick(Sender: TObject);
    procedure rg_EffectFromClick(Sender: TObject);
    procedure tb_EffectPercentChange(Sender: TObject);
    procedure com_EffectChange(Sender: TObject);
    procedure cb_ColorChange(Sender: TObject);
    procedure ed_GammaChange(Sender: TObject);
    procedure tb_SepiaChange(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.cb_ColorChange(Sender: TObject);
begin
  EffectPNGImage1.Effects.ColorizeColor := cb_Color.Selected;
end;

procedure TForm1.com_EffectChange(Sender: TObject);
begin
  case com_Effect.ItemIndex of
    0: EffectPNGImage1.Effects.Effect := peNothing;
    1: EffectPNGImage1.Effects.Effect := peColorize;
    2: EffectPNGImage1.Effects.Effect := peGamma;
    3: EffectPNGImage1.Effects.Effect := peGrayScale;
    4: EffectPNGImage1.Effects.Effect := peInvert;
    5: EffectPNGImage1.Effects.Effect := peSepia;
  end;
end;

procedure TForm1.ed_GammaChange(Sender: TObject);
begin
  EffectPNGImage1.Effects.GammaValue := ed_Gamma.Value; 
end;

procedure TForm1.rg_EffectFromClick(Sender: TObject);
begin
  case rg_EffectFrom.ItemIndex of
    0: EffectPNGImage1.Effects.EffectFrom := efTop;
    1: EffectPNGImage1.Effects.EffectFrom := efLeft;
    2: EffectPNGImage1.Effects.EffectFrom := efRight;
    3: EffectPNGImage1.Effects.EffectFrom := efBottom;
  end;
end;

procedure TForm1.rg_MirrorClick(Sender: TObject);
begin
  case rg_Mirror.ItemIndex of
    0: EffectPNGImage1.Effects.Mirror := mNothing;
    1: EffectPNGImage1.Effects.Mirror := mHorizontal;
    2: EffectPNGImage1.Effects.Mirror := mVertical;
    3: EffectPNGImage1.Effects.Mirror := mCrossOver;
  end;
end;

procedure TForm1.tb_AlphaChange(Sender: TObject);
begin
  EffectPNGImage1.Effects.Alpha := tb_Alpha.Position;
end;

procedure TForm1.tb_EffectPercentChange(Sender: TObject);
begin
  EffectPNGImage1.Effects.EffectPercent := tb_EffectPercent.Position;
end;

procedure TForm1.tb_SepiaChange(Sender: TObject);
begin
  EffectPNGImage1.Effects.SepiaDepth := tb_Sepia.Position;
end;

end.
