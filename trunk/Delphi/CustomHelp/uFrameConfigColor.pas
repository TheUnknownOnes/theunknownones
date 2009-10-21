unit uFrameConfigColor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, ExtCtrls, CategoryButtons;

type
  TFrameConfigColor = class(TFrame)
    catbtnTopics: TCategoryButtons;
    ColorBox1: TColorBox;
    procedure ColorBox1Change(Sender: TObject);
  private
    function GetColor: TColor;
    procedure SetColor(const Value: TColor);
    { Private-Deklarationen }
  public
    property SelectedColor : TColor read GetColor write SetColor;
  end;

implementation

{$R *.dfm}

procedure TFrameConfigColor.ColorBox1Change(Sender: TObject);
begin
  catbtnTopics.Categories[0].Color:=ColorBox1.Selected;
end;

function TFrameConfigColor.GetColor: TColor;
begin
  Result:=ColorBox1.Selected;
end;

procedure TFrameConfigColor.SetColor(const Value: TColor);
begin
  ColorBox1.Selected:=Value;
  catbtnTopics.Categories[0].Color:=Value;
end;

end.
