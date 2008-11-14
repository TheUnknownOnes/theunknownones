//**********************************************************
// Developed by TheUnkownOnes.net
//
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit uCNA2ValueEditorColor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uCNA2ValueEditors,
  TypInfo, StdCtrls, ExtCtrls;

type
  Tcna2ValueEditorColor = class(Tcna2ValueEditor)
    com_Color: TColorBox;
    procedure com_ColorEnter(Sender: TObject);
  private
    { Private-Deklarationen }
  protected
    function GetValue : Variant; override;
  public
    function GetDesiredHeight : Integer; override;

    procedure Init(ATypeInfo : PTypeInfo;
                   AValue : Variant;
                   AExpressionEvaluation : Boolean); override;

    class function CanHandle(ATypeInfo : PTypeInfo) : Boolean; override;
  end;


implementation

{$R *.dfm}

{ Tcna2ValueEditorColor }

class function Tcna2ValueEditorColor.CanHandle(ATypeInfo: PTypeInfo): Boolean;
begin
  Result := (ATypeInfo.Kind = tkInteger) and (ATypeInfo.Name = 'TColor'); 
end;

procedure Tcna2ValueEditorColor.com_ColorEnter(Sender: TObject);
begin
  inherited;
  if com_Color.Selected = clBlack then
    com_Color.Selected := TColor(FValue);
end;

function Tcna2ValueEditorColor.GetDesiredHeight: Integer;
begin
  Result := com_Color.Height;
end;

function Tcna2ValueEditorColor.GetValue: Variant;
begin
  Result := com_Color.Selected;
end;

procedure Tcna2ValueEditorColor.Init(ATypeInfo: PTypeInfo; AValue: Variant;
  AExpressionEvaluation: Boolean);
begin
  inherited;

end;

end.
