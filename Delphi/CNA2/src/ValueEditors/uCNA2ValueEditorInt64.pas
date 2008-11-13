//**********************************************************
// Developed by TheUnkownOnes.net
//
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit uCNA2ValueEditorInt64;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uCNA2ValueEditors, StdCtrls, TypInfo;

type
  Tcna2ValueEditorInt64 = class(Tcna2ValueEditor)
    ed_Value: TEdit;
    procedure ed_ValueKeyPress(Sender: TObject; var Key: Char);
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

{ Tcna2ValueEditorInt64 }

class function Tcna2ValueEditorInt64.CanHandle(ATypeInfo: PTypeInfo): Boolean;
begin
  Result := ATypeInfo.Kind = tkInt64;
end;

procedure Tcna2ValueEditorInt64.ed_ValueKeyPress(Sender: TObject;
  var Key: Char);
begin
  if Key > #32 then
  begin
    if not (Key in ['0'..'9', '-']) then
      Key := #0;
  end;
end;

function Tcna2ValueEditorInt64.GetDesiredHeight: Integer;
begin
  Result := ed_Value.Height;
end;

function Tcna2ValueEditorInt64.GetValue: Variant;
begin
  Result := StrToInt64Def(ed_Value.Text, FValue)
end;

procedure Tcna2ValueEditorInt64.Init(ATypeInfo: PTypeInfo;
                                      AValue: Variant;
                                      AExpressionEvaluation : Boolean);
begin
  inherited;

  ed_Value.Text := IntToStr(AValue);
end;

end.
