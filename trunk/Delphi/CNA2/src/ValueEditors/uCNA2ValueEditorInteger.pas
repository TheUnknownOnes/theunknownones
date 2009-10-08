//**********************************************************
// Developed by TheUnkownOnes.net
//
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit uCNA2ValueEditorInteger;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uCNA2ValueEditors, StdCtrls, TypInfo;

type
  Tcna2ValueEditorInteger = class(Tcna2ValueEditor)
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

{ Tcna2ValueEditorInteger }

class function Tcna2ValueEditorInteger.CanHandle(ATypeInfo: PTypeInfo): Boolean;
begin
  Result := ATypeInfo.Kind = tkInteger;
end;

procedure Tcna2ValueEditorInteger.ed_ValueKeyPress(Sender: TObject;
  var Key: Char);
begin
  if Key > #32 then
  begin
    if not (Key in ['0'..'9', '-']) then
      Key := #0;
  end;
end;

function Tcna2ValueEditorInteger.GetDesiredHeight: Integer;
begin
  Result := ed_Value.Height;
end;

function Tcna2ValueEditorInteger.GetValue: Variant;
begin
  Result := StrToIntDef(ed_Value.Text, FValue);
end;

procedure Tcna2ValueEditorInteger.Init(ATypeInfo: PTypeInfo;
                                      AValue: Variant;
                                      AExpressionEvaluation : Boolean);
begin
  inherited;

  ed_Value.Text := IntToStr(AValue);
end;

end.
