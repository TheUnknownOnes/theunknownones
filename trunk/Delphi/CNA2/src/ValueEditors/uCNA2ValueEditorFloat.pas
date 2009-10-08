//**********************************************************
// Developed by TheUnkownOnes.net
//
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit uCNA2ValueEditorFloat;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uCNA2ValueEditors, StdCtrls,
  TypInfo;

type
  Tcna2ValueEditorFloat = class(Tcna2ValueEditor)
    ed_Value: TEdit;
    procedure ed_ValueKeyPress(Sender: TObject; var Key: Char);
  private
    Flfs : TFormatSettings;
  protected
    function GetValue : Variant; override;
  public
    function GetDesiredHeight : Integer; override;

    procedure Init(ATypeInfo : PTypeInfo;
                   AValue : Variant;
                   AExpressionEvaluation : Boolean); override;

    class function CanHandle(ATypeInfo : PTypeInfo) : Boolean; override;
  end;

var
  cna2ValueEditorFloat: Tcna2ValueEditorFloat;

implementation

{$R *.dfm}

class function Tcna2ValueEditorFloat.CanHandle(ATypeInfo: PTypeInfo): Boolean;
begin
  Result := ATypeInfo.Kind = tkFloat;
end;

procedure Tcna2ValueEditorFloat.ed_ValueKeyPress(Sender: TObject; var Key: Char);
begin
  if Key > #32 then
  begin
    if not (Key in ['0'..'9', '-', Flfs.DecimalSeparator]) then
      Key := #0;
  end;
end;

function Tcna2ValueEditorFloat.GetDesiredHeight: Integer;
begin
  Result := ed_Value.Height; 
end;

function Tcna2ValueEditorFloat.GetValue: Variant;
begin
  Result := StrToFloatDef(ed_Value.Text, FValue, Flfs);
end;

procedure Tcna2ValueEditorFloat.Init(ATypeInfo: PTypeInfo;
                                      AValue: Variant;
                                      AExpressionEvaluation : Boolean);
begin
  inherited;

  GetLocaleFormatSettings(LOCALE_USER_DEFAULT, Flfs);

  ed_Value.Text := FloatToStr(AValue, Flfs)
end;

end.
