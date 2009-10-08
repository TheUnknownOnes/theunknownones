//**********************************************************
// Developed by TheUnkownOnes.net
//
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit uCNA2ValueEditorString;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uCNA2ValueEditors, StdCtrls,
  TypInfo,
  StrUtils;

type
  Tcna2ValueEditorString = class(Tcna2ValueEditor)
    ed_Value: TEdit;
    mem_Hint: TMemo;
  private
    { Private-Deklarationen }

  protected
    function GetValue : Variant; override;

    procedure ApplyExpressions;
  public
    function GetDesiredHeight : Integer; override;

    procedure Init(ATypeInfo : PTypeInfo;
                   AValue : Variant;
                   AExpressionEvaluation : Boolean); override;
    
    class function CanHandle(ATypeInfo : PTypeInfo) : Boolean; override;
  end;

implementation

{$R *.dfm}

{ Tcna2ValueString }

procedure Tcna2ValueEditorString.ApplyExpressions;
var
  p1, p2 : Integer;
  s : WideString;
begin
  p2 := 0;

  s := ed_Value.Text;

  p1 := PosEx('|', s);

  if p1 > 0 then
  begin
    Delete(s, p1, 1);
    Dec(p1);

    p2 := PosEx('|', s, p1 + 1);

    if p2 > 0 then
    begin
      Delete(s, p2, 1);
      Dec(p2);
    end;
  end;

  ed_Value.Text := s;

  if p1 > 0 then
  begin
    ed_Value.SelStart := p1;

    if p2 > 0 then
      ed_Value.SelLength := p2 - p1;
  end;
end;

class function Tcna2ValueEditorString.CanHandle(ATypeInfo: PTypeInfo): Boolean;
begin
  Result := ATypeInfo.Kind in [tkString,
                               tkLString,
                               tkWString];
end;


function Tcna2ValueEditorString.GetDesiredHeight: Integer;
begin
  Result := ed_Value.Height;
  if mem_Hint.Visible then
    Inc(Result, mem_Hint.Height);
end;

function Tcna2ValueEditorString.GetValue: Variant;
begin
  Result := ed_Value.Text;
end;

procedure Tcna2ValueEditorString.Init(ATypeInfo: PTypeInfo;
                                      AValue: Variant;
                                      AExpressionEvaluation : Boolean);
begin
  inherited;

  mem_Hint.Visible := not AExpressionEvaluation;

  ed_Value.Text := AValue;
  if AExpressionEvaluation  then
    ApplyExpressions
  else
    ed_Value.SelectAll;
end;

end.
