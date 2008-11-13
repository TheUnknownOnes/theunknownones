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
    Label1: TLabel;
    procedure ed_ValueEnter(Sender: TObject);
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

{ Tcna2ValueString }

class function Tcna2ValueEditorString.CanHandle(ATypeInfo: PTypeInfo): Boolean;
begin
  Result := ATypeInfo.Kind in [tkString,
                               tkLString,
                               tkWString];
end;

procedure Tcna2ValueEditorString.ed_ValueEnter(Sender: TObject);
var
  p1, p2 : Integer;
  s : WideString;
begin
  if FExpressionEvaluation then
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
  end
  else
    ed_Value.SelectAll;
end;

function Tcna2ValueEditorString.GetDesiredHeight: Integer;
begin
  Result := ed_Value.Height;
  if not FExpressionEvaluation then
    Inc(Result, Label1.Height);
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

  Label1.Visible := not AExpressionEvaluation;

  ed_Value.Text := AValue;
end;

end.
