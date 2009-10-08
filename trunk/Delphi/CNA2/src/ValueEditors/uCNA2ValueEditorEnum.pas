//**********************************************************
// Developed by TheUnkownOnes.net
//
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit uCNA2ValueEditorEnum;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uCNA2ValueEditors, StdCtrls, ExtCtrls,
  TypInfo,
  uRTTIHelper,
  WideStrings;

type
  Tcna2ValueEditorEnum = class(Tcna2ValueEditor)
    rg_Values: TRadioGroup;
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

{ Tcna2ValueEditorEnum }

class function Tcna2ValueEditorEnum.CanHandle(ATypeInfo: PTypeInfo): Boolean;
begin
  Result := ATypeInfo.Kind = tkEnumeration;
end;

function Tcna2ValueEditorEnum.GetDesiredHeight: Integer;
begin
  Result := rg_Values.Height;
end;

function Tcna2ValueEditorEnum.GetValue: Variant;
begin
  if rg_Values.ItemIndex = -1 then
    Result := FValue
  else
    Result := Integer(rg_Values.Items.Objects[rg_Values.ItemIndex]);
end;

procedure Tcna2ValueEditorEnum.Init(ATypeInfo: PTypeInfo;
                                      AValue: Variant;
                                      AExpressionEvaluation : Boolean);
var
  sl : TStrings;
  idx : Integer;
begin
  inherited;

  sl := TStringList.Create;
  try
    rttihEnumToList(ATypeInfo, sl);

    for idx := 0 to sl.Count - 1 do
      rg_Values.Items.AddObject(sl.Names[idx], sl.Objects[idx]);
  finally
    sl.Free;
  end;

  rg_Values.ItemIndex := rg_Values.Items.IndexOfObject(TObject(Integer(AValue)));
end;

end.
