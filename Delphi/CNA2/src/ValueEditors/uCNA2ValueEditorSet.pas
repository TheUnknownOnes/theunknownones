//**********************************************************
// Developed by TheUnkownOnes.net
//
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit uCNA2ValueEditorSet;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uCNA2ValueEditors, StdCtrls, CheckLst,
  TypInfo,
  uRTTIHelper;

type
  Tcna2ValueEditorSet = class(Tcna2ValueEditor)
    lst_Values: TCheckListBox;
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

{ Tcna2ValueEditorSet }

class function Tcna2ValueEditorSet.CanHandle(ATypeInfo: PTypeInfo): Boolean;
begin
  Result := ATypeInfo.Kind = tkSet;
end;

function Tcna2ValueEditorSet.GetDesiredHeight: Integer;
begin
  Result := lst_Values.Height;
end;

function Tcna2ValueEditorSet.GetValue: Variant;
var
  idx : Integer;
  v : Integer;
begin
  v := 0;

  for idx := 0 to lst_Values.Items.Count - 1 do
  begin
    if lst_Values.Checked[idx] then
      v := v or Integer(lst_Values.Items.Objects[idx]);
  end;

  Result := v;
end;

procedure Tcna2ValueEditorSet.Init(ATypeInfo: PTypeInfo;
                                      AValue: Variant;
                                      AExpressionEvaluation : Boolean);
var
  sl : TStringList;
  idx,
  idxItem : Integer;
  Element : Integer;
  v : Integer;
begin
  inherited;

  v := AValue;

  sl := TStringList.Create;
  try
    rttihSetToList(ATypeInfo, sl);

    for idx := 0 to sl.Count - 1 do
    begin
      Element := Integer(sl.Objects[idx]);

      idxItem := lst_Values.Items.AddObject(sl.Names[idx], sl.Objects[idx]);
      lst_Values.Checked[idx] := (v and Element) = Element;
    end;
  finally
    sl.Free;
  end;
end;

end.
