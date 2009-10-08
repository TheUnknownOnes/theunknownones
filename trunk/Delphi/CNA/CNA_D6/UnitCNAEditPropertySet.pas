//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit UnitCNAEditPropertySet;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, UnitCNAEditProperty, StdCtrls, ExtCtrls, CheckLst;

type
  TFormCNAPropEdSet = class(TFormCNAPropEdBase)
    cList: TCheckListBox;
  private
  protected
    function NewValue: String; override;
  public
    procedure GetNewValue(OldValue : String;
                          PropertyName : String;
                          PropInfoString : String;
                          IsRuntime : Boolean;
                          var Result : String;
                          var ShowInputBox : Boolean); override;
  end;

var
  FormCNAPropEdSet: TFormCNAPropEdSet;

implementation

{$R *.dfm}

{ TFormCNAPropEdSet }

function TFormCNAPropEdSet.NewValue: String;
var
  iResult : Integer;
  i : Integer;
begin
  iResult:=0;
  for i:=0 to cList.Items.Count-1 do
  begin
    if (cList.Checked[i]) then
      iResult:=(iResult or Integer(cList.Items.Objects[i]));
  end;
  Result:=IntToStr(iResult);
end;

procedure TFormCNAPropEdSet.GetNewValue(OldValue, PropertyName,
  PropInfoString: String;IsRuntime : Boolean; var Result: String; var ShowInputBox : Boolean);
var
  tmpStr : TStrings;
  i : Integer;
begin
  tmpStr:=TStringList.Create;
  tmpStr.CommaText:=Copy(PropInfoString,2,Length(PropInfoString)-2);

  for i:=0 to tmpStr.Count-1 do
  begin
    cList.Items.AddObject(tmpStr.Names[i],TObject(StrToInt(tmpStr.Values[tmpStr.Names[i]])));
    if (StrToIntDef(Result,0) and StrToInt(tmpStr.Values[tmpStr.Names[i]])) = StrToInt(tmpStr.Values[tmpStr.Names[i]]) then
      cList.Checked[i]:=true;
  end;
      
  tmpStr.Free;
  inherited;
end;

end.
