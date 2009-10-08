//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit UnitCNAEditPropertyEnumeration;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, UnitCNAEditProperty, StdCtrls, ExtCtrls;

type
  TFormCNAPropEdEnum = class(TFormCNAPropEdBase)
    rgEnum: TRadioGroup;
    procedure rgEnumClick(Sender: TObject);
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

implementation

{$R *.dfm}

{ TFormCNAPropEdEnum }



{ TFormCNAPropEdEnum }

function TFormCNAPropEdEnum.NewValue: String;
begin
  Result:=IntToStr(Integer(rgEnum.Items.Objects[rgEnum.ItemIndex]));
end;

procedure TFormCNAPropEdEnum.GetNewValue(OldValue, PropertyName,
  PropInfoString: String;IsRuntime : Boolean; var Result: String; var ShowInputBox : Boolean);
var
  tmpStr : TStrings;
  i : Integer;
begin
  tmpStr:=TStringList.Create;
  tmpStr.CommaText:=Copy(PropInfoString,2,Length(PropInfoString)-2);
  for i:=0 to tmpStr.Count-1 do
    rgEnum.Items.AddObject(tmpStr.Names[i],TObject(StrToInt(tmpStr.ValueFromIndex[i])));
  rgEnum.ItemIndex:=rgEnum.Items.IndexOfObject(TObject(StrToIntDef(Result,0)));
  tmpStr.Free;
  inherited ;
end;

procedure TFormCNAPropEdEnum.rgEnumClick(Sender: TObject);
begin
  inherited;
  btnOK.Enabled:=rgEnum.ItemIndex>=0;
end;

end.
