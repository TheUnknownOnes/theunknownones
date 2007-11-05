//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit UnitCNAEditPropertyFloat;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, UnitCNAEditProperty, StdCtrls, ExtCtrls;

type
  TFormCNAPropEdFloat = class(TFormCNAPropEdBase)
    lblOldValue: TLabel;
    edOldValue: TEdit;
    lblNewValue: TLabel;
    edNewValue: TEdit;
    procedure FormShow(Sender: TObject);
    procedure edNewValueKeyPress(Sender: TObject; var Key: Char);
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

{ TFormCNAPropEdFloat }

function TFormCNAPropEdFloat.NewValue: String;
var
  Res : Extended;
begin
  if (TryStrToFloat(edNewValue.Text,Res)) then
    Result:=edNewValue.Text
  else
    Result:='0';
end;

procedure TFormCNAPropEdFloat.GetNewValue(OldValue, PropertyName,
  PropInfoString: String;IsRuntime : Boolean; var Result: String; var ShowInputBox : Boolean);
begin
  edOldValue.Text:=OldValue;
  edNewValue.Text:=FloatToStr(StrToFloatDef(Result,0));
  
  inherited;
end;

procedure TFormCNAPropEdFloat.edNewValueKeyPress(Sender: TObject;
  var Key: Char);
var
  Res : Extended;
begin
  if (Key>=#32) then
  begin
    if not (TryStrToFloat(edNewValue.Text+Key,Res)) then
      Key:=#0;
 end;
end;

procedure TFormCNAPropEdFloat.FormShow(Sender: TObject);
begin
  edNewValue.SetFocus;
  edNewValue.SelectAll;
end;

end.
