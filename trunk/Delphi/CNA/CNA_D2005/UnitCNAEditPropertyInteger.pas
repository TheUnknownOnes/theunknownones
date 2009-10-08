unit UnitCNAEditPropertyInteger;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, UnitCNAEditProperty, StdCtrls, ExtCtrls, Mask;

type
  TFormCNAPropEdInteger = class(TFormCNAPropEdBase)
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

{ TFormCNAPropEdBase1 }

function TFormCNAPropEdInteger.NewValue: String;
var
  I       : Integer;
  I64     : Int64;
begin
  case Self.Tag of
    32:
    begin
      if TryStrToInt(edNewValue.Text,i) then
        Result:=IntToStr(i)
      else
        Result:='0';
    end;
    64:
    begin
      if TryStrToInt64(edNewValue.Text,I64) then
        Result:=IntToStr(I64)
      else
        Result:='0';
    end;
  end;
end;

procedure TFormCNAPropEdInteger.GetNewValue(OldValue, PropertyName,
  PropInfoString: String;IsRuntime : Boolean; var Result: String; var ShowInputBox : Boolean);
begin
  edOldValue.Text:=OldValue;
  edNewValue.Text:=IntToStr(StrTointDef(Result,0));
  inherited;
end;

procedure TFormCNAPropEdInteger.edNewValueKeyPress(Sender: TObject;
  var Key: Char);
var
  I       : Integer;
  I64     : Int64;
  IntTest : Boolean;
  StrVal  : String;
begin
  IntTest:=False;
  if (Key>=#32) then
  begin
    if edNewValue.Text=EmptyStr then
      StrVal:='0'
    else
      StrVal:=edNewValue.Text;
    Insert(Key,StrVal,edNewValue.SelStart+1);
    case Self.Tag of
      32:
        IntTest:=TryStrToInt(StrVal,i);
      64:
        IntTest:=TryStrToInt64(StrVal,I64);
	  end;
    if not IntTest then
      Key:=#0;
  end;
end;

procedure TFormCNAPropEdInteger.FormShow(Sender: TObject);
begin
  edNewValue.SetFocus;
  edNewValue.SelectAll;
end;

end.
