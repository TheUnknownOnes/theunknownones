//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit UnitCNAEditPropertyString;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, UnitCNAEditProperty, StdCtrls, ExtCtrls,StrUtils;

type
  TFormCNAPropEdString = class(TFormCNAPropEdBase)
    lblOldValue: TLabel;
    edOldValue: TEdit;
    lblNewValue: TLabel;
    edNewValue: TEdit;
    procedure FormShow(Sender: TObject);
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

procedure TFormCNAPropEdString.FormShow(Sender: TObject);
begin
  edNewValue.SetFocus;
  edNewValue.SelectAll;
end;

function TFormCNAPropEdString.NewValue: String;
begin
  Result:=edNewValue.Text;
end;

procedure TFormCNAPropEdString.GetNewValue(OldValue, PropertyName,
  PropInfoString: String;IsRuntime : Boolean; var Result: String; var ShowInputBox : Boolean);
begin
  edOldValue.Text:=OldValue;
  edNewValue.Text:=IfThen(Result='<!--NILORNULL-->','',Result);
  inherited;
end;

end.
