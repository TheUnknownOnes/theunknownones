unit uFormEditExpression;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  Tform_EditExpression = class(TForm)
    lbl_Expression: TLabel;
    ed_Expression: TEdit;
    pan_Bottom: TPanel;
    btn_OK: TButton;
    btn_Cancel: TButton;
    lbl_ReplaceBy: TLabel;
    ed_ReplaceBy: TEdit;
  private
    { Private-Deklarationen }
  public
    class function Execute(var AExpression, AReplaceBy : String) : Boolean;
  end;

implementation

{$R *.dfm}

{ Tform_EditExpression }

class function Tform_EditExpression.Execute(var AExpression,
  AReplaceBy: String): Boolean;
var
  Form : Tform_EditExpression;
begin
  Form := Tform_EditExpression.Create(nil);
  try
    Form.ed_Expression.Text := AExpression;
    Form.ed_ReplaceBy.Text := AReplaceBy;

    Result := IsPositiveResult(form.ShowModal);

    if Result then
    begin
      AExpression := form.ed_Expression.Text;
      AReplaceBy := form.ed_ReplaceBy.Text;
    end;
    
  finally
    Form.Release;
  end;
end;

end.
