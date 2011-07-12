unit uFormConfig;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TformConfig = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    edPath: TEdit;
    Label2: TLabel;
  private
    { Private-Deklarationen }
  public
    class function Execute(var APath : String) : Boolean;
  end;

implementation

{$R *.dfm}

{ TformConfig }

class function TformConfig.Execute(var APath: String): Boolean;
var
  form : TformConfig;
begin
  form := TformConfig.Create(nil);
  try
    form.edPath.Text := APath;

    Result := IsPositiveResult(form.ShowModal);
    if Result then
      APath := Trim(form.edPath.Text);
  finally
    form.Free;
  end;
end;

end.
