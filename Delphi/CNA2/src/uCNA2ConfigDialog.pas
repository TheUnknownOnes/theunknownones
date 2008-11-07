unit uCNA2ConfigDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls;

type
  TCNA2ConfigDialog = class(TForm)
    PC: TPageControl;
    ts_ComponentSettings: TTabSheet;
    TreeView1: TTreeView;
  private
    { Private-Deklarationen }
  public
    class procedure Execute;
  end;


implementation

uses uCNA2Settings;

{$R *.dfm}

{ TCNA2ConfigDialog }

class procedure TCNA2ConfigDialog.Execute;
var
  Form : TCNA2ConfigDialog;
begin
  Form := TCNA2ConfigDialog.Create(nil);
  try
    Form.ShowModal;
  finally
    Form.Free;
  end;
end;

end.
