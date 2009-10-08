//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit UnitCNAFormConfig;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls;

type
  TFormConfig = class(TForm)
    TV: TTreeView;
    ComboBox1: TComboBox;
    procedure FormDestroy(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  FormConfig: TFormConfig;

implementation

{$R *.dfm}

procedure TFormConfig.FormDestroy(Sender: TObject);
var
  i : Integer;
begin
  for i:=TV.Items.Count-1 downto 0 do
  begin
    TObject(Tv.Items[i].Data).Free;
    Tv.Items[i].free;
  end;
end;

end.
