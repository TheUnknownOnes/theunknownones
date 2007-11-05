//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit UnitCNAGetName;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Buttons;

type
  TformGetName = class(TForm)
    IMG: TImage;
    Label1: TLabel;
    edName: TEdit;
    panIndent: TPanel;
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private-Deklarationen }
  public
    PrefixLength : Integer;
  end;

implementation

{$R *.dfm}

procedure TformGetName.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if (ModalResult=mrNone) then
    ModalResult:=mrCancel;
end;

procedure TformGetName.FormShow(Sender: TObject);
begin
  edName.SetFocus;
  if (PrefixLength=-1) then
    edName.SelectAll
  else
  begin
    edName.SelStart:=PrefixLength;
    edName.SelLength:=Length(edName.Text)-PrefixLength;
  end;
end;

end.
