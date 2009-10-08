unit uHTMLEditCell;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, JvExControls, JvComponent, JvColorBox, JvColorButton, StdCtrls,
  Buttons, ExtCtrls, Spin, Mask, JvExMask, JvSpin;

type
  Tform_ITEditCell = class(TForm)
    gb_Color: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    btn_BGColor: TJvColorButton;
    btn_BorderColor: TJvColorButton;
    gb_Span: TGroupBox;
    ed_ColSpan: TJvSpinEdit;
    ed_RowSpan: TJvSpinEdit;
    lbl_Cols: TLabel;
    lbl_Rows: TLabel;
    pan_Bottom: TPanel;
    btn_OK: TBitBtn;
    btn_Cancel: TBitBtn;
    pan_Top: TPanel;
    pan_RightDelim: TPanel;
    Image1: TImage;
    Label3: TLabel;
    cb_Border3d: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure ed_RowSpanChange(Sender: TObject);
    procedure ed_ColSpanChange(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  form_ITEditCell: Tform_ITEditCell;

implementation

{$R *.dfm}

procedure Tform_ITEditCell.ed_ColSpanChange(Sender: TObject);
begin
  if ed_ColSpan.Value>1 then
    lbl_Cols.Caption:='Spalten'
  else
    lbl_Cols.Caption:='Spalte';
end;

procedure Tform_ITEditCell.ed_RowSpanChange(Sender: TObject);
begin
  if ed_RowSpan.Value>1 then
    lbl_Rows.Caption:='Zeilen'
  else
    lbl_Rows.Caption:='Zeile'
end;

procedure Tform_ITEditCell.FormShow(Sender: TObject);
begin
  ed_RowSpanChange(Sender);
  ed_ColSpanChange(Sender);
end;

end.
