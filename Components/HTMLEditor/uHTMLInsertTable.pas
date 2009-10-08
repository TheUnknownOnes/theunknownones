unit uHTMLInsertTable;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, JvExStdCtrls, JvEdit, JvValidateEdit,
  Spin, JvExControls, JvComponent, JvColorBox, JvColorButton, Mask, JvExMask,
  JvSpin;

type
  Tform_ITInsertTable = class(TForm)
    pan_Bottom: TPanel;
    btn_OK: TBitBtn;
    btn_Cancel: TBitBtn;
    gb_Dimension: TGroupBox;
    Label1: TLabel;
    ed_Rows: TJvValidateEdit;
    Label2: TLabel;
    ed_Cols: TJvValidateEdit;
    gb_PropTable: TGroupBox;
    Label3: TLabel;
    ed_TableWidth: TJvSpinEdit;
    Label4: TLabel;
    Label5: TLabel;
    ed_CellPadding: TJvSpinEdit;
    Label6: TLabel;
    Label7: TLabel;
    ed_CellSpacing: TJvSpinEdit;
    Label8: TLabel;
    Label9: TLabel;
    ed_BorderWidth: TJvSpinEdit;
    Label10: TLabel;
    btn_BorderColor: TJvColorButton;
    pan_Top: TPanel;
    Label11: TLabel;
    Image1: TImage;
    pan_RightDelim: TPanel;
    cb_Border3D: TCheckBox;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  form_ITInsertTable: Tform_ITInsertTable;

implementation

{$R *.dfm}

end.
