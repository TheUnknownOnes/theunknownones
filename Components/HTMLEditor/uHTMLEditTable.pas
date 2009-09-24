unit uHTMLEditTable;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin, JvExControls, JvComponent, JvColorBox, JvColorButton,
  Buttons, ExtCtrls, Mask, JvExMask, JvSpin;

type
  Tform_ITEditTable = class(TForm)
    gb_Border: TGroupBox;
    Label1: TLabel;
    ed_BorderWidth: TJvSpinEdit;
    Label2: TLabel;
    btn_BorderColor: TJvColorButton;
    Label3: TLabel;
    Label4: TLabel;
    ed_CellPadding: TJvSpinEdit;
    ed_CellSpacing: TJvSpinEdit;
    gb_TableSize: TGroupBox;
    Label5: TLabel;
    ed_Width: TJvSpinEdit;
    Label6: TLabel;
    pan_Bottom: TPanel;
    btn_OK: TBitBtn;
    btn_Cancel: TBitBtn;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    pan_Top: TPanel;
    Image1: TImage;
    pan_RightDelim: TPanel;
    Label10: TLabel;
    cb_Border3D: TCheckBox;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  form_ITEditTable: Tform_ITEditTable;

implementation

{$R *.dfm}

end.
