unit uHTMLInsertCell;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls;

type
  Tform_ITInsertCell = class(TForm)
    pan_Bottom: TPanel;
    btn_OK: TBitBtn;
    btn_Cancel: TBitBtn;
    pan_OrigCell: TPanel;
    pan_Top: TPanel;
    rb_TopRight: TRadioButton;
    rb_TopLeft: TRadioButton;
    rb_BottomLeft: TRadioButton;
    rb_BottomRight: TRadioButton;
    Label10: TLabel;
    Image1: TImage;
    pan_RightDelim: TPanel;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  form_ITInsertCell: Tform_ITInsertCell;

implementation

{$R *.dfm}

end.
