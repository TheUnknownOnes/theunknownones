unit uch2FrameHelpItemDecoration;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, ExtCtrls, uch2Main;

type
  Tch2FrameHelpItemDecoration = class(TFrame)
    cb_Bold: TCheckBox;
    cb_Italic: TCheckBox;
    cb_Underline: TCheckBox;
    cb_Strike: TCheckBox;
    cob_Text: TColorBox;
    Label1: TLabel;
    cob_Back: TColorBox;
    Label2: TLabel;
    lbl_Caption: TLabel;
    procedure cob_TextChange(Sender: TObject);
    procedure cob_BackChange(Sender: TObject);
    procedure cb_BoldClick(Sender: TObject);
  private
    FInternalChange : Boolean;

    FOnChange: TNotifyEvent;
    function GetCaption: String;
    procedure SetCaption(const Value: String);
    function GetDeco: Tch2HelpItemDecoration;
    procedure SetDeco(const Value: Tch2HelpItemDecoration);

    procedure ApplyOnCaption;

    function GetFontStyles : TFontStyles;

    procedure DoChange;
  public
    constructor Create(AOwner: TComponent); override;

    procedure ResetToDefault;

    property Caption : String read GetCaption write SetCaption;
    property Decoration : Tch2HelpItemDecoration read GetDeco write SetDeco;

    property OnChange : TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

{$R *.dfm}

{ Tch2FrameHelpItemDecoration }

procedure Tch2FrameHelpItemDecoration.ApplyOnCaption;
var
  fs : TFontStyles;
begin
  if cob_Back.Selected <> clDefault then
    lbl_Caption.Color := cob_Back.Selected
  else
    lbl_Caption.Color := clBtnFace;

  if cob_Text.Selected <> clDefault then
    lbl_Caption.Font.Color := cob_Text.Selected
  else
    lbl_Caption.Font.Color := clWindowText;

  lbl_Caption.Font.Style := GetFontStyles;
end;

procedure Tch2FrameHelpItemDecoration.cb_BoldClick(Sender: TObject);
begin
  ApplyOnCaption;
  DoChange;
end;

procedure Tch2FrameHelpItemDecoration.cob_BackChange(Sender: TObject);
begin
  ApplyOnCaption;
  DoChange
end;

procedure Tch2FrameHelpItemDecoration.cob_TextChange(Sender: TObject);
begin
  ApplyOnCaption;
  DoChange;
end;

constructor Tch2FrameHelpItemDecoration.Create(AOwner: TComponent);
begin
  inherited;

  FInternalChange := false;
end;

procedure Tch2FrameHelpItemDecoration.DoChange;
begin
  if (not FInternalChange) and Assigned(FOnChange) then
  begin
    FOnChange(Self);
  end;
end;

function Tch2FrameHelpItemDecoration.GetCaption: String;
begin
  Result := lbl_Caption.Caption;
end;

function Tch2FrameHelpItemDecoration.GetDeco: Tch2HelpItemDecoration;
var
  d : Tch2HelpItemDecoration;
begin
  d.TextColor := cob_Text.Selected;
  d.BackColor := cob_Back.Selected;
  d.FontStyles := GetFontStyles;

  Result := d;
end;

function Tch2FrameHelpItemDecoration.GetFontStyles: TFontStyles;
begin
  Result := [];
  if cb_Bold.Checked then Include(Result, fsBold);
  if cb_Italic.Checked then Include(Result, fsItalic);
  if cb_Underline.Checked then Include(Result, fsUnderline);
  if cb_Strike.Checked then Include(Result, fsStrikeOut);
end;

procedure Tch2FrameHelpItemDecoration.ResetToDefault;
begin
  cob_Text.Selected := clDefault;
  cob_Back.Selected := clDefault;
  cb_Bold.Checked := false;
  cb_Italic.Checked := false;
  cb_Underline.Checked := false;
  cb_Strike.Checked := false;
  lbl_Caption.Caption := 'Sample';
end;

procedure Tch2FrameHelpItemDecoration.SetCaption(const Value: String);
begin
  lbl_Caption.Caption := Value;
end;

procedure Tch2FrameHelpItemDecoration.SetDeco(
  const Value: Tch2HelpItemDecoration);
begin
  FInternalChange := true;
  try
    cob_Text.Selected := Value.TextColor;
    cob_Back.Selected := Value.BackColor;

    cb_Bold.Checked := fsBold in Value.FontStyles;
    cb_Italic.Checked := fsItalic in Value.FontStyles;
    cb_Underline.Checked := fsUnderline in Value.FontStyles;
    cb_Strike.Checked := fsStrikeOut in Value.FontStyles;
  finally
    FInternalChange := false;
  end;

  ApplyOnCaption;
end;

end.
