unit uFrameTest;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, uEffectPNGToolbar, ToolWin, ExtCtrls, StrUtils,
  uPattExCommon;

type
  TframeTest = class(TFrame)
    GroupBox1: TGroupBox;
    memSource: TMemo;
    dlgOpen: TOpenDialog;
    GroupBox2: TGroupBox;
    Splitter1: TSplitter;
    EffectPNGToolBar1: TEffectPNGToolBar;
    btnLoad: TEffectPNGToolButton;
    ToolButton1: TToolButton;
    btnClose: TEffectPNGToolButton;
    memMatches: TMemo;
    Panel1: TPanel;
    Label1: TLabel;
    ed_MatchDelimiter: TEdit;
    procedure btnLoadClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure ed_MatchDelimiterChange(Sender: TObject);
  private
    FMatchDelim : String;
  public
    constructor Create(AOwner: TComponent); override;

    procedure AddMatch(AExpression : TExpression; AMatch : String);
    procedure Error(AExpression : TExpression; AException : Exception);
  end;

implementation

uses uFormMain;

{$R *.dfm}

procedure TframeTest.AddMatch(AExpression : TExpression; AMatch: String);
begin
  memMatches.Lines.Add(AMatch);
  if Length(FMatchDelim) > 0 then
    memMatches.Lines.Add(FMatchDelim);
end;

procedure TframeTest.btnCloseClick(Sender: TObject);
begin
  formMain.CloseTab;
end;

procedure TframeTest.btnLoadClick(Sender: TObject);
begin
  if dlgOpen.Execute then
  begin
    memSource.Lines.LoadFromFile(dlgOpen.FileName);
    if Parent is TTabSheet then
      TTabSheet(Parent).Caption := ExtractFileName(dlgOpen.FileName);
  end;
end;

constructor TframeTest.Create(AOwner: TComponent);
begin
  inherited;
  ed_MatchDelimiter.Text := DupeString('-', 40);
end;

procedure TframeTest.ed_MatchDelimiterChange(Sender: TObject);
begin
  FMatchDelim := ed_MatchDelimiter.Text;
end;

procedure TframeTest.Error(AExpression : TExpression; AException: Exception);
begin
  memMatches.Lines.Add('Error in expression: ' + AExpression.Expression);
  memMatches.Lines.Add(AException.Message);
end;

end.
