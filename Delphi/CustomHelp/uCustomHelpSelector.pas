{-----------------------------------------------------------------------------
 Purpose: Form to select which help should be showed 
 
 (c) by TheUnknownOnes
 see http://www.TheUnknownOnes.net
-----------------------------------------------------------------------------}

unit uCustomHelpSelector;
              
interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, HelpIntfs, OleCtrls, StdCtrls, ExtCtrls, ActiveX, mshtml, ComCtrls;

type
  TFormHelpSelector = class(TForm)
    ListBox1: TListView;
    Panel1: TPanel;
    btnOk: TButton;
    procedure ListBox1DblClick(Sender: TObject);
    procedure ListBox1KeyPress(Sender: TObject; var Key: Char);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ListBox1Compare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
  public
    class function Execute(Keywords: TStrings; out SelectedIndex: Integer;
      out SelectedUrl: String): Boolean; static;
  end;

  THelpSelector = class(TInterfacedObject, IHelpSelector)
  protected
    function SelectKeyword(Keywords: TStrings) : Integer;
    function TableOfContents(Contents: TStrings): Integer;
  end;

implementation

uses
  UrlMon, StrUtils, ComObj, ShellAPI, uCustomHelpMain, uCustomHelpIDEIntegration, 
  Math;

{$R *.dfm}

class function TFormHelpSelector.Execute(Keywords: TStrings;
                                   out SelectedIndex: Integer;
                                   out SelectedUrl: String): Boolean;
var
  idx : Integer;   
  c, d, u : String;
  o : Integer;
begin             
  Result:=False;
  with TFormHelpSelector.Create(nil) do
  begin
    for idx := 0 to KeyWords.Count - 1 do
    begin
      if not AnsiStartsText('ms-help://',KeyWords[idx]) then
      begin
        if TCustomHelp.DecodeURL(Keywords[idx], c, d, u, o) then
        with ListBox1.Items.Add do
        begin
          Caption:=c;
          SubItems.Add(d);
          SubItems.Add(u);
          SubItems.Add(IntToStr(idx));
          Data:=Pointer(o);
        end;
      end;
    end;

    ListBox1.ItemIndex:=0;

    if (ShowModal=mrOk) and (ListBox1.ItemIndex > -1) then
    begin
      Result:=True;
      SelectedIndex:=StrToInt(ListBox1.Selected.SubItems[2]);
      SelectedUrl:=TCustomHelp.EncodeURL(ListBox1.Selected.Caption,
                                         ListBox1.Selected.SubItems[0],
                                         ListBox1.Selected.SubItems[1],
                                         0);
    end;
    Free;
  end;
end;

function THelpSelector.SelectKeyword(Keywords: TStrings): Integer;
var
  idx : integer;
  u : String;
begin
  idx:=-1;
  if TFormHelpSelector.Execute(Keywords, idx, u) then
    Result:=idx;
end;

function THelpSelector.TableOfContents(Contents: TStrings): Integer;
begin
  Result:=0;
end;

procedure TFormHelpSelector.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    ModalResult := mrCancel;
end;

procedure TFormHelpSelector.ListBox1Compare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
begin
  Compare:=CompareValue(Integer(Item1.Data),Integer(Item2.Data));
end;

procedure TFormHelpSelector.ListBox1DblClick(Sender: TObject);
begin      
  ModalResult:=mrOk;
end;

procedure TFormHelpSelector.ListBox1KeyPress(Sender: TObject; var Key: Char);
begin
  if Key=#13 then
    ListBox1DblClick(Sender);
end;

end.
