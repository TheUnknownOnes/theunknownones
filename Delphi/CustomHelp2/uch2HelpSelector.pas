unit uch2HelpSelector;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, HelpIntfs, StdCtrls, ComCtrls, ExtCtrls;

type
  Tch2formHelpSelector = class(TForm)
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    lst_Keywords: TListBox;
    tv_Help: TTreeView;
    Progress: TProgressBar;
    tm_DelayedFirstSearch: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure lst_KeywordsClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tm_DelayedFirstSearchTimer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tv_HelpDblClick(Sender: TObject);
  private
    procedure SearchForHelp(AKeyword : String);
  public
    class procedure Execute;
  end;


implementation

uses uch2Main;

{$R *.dfm}

{ Tch2formHelpSelector }

class procedure Tch2formHelpSelector.Execute;
var
  form : Tch2formHelpSelector;
begin
  form := Tch2formHelpSelector.Create(nil);
  try
    form.ShowModal;
  finally
    form.Free;
  end;
end;

procedure Tch2formHelpSelector.FormCreate(Sender: TObject);
var
  idx : Integer;
begin
  lst_Keywords.Items.Assign(ch2Main.HelpViewer.Keywords);

  for idx := 0 to ch2Main.Providers.Count - 1 do
  begin
    ch2Main.Provider[idx].StartHelpSession;
  end;
end;

procedure Tch2formHelpSelector.FormDestroy(Sender: TObject);
var
  idx : Integer;
begin
  for idx := 0 to ch2Main.Providers.Count - 1 do
  begin
    ch2Main.Provider[idx].StopHelpSession;
  end;
end;

procedure Tch2formHelpSelector.FormShow(Sender: TObject);
begin
  tm_DelayedFirstSearch.Enabled := true;
end;

procedure Tch2formHelpSelector.lst_KeywordsClick(Sender: TObject);
begin
  SearchForHelp(lst_Keywords.Items[lst_Keywords.ItemIndex]);
end;

procedure Tch2formHelpSelector.SearchForHelp(AKeyword: String);
var
  idx : Integer;
begin
  tv_Help.Items.Clear;
  Progress.Position := 0;
  Progress.Max := ch2Main.Providers.Count - 1;

  for idx := 0 to ch2Main.Providers.Count - 1 do
  begin
    ch2Main.Provider[idx].FillHelpTree(AKeyword, tv_Help);
    Progress.Position := idx;
    Progress.Repaint;
  end;

  Progress.Position := 0;
end;

procedure Tch2formHelpSelector.tm_DelayedFirstSearchTimer(Sender: TObject);
begin
  tm_DelayedFirstSearch.Enabled := false;
  lst_Keywords.ItemIndex := lst_Keywords.Items.IndexOf(ch2Main.HelpViewer.HelpString);
  lst_KeywordsClick(Sender);
end;

procedure Tch2formHelpSelector.tv_HelpDblClick(Sender: TObject);
var
  idx : Integer;
begin
  if Assigned(tv_Help.Selected) then
  begin
    for idx := 0 to ch2Main.Providers.Count - 1 do
    begin
      ch2Main.Provider[idx].HandleDoubleClick(tv_help.Selected, lst_Keywords.Items[lst_Keywords.ItemIndex]);
    end;
  end;

  ModalResult := mrOk;
end;

end.
