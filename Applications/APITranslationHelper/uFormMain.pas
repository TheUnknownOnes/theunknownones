unit uFormMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, uEffectPNGToolbar, ToolWin,
  Menus, uSysTools,
  IniFiles, RegExpr, Math, JvComponentBase, JvDragDrop;

type
  Tform_Main = class(TForm)
    gb_Source: TGroupBox;
    gb_Result: TGroupBox;
    pan_Client: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    EffectPNGToolBar1: TEffectPNGToolBar;
    btn_LoadSource: TEffectPNGToolButton;
    dlg_LoadSource: TOpenDialog;
    EffectPNGToolBar2: TEffectPNGToolBar;
    btn_SaveResult: TEffectPNGToolButton;
    dlg_SaveResult: TSaveDialog;
    dlg_LoadExpressions: TOpenDialog;
    dlg_SaveExpressions: TSaveDialog;
    pan_right: TPanel;
    gb_RegList: TGroupBox;
    lv_Expressions: TListView;
    EffectPNGToolBar3: TEffectPNGToolBar;
    btn_LoadExpression: TEffectPNGToolButton;
    btn_SaveExpressions: TEffectPNGToolButton;
    ToolButton3: TToolButton;
    btn_NewExpression: TEffectPNGToolButton;
    btn_RemoveExpression: TEffectPNGToolButton;
    btn_EditExpression: TEffectPNGToolButton;
    gb_Exec: TGroupBox;
    cb_RunPerLine: TCheckBox;
    btn_Run: TButton;
    btn_MoveUp: TEffectPNGToolButton;
    btn_MoveDown: TEffectPNGToolButton;
    cb_ModifierI: TCheckBox;
    cb_ModifierS: TCheckBox;
    cb_ModifierG: TCheckBox;
    pan_Progress: TPanel;
    pb_All: TProgressBar;
    pb_Expression: TProgressBar;
    cb_RemoveEmptyLines: TCheckBox;
    ed_Source: TMemo;
    ed_Result: TMemo;
    dt_Source: TJvDropTarget;
    procedure btn_LoadSourceClick(Sender: TObject);
    procedure btn_SaveResultClick(Sender: TObject);
    procedure btn_NewExpressionClick(Sender: TObject);
    procedure btn_RemoveExpressionClick(Sender: TObject);
    procedure btn_EditExpressionClick(Sender: TObject);
    procedure lv_ExpressionsDblClick(Sender: TObject);
    procedure btn_SaveExpressionsClick(Sender: TObject);
    procedure btn_LoadExpressionClick(Sender: TObject);
    procedure btn_RunClick(Sender: TObject);
    procedure btn_MoveUpClick(Sender: TObject);
    procedure lv_ExpressionsCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure btn_MoveDownClick(Sender: TObject);
    procedure cb_RunPerLineClick(Sender: TObject);
    procedure dt_SourceDragAccept(Sender: TJvDropTarget;
      var Accept: Boolean);
    procedure dt_SourceDragDrop(Sender: TJvDropTarget;
      var Effect: TJvDropEffect; Shift: TShiftState; X, Y: Integer);
    procedure dt_SourceDragEnter(Sender: TJvDropTarget;
      var Effect: TJvDropEffect);
  private
    function DoWork(AText : String) : String;
    function ReplaceSpecialChars(AText : String) : String;
  public
    { Public-Deklarationen }
  end;

var
  form_Main: Tform_Main;

implementation

uses uFormEditExpression;

{$R *.dfm}

procedure Tform_Main.btn_EditExpressionClick(Sender: TObject);
var
  Exp,
  Repl : String;
begin
  if lv_Expressions.ItemIndex > -1 then
  begin
    Exp := lv_Expressions.Selected.SubItems[0];
    Repl := lv_Expressions.Selected.SubItems[1];

    if Tform_EditExpression.Execute(Exp, Repl) then
    begin
      lv_Expressions.Selected.SubItems[0] := Exp;
      lv_Expressions.Selected.SubItems[1] := Repl;
    end;
  end;
end;

procedure Tform_Main.btn_LoadExpressionClick(Sender: TObject);
var
  idx : Integer;
  Ini : TIniFile;
  Li : TListItem;
  Section : String;
begin
  if dlg_LoadExpressions.Execute then
  begin
    lv_Expressions.Clear;

    Ini := TIniFile.Create(dlg_LoadExpressions.FileName);
    try

      for idx := 0 to Ini.ReadInteger('General', 'ItemCount', 0) - 1 do
      begin
        Section := Format('Item%.3d', [idx]);

        Li := lv_Expressions.Items.Add;

        Li.Data := Pointer(Ini.ReadInteger(Section, 'Pos', idx));
        Li.Checked := Ini.ReadBool(Section, 'Active', true);
        Li.SubItems.Add(Ini.ReadString(Section, 'Expression', '.*'));
        Li.SubItems.Add(Ini.ReadString(Section, 'ReplaceBy', '$0'));
      end;

    finally
      Ini.Free;
    end;
      
  end;
end;

procedure Tform_Main.btn_LoadSourceClick(Sender: TObject);
begin
  if dlg_LoadSource.Execute then
  begin
    ed_Source.Lines.LoadFromFile(dlg_LoadSource.FileName);
  end;
end;

procedure Tform_Main.btn_MoveDownClick(Sender: TObject);
var
  Pos : Integer;
  Next : TListItem;
  Li : TListItem;
begin
  Next := nil;

  if lv_Expressions.ItemIndex > -1 then
  begin
    Li := lv_Expressions.Selected;

    Pos := Integer(Li.Data);

    if Pos < lv_Expressions.Items.Count then
      Next := lv_Expressions.FindData(0, Pointer(Pos + 1), true, true);

    if Assigned(Next) then
    begin
      lv_Expressions.items.BeginUpdate;
      try
        Next.Data := Pointer(Integer(Next.Data) - 1);
        Li.Data := Pointer(Integer(Li.Data) + 1);
        lv_Expressions.SortType := stData;
      finally
        lv_Expressions.Items.EndUpdate;
      end;
    end;

  end;
end;

procedure Tform_Main.btn_MoveUpClick(Sender: TObject);
var
  Pos : Integer;
  Prev : TListItem;
  Li : TListItem;
begin
  Prev := nil;

  if lv_Expressions.ItemIndex > -1 then
  begin
    Li := lv_Expressions.Selected;

    Pos := Integer(Li.Data);

    if Pos > 0 then
      Prev := lv_Expressions.FindData(0, Pointer(Pos - 1), true, true);

    if Assigned(Prev) then
    begin
      lv_Expressions.items.BeginUpdate;
      try
        Prev.Data := Pointer(Integer(Prev.Data) + 1);
        Li.Data := Pointer(Integer(Li.Data) - 1);
        lv_Expressions.SortType := stData;
      finally
        lv_Expressions.Items.EndUpdate;
      end;
    end;

  end;
end;

procedure Tform_Main.btn_NewExpressionClick(Sender: TObject);
var
  Li : TListItem;
begin
  Li := lv_Expressions.Items.Add;
  Li.Checked := true;
  Li.SubItems.Add('.*');
  Li.SubItems.Add('$0');
  Li.Selected := true;
  Li.Data := Pointer(lv_Expressions.Items.Count - 1);

  btn_EditExpression.Click;
end;

procedure Tform_Main.btn_RemoveExpressionClick(Sender: TObject);
begin
  if lv_Expressions.ItemIndex > -1 then
  begin
    lv_Expressions.DeleteSelected;
  end;
end;

procedure Tform_Main.btn_RunClick(Sender: TObject);
var
  Line : String;
  idx : Integer;
  Lines : TStrings;
begin
  if lv_Expressions.Items.Count = 0 then
    exit;

  if cb_RunPerLine.Checked then
  begin
    ed_Result.Clear;

    Lines := TStringList.Create;

    pb_All.Max := ed_Source.Lines.Count - 1;

    for idx := 0 to ed_Source.Lines.Count - 1 do
    begin
      pb_All.Position := idx;
      pb_All.Repaint;

      Line:= DoWork(ed_Source.Lines[idx]);

      if cb_RemoveEmptyLines.Checked then
      begin
        if Trim(Line) <> EmptyStr then
          Lines.Add(Line);
      end
      else
        Lines.Add(Line);

    end;

    ed_Result.Lines.Assign(Lines);

    Lines.Free;
  end
  else
  begin
    pb_All.Position := pb_All.Min;
    pb_All.Repaint;

    ed_Result.Lines.Text := DoWork(ed_Source.Lines.Text);

    pb_All.Position := pb_All.Max;
  end;
end;

procedure Tform_Main.btn_SaveExpressionsClick(Sender: TObject);
var
  Ini : TIniFile;
  idx : Integer;
  LI : TListItem;
  Section : String;
begin
  if dlg_SaveExpressions.Execute then
  begin
    Ini := TIniFile.Create(dlg_SaveExpressions.FileName);
    try
      Ini.WriteInteger('General', 'ItemCount', lv_Expressions.Items.Count);

      for idx := 0 to lv_Expressions.Items.Count - 1 do
      begin
        Section := Format('Item%.3d', [idx]);

        Li := lv_Expressions.Items[idx];

        Ini.WriteBool(Section, 'Active', Li.Checked);
        Ini.WriteString(Section, 'Expression', Li.SubItems[0]);
        Ini.WriteString(Section, 'ReplaceBy', Li.SubItems[1]);
        Ini.WriteInteger(Section, 'Pos', Integer(Li.Data));
      end;
    finally
      Ini.Free;
    end;
  end;
end;

procedure Tform_Main.btn_SaveResultClick(Sender: TObject);
begin
  if dlg_SaveResult.Execute then
  begin
    ed_Result.Lines.SaveToFile(dlg_SaveResult.FileName);
  end;
end;

procedure Tform_Main.cb_RunPerLineClick(Sender: TObject);
begin
  cb_RemoveEmptyLines.Enabled := cb_RunPerLine.Checked;
end;

function Tform_Main.DoWork(AText : String) : String;
var
  Reg : TRegExpr;
  idx : Integer;
  Li : TListItem;
begin
  Result := AText;
  
  Reg := TRegExpr.Create;
  try
    Reg.InputString := AText;

    Reg.ModifierI := cb_ModifierI.Checked;
    Reg.ModifierS := cb_ModifierS.Checked;
    Reg.ModifierG := cb_ModifierG.Checked;
    Reg.ModifierM := cb_RunPerLine.Checked = false;

    pb_Expression.Max := lv_Expressions.Items.Count - 1;

    for idx := 0 to lv_Expressions.Items.Count - 1 do
    begin
      pb_Expression.Position := idx;
      pb_Expression.Repaint;

      Li := lv_Expressions.Items[idx];

      if Li.Checked then
      begin
        Reg.Expression := li.SubItems[0];
        if reg.Exec then
          Reg.InputString := reg.Replace(reg.InputString, ReplaceSpecialChars(li.SubItems[1]), true);
      end;
    end;

    Result := Reg.InputString;
  finally
    reg.Free;
  end;
end;

procedure Tform_Main.dt_SourceDragAccept(Sender: TJvDropTarget;
  var Accept: Boolean);
begin
  Accept := true;
end;

procedure Tform_Main.dt_SourceDragDrop(Sender: TJvDropTarget;
  var Effect: TJvDropEffect; Shift: TShiftState; X, Y: Integer);
var
  Files : TStringList;
begin
  Files := TStringList.Create;
  try
    Sender.GetFilenames(Files);

    if Files.Count > 0 then
      ed_Source.Lines.LoadFromFile(Files[0]);
  finally
    Files.Free;
  end;
end;

procedure Tform_Main.dt_SourceDragEnter(Sender: TJvDropTarget;
  var Effect: TJvDropEffect);
begin
  Effect := deCopy;
end;

procedure Tform_Main.lv_ExpressionsCompare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
begin
  Compare := CompareValue(Integer(Item1.Data), Integer(Item2.Data));
end;

procedure Tform_Main.lv_ExpressionsDblClick(Sender: TObject);
begin
  btn_EditExpression.Click;
end;

function Tform_Main.ReplaceSpecialChars(AText: String): String;
begin
  Result := MultipleStringReplace(AText, ['\t',
                                          '\n',
                                          '\r',
                                          '\f',
                                          '\a',
                                          '\e'],
                                         [#$09,
                                          #$0A,
                                          #$0D,
                                          #$0C,
                                          #$07,
                                          #$1B], [rfReplaceAll]);
end;

end.
