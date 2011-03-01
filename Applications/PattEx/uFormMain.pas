unit uFormMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, RegExpr, StrUtils, StdCtrls, ExtCtrls, VirtualTrees, ComCtrls,
  uEffectPNGToolbar, ToolWin, uCustomNodeData, VTEditors, ImgList,
  uImageListProvider, uBaseImageList, uPNGImageList, Menus,
  IniFiles, activex, uFrameTest;

type
  TformMain = class(TForm)
    GroupBox1: TGroupBox;
    TVExpressions: TVirtualStringTree;
    EffectPNGToolBar1: TEffectPNGToolBar;
    btnDelExpr: TEffectPNGToolButton;
    btnAddExpr: TEffectPNGToolButton;
    btnMoveUp: TEffectPNGToolButton;
    btnMoveDown: TEffectPNGToolButton;
    PC: TPageControl;
    pbTests: TProgressBar;
    ts1: TTabSheet;
    tsAddTab: TTabSheet;
    PNGImageList1: TPNGImageList;
    ImageListProvider1: TImageListProvider;
    frameTest1: TframeTest;
    ToolButton1: TToolButton;
    btnLoad: TEffectPNGToolButton;
    btnSave: TEffectPNGToolButton;
    dlgLoad: TOpenDialog;
    dlgSave: TSaveDialog;
    btnRun: TEffectPNGToolButton;
    ToolButton2: TToolButton;
    btnCreateCode: TEffectPNGToolButton;
    ToolButton3: TToolButton;
    btn_Pause: TEffectPNGToolButton;
    procedure btnAddExprClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TVExpressionsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure TVExpressionsEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
    procedure TVExpressionsCreateEditor(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure TVExpressionsNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; NewText: string);
    procedure TVExpressionsChecked(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure btnDelExprClick(Sender: TObject);
    procedure btnMoveUpClick(Sender: TObject);
    procedure btnMoveDownClick(Sender: TObject);
    procedure PCChange(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnRunClick(Sender: TObject);
    procedure btnCreateCodeClick(Sender: TObject);
    procedure TVExpressionsDragDrop(Sender: TBaseVirtualTree; Source: TObject;
      DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState;
      Pt: TPoint; var Effect: Integer; Mode: TDropMode);
    procedure TVExpressionsDragOver(Sender: TBaseVirtualTree; Source: TObject;
      Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode;
      var Effect: Integer; var Accept: Boolean);
    procedure btn_PauseClick(Sender: TObject);
    procedure TVExpressionsNodeMoved(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
  private
    FUpdateCount : Integer;

    function FindNode(AID : String) : PVirtualNode;
  public
    procedure Changed;
    procedure CloseTab();
    procedure BeginUpdate;
    procedure EndUpdate;

    procedure DoRegEx(ACurrentText : String; ANode : PVirtualNode; AFrame : TframeTest);
  end;

var
  formMain: TformMain;

implementation

uses uFormCode, uPattExCommon;

{$R *.dfm}

procedure TformMain.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TformMain.btnAddExprClick(Sender: TObject);
begin
  TExpression.Create(TVExpressions, nil);
end;

procedure TformMain.btnCreateCodeClick(Sender: TObject);
var
  f : TformCode;
begin
  f := TformCode.Create(Application);
  try
    f.ShowModal;
  finally
    f.Free;
  end;
end;

procedure TformMain.btnDelExprClick(Sender: TObject);
var
  nd : PVirtualNode;
begin
  nd := TVExpressions.GetFirstSelected;
  if Assigned(nd) then
  begin
    if (MessageBox(0, 'Do you really want to delete this expression?', 'Delete', MB_ICONWARNING or MB_YESNO or MB_DEFBUTTON2) = idYes) then
      TVExpressions.DeleteNode(nd);
  end;
end;

procedure TformMain.btnLoadClick(Sender: TObject);
var
  ini : TIniFile;
  sl : TStringList;
  e : TExpression;
  s : String;
begin
  if dlgLoad.Execute then
  begin
    TVExpressions.Clear;

    ini := TIniFile.Create(dlgLoad.FileName);
    TVExpressions.BeginUpdate;
    BeginUpdate;
    sl := TStringList.Create;
    BeginUpdate;
    try
      ini.ReadSections(sl);

      for s in sl do
      begin
        e := TExpression.Create(TVExpressions, FindNode(ini.ReadString(s, 'ParentID', '-1')));

        e.ID := s;
        e.Expression := ini.ReadString(s, 'Expression', '.*');
        e.ReturnSubstitution := ini.ReadString(s, 'ReturnSubst', '$0');
        e.ChildSubstitution := ini.ReadString(s, 'ChildSubst', '$0');
        e.ModifierG := ini.ReadBool(s, 'ModG', RegExprModifierG);
        e.ModifierI := ini.ReadBool(s, 'ModI', RegExprModifierI);
        e.ModifierM := ini.ReadBool(s, 'ModM', RegExprModifierM);
        e.ModifierR := ini.ReadBool(s, 'ModR', RegExprModifierR);
        e.ModifierS := ini.ReadBool(s, 'ModS', RegExprModifierS);
        e.ModifierX := ini.ReadBool(s, 'ModX', RegExprModifierX);
        e.FirstMatchOnly := ini.ReadBool(s, 'FirstMatchOnly', false);

        if ini.ReadBool(s, 'Enabled', true) then
          TVExpressions.CheckState[e.Node] := csCheckedNormal
        else
          TVExpressions.CheckState[e.Node] := csUncheckedNormal;
      end;

    finally
      TVExpressions.EndUpdate;
      EndUpdate;
      EndUpdate;
      sl.Free;
      ini.Free;
    end;
  end;
end;

procedure TformMain.btnMoveDownClick(Sender: TObject);
var
  nd,
  trgt : PVirtualNode;
begin
  nd := TVExpressions.GetFirstSelected;
  trgt := TVExpressions.GetNext(nd);

  if Assigned(nd) and Assigned(trgt) then
  begin
    TVExpressions.MoveTo(nd, trgt, amInsertAfter, false);
    Changed;
  end;

end;

procedure TformMain.btnMoveUpClick(Sender: TObject);
var
  nd,
  trgt : PVirtualNode;
begin
  nd := TVExpressions.GetFirstSelected;
  trgt := TVExpressions.GetPrevious(nd);

  if Assigned(nd) and Assigned(trgt) then
  begin
    TVExpressions.MoveTo(nd, trgt, amInsertBefore, false);
    Changed;
  end;
end;

procedure TformMain.btnRunClick(Sender: TObject);
begin
  Changed;
end;

procedure TformMain.btnSaveClick(Sender: TObject);
var
  ini : TIniFile;
  s : String;
  node : PVirtualNode;
  e : TExpression;
begin
  if dlgSave.Execute then
  begin
    if FileExists(dlgSave.FileName) then
      DeleteFile(dlgSave.FileName);

    ini := TIniFile.Create(dlgSave.FileName);
    try
      node := TVExpressions.GetFirst;
      while Assigned(node) do
      begin
        e := TExpression(TExpression.GetNodeData(Node, TVExpressions));
        s := e.ID;

        ini.WriteString(s, 'Expression', e.Expression);
        ini.WriteString(s, 'ReturnSubst', e.ReturnSubstitution);
        ini.WriteString(s, 'ChildSubst', e.ChildSubstitution);
        ini.WriteBool(s, 'ModG', e.ModifierG);
        ini.WriteBool(s, 'ModI', e.ModifierI);
        ini.WriteBool(s, 'ModM', e.ModifierM);
        ini.WriteBool(s, 'ModR', e.ModifierR);
        ini.WriteBool(s, 'ModS', e.ModifierS);
        ini.WriteBool(s, 'ModX', e.ModifierX);
        ini.WriteBool(s, 'FirstMatchOnly', e.FirstMatchOnly);
        ini.WriteBool(s, 'Enabled', TVExpressions.CheckState[Node] = csCheckedNormal);

        if Node.Parent <> TVExpressions.RootNode then
        begin
          e := TExpression(TExpression.GetNodeData(Node.Parent, TVExpressions));
          ini.WriteString(s, 'ParentID', e.ID);
        end;

        node := TVExpressions.GetNext(Node);
      end;
    finally
      ini.Free;
    end;
  end;
end;

procedure TformMain.btn_PauseClick(Sender: TObject);
begin
  Changed;
end;

procedure TformMain.Changed;
var
  node : PVirtualNode;
  curtext : String;
  idx : Integer;
  f : TframeTest;
begin
  if FUpdateCount > 0 then exit;
  if btn_Pause.Down then exit;

  pbTests.Max := pc.PageCount - 1;
  try
    for idx := 0 to PC.PageCount - 2 do
    begin
      f := TframeTest(PC.Pages[idx].Controls[0]);
      curtext := f.memSource.Lines.Text;
      f.memMatches.Clear;
      node := TVExpressions.GetFirst();

      while Assigned(node) do
      begin
        if TVExpressions.CheckState[node] = csCheckedNormal then
          DoRegEx(curtext, node, f);
        node := TVExpressions.GetNextSibling(Node);
      end;

      pbTests.StepBy(1);
    end;
  finally
    pbTests.Position := 0;
  end;
end;

procedure TformMain.CloseTab();
begin
  pc.ActivePage.Free;
  pc.SelectNextPage(false);
  if pc.ActivePage = tsAddTab then
  begin
    pc.SelectNextPage(true);
    PCChange(PC);
  end;
end;

procedure TformMain.DoRegEx(ACurrentText: String; ANode: PVirtualNode;
  AFrame: TframeTest);
var
  e : TExpression;
  Node : PVirtualNode;
begin
  try
    e := TExpression(TExpression.GetNodeData(ANode, TVExpressions));

    if e.RegEx.Exec(ACurrentText) then
    begin
      repeat
        if e.ReturnSubstitution <> '' then
          AFrame.AddMatch(e, e.RegEx.Substitute(e.ReturnSubstitution));

        if e.ChildSubstitution <> '' then
        begin
          Node := TVExpressions.GetFirstChild(ANode);
          while Assigned(Node) do
          begin
            if TVExpressions.CheckState[Node] = csCheckedNormal then
              DoRegEx(e.RegEx.Substitute(e.ChildSubstitution), Node, AFrame);
            Node := TVExpressions.GetNextSibling(Node);
          end;
        end;

        if e.FirstMatchOnly then
          break;
      until not e.RegEx.ExecNext;
    end;
  except
    on Err : Exception do
    begin
      AFrame.Error(e, Err);
      exit;
    end;
  end;
end;

procedure TformMain.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount <= 0 then
  begin
    FUpdateCount := 0;
    Changed;
  end;
end;

function TformMain.FindNode(AID: String): PVirtualNode;
var
  Node : PVirtualNode;
  e : TExpression;
begin
  Result := nil;

  Node := TVExpressions.GetFirst;
  while Assigned(Node) do
  begin
    e := TExpression(TExpression.GetNodeData(Node));

    if e.ID = AID then
    begin
      Result := Node;
      break;
    end;

    Node := TVExpressions.GetNext(Node);
  end;
end;

procedure TformMain.FormCreate(Sender: TObject);
begin
  FUpdateCount := 0;
  TNodeDataEventHandler.InitEvents(TVExpressions);
end;

procedure TformMain.PCChange(Sender: TObject);
var
  ts : TTabSheet;
begin
  if PC.ActivePage = tsAddTab then
  begin
    ts := TTabSheet.Create(PC);
    ts.Caption := 'Test';
    ts.PageControl := PC;
    ts.PageIndex := tsAddTab.PageIndex;
    Pc.ActivePageIndex := ts.PageIndex;
    TframeTest.Create(ts).Parent := ts;
  end;
end;

procedure TformMain.TVExpressionsChecked(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  Changed;
end;

procedure TformMain.TVExpressionsCreateEditor(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
begin
  case Column of
    0..2: EditLink := TStringEditLink2.Create;
    3..9: EditLink := TComboEditLink.Create(BoolToStr(true, true) + ',' + BoolToStr(false, true), csDropDownList);
  end;
end;

procedure TformMain.TVExpressionsDragDrop(Sender: TBaseVirtualTree;
  Source: TObject; DataObject: IDataObject; Formats: TFormatArray;
  Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);
var
  Node : PVirtualNode;
  DropMode : TVTNodeAttachMode;
begin
  Node := Sender.DropTargetNode;
  case Mode of
    dmAbove: DropMode := amInsertBefore;
    dmOnNode: DropMode := amAddChildLast;
    dmBelow: DropMode := amInsertAfter;
    else
      exit;
  end;
  Sender.ProcessDrop(DataObject, Node, Effect, DropMode);
end;

procedure TformMain.TVExpressionsDragOver(Sender: TBaseVirtualTree;
  Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint;
  Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
begin
  Accept := true;
end;

procedure TformMain.TVExpressionsEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed := true;
end;

procedure TformMain.TVExpressionsGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  e : TExpression;
begin
  e := TExpression(TExpression.GetNodeData(Node, Sender));

  case Column of
    0: CellText := E.Expression;
    1: CellText := E.ReturnSubstitution;
    2: CellText := E.ChildSubstitution;
    3: CellText := BoolToStr(e.ModifierG, true);
    4: CellText := BoolToStr(e.ModifierI, true);
    5: CellText := BoolToStr(e.ModifierM, true);
    6: CellText := BoolToStr(e.ModifierR, true);
    7: CellText := BoolToStr(e.ModifierS, true);
    8: CellText := BoolToStr(e.ModifierX, true);
    9: CellText := BoolToStr(e.FirstMatchOnly, true);
  end;
end;

procedure TformMain.TVExpressionsNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; NewText: string);
var
  e : TExpression;
begin
  e := TExpression(TExpression.GetNodeData(Node, Sender));

   case Column of
    0: e.Expression := NewText;
    1: e.ReturnSubstitution := NewText;
    2: e.ChildSubstitution := NewText;
    3: e.ModifierG := StrToBoolDef(NewText, RegExprModifierG);
    4: e.ModifierI := StrToBoolDef(NewText, RegExprModifierI);
    5: e.ModifierM := StrToBoolDef(NewText, RegExprModifierM);
    6: e.ModifierS := StrToBoolDef(NewText, RegExprModifierS);
    7: e.ModifierR := StrToBoolDef(NewText, RegExprModifierR);
    8: e.ModifierX := StrToBoolDef(NewText, RegExprModifierX);
    9: e.FirstMatchOnly := StrToBoolDef(NewText, false)
  end;
end;

procedure TformMain.TVExpressionsNodeMoved(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  Changed;
end;

end.
