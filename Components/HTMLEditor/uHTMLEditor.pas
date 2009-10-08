unit uHTMLEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, OleCtrls, SHDocVw, ImgList, ComCtrls, ToolWin, StdCtrls, MSHTML, Activex,
  JvDialogs, StrUtils, ExtCtrls, Menus, Buttons, JclFileUtils, JvExControls,
  JvComponent, JvTimeLine, Spin, CheckLst, JvColorBox, JvColorButton;

type
  TfrmHTMLEditor = class(TFrame)
    Browser: TWebBrowser;
    iml_tb_Font: TImageList;
    dlg_Color: TJvColorDialog;
    iml_tb_Align: TImageList;
    iml_tb_Lists: TImageList;
    iml_tb_Table: TImageList;
    pan_TopDelimi: TPanel;
    iml_TL: TImageList;
    pum_Cats: TPopupMenu;
    mi_NewCat: TMenuItem;
    mi_DelCat: TMenuItem;
    iml_tb_Links: TImageList;
    CoolBar2: TPanel;
    tb_Table: TToolBar;
    tbtn_InsertTable: TToolButton;
    tbtn_ToolButton3: TToolButton;
    tbtn_AddRow: TToolButton;
    tbtn_DelRow: TToolButton;
    tbtn_AddCol: TToolButton;
    tbtn_DelCol: TToolButton;
    tbtn_AddCell: TToolButton;
    tbtn_DelCell: TToolButton;
    tbtn_ToolButton4: TToolButton;
    pan_img_Edit: TPanel;
    tbtn_PropTable: TToolButton;
    tbtn_PopRow: TToolButton;
    tbtn_PropCol: TToolButton;
    tbtn_PropCell: TToolButton;
    CoolBar1: TPanel;
    tb_Font: TToolBar;
    com_FontName: TComboBox;
    tbtn_ToolButton1: TToolButton;
    tbtn_Bold: TToolButton;
    tbtn_Italic: TToolButton;
    tbtn_Underline: TToolButton;
    tbtn_Strike: TToolButton;
    tbtn_ToolButton2: TToolButton;
    tbtn_FontColor: TToolButton;
    tb_Align: TToolBar;
    tbtn_Left: TToolButton;
    tbtn_Center: TToolButton;
    tbtn_Right: TToolButton;
    tb_Lists: TToolBar;
    tbtn_Numbering: TToolButton;
    tbtn_Bullets: TToolButton;
    tbtn_ToolButton5: TToolButton;
    tbtn_Indent: TToolButton;
    tbtn_Outdent: TToolButton;
    tb_Links: TToolBar;
    tbtn_EditLink: TToolButton;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton5: TToolButton;
    tbtn_HR: TToolButton;
    tm_WatchSelection: TTimer;
    com_FontSize: TComboBox;
    tbtn_EditImage: TToolButton;
    tm_Init: TTimer;
    img_Edit: TImage;
    procedure tm_WatchSelectionTimer(Sender: TObject);
    procedure tbtn_HRClick(Sender: TObject);
    procedure tbtn_EditLinkClick(Sender: TObject);
    procedure ed_TitleChange(Sender: TObject);
    procedure tbtn_AddCellMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure tbtn_DelCellClick(Sender: TObject);
    procedure tbtn_OutdentClick(Sender: TObject);
    procedure tbtn_IndentClick(Sender: TObject);
    procedure tbtn_PropColClick(Sender: TObject);
    procedure tbtn_DelColClick(Sender: TObject);
    procedure tbtn_AddColMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure tbtn_DelRowClick(Sender: TObject);
    procedure tbtn_AddRowMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure tbtn_PropCellClick(Sender: TObject);
    procedure tbtn_PopRowClick(Sender: TObject);
    procedure tbtn_PropTableClick(Sender: TObject);
    procedure tbtn_InsertTableClick(Sender: TObject);
    procedure tbtn_NumberingClick(Sender: TObject);
    procedure tbtn_BulletsClick(Sender: TObject);
    procedure tbtn_RightClick(Sender: TObject);
    procedure tbtn_CenterClick(Sender: TObject);
    procedure tbtn_LeftClick(Sender: TObject);
    procedure tbtn_FontColorClick(Sender: TObject);
    procedure tbtn_StrikeClick(Sender: TObject);
    procedure tbtn_UnderlineClick(Sender: TObject);
    procedure tbtn_ItalicClick(Sender: TObject);
    procedure tbtn_BoldClick(Sender: TObject);
    procedure com_FontSizeChange(Sender: TObject);
    procedure com_FontNameChange(Sender: TObject);
    procedure com_FontSizeKeyPress(Sender: TObject; var Key: Char);
    procedure SpellChecker1Cancel(Sender: TObject);
    procedure tbtn_EditImageClick(Sender: TObject);
    procedure tm_InitTimer(Sender: TObject);
  private
    FBreakSpellCheck: Boolean;
    FDoc : IHTMLDocument2;
    FAppPath : String;

    procedure AfterConstruction; override;

    procedure BrowserSetFocus();
    function GetCurTable(var ATable : IHTMLTable) : Boolean;
    function GetCurRow(var ARow : IHTMLTableRow) : Boolean;
    function GetCurCell(var ACell : IHTMLTableCell) : Boolean;

    procedure LoadEditFromStream(AStream : TStream);
    procedure SaveEditToStream(AStream : TStream);

    procedure Init();

    function ColorToHTMLColor(AColor : TColor) : String;
    function HTMLColorToColor(AHTMLColor : String) : TColor;

    function GetCurLink(var ALink : IHTMLAnchorElement) : Boolean;
    function GetCurImage(var AImage: IHTMLImgElement): Boolean;
  end;

  THTMLEditor = class(TCustomPanel)
  private
    FFrame : TfrmHTMLEditor;
    function GetDocument: IHTMLDocument2;
  public
    constructor Create(AOwner: TComponent); override;

    property Document: IHTMLDocument2 read GetDocument;
    procedure Load(AString: String);
  published
    property Align;
  end;

{$REGION 'const'}
const
  FORM_CAPTION='InTeam-Editor [%s]';
{$ENDREGION}

procedure Register;

implementation

uses uHtmlInsertTable, uHtmlEditTable, uHtmlEditCell, uHtmlInsertCell,
     DateUtils, JvSpin;

{$R *.dfm}

{ Tform_ITEditor }

{$REGION 'Initialization-Stuff'}

procedure TfrmHTMLEditor.Init();
var
  idx : Integer;
begin    
  FAppPath:=IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName));

  Browser.Navigate('about:blank');
  tm_Init.Enabled:=True;

{$REGION 'TextFeld initialisieren'}
  FDoc:=Browser.Document as IHTMLDocument2;
  FDoc.designMode:='On';

//  LoadEditFromStream(ArticleData.Text);
{$ENDREGION}

{$REGION 'Comboboxen füllen'}
  com_FontName.Clear;
  com_FontName.Items.Assign(Screen.Fonts);
  com_FontName.ItemIndex:=com_FontName.Items.IndexOf(Self.Font.Name);

  com_FontSize.Clear;
  for idx := 1 to 7 do
    com_FontSize.Items.AddObject(IntToStr(idx),TObject(idx));
  com_FontSize.Text:='3';
{$ENDREGION} 
end;


{$ENDREGION}

{$REGION 'FontName'}

procedure TfrmHTMLEditor.com_FontNameChange(Sender: TObject);
begin
  FDoc.execCommand('FontName',false,com_FontName.Text);
  BrowserSetFocus;
end;

{$ENDREGION}

{$REGION 'FontSize'}

procedure TfrmHTMLEditor.com_FontSizeChange(Sender: TObject);
begin
  FDoc.execCommand('FontSize',false,com_FontSize.Text);
end;

procedure TfrmHTMLEditor.com_FontSizeKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key='+') and (com_FontSize.ItemIndex<com_FontSize.Items.Count-1) then
    com_FontSize.ItemIndex:=com_FontSize.ItemIndex+1;
  if (Key='-') and (com_FontSize.ItemIndex>0) then
    com_FontSize.ItemIndex:=com_FontSize.ItemIndex-1;
  if not (Key in ['0'..'9']) then
    Key:=#0;
  com_FontSizeChange(Sender);
end;

{$ENDREGION}

{$REGION 'Bold/Italic/Underline/StrikeThrough/...'}

procedure TfrmHTMLEditor.tbtn_BoldClick(Sender: TObject);
begin
  FDoc.execCommand('Bold',false,'');
end;

procedure TfrmHTMLEditor.tbtn_ItalicClick(Sender: TObject);
begin
  FDoc.execCommand('Italic',false,'');
end;

procedure TfrmHTMLEditor.tbtn_StrikeClick(Sender: TObject);
begin
  FDoc.execCommand('StrikeThrough',false,'');
end;

procedure TfrmHTMLEditor.tbtn_UnderlineClick(Sender: TObject);
begin
  FDoc.execCommand('Underline',false,'');
end;

{$ENDREGION}

{$REGION 'FontColor'}

procedure TfrmHTMLEditor.tbtn_FontColorClick(Sender: TObject);
begin
  if dlg_Color.Execute(Self.Handle) then
  begin
    FDoc.execCommand('ForeColor',false,ColorToHTMLColor(dlg_Color.Color));
    BrowserSetFocus;
  end;
end;

{$ENDREGION}

{$REGION 'Alignment'}

procedure TfrmHTMLEditor.tbtn_LeftClick(Sender: TObject);
begin
  FDoc.execCommand('JustifyLeft',false,'');
end;

procedure TfrmHTMLEditor.tbtn_CenterClick(Sender: TObject);
begin
  FDoc.execCommand('JustifyCenter',false,'');
end;

procedure TfrmHTMLEditor.tbtn_RightClick(Sender: TObject);
begin
  FDoc.execCommand('JustifyRight',false,'');
end;

{$ENDREGION}

{$REGION 'Misc'}

procedure TfrmHTMLEditor.tm_InitTimer(Sender: TObject);
begin
  if Browser.ReadyState = READYSTATE_COMPLETE then
  begin
    tm_Init.Enabled:=False;
  end;
end;

procedure TfrmHTMLEditor.tm_WatchSelectionTimer(Sender: TObject);
begin
  try
    if not com_FontName.Focused then
      com_FontName.Text:=FDoc.queryCommandValue('FontName');
    if not com_FontSize.Focused then
      com_FontSize.Text:=FDoc.queryCommandValue('FontSize');

    tbtn_Bold.Down:=StrToBoolDef(FDoc.queryCommandValue('Bold'),false);
    tbtn_Italic.Down:=StrToBoolDef(FDoc.queryCommandValue('Italic'),false);
    tbtn_Underline.Down:=StrToBoolDef(FDoc.queryCommandValue('Underline'),false);
    tbtn_Strike.Down:=StrToBoolDef(FDoc.queryCommandValue('StrikeThrough'),false);

    tbtn_Left.Down:=StrToBoolDef(FDoc.queryCommandValue('JustifyLeft'),false);
    tbtn_Center.Down:=StrToBoolDef(FDoc.queryCommandValue('JustifyCenter'),false);
    tbtn_Right.Down:=StrToBoolDef(FDoc.queryCommandValue('JustifyRight'),false);

    tbtn_Numbering.Down:=StrToBoolDef(FDoc.queryCommandValue('InsertOrderedList'),false);
    tbtn_Bullets.Down:=StrToBoolDef(FDoc.queryCommandValue('InsertUnorderedList'),false);
  except end;
end;

procedure TfrmHTMLEditor.AfterConstruction;
begin
  inherited;
  Init;
end;

procedure TfrmHTMLEditor.BrowserSetFocus;
begin
  Windows.SetFocus(FindWindowEx(FindWindowEx(Browser.Handle,0,Pchar('Shell DocObject View'),nil),0,Pchar('Internet Explorer_Server'),nil));
end;

function TfrmHTMLEditor.ColorToHTMLColor(AColor : TColor): String;
var
  ColorString : String;
begin
  ColorString:=IntToHex(AColor,8);
  ColorString:=RightStr(ColorString,Length(ColorString)-2);
  Result:='#'+Copy(ColorString,5,2)+Copy(ColorString,3,2)+Copy(ColorString,1,2);
end;

function TfrmHTMLEditor.HTMLColorToColor(AHTMLColor: String): TColor;
begin
  if AnsiStartsText('#',AHTMLColor) then
    AHTMLColor:=RightStr(AHTMLColor,Length(AHTMLColor)-1);
  AHTMLColor:=Copy(AHTMLColor,5,2)+Copy(AHTMLColor,3,2)+Copy(AHTMLColor,1,2);
  if AHTMLColor='' then AHTMLColor:='FFFFFF';
  try
    Result:=TColor(StrToInt('$'+AHTMLColor));
  except
    Result:=clWhite;
  end;
end;

procedure TfrmHTMLEditor.LoadEditFromStream(AStream: TStream);
var
  StreamAdap : TStreamAdapter;
begin
  AStream.Seek(soFromBeginning,0);
  StreamAdap:=TStreamAdapter.Create(AStream);
  (Browser.Document as IPersistStreamInit).Load(StreamAdap);
end;

procedure TfrmHTMLEditor.SaveEditToStream(AStream: TStream);
var
  StreamAdap : TStreamAdapter;
  //sl : TStrings;
begin
  {sl:=TStringList.Create;
  sl.Text:='<html>'+(Browser.Document as IHTMLDocument3).documentElement.innerHTML+'</html>';
  sl.SaveToStream(AStream);
  sl.free;}
  StreamAdap:=TStreamAdapter.Create(AStream);
  (Browser.Document as IPersistStreamInit).Save(StreamAdap,false);
end;

procedure TfrmHTMLEditor.SpellChecker1Cancel(Sender: TObject);
begin
  FBreakSpellCheck:=True;
end;

procedure TfrmHTMLEditor.ed_TitleChange(Sender: TObject);
begin

end;

{$ENDREGION}

{$REGION 'Lists'}

procedure TfrmHTMLEditor.tbtn_BulletsClick(Sender: TObject);
begin
  FDoc.execCommand('InsertUnorderedList',false,'');
end;

procedure TfrmHTMLEditor.tbtn_NumberingClick(Sender: TObject);
begin
  FDoc.execCommand('InsertOrderedList',false,'');
end;

procedure TfrmHTMLEditor.tbtn_IndentClick(Sender: TObject);
begin
  FDoc.execCommand('Indent',false,'');
end;

procedure TfrmHTMLEditor.tbtn_OutdentClick(Sender: TObject);
begin
  FDoc.execCommand('Outdent',false,'');
end;

procedure TfrmHTMLEditor.tbtn_HRClick(Sender: TObject);
var
  Line : IHTMLHRElement;
  SelObject : IHTMLControlRange;
  SelText : IHTMLTxtRange;
  Node : IHTMLDOMNode;
begin
  Line:=Fdoc.createElement('hr') as IHTMLHRElement;
  if (FDoc.selection.type_='Control') then
  begin
    SelObject:=FDoc.Selection.createRange as IHTMLControlRange;
    Node:=SelObject.item(0) as IHTMLDOMNode;
  end
  else
  begin
    SelText:=FDoc.Selection.createRange as IHTMLTxtRange;
    if SelText.parentElement=nil then
      Node:=FDoc.Body as IHTMLDOMNode
    else
      Node:=SelText.ParentElement as IHTMLDomNode;
  end;
  try
    Node.appendChild(Line as IHTMLDomNode);

  except end;
end;

{$ENDREGION}

{$REGION 'Tabellen-Helper'}

function TfrmHTMLEditor.GetCurCell(var ACell: IHTMLTableCell): Boolean;
var
  SelText : IHTMLTxtRange;
  Parent : IHTMLElement;
begin
  Result:=false;
  if (FDoc.selection.type_='None') or (FDoc.selection.type_='Text') then
  begin
    SelText:=FDoc.selection.CreateRange as IHTMLTxtRange;
    Parent:=SelText.parentElement;
    while Parent<>nil do
      if Supports(Parent,IHTMLTableCell,ACell) then
      begin
        Result:=true;
        break;
      end
      else
        Parent:=Parent.parentElement;
  end;
end;

function TfrmHTMLEditor.GetCurRow(var ARow: IHTMLTableRow): Boolean;
var
  Cell : IHTMLTableCell;
  Parent : IHTMLDOMNode;
begin
  Result:=false;
  if GetCurCell(Cell) then
  begin
    Parent:=(Cell as IHTMLDOMNode).parentNode;
    while Parent<>nil do
      if Supports(Parent,IHTMLTableRow,ARow) then
      begin
        Result:=true;
        break;
      end
      else
        Parent:=Parent.parentNode;
  end;
end;

function TfrmHTMLEditor.GetCurTable(var ATable: IHTMLTable): Boolean;
var
  SelObject : IHTMLControlRange;
  Row : IHTMLTableRow;
  Parent : IHTMLDOMNode;
begin
  Result:=false;
  if (FDoc.selection.type_='Control') then
  begin
    SelObject:=FDoc.selection.createRange as IHTMLControlRange;
    if Supports(SelObject.item(0),IHTMLTable,ATable) then
      Result:=true;
  end
  else //keine Tabelle selektiert, hoffentlich stehen mit dem Cursor in der Tabelle
  begin
    if GetCurRow(Row) then
    begin
      Parent:=(Row as IHTMLDOMNode).parentNode;
      while Parent<>nil do
        if (Supports(Parent,IHTMLTable,ATable)) then
        begin
          Result:=true;
          exit;
        end
        else
          Parent:=Parent.parentNode
    end;
  end;
end;

{$ENDREGION}

{$REGION 'Tabellen-Teile hinzufügen/löschen'}

procedure TfrmHTMLEditor.tbtn_InsertTableClick(Sender: TObject);
var
  tbl : IHTMLTable;
  idxRow, idxCol : Integer;
  Row : IHTMLTableRow;
  Cell : IHTMLTableCell;
  SelText : IHTMLTxtRange;
  SelObject : IHTMLControlRange;
  Node : IHTMLDOMNode;
begin
  Application.CreateForm(Tform_ITInsertTable, form_ITInsertTable);
  try
    if (form_ITInsertTable.ShowModal=mrOk) and
        (form_ITInsertTable.ed_Rows.Value>0) and
        (form_ITInsertTable.ed_Cols.Value>0) then
    begin

      //Eine Tabelle irgendwo im Speichern
      tbl:=Fdoc.createElement('table') as IHTMLTable;
      {$REGION 'Tablleneigenschaften setzen'}
      tbl.border:=IntToStr(form_ITInsertTable.ed_BorderWidth.AsInteger);
      if form_ITInsertTable.cb_Border3D.Checked then
        tbl.borderColor:=''
      else
        tbl.borderColor:=ColorToHTMLColor(form_ITInsertTable.btn_BorderColor.Color);
      tbl.cellSpacing:=IntToStr(form_ITInsertTable.ed_CellSpacing.AsInteger);
      tbl.cellPadding:=IntToStr(form_ITInsertTable.ed_CellPadding.AsInteger);
      tbl.width:=IntToStr(form_ITInsertTable.ed_TableWidth.AsInteger)+'%';
      {$ENDREGION}
      for idxRow := 0 to form_ITInsertTable.ed_Rows.Value-1 do
      begin
        Row:=tbl.insertRow(idxRow) as IHTMLTableRow;
        for idxCol := 0 to form_ITInsertTable.ed_Cols.Value-1 do
        begin
          Cell:=Row.insertCell(idxCol) as IHTMLTableCell;
          if not form_ITInsertTable.cb_Border3D.Checked then
          begin
            Cell.borderColor:=tbl.borderColor;
            Cell.borderColorLight:=tbl.borderColorLight;
            Cell.borderColorDark:=tbl.borderColorDark;
          end;
        end;
      end;

      //Stelle suchen, wo wir einfügen sollen
      if (FDoc.selection.type_='Control') then
      begin
        SelObject:=FDoc.Selection.createRange as IHTMLControlRange;
        Node:=SelObject.item(0) as IHTMLDOMNode;
      end
      else
      begin
        SelText:=FDoc.Selection.createRange as IHTMLTxtRange;
        if SelText.parentElement=nil then
          Node:=FDoc.Body as IHTMLDOMNode
        else
          Node:=SelText.ParentElement as IHTMLDomNode;
      end;
      //... und einfügen
      Node.appendChild(tbl as IHTMLDomNode);
    end;
  finally
    form_ITInsertTable.Release;
  end;
end;


procedure TfrmHTMLEditor.tbtn_AddCellMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Tbl : IHTMLTable;
  Row : IHTMLTableRow;
  Cell : IHTMLTableCell;
  idxRow,idxCol : Integer;
begin
  if GetCurTable(tbl) and GetCurRow(Row) and GetCurCell(Cell) then
  begin
    Application.CreateForm(Tform_ITInsertCell, form_ITInsertCell);
    try
      if form_ITInsertCell.ShowModal=mrOk then
      begin
        idxRow:=Row.rowIndex;
        idxCol:=Cell.cellIndex;
        with form_ITInsertCell do
        begin
          if rb_TopLeft.Checked then begin Dec(idxRow); Dec(idxCol) end;
          if rb_TopRight.Checked then Dec(idxRow);
          if rb_BottomLeft.Checked then Dec(idxCol);
        end;
        (tbl.rows.item(idxRow,idxRow) as IHTMLTableRow).insertCell(idxCol);
      end;
    finally
      form_ITInsertCell.Release;
    end;
  end;
end;

procedure TfrmHTMLEditor.tbtn_AddColMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Tbl : IHTMLTable;
  Row : IHTMLTableRow;
  Cell,
  NewCell : IHTMLTableCell;
  idx : Integer;
begin
  if GetCurTable(Tbl) and GetCurCell(Cell) then
  begin
    for idx := 0 to Tbl.rows.length - 1 do
      if Supports(Tbl.rows.item(idx,idx),IHTMLTableRow,Row) then
      begin
        if ssShift in Shift then
          NewCell:=Row.insertCell(Cell.cellIndex) as IHTMLTableCell
        else
          NewCell:=Row.insertCell(Cell.cellIndex+1) as IHTMLTableCell;
        NewCell.bgColor:=Cell.bgColor;
        NewCell.borderColor:=Cell.borderColor;
      end;
  end;
end;

procedure TfrmHTMLEditor.tbtn_AddRowMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Row,NewRow : IHTMLTableRow;
  Tbl : IHTMLTable;
  idx : Integer;
begin
  if GetCurRow(Row) and GetCurTable(tbl) then
  begin
    if ssShift in Shift then
      NewRow:=tbl.insertRow(Row.rowIndex) as IHTMLTableRow
    else
      NewRow:=tbl.insertRow(Row.rowIndex+1) as IHTMLTableRow;
    for idx := 0 to Row.cells.length - 1 do
      NewRow.insertCell(idx);
  end;
end;

procedure TfrmHTMLEditor.tbtn_DelCellClick(Sender: TObject);
var
  Row : IHTMLTableRow;
  Cell : IHTMLTableCell;
begin
  if GetCurRow(Row) and GetCurCell(Cell) then
    Row.deleteCell(Cell.cellIndex);
end;

procedure TfrmHTMLEditor.tbtn_DelColClick(Sender: TObject);
var
  Tbl : IHTMLTable;
  Row : IHTMLTableRow;
  Cell : IHTMLTableCell;
  CellIndex : INteger;
  idx : Integer;
begin
  if GetCurTable(Tbl) and GetCurCell(Cell) then
  begin
    CellIndex:=Cell.cellIndex;
    for idx := 0 to Tbl.rows.length - 1 do
      if Supports(Tbl.rows.item(idx,idx),IHTMLTableRow,Row) then
        Row.deleteCell(CellIndex);
  end;
end;

procedure TfrmHTMLEditor.tbtn_DelRowClick(Sender: TObject);
var
  Row : IHTMLTableRow;
  Tbl : IHTMLTable;
begin
  if GetCurRow(Row) and GetCurTable(Tbl) then
    Tbl.deleteRow(Row.rowIndex);
end;

{$ENDREGION}

{$REGION 'Tabellen-Teile bearbeiten'}

procedure TfrmHTMLEditor.tbtn_PropTableClick(Sender: TObject);
var
  tbl:IHTMLTable;
begin
  if (GetCurTable(tbl)) then
  begin
    Application.CreateForm(Tform_ITEditTable, form_ITEditTable);
    try
      if (AnsiEndsText('%',tbl.width)) then
        form_ITEditTable.ed_Width.Value:=StrToInt(LeftStr(tbl.width,Length(tbl.width)-1))
      else
        form_ITEditTable.ed_Width.Value:=StrToInt(tbl.width);
      form_ITEditTable.ed_BorderWidth.Value:=StrToInt(tbl.border);
      form_ITEditTable.ed_CellPadding.Value:=StrToInt(tbl.cellPadding);
      form_ITEditTable.ed_CellSpacing.Value:=StrToInt(tbl.cellSpacing);
      form_ITEditTable.btn_BorderColor.Color:=HTMLColorToColor(tbl.borderColor);
      form_ITEditTable.cb_Border3D.Checked:=AnsiSameText(tbl.borderColor,'');
      if form_ITEditTable.ShowModal=mrOK then
      begin
        tbl.border:=IntToStr(form_ITEditTable.ed_BorderWidth.AsInteger);
        if form_ITEditTable.cb_Border3D.Checked then
          tbl.borderColor:=''
        else
          tbl.borderColor:=ColorToHTMLColor(form_ITEditTable.btn_BorderColor.Color);
        tbl.cellSpacing:=IntToStr(form_ITEditTable.ed_CellSpacing.AsInteger);
        tbl.cellPadding:=IntToStr(form_ITEditTable.ed_CellPadding.AsInteger);
        tbl.width:=InttoStr(form_ITEditTable.ed_Width.AsInteger)+'%';
      end;
    finally
      form_ITEditTable.Release;
    end;
  end;
  BrowserSetFocus;
end;

procedure TfrmHTMLEditor.tbtn_PropColClick(Sender: TObject);
var
  Tbl : IHTMLTable;
  row : IHTMLTableRow;
  Cell : IHTMLTableCell;
  idx : INteger;
begin
  if GetCurTable(tbl) and GetCurCell(Cell) then
  begin
    dlg_Color.Color:=HTMLColorToColor(Cell.bgColor);
    if dlg_Color.Execute(Self.Handle) then
    begin
      for idx := 0 to Tbl.rows.length - 1 do
        if Supports(Tbl.rows.item(idx,idx),IHTMLTableRow,Row) then
          if Supports(Row.cells.item(Cell.cellIndex,Cell.cellIndex),IHTMLTableCell,Cell) then
            Cell.bgColor:=ColorToHTMLColor(dlg_Color.Color);
    end;
  end
  else
    MessageDlg('Bitte setzen Sie den Cursor in die gewünschte Spalte in der Tabelle.', mtWarning, [mbOK], 0);
end;

procedure TfrmHTMLEditor.tbtn_PropCellClick(Sender: TObject);
var
  Cell : IHTMLTableCell;
begin
  if GetCurCell(Cell) then
  begin
    Application.CreateForm(Tform_ITEditCell, form_ITEditCell);
    try
      form_ITEditCell.cb_Border3d.Checked:=AnsiSameText(Cell.borderColor,'');
      form_ITEditCell.btn_BGColor.Color:=HTMLColorToColor(Cell.bgColor);
      form_ITEditCell.btn_BorderColor.Color:=HTMLColorToColor(Cell.borderColor);
      if Cell.colSpan>0 then
        form_ITEditCell.ed_ColSpan.Value:=Cell.colSpan;
      if Cell.rowSpan>0 then
        form_ITEditCell.ed_RowSpan.Value:=Cell.rowSpan;
      if form_ITEditCell.ShowModal=mrOk then
      begin
        Cell.bgColor:=ColorToHTMLColor(form_ITEditCell.btn_BGColor.Color);
        if form_ITEditCell.cb_Border3d.Checked then
          Cell.borderColor:=''
        else
          Cell.borderColor:=ColorToHTMLColor(form_ITEditCell.btn_BorderColor.Color);
        Cell.rowSpan:=form_ITEditCell.ed_RowSpan.AsInteger;
        Cell.colSpan:=form_ITEditCell.ed_ColSpan.AsInteger;
      end;
    finally
      form_ITEditCell.Release;
    end;
  end;
  BrowserSetFocus;
end;

procedure TfrmHTMLEditor.tbtn_PopRowClick(Sender: TObject);
var
  row : IHTMLTableRow;
begin
  if GetCurRow(Row) then
  begin
    dlg_Color.Color:=HTMLColorToColor(Row.bgColor);
    if dlg_Color.Execute(Self.Handle) then
      Row.bgColor:=ColorToHTMLColor(dlg_Color.Color);
  end
  else
    MessageDlg('Bitte setzen Sie den Cursor in die gewünschte Zeile in der Tabelle.', mtWarning, [mbOK], 0);
end;
{$ENDREGION}

{$REGION 'URLHandling'}

function TfrmHTMLEditor.GetCurLink(var ALink : IHTMLAnchorElement) : Boolean;
var
  SelText : IHTMLTxtRange;
  Parent : IHTMLElement;
begin
  Result:=false;
  if (FDoc.selection.type_='None') or (FDoc.selection.type_='Text') then
  begin
    SelText:=FDoc.selection.CreateRange as IHTMLTxtRange;
    Parent:=SelText.parentElement;
    while Parent<>nil do
      if Supports(Parent,IHTMLAnchorElement,ALink) then
      begin
        Result:=true;
        break;
      end
      else
        Parent:=Parent.parentElement;
  end;
end;

function TfrmHTMLEditor.GetCurImage(var AImage: IHTMLImgElement): Boolean;
var
  SelObject : IHTMLControlRange;
  Row : IHTMLTableRow;
  Parent : IHTMLDOMNode;
begin
  Result:=false;
  if (FDoc.selection.type_='Control') then
  begin
    SelObject:=FDoc.selection.createRange as IHTMLControlRange;
    if Supports(SelObject.item(0), IHTMLImgElement, AImage) then
      Result:=true;
  end;
end;

procedure TfrmHTMLEditor.tbtn_EditImageClick(Sender: TObject);
var
  Image : IHTMLImgElement;
  SelObject : IHTMLControlRange;
  SelText : IHTMLTxtRange;
  Node : IHTMLDOMNode;
  IsNew : Boolean;
  idxArticle : INteger;
begin
  IsNew:=false;
  if not GetCurImage(Image) then
  begin
    IsNew:=true;
    Image:=Fdoc.createElement('img') as IHTMLImgElement;
    Image.src:='http://';
    if (FDoc.selection.type_='Control') then
    begin
      SelObject:=FDoc.Selection.createRange as IHTMLControlRange;
      Node:=SelObject.item(0) as IHTMLDOMNode;
    end
    else
    begin
      SelText:=FDoc.Selection.createRange as IHTMLTxtRange;
      if SelText.parentElement=nil then
        Node:=FDoc.Body as IHTMLDOMNode
      else
        Node:=SelText.ParentElement as IHTMLDomNode;
    end;
    Node.appendChild(Image as IHTMLDomNode);
  end;

  Image.src:=InputBox('Bildquelle', '', Image.src);
end;

procedure TfrmHTMLEditor.tbtn_EditLinkClick(Sender: TObject);
var
  Link : IHTMLAnchorElement;
  SelObject : IHTMLControlRange;
  SelText : IHTMLTxtRange;
  Node : IHTMLDOMNode;
  IsNew : Boolean;
  idxArticle : INteger;
begin
  IsNew:=false;
  if not GetCurLink(Link) then
  begin
    IsNew:=true;
    Link:=Fdoc.createElement('a') as IHTMLAnchorElement;
    Link.href:='http://';
    (Link as IHTMLElement).innerText:='Link zu ...';
    if (FDoc.selection.type_='Control') then
    begin
      SelObject:=FDoc.Selection.createRange as IHTMLControlRange;
      Node:=SelObject.item(0) as IHTMLDOMNode;
    end
    else
    begin
      SelText:=FDoc.Selection.createRange as IHTMLTxtRange;
      if SelText.parentElement=nil then
        Node:=FDoc.Body as IHTMLDOMNode
      else
        Node:=SelText.ParentElement as IHTMLDomNode;
    end;
    Node.appendChild(Link as IHTMLDomNode);
  end;

  Link.href:=InputBox('Linkziel', '', Link.href);
end;

{$ENDREGION}

{ THTMLEditor }

constructor THTMLEditor.Create(AOwner: TComponent);
begin
  inherited;
  FFrame:=TfrmHTMLEditor.Create(Self);
  FFrame.Parent:=Self;
end;

procedure Register;
begin
  RegisterComponents('TUO', [THTMLEditor]);
end;

function THTMLEditor.GetDocument: IHTMLDocument2;
begin
  Result:=FFrame.Browser.Document as IHTMLDocument2;
end;

procedure THTMLEditor.Load(AString: String);
var
  ss : TStringStream;
begin
  ss:=TStringStream.Create(AString);
  try
    FFrame.LoadEditFromStream(ss);
  finally
    ss.Free;
  end;
end;

end.
