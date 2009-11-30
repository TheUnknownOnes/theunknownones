{----------------------------------------------------------------------------- 
 Project: Settings
 Purpose: Contains the form for selecting the properties to save 
 Created: 12.08.2008 08:18:50
 
 (c) by TheUnknownOnes under dwywbdbu license - see http://theunknownones.googlecode.com/svn/ for the license
 see http://www.TheUnknownOnes.net
-----------------------------------------------------------------------------}

unit uForm_EditSettingsComponentPropertyList;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls,
  uSettingsBase, uSettingsRTTI,
  WideStrUtils, Menus, StdCtrls, ExtCtrls, WideStrings, ImgList;

type
  Tform_EditComponentPropertyList = class(TForm)
    pan_Bottom: TPanel;
    btn_OK: TButton;
    btn_Cancel: TButton;   
    pum_TV: TPopupMenu;
    mi_CheckAll: TMenuItem;
    mi_UncheckAll: TMenuItem;
    mi_InvertChecks: TMenuItem;
    tv_Properties: TTreeView;
    iml_TV: TImageList;
    procedure FormShow(Sender: TObject);
    procedure mi_CheckAllClick(Sender: TObject);
    procedure mi_UncheckAllClick(Sender: TObject);
    procedure mi_InvertChecksClick(Sender: TObject);
    procedure btn_OKClick(Sender: TObject);
    procedure tv_PropertiesDeletion(Sender: TObject; Node: TTreeNode);
    procedure tv_PropertiesMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure tv_PropertiesKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FOrigList,
    FAllProperties,
    FWorkList : TsrPropertyList;
    function GetToSave(APropertyPath: TSettingName): Boolean;
    procedure SetToSave(APropertyPath: TSettingName; const Value: Boolean);

    function GetPropertyNode(APath : TSettingName): TTreeNode;
    function GetNamedNode(AName : TSettingName; AParent : TTreeNode) : TTreeNode;

    procedure PrepareImageList;
    function GetNodeChecked(ANode: TTreeNode): Boolean;
    procedure SetNodeChecked(ANode: TTreeNode; const Value: Boolean);

    property ToSave[APropertyPath : TSettingName] : Boolean read GetToSave write SetToSave;
    property NodeChecked[ANode : TTreeNode] : Boolean read GetNodeChecked write SetNodeChecked;
  public
  
  end;

  TNodeData = record
    PropertyPath : TSettingName;
  end;
  PNodeData = ^TNodeData;


function ShowComponentPropertyListEditor(const AList : TsrPropertyList;
                                         const AComponent : TObject) : Boolean;

implementation

{$R *.dfm}

function ShowComponentPropertyListEditor(const AList : TsrPropertyList;
                                         const AComponent : TObject) : Boolean;
var
  Form : Tform_EditComponentPropertyList;
begin
  Form := Tform_EditComponentPropertyList.Create(nil);
  try
    Form.FOrigList := AList;
    Form.FWorkList := TsrPropertyList.Create(nil);
    Form.FAllProperties := TsrPropertyList.Create(nil);
    Form.FAllProperties.ReadPropertiesFromObject(AComponent);
    try
      Form.FWorkList.Assign(AList);

      Result := IsPositiveResult(form.ShowModal);

      if Result then
        AList.Assign(Form.FWorkList);

    finally
      Form.FAllProperties.Free;
      Form.FWorkList.Free;
    end;

  finally
    Form.Release;
  end;
end;

{ Tform_EditComponentPropertyList }

procedure Tform_EditComponentPropertyList.btn_OKClick(Sender: TObject);
var
  idx : Integer;
  Node : TTreeNode;
begin
  for idx := 0 to tv_Properties.Items.Count - 1 do
  begin
    Node := tv_Properties.Items[idx];

    if Assigned(Node.Data) then
      ToSave[PNodeData(Node.Data).PropertyPath] := NodeChecked[Node];
  end;
end;

procedure Tform_EditComponentPropertyList.FormShow(Sender: TObject);
var
  idx : Integer;
  Node : TTreeNode;
  Data : PNodeData;
begin
  PrepareImageList;

  for idx := 0 to FAllProperties.Count - 1 do
  begin
    Node := GetPropertyNode(FAllProperties[idx]);

    New(Data);
    Node.Data := Data;
    Data.PropertyPath := FAllProperties[idx];

    NodeChecked[Node] := ToSave[FAllProperties[idx]];
  end;
end;

function Tform_EditComponentPropertyList.GetNamedNode(AName: TSettingName;
  AParent: TTreeNode): TTreeNode;
var
  idx : Integer;
  Node : TTreeNode;
begin
  Result := nil;

  for idx := 0 to tv_Properties.Items.Count - 1 do
  begin
    Node := tv_Properties.Items[idx];

    if (Node.Parent = AParent) and
       (SettingsNameStringMatches(Node.Text, AName, false)) then
    begin
      Result := Node;
      exit;
    end;
  end;

  if not Assigned(Result) then
  begin
    Result := tv_Properties.Items.AddChild(AParent, AName);
    Result.StateIndex := 0;
  end;
end;

function Tform_EditComponentPropertyList.GetNodeChecked(
  ANode: TTreeNode): Boolean;
begin
  Result := ANode.StateIndex = 2;
end;

function Tform_EditComponentPropertyList.GetPropertyNode(
  APath: TSettingName): TTreeNode;
var
  Splitter : TWideStringList;
  idx : Integer;
begin
  Splitter := TWideStringList.Create;
  try
    SettingsSplitPath(APath, Splitter);

    Result := nil;

    for idx := 0 to Splitter.Count - 1 do
      Result := GetNamedNode(Splitter[idx], Result);

  finally
    Splitter.Free;
  end;
end;

function Tform_EditComponentPropertyList.GetToSave(
  APropertyPath: TSettingName): Boolean;
begin
  Result := FWorkList.IndexOf(APropertyPath) > -1;
end;

procedure Tform_EditComponentPropertyList.mi_CheckAllClick(Sender: TObject);
var
  idx : Integer;
begin
  for idx := 0 to tv_Properties.Items.Count - 1 do
  begin
    if (tv_Properties.SelectionCount <= 1) or
       tv_Properties.Items[idx].Selected then
      NodeChecked[tv_Properties.Items[idx]] := true;
  end;
end;

procedure Tform_EditComponentPropertyList.mi_InvertChecksClick(Sender: TObject);
var
  idx : Integer;
begin
  for idx := 0 to tv_Properties.Items.Count - 1 do
  begin
    if (tv_Properties.SelectionCount <= 1) or
       tv_Properties.Items[idx].Selected then
      NodeChecked[tv_Properties.Items[idx]] := NodeChecked[tv_Properties.Items[idx]] xor true;
  end;
end;

procedure Tform_EditComponentPropertyList.mi_UncheckAllClick(Sender: TObject);
var
  idx : Integer;
begin
  for idx := 0 to tv_Properties.Items.Count - 1 do
  begin
    if (tv_Properties.SelectionCount <= 1) or
       tv_Properties.Items[idx].Selected then
      NodeChecked[tv_Properties.Items[idx]] := false;
  end;
end;

procedure Tform_EditComponentPropertyList.PrepareImageList;
var
  BMP : TBitmap;
const
  UNCHECKED : array[0..1] of Byte = ($65, $66);
  CHECKED : array[0..2] of Byte = ($65, $66, $62);
  x = 2;
  y = 2;
begin
  BMP := TBitmap.Create;
  try
    BMP.Width := iml_TV.Width;
    BMP.Height := iml_TV.Height;
    BMP.PixelFormat := pf24bit;

    bmp.Canvas.Brush.Color := clFuchsia;
    bmp.Canvas.Brush.Style := bsSolid;
    bmp.Canvas.FillRect(bmp.Canvas.ClipRect);

    iml_TV.AddMasked(BMP, clFuchsia);

    BMP.Canvas.Pen.Color := clBlack;
    BMp.Canvas.Pen.Style := psSolid;
    BMp.Canvas.Brush.Style := bsClear;
    BMp.Canvas.Font.Name := 'Marlett';
    BMP.Canvas.Font.Size := 10;

    BMP.Canvas.TextOut(x, y, Chr(UNCHECKED[0]));
    BMP.Canvas.TextOut(x, y, Chr(UNCHECKED[1]));

    iml_TV.AddMasked(BMP, clFuchsia);

    bmp.Canvas.Brush.Color := clFuchsia;
    bmp.Canvas.Brush.Style := bsSolid;
    bmp.Canvas.FillRect(bmp.Canvas.ClipRect);
    BMp.Canvas.Brush.Style := bsClear;

    BMP.Canvas.TextOut(x, y, Chr(CHECKED[0]));
    BMP.Canvas.TextOut(x, y, Chr(CHECKED[1]));
    BMP.Canvas.TextOut(x, y, Chr(CHECKED[2]));

    iml_TV.AddMasked(BMP, clFuchsia);
  finally
    BMP.Free;
  end;
end;

procedure Tform_EditComponentPropertyList.SetNodeChecked(ANode: TTreeNode;
  const Value: Boolean);
begin
  if not Assigned(ANode.Data) then
    exit;
    
  if Value then
    ANode.StateIndex := 2
  else
    ANode.StateIndex := 1;
end;

procedure Tform_EditComponentPropertyList.SetToSave(APropertyPath: TSettingName;
  const Value: Boolean);
var
  idx : Integer;
begin
  idx := FWorkList.IndexOf(APropertyPath);

  if Value then
  begin
    if idx = -1 then
      FWorkList.Add(APropertyPath);
  end
  else
  begin
    if idx > -1 then
      FWorkList.Delete(idx);                                   
  end;

end;

procedure Tform_EditComponentPropertyList.tv_PropertiesDeletion(Sender: TObject;
  Node: TTreeNode);
begin
  if Assigned(Node.Data) then
  begin
    Dispose(PNodeData(Node.Data));
    Node.Data := nil;
  end;
end;

procedure Tform_EditComponentPropertyList.tv_PropertiesKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var
  idx : Integer;
  Node : TTreeNode;
begin
  if Key in [VK_SPACE] then
  begin
    for idx := 0 to tv_Properties.Items.Count - 1 do
    begin
      Node := tv_Properties.Items[idx];

      if Node.Selected then
        NodeChecked[Node] := NodeChecked[Node] xor true;
    end;
  end;
  
end;

procedure Tform_EditComponentPropertyList.tv_PropertiesMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Node : TTreeNode;
begin
  Node := tv_Properties.GetNodeAt(X, Y);

  if Assigned(Node) and
    (htOnStateIcon in tv_Properties.GetHitTestInfoAt(X, Y)) then
    NodeChecked[Node] := NodeChecked[Node] xor true;
end;

end.
