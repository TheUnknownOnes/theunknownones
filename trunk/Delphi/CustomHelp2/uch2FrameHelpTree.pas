unit uch2FrameHelpTree;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, uch2Main, ImgList, uch2Data;

type
  Tch2FrameHelpTree = class(TFrame)
    TreeView1: TTreeView;
    Panel1: TPanel;
    Label1: TLabel;
    cbKeywords: TComboBox;
    procedure cbKeywordsCloseUp(Sender: TObject);
    procedure cbKeywordsKeyPress(Sender: TObject; var Key: Char);
    procedure TreeView1AdvancedCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
      var PaintImages, DefaultDraw: Boolean);
    procedure TreeView1DblClick(Sender: TObject);
    procedure TreeView1KeyPress(Sender: TObject; var Key: Char);
    procedure TreeView1Expanded(Sender: TObject; Node: TTreeNode);
  public
    function AddHelpItem(AHelpItem : Ich2HelpItem; AParent : Pointer = nil) : Pointer;
    procedure Init(const AHelpString: String; const Ach2Keywords: TStringList; ASeachImmediate : Boolean = true);

    procedure ShowHelp(FKeyword: String);
  end;

implementation

uses
  Registry;

const
  REG_KEY_SETTINGS_STATS = '\Stats\';
  REG_VALUE_STATS_EXPANDED = 'Expanded';

  IMG_INDENT_PLUSMINUS = 35;
  IMG_INDENT_IMAGE = 18;

{$R *.dfm}

type
  TNodeData = class(TObject)
  private
    FItemInterface : Ich2HelpItem;
    function GetExpanded: Boolean;
    procedure SetExpanded(const Value: Boolean);
  public
    constructor Create(AItemInterface: Ich2HelpItem);
    destructor Destroy; override;
    procedure Execute;

    property Expanded: Boolean read GetExpanded write SetExpanded;
  end;

{ TFrame1 }

function Tch2FrameHelpTree.AddHelpItem(AHelpItem: Ich2HelpItem;
  AParent: Pointer): Pointer;
var
  Node : TTreeNode;
begin
  Node:=TreeView1.Items.AddChild(AParent, AHelpItem.GetCaption);
  Node.Data:=TNodeData.Create(AHelpItem);

  if Assigned(AParent) then
  begin
    if TNodeData(TTreeNode(AParent).Data).Expanded then
      TTreeNode(AParent).Expanded:=True;
  end;

  if ifProvidesHelp in AHelpItem.GetFlags then
    Node.ImageIndex:=2
  else
    Node.ImageIndex:=-1;

  Result:=Node;
end;

procedure Tch2FrameHelpTree.cbKeywordsCloseUp(Sender: TObject);
begin
  if cbKeywords.ItemIndex>=0 then
    ShowHelp(cbKeywords.Items[cbKeywords.ItemIndex]);
end;

procedure Tch2FrameHelpTree.cbKeywordsKeyPress(Sender: TObject; var Key: Char);
begin
  if Key=#13 then
    ShowHelp(cbKeywords.Text);
end;

procedure Tch2FrameHelpTree.Init(const AHelpString: String;
  const Ach2Keywords: TStringList; ASeachImmediate : Boolean = true);
begin
  cbKeywords.Items.Text:=Ach2Keywords.Text;
  cbKeywords.Text:=AHelpString;
  if ASeachImmediate then
    ShowHelp(AHelpString);
end;

procedure Tch2FrameHelpTree.ShowHelp(FKeyword: String);
var
  Intf : IInterface;
  IProv : Ich2Provider absolute Intf;
  IGUI : Ich2GUI;
begin
  Screen.Cursor := crHourGlass;
  TreeView1.Items.BeginUpdate;
  try
    while Treeview1.Items.Count>0 do
    begin
      TObject(TreeView1.Items[TreeView1.Items.Count-1].Data).Free;
      TreeView1.Items[TreeView1.Items.Count-1].Free;
    end;

    for intf in ch2Main.Providers do
    begin
      IProv.ProvideHelp(FKeyword, ch2Main.CurrentGUI);
    end;
  finally
    TreeView1.Items.EndUpdate;
    Screen.Cursor := crDefault;
  end;
end;

procedure Tch2FrameHelpTree.TreeView1AdvancedCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
  var PaintImages, DefaultDraw: Boolean);
var
  HelpItem : Ich2HelpItem;
  Deco : Tch2HelpItemDecoration;
  Rect : TRect;
  s    : String;
  bmp  : TIcon;
begin
  DefaultDraw:=False;
  PaintImages:=False;
  try
    HelpItem := TNodeData(Node.Data).FItemInterface;
    Sender.Canvas.Font:=TreeView1.Font;

    Deco:=HelpItem.GetDecoration;
    if Deco.BackColor=clDefault then
      Sender.Canvas.Brush.Color:=clWindow
    else
      Sender.Canvas.Brush.Color:=Deco.BackColor;

    if Deco.TextColor=clDefault then
      Sender.Canvas.Font.Color:=clWindowText
    else
      Sender.Canvas.Font.Color:=Deco.TextColor;

    Sender.Canvas.Font.Style:=Deco.FontStyles;

    if (Stage=cdPrePaint) then
    begin
      Rect:=Node.DisplayRect(False);
      Sender.Canvas.FillRect(Rect);

      Rect:=Node.DisplayRect(True);
      s:=Node.Text;
      if trim(HelpItem.GetDescription)<>'' then
        s:=s+' ('+HelpItem.GetDescription+')';

      Rect.Right:=Rect.Left+Sender.Canvas.TextExtent(s).cx;
      Sender.Canvas.TextRect(Rect, s);

      bmp:=TIcon.Create;
      if Node.Expanded then
        TreeView1.Images.GetIcon(iiMinus, bmp)
      else
      if Node.HasChildren then
        TreeView1.Images.GetIcon(iiPlus,bmp);

      Sender.Canvas.Draw(Rect.Left-IMG_INDENT_PLUSMINUS, Rect.Top, bmp);

      if ifProvidesHelp in HelpItem.GetFlags then
      begin
        TreeView1.Images.GetIcon(iiHelp,bmp);
        if (Node.Parent=Nil) and (not Node.HasChildren) then
          Sender.Canvas.Draw(Rect.Left - IMG_INDENT_PLUSMINUS , Rect.Top, bmp)
        else
          Sender.Canvas.Draw(Rect.Left - IMG_INDENT_IMAGE , Rect.Top, bmp);
      end;
      bmp.Free;

      Rect:=Node.DisplayRect(False);
      if cdsSelected in State then
      begin
        Sender.Canvas.DrawFocusRect(Rect);
      end;
    end;
  except
    DefaultDraw:=true;
    PaintImages:=True;
  end;
end;

procedure Tch2FrameHelpTree.TreeView1DblClick(Sender: TObject);
begin
  if Assigned(TreeView1.Selected) and Assigned(TreeView1.Selected.Data) then
    TNodeData(TreeView1.Selected.Data).Execute;
end;

procedure Tch2FrameHelpTree.TreeView1Expanded(Sender: TObject; Node: TTreeNode);
begin
  TNodeData(Node.Data).Expanded:=Node.Expanded;
end;

procedure Tch2FrameHelpTree.TreeView1KeyPress(Sender: TObject; var Key: Char);
begin
  if Key=#13 then
    TreeView1DblClick(Sender);
end;

{ TNodeData }

constructor TNodeData.Create(AItemInterface: Ich2HelpItem);
begin
  FItemInterface:=AItemInterface;
end;

destructor TNodeData.Destroy;
begin
  FItemInterface:=nil;
  inherited;
end;

procedure TNodeData.Execute;
begin
  if ifProvidesHelp in FItemInterface.GetFlags then
    FItemInterface.ShowHelp;
end;

function TNodeData.GetExpanded: Boolean;
var
  Reg : TRegistry;
  hi : Ich2HelpItem;
begin
  Result:=False;

  Reg := TRegistry.Create(KEY_ALL_ACCESS);
  try
    Reg.RootKey := HKEY_CURRENT_USER;


    if ifSaveStats in FItemInterface.GetFlags then
    begin
      if Reg.OpenKey(ch2Main.RegRootKeyGUI[ch2Main.CurrentGUI.GetGUID] +
                              REG_KEY_SETTINGS_STATS +
                              GUIDToString(FItemInterface.GetGUID), true) then
      begin
        if reg.ValueExists(REG_VALUE_STATS_EXPANDED) then
          Result:=reg.ReadBool(REG_VALUE_STATS_EXPANDED);
        reg.CloseKey;
      end;
    end;

  finally
    Reg.Free;
  end;
end;

procedure TNodeData.SetExpanded(const Value: Boolean);
var
  Reg : TRegistry;
  hi : Ich2HelpItem;
begin
  Reg := TRegistry.Create(KEY_ALL_ACCESS);
  try
    Reg.RootKey := HKEY_CURRENT_USER;


    if ifSaveStats in FItemInterface.GetFlags then
    begin
      if Reg.OpenKey(ch2Main.RegRootKeyGUI[ch2Main.CurrentGUI.GetGUID] +
                              REG_KEY_SETTINGS_STATS +
                              GUIDToString(FItemInterface.GetGUID), true) then
      begin
        reg.WriteBool(REG_VALUE_STATS_EXPANDED, Value);
        reg.CloseKey;
      end;
    end;

  finally
    Reg.Free;
  end;
end;

end.
