unit uch2GUIDockableTree;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DockForm, DeskUtil, uch2Main, StdCtrls, ExtCtrls, ComCtrls, ImgList;

type
  Tch2GUIDockableTree = class;

  Tch2FormGUIDockableTree = class(TDockableForm)
    Panel1: TPanel;
    Label1: TLabel;
    cbKeywords: TComboBox;
    TreeView1: TTreeView;
    ImageList1: TImageList;
    procedure cbKeywordsKeyPress(Sender: TObject; var Key: Char);
    procedure cbKeywordsCloseUp(Sender: TObject);
    procedure TreeView1Deletion(Sender: TObject; Node: TTreeNode);
    procedure TreeView1DblClick(Sender: TObject);
    procedure TreeView1AdvancedCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
      var PaintImages, DefaultDraw: Boolean);
    procedure FormCreate(Sender: TObject);
  private
    FGUI : Tch2GUIDockableTree;
    procedure ShowHelp(FKeyword: String);
  end;

  Tch2GUIDockableTree = class(TInterfacedObject, Ich2GUI)
  private
    FForm : Tch2FormGUIDockableTree;

    {$REGION 'Ich2HelpGUI'}
    function GetName : String;
    function GetDescription : String;
    function GetGUID : TGUID;

    procedure Show(const AHelpString : String; const Ach2Keywords : TStringList);

    function AddHelpItem(AHelpItem : Ich2HelpItem; AParent : Pointer = nil) : Pointer;
    {$ENDREGION}
  public
    constructor Create;
    destructor Destroy; override;
  end;


type
  TDockableFormClass = Class Of TDockableForm;

procedure CreateDockableForm(var FormVar: TDockableForm; FormClass: TDockableFormClass);
procedure FreeDockableForm(var FormVar: TDockableForm);
procedure ShowDockableForm(Form: TDockableForm);

implementation

{$R *.dfm}

type
  TNodeData = class(TObject)
  private
    FItemInterface : Ich2HelpItem;
  public
    constructor Create(AItemInterface: Ich2HelpItem);
    destructor Destroy; override;
    procedure Execute;
  end;


{$Region 'DockableForm Routines'}
procedure RegisterDockableForm(FormClass: TDockableFormClass;
  var FormVar; const FormName: string);
begin
  if @RegisterFieldAddress <> nil then
    RegisterFieldAddress(FormName, @FormVar);
  RegisterDesktopFormClass(FormClass, FormName, FormName);
end;

procedure UnRegisterDockableForm(var FormVar; const FormName: string);
begin
  if @UnregisterFieldAddress <> nil then
    UnregisterFieldAddress(@FormVar);
end;

procedure ShowDockableForm(Form: TDockableForm);
begin
  if not Assigned(Form) then
    Exit;
  if not Form.Floating then
  begin
    Form.ForceShow;
    FocusWindow(Form);
  end
  else
    Form.Show;
end;

procedure CreateDockableForm(var FormVar: TDockableForm; FormClass: TDockableFormClass);
begin
  TCustomForm(FormVar) := FormClass.Create(nil);
  RegisterDockableForm(FormClass, FormVar, TCustomForm(FormVar).Name);
end;

procedure FreeDockableForm(var FormVar: TDockableForm);
begin
  if Assigned(FormVar) then
  begin
    FormVar.Hide;
    UnRegisterDockableForm(FormVar, FormVar.Name);
    {FormVar.Release;
    FormVar:=nil;  }
    FreeAndNil(FormVar);
  end;
end;
{$EndRegion}

{ Tch2GUIDockableTree }

function Tch2GUIDockableTree.AddHelpItem(AHelpItem: Ich2HelpItem;
  AParent: Pointer): Pointer;
var
  Node : TTreeNode;
begin
  Node:=FForm.TreeView1.Items.AddChild(AParent, AHelpItem.GetCaption);
  Node.Data:=TNodeData.Create(AHelpItem);
  if ifProvidesHelp in AHelpItem.GetFlags then
    Node.ImageIndex:=2
  else
    Node.ImageIndex:=-1;

  Result:=Node;
end;

constructor Tch2GUIDockableTree.Create;
begin
  inherited;
  CreateDockableForm(TDockableForm(FForm), Tch2FormGUIDockableTree);
  FForm.FGUI:=self;
end;

destructor Tch2GUIDockableTree.Destroy;
begin
  FreeDockableForm(TDockableForm(FForm));
  inherited;
end;

function Tch2GUIDockableTree.GetDescription: String;
begin
  Result:='A dockable help with a treeview';
end;

function Tch2GUIDockableTree.GetGUID: TGUID;
begin
  Result:=StringToGUID('{CD0B4F74-6B2C-4851-8E0B-D652005AC7FB}');
end;

function Tch2GUIDockableTree.GetName: String;
begin
  Result:='DockableHelpGUI'
end;

procedure Tch2GUIDockableTree.Show(const AHelpString: String;
  const Ach2Keywords: TStringList);
begin
  FForm.cbKeywords.Items.Text:=Ach2Keywords.Text;
  FForm.cbKeywords.Text:=AHelpString;
  FForm.ShowHelp(AHelpString);
  ShowDockableForm(TDockableForm(Self.FForm));
end;

procedure Tch2FormGUIDockableTree.cbKeywordsCloseUp(Sender: TObject);
begin
  if cbKeywords.ItemIndex>=0 then
    ShowHelp(cbKeywords.Items[cbKeywords.ItemIndex]);
end;

procedure Tch2FormGUIDockableTree.cbKeywordsKeyPress(Sender: TObject;
  var Key: Char);
begin
  if Key=#13 then
    ShowHelp(cbKeywords.Text);
end;

procedure Tch2FormGUIDockableTree.FormCreate(Sender: TObject);
begin
  Self.AutoSave:=True;
  Self.DeskSection:='CustomHelp2';
end;

procedure Tch2FormGUIDockableTree.ShowHelp(FKeyword: String);
var
  Intf : IInterface;
  IProv : Ich2Provider absolute Intf;
begin
  TreeView1.Items.BeginUpdate;
  try
    while Treeview1.Items.Count>0 do
    begin
      TObject(TreeView1.Items[TreeView1.Items.Count-1].Data).Free;
      TreeView1.Items[TreeView1.Items.Count-1].Free;
    end;

    for intf in ch2Main.Providers do
    begin
      IProv.ProvideHelp(FKeyword, FGUI);
    end;
  finally
    TreeView1.Items.EndUpdate;
  end;
end;

procedure Tch2FormGUIDockableTree.TreeView1AdvancedCustomDrawItem(
  Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState;
  Stage: TCustomDrawStage; var PaintImages, DefaultDraw: Boolean);
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
      Sender.Canvas.TextRect(Rect, s);

      bmp:=TIcon.Create;
      if Node.Expanded then
        ImageList1.GetIcon(0, bmp)
      else
      if Node.HasChildren then
        ImageList1.GetIcon(1,bmp);

      Sender.Canvas.Draw(Rect.Left-35, Rect.Top, bmp);

      if ifProvidesHelp in HelpItem.GetFlags then
      begin
        ImageList1.GetIcon(2,bmp);
        Sender.Canvas.Draw(Rect.Left - 18 , Rect.Top, bmp);
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

procedure Tch2FormGUIDockableTree.TreeView1DblClick(Sender: TObject);
begin
  if Assigned(TreeView1.Selected) and Assigned(TreeView1.Selected.Data) then
    TNodeData(TreeView1.Selected.Data).Execute;
end;

procedure Tch2FormGUIDockableTree.TreeView1Deletion(Sender: TObject;
  Node: TTreeNode);
begin
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

initialization
  ch2Main.RegisterGUI(Tch2GUIDockableTree.Create as Ich2GUI);

finalization

end.
