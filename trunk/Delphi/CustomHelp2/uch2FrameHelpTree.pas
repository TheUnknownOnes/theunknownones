unit uch2FrameHelpTree;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, uch2Main, ImgList, uch2Data, SyncObjs;

type
  Tch2FrameHelpTree = class(TFrame)
    TreeView1: TTreeView;
    Panel1: TPanel;
    Label1: TLabel;
    cbKeywords: TComboBox;
    StatusBar: TStatusBar;
    Timer1: TTimer;
    procedure cbKeywordsCloseUp(Sender: TObject);
    procedure cbKeywordsKeyPress(Sender: TObject; var Key: Char);
    procedure TreeView1AdvancedCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
      var PaintImages, DefaultDraw: Boolean);
    procedure TreeView1DblClick(Sender: TObject);
    procedure TreeView1KeyPress(Sender: TObject; var Key: Char);
    procedure TreeView1Expanded(Sender: TObject; Node: TTreeNode);
    procedure TreeView1Compare(Sender: TObject; Node1, Node2: TTreeNode;
      Data: Integer; var Compare: Integer);
    procedure Timer1Timer(Sender: TObject);
  private
    FLock : TCriticalSection;
    FHelpItem : Ich2HelpItem;
    FHelpItemParent : Pointer;
    FAddHelpItemResult : Pointer;
    procedure DoAddHelpItem;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

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

var
  ProviderThreads : TThreadList;

type
  TProviderThread = class(TThread)
  protected
    FKeyword : String;
    FGUI : Ich2GUI;
    FProvider : Ich2Provider;

    procedure Execute; override;
  public
    constructor Create(AProvider : Ich2Provider; AKeyword : String; AGUI : Ich2GUI);
    destructor Destroy; override;
  end;

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
    property Item : Ich2HelpItem read FItemInterface;
  end;

{ TFrame1 }

function Tch2FrameHelpTree.AddHelpItem(AHelpItem: Ich2HelpItem;
  AParent: Pointer): Pointer;
begin
  Result := nil;

  FLock.Enter;
  try
    FHelpItem := AHelpItem;
    FHelpItemParent := AParent;
    FAddHelpItemResult := nil;
    TThread.Synchronize(nil {TThread.CurrentThread}, DoAddHelpItem);
    Result := FAddHelpItemResult;
  finally
    FLock.Leave;
  end;
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

constructor Tch2FrameHelpTree.Create(AOwner: TComponent);
begin
  inherited;
  FLock := TCriticalSection.Create;
end;

destructor Tch2FrameHelpTree.Destroy;
begin
  FLock.Free;
  inherited;
end;

procedure Tch2FrameHelpTree.DoAddHelpItem;
var
  Node : TTreeNode;
begin
  Node:=TreeView1.Items.AddChildObject(FHelpItemParent,
                                       FHelpItem.GetCaption + FHelpItem.GetDescription,
                                       TNodeData.Create(FHelpItem));

  if Assigned(FHelpItemParent) then
  begin
    if TNodeData(TTreeNode(FHelpItemParent).Data).Expanded then
      TTreeNode(FHelpItemParent).Expanded:=True;
  end;

  if ifProvidesHelp in FHelpItem.GetFlags then
    Node.ImageIndex:=2
  else
    Node.ImageIndex:=-1;

  FAddHelpItemResult:=Node;

  TreeView1.CustomSort(nil, 0);
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
begin
  Screen.Cursor := crHourGlass;
  TreeView1.Items.BeginUpdate;
  try
    while Treeview1.Items.Count>0 do
    begin
      TObject(TreeView1.Items[TreeView1.Items.Count-1].Data).Free;
      TreeView1.Items[TreeView1.Items.Count-1].Free;
    end;

    if trim(FKeyword)<>'' then
    begin
      for intf in ch2Main.Providers do
      begin
        TProviderThread.Create(IProv, FKeyword, ch2Main.CurrentGUI);
      end;
    end;
  finally
    TreeView1.Items.EndUpdate;
    Screen.Cursor := crDefault;
  end;
end;

procedure Tch2FrameHelpTree.Timer1Timer(Sender: TObject);
var
  lst : TList;
begin
  lst := ProviderThreads.LockList;
  try
    StatusBar.Panels[0].Text := Format('%d', [lst.Count]) + ' provider active';
  finally
    ProviderThreads.UnlockList;
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
      s:=HelpItem.GetCaption;
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

procedure Tch2FrameHelpTree.TreeView1Compare(Sender: TObject; Node1,
  Node2: TTreeNode; Data: Integer; var Compare: Integer);
var
  NodeData1,
  NodeData2 : TNodeData;
begin
  NodeData1 := TNodeData(Node1.Data);
  NodeData2 := TNodeData(Node2.Data);
  Compare := NodeData2.Item.GetPriority - NodeData1.Item.GetPriority;
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

{ TProviderThread }

constructor TProviderThread.Create(AProvider : Ich2Provider; AKeyword: String; AGUI: Ich2GUI);
begin
  inherited Create(true);
  FreeOnTerminate := true;
  FKeyword := AKeyword;
  FGUI := AGUI;
  FProvider := AProvider;
  ProviderThreads.Add(Self);
  Resume;
end;

destructor TProviderThread.Destroy;
begin
  ProviderThreads.Remove(Self);
  inherited;
end;

procedure TProviderThread.Execute;
begin
  try
    FProvider.ProvideHelp(FKeyword, FGUI);
  except
  end;
end;

initialization
  ProviderThreads := TThreadList.Create;

finalization
  ProviderThreads.Free;

end.
