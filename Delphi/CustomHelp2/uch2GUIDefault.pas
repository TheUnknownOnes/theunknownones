unit uch2GUIDefault;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uch2Main, StdCtrls, ComCtrls, ExtCtrls;

type
  TNodeData = record
    HelpItem : Ich2HelpItem;
  end;
  PNodeData = ^TNodeData;

  Tch2GUIDefault = class;

  Tch2FormGUIDefault = class(TForm)
    GroupBox1: TGroupBox;
    com_Keywords: TComboBox;
    GroupBox2: TGroupBox;
    TV: TTreeView;
    tm_RunFirstSearch: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure com_KeywordsChange(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure tm_RunFirstSearchTimer(Sender: TObject);
    procedure TVDeletion(Sender: TObject; Node: TTreeNode);
    procedure TVAdvancedCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
      State: TCustomDrawState; Stage: TCustomDrawStage; var PaintImages,
      DefaultDraw: Boolean);
  private
    FHelpString : String;
    FKeywords : TStringList;
    FGUI : Ich2GUI;

    procedure DoSearch(AKeyword : String);
  public
    function AddHelpItem(AHelpItem : Ich2HelpItem; AParent : Pointer = nil) : Pointer;
  end;

  Tch2GUIDefault = class(TInterfacedObject, Ich2GUI)
  private
    FForm : Tch2FormGUIDefault;

    {$REGION 'Ich2HelpGUI'}
    function GetName : String;
    function GetDescription : String;
    function GetGUID : TGUID;

    procedure Show(const AHelpString : String; const Ach2Keywords : TStringList);

    function AddHelpItem(AHelpItem : Ich2HelpItem; AParent : Pointer = nil) : Pointer;
    {$ENDREGION}
  public

  end;

implementation

{$R *.dfm}

{ Tch2GUIDefault }

function Tch2GUIDefault.AddHelpItem(AHelpItem : Ich2HelpItem; AParent : Pointer = nil) : Pointer;
begin
  Result := FForm.AddHelpItem(AHelpItem, AParent);
end;

function Tch2GUIDefault.GetDescription: String;
begin
  Result := 'The default GUI. Made without thinking about. :)'
end;

function Tch2GUIDefault.GetGUID: TGUID;
const
  g : TGUID = '{A533134A-065A-430B-960C-4EF5CACE470B}';
begin
  Result := g;
end;

function Tch2GUIDefault.GetName: String;
begin
  Result := 'DefaultGUI';
end;

procedure Tch2GUIDefault.Show(const AHelpString : String; const Ach2Keywords: TStringList);
begin
  FForm := Tch2FormGUIDefault.Create(nil);
  try
    FForm.FKeywords.Assign(Ach2Keywords);
    FForm.FHelpString := AHelpString;
    FForm.FGUI := Self as Ich2GUI;

    FForm.ShowModal;
  finally
    FForm.Free;
  end;
end;

{ Tch2FormGUIDefault }

function Tch2FormGUIDefault.AddHelpItem(AHelpItem : Ich2HelpItem; AParent : Pointer = nil) : Pointer;
var
  Node : TTreeNode;
  NodeData : PNodeData;
begin
  Node := tv.Items.AddChild(TTreeNode(AParent), AHelpItem.GetCaption);

  New(NodeData);
  NodeData^.HelpItem := AHelpItem;
  Node.Data := NodeData;
  Result := Node;
end;

procedure Tch2FormGUIDefault.com_KeywordsChange(Sender: TObject);
begin
  DoSearch(com_Keywords.Text);
end;

procedure Tch2FormGUIDefault.DoSearch(AKeyword: String);
var
  i : IInterface;
  p : Ich2Provider absolute i;
begin
  tv.Items.Clear;

  Screen.Cursor := crHourGlass;
  tv.Items.BeginUpdate;
  try

    for i in ch2Main.Providers do
    begin
      p.ProvideHelp(AKeyword, FGUI);
    end;

  finally
    Screen.Cursor := crDefault;
    tv.Items.EndUpdate;
  end;
end;

procedure Tch2FormGUIDefault.FormCreate(Sender: TObject);
begin
  FKeywords := TStringList.Create;
end;

procedure Tch2FormGUIDefault.FormDestroy(Sender: TObject);
begin
  FKeywords.Free;
end;

procedure Tch2FormGUIDefault.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    ModalResult := mrCancel;
end;

procedure Tch2FormGUIDefault.FormShow(Sender: TObject);
begin
  Caption := 'Find help for "' + FHelpString + '"';
  com_Keywords.Items.Assign(FKeywords);
  com_Keywords.ItemIndex := com_Keywords.Items.IndexOf(FHelpString);
  tm_RunFirstSearch.Enabled := true;
end;

procedure Tch2FormGUIDefault.tm_RunFirstSearchTimer(Sender: TObject);
begin
  tm_RunFirstSearch.Enabled := false;
  com_KeywordsChange(Sender);
end;

procedure Tch2FormGUIDefault.TVAdvancedCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
  var PaintImages, DefaultDraw: Boolean);
begin
  PaintImages := true;
  DefaultDraw := true;

  if ifHasForeColor in PNodeData(Node.Data)^.HelpItem.GetFlags then
    Sender.Canvas.Font.Color := PNodeData(Node.Data)^.HelpItem.GetForeColor;

  if ifHasBackColor in PNodeData(Node.Data)^.HelpItem.GetFlags then
    Sender.Canvas.Brush.Color := PNodeData(Node.Data)^.HelpItem.GetBackColor;
end;

procedure Tch2FormGUIDefault.TVDeletion(Sender: TObject; Node: TTreeNode);
begin
  Dispose(PNodeData(Node.Data));
end;

initialization
  ch2Main.RegisterGUI(Tch2GUIDefault.Create as Ich2GUI);

finalization


end.
