unit uch2GUIDefault;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uch2Main, StdCtrls, ComCtrls, ExtCtrls, Registry;

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
    procedure TVDblClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FHelpString : String;
    FKeywords : TStringList;
    FGUI : Ich2GUI;

    procedure DoSearch(AKeyword : String);

    procedure SaveItemStats;
    procedure LoadItemStats;

    procedure LoadSettings;
    procedure SaveSettings;
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

const
  Settings_Key_Stats = '\Stats\';
  Settings_Value_Expanded = 'Expanded';
  Settings_Value_Size = 'Size';

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
  SaveItemStats;

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

  LoadItemStats;
end;

procedure Tch2FormGUIDefault.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
  SaveItemStats;
  SaveSettings;
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
  LoadSettings;

  Caption := 'Find help for "' + FHelpString + '"';
  com_Keywords.Items.Assign(FKeywords);
  com_Keywords.ItemIndex := com_Keywords.Items.IndexOf(FHelpString);
  tm_RunFirstSearch.Enabled := true;
end;

procedure Tch2FormGUIDefault.LoadItemStats;
var
  Reg : TRegistry;
  t : TTreeNode;
  hi : Ich2HelpItem;
begin
  Reg := TRegistry.Create(KEY_ALL_ACCESS);
  try
    Reg.RootKey := HKEY_CURRENT_USER;

    for t in tv.Items do
    begin
      hi := PNodeData(t.Data)^.HelpItem;

      if ifSaveStats in hi.GetFlags then
      begin
        if Reg.OpenKey(ch2Main.RegRootKeyGUI[FGUI.GetGUID] + Settings_Key_Stats + GUIDToString(hi.GetGUID), false) then
        begin
          t.Expanded := reg.ReadBool(Settings_Value_Expanded);
          reg.CloseKey;
        end;
      end;
    end;

  finally
    Reg.Free;
  end;
end;

procedure Tch2FormGUIDefault.LoadSettings;
var
  Reg : TRegistry;
  r : TRect;
begin
  Reg := TRegistry.Create(KEY_ALL_ACCESS);
  try
    REG.RootKey := HKEY_CURRENT_USER;

    if Reg.OpenKey(ch2Main.RegRootKeyGUI[FGUI.GetGUID], true) then
    begin
      if Reg.ValueExists(Settings_Value_Size) then
      begin
        Reg.ReadBinaryData(Settings_Value_Size, r, SizeOf(r));

        Left := r.Left;
        Top := r.Top;
        Width := r.Right - r.Left;
        Height := r.Bottom - r.Top;
      end;

      Reg.CloseKey;
    end;

  finally
    Reg.Free;
  end;
end;

procedure Tch2FormGUIDefault.SaveItemStats;
var
  Reg : TRegistry;
  t : TTreeNode;
  hi : Ich2HelpItem;
begin
  Reg := TRegistry.Create(KEY_ALL_ACCESS);
  try
    Reg.RootKey := HKEY_CURRENT_USER;

    for t in tv.Items do
    begin
      hi := PNodeData(t.Data)^.HelpItem;

      if ifSaveStats in hi.GetFlags then
      begin
        if Reg.OpenKey(ch2Main.RegRootKeyGUI[FGUI.GetGUID] + Settings_Key_Stats + GUIDToString(hi.GetGUID), true) then
        begin
          reg.WriteBool(Settings_Value_Expanded, t.Expanded);
          reg.CloseKey;
        end;
      end;
    end;

  finally
    Reg.Free;
  end;
end;

procedure Tch2FormGUIDefault.SaveSettings;
var
  Reg : TRegistry;
  r : TRect;
begin
  Reg := TRegistry.Create(KEY_ALL_ACCESS);
  try
    REG.RootKey := HKEY_CURRENT_USER;

    r := Rect(Left, Top, Left + Width, Top + Height);

    if Reg.OpenKey(ch2Main.RegRootKeyGUI[FGUI.GetGUID], true) then
    begin
      Reg.WriteBinaryData(Settings_Value_Size, r, SizeOf(r));

      Reg.CloseKey;
    end;

  finally
    Reg.Free;
  end;
end;

procedure Tch2FormGUIDefault.tm_RunFirstSearchTimer(Sender: TObject);
begin
  tm_RunFirstSearch.Enabled := false;
  com_KeywordsChange(Sender);
end;


procedure Tch2FormGUIDefault.TVAdvancedCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
  var PaintImages, DefaultDraw: Boolean);
var
  flags : Tch2HelpItemFlags;
begin
  PaintImages := true;
  DefaultDraw := true;

  flags := PNodeData(Node.Data)^.HelpItem.GetFlags;

  if ifHasForeColor in flags then
    Sender.Canvas.Font.Color := PNodeData(Node.Data)^.HelpItem.GetForeColor;

  if ifHasBackColor in flags then
    Sender.Canvas.Brush.Color := PNodeData(Node.Data)^.HelpItem.GetBackColor;

  if ifHasFontStyles in flags then
    Sender.Canvas.Font.Style := PNodeData(Node.Data)^.HelpItem.GetFontStyles;
end;

procedure Tch2FormGUIDefault.TVDblClick(Sender: TObject);
var
  hi : Ich2HelpItem;
begin
  if Assigned(tv.Selected) then
  begin
    hi := PNodeData(tv.Selected.Data)^.HelpItem;

    if ifProvidesHelp in hi.GetFlags then
    begin
      hi.ShowHelp;
      ModalResult := mrOk;
    end;
  end;
end;

procedure Tch2FormGUIDefault.TVDeletion(Sender: TObject; Node: TTreeNode);
begin
  Dispose(PNodeData(Node.Data));
end;

initialization
  ch2Main.RegisterGUI(Tch2GUIDefault.Create as Ich2GUI);

finalization


end.
