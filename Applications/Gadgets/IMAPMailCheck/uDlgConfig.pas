unit uDlgConfig;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Spin, ComCtrls, uEffectPNGToolbar, ToolWin,
  uData, VirtualTrees, uSettingsBase, uSettingsLinksDefault,
  uSettingsLinkVirtualTrees, VTEditors, IdBaseComponent, IdComponent,
  IdTCPConnection, IdTCPClient, IdMessageClient, IdIMAP4, uCheckComboEditLink,
  Mask, JvExMask, JvToolEdit;

type
  TNodeData = record
    OrigAccount,
    TempAccount : TAccount;
    Deleted : Boolean;
  end;
  PNodeData = ^TNodeData;

  Tform_Config = class(TForm)
    Panel1: TPanel;
    btn_OK: TButton;
    btn_Cancel: TButton;
    gb_Accounts: TGroupBox;
    EffectPNGToolBar1: TEffectPNGToolBar;
    btn_Add: TEffectPNGToolButton;
    btn_Delete: TEffectPNGToolButton;
    VST: TVirtualStringTree;
    SettingsLinkForm: TSettingsLinkForm;
    SettingsLinkVST: TSettingsLinkVST;
    IMAP: TIdIMAP4;
    gb_Misc: TGroupBox;
    Panel2: TPanel;
    Label1: TLabel;
    ed_MailTool: TJvFilenameEdit;
    procedure btn_OKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure VSTGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTCreateEditor(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure VSTNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; NewText: WideString);
    procedure VSTEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
    procedure btn_AddClick(Sender: TObject);
    procedure btn_DeleteClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure VSTFocusChanging(Sender: TBaseVirtualTree; OldNode,
      NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex;
      var Allowed: Boolean);
  private
    FMailboxes : TStringList;

    function GetNodeData(ANode : PVirtualNode; out ANodeData : PNodeData) : Boolean;
  public
    class function Execute() : Boolean;
  end;

implementation

{$R *.dfm}

{ Tform_Config }

procedure Tform_Config.btn_AddClick(Sender: TObject);
var
  NData : PNodeData;
  Node : PVirtualNode;
begin
  Node := vst.AddChild(nil);

  if GetNodeData(Node, NData) then
  begin
    NData.OrigAccount := nil;
    NData.TempAccount := TAccount.Create;
    NData.Deleted := false;
  end;
end;

procedure Tform_Config.btn_DeleteClick(Sender: TObject);
var
  Node : PVirtualNode;
  NData : PNodeData;
begin
  vst.BeginUpdate;
  try
    Node := vst.GetFirstSelected();

    while Assigned(Node) do
    begin
      if GetNodeData(Node, NData) then
      begin
        NData.Deleted := true;
        vst.IsVisible[Node] := false;
      end;

      Node := vst.GetNextSelected(Node);
    end;
  finally
    vst.EndUpdate;
  end;
  
end;

procedure Tform_Config.btn_OKClick(Sender: TObject);
var
  Node : PVirtualNode;
  NData : PNodeData;
begin
  Node := vst.GetFirst();
  while Assigned(Node) do
  begin
    if GetNodeData(Node, NData) then
    begin
      if NData.Deleted then
        Data.Accounts.Remove(NData.OrigAccount)
      else
      begin
        if not Assigned(NData.OrigAccount) then
        begin
          NData.OrigAccount := TAccount.Create;
          Data.Accounts.Add(NData.OrigAccount);
        end;
        NData.OrigAccount.Assign(NData.TempAccount);
      end;
    end;

    Node := vst.GetNext(Node);
  end;

  Data.Settings.MailTool := ed_MailTool.FileName;

  Data.SaveSettings;
end;

class function Tform_Config.Execute: Boolean;
var
  form : Tform_Config;
begin
  form := Tform_Config.Create(nil);
  try
    Result := IsPositiveResult(form.ShowModal)
  finally
    form.Free;
  end;
end;


procedure Tform_Config.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SettingsLinkForm.SaveSettings;
  SettingsLinkVST.SaveSettings;
  Data.SettingsFile.Save;

  Action := caHide;

  FMailboxes.Free;
end;

procedure Tform_Config.FormCreate(Sender: TObject);
var
  idx : Integer;
  Node : PVirtualNode;
  NData : PNodeData;
begin
  VST.BeginUpdate;
  try
    for idx := 0 to Data.Accounts.Count - 1 do
    begin
      Node := VST.AddChild(nil);

      if GetNodeData(Node, NData) then
      begin
        NData.OrigAccount := TAccount(Data.Accounts[idx]);
        NData.TempAccount := TAccount.Create(NData.OrigAccount);
        NData.Deleted := false;
      end;
    end;
  finally
    VST.EndUpdate;
  end;

  FMailboxes := TStringList.Create;
  FMailboxes.StrictDelimiter := true;

  ed_MailTool.Text := Data.Settings.MailTool;
end;

procedure Tform_Config.FormShow(Sender: TObject);
begin
  SettingsLinkForm.ApplySettings;
  SettingsLinkVST.ApplySettings;
end;

function Tform_Config.GetNodeData(ANode: PVirtualNode;
  out ANodeData: PNodeData): Boolean;
var
  Data : Pointer;
begin
  Result := Assigned(ANode);

  if Result then
  begin
    Data := VST.GetNodeData(ANode);
    Result := Assigned(Data);

    if Result then
      ANodeData := PNodeData(Data);
  end;
end;

procedure Tform_Config.VSTCreateEditor(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
var
  NData : PNodeData;
  CheckCombo : TCheckedComboEditLink;
begin
  if GetNodeData(Node, NData) then
  begin
    case Column of
      0: EditLink := TEditEditLink.Create();
      1: EditLink := TSpinEditLink.Create(true, 1, 1, MAXWORD);
      2,3: EditLink := TEditEditLink.Create();
      4:
      begin
        CheckCombo := TCheckedComboEditLink.Create();
        CheckCombo.Items := FMailboxes;
        EditLink := CheckCombo;
      end;
      5: EditLink := TSpinEditLink.Create(true, 1, 1, 600);
    end;
  end;
end;

procedure Tform_Config.VSTEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; var Allowed: Boolean);
var
  NData  : PNodeData;
begin
  case Column of
    4:
    begin
      FMailboxes.Clear;
      
      Allowed := GetNodeData(Node, NData);

      if Allowed then
      begin
        NData.TempAccount.AssignTo(IMAP);

        Screen.Cursor := crHourGlass;
        try
          try
            IMAP.Connect(1000);

            if imap.ConnectionState = csAuthenticated then
              imap.ListMailBoxes(FMailboxes);
          except
            on e : Exception do
            begin
              MessageDlg('Can not determine mailboxes.'+#13+#10+'Are the connection parameters valid?', mtError, [mbOK], 0);
              Allowed := false;
            end;
          end;

          Allowed := FMailboxes.Count > 0;
        finally
          Screen.Cursor := crDefault;
          if imap.Connected then
            imap.Disconnect;          
        end;
      end;
      
    end;
    else
      Allowed := true;
  end;
end;

procedure Tform_Config.VSTFocusChanging(Sender: TBaseVirtualTree; OldNode,
  NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex;
  var Allowed: Boolean);
begin
  btn_Delete.Enabled := Assigned(NewNode);
end;

procedure Tform_Config.VSTFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  NData : PNodeData;
begin
  if GetNodeData(Node, NData) then
    NData.TempAccount.Free;
end;

procedure Tform_Config.VSTGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TNodeData);
end;

procedure Tform_Config.VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
var
  NData : PNodeData;
begin
  if GetNodeData(Node, NData) then
  begin
    case Column of
      0: CellText := NData.TempAccount.Host;
      1: CellText := IntToStr(NData.TempAccount.Port);
      2: CellText := NData.TempAccount.Username;
      3: CellText := NData.TempAccount.Password;
      4: CellText := NData.TempAccount.Mailboxes;
      5: CellText := IntToStr(NData.TempAccount.CheckInterval);
    end;
  end;
end;

procedure Tform_Config.VSTNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; NewText: WideString);
var
  NData : PNodeData;
begin
  if GetNodeData(Node, NData) then
  begin
    case Column of
      0: NData.TempAccount.Host := NewText;
      1: NData.TempAccount.Port := StrToIntDef(NewText, 143);
      2: NData.TempAccount.Username := NewText;
      3: NData.TempAccount.Password := NewText;
      4: NData.TempAccount.Mailboxes := NewText;
      5: NData.TempAccount.CheckInterval := StrToIntDef(NewText, 10);  
    end;
  end;
end;

end.
