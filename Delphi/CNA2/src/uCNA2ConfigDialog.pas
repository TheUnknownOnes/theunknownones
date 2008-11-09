unit uCNA2ConfigDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls,
  ToolsAPI, ExtCtrls,
  uRTTIHelper,
  uCNA2Settings,
  uSettingsBase, ImgList,
  WideStrUtils, ToolWin;

type
  TNodeType = (ntProfile, ntGroup, ntComponent);
  TNodeData = record
    NodeType : TNodeType;
    Setting : TSettingName;
  end;
  PNodeData = ^TNodeData;

  TCNA2ConfigDialog = class(TForm)
    PC: TPageControl;
    ts_Rules: TTabSheet;
    gb_PGC: TGroupBox;
    lv_Components: TListView;
    tv_Profiles: TTreeView;
    Splitter1: TSplitter;
    Label1: TLabel;
    iml_TreeProfiles: TImageList;
    TB: TToolBar;
    btn_AddProfile: TToolButton;
    iml_TB: TImageList;
    btn_AddGroup: TToolButton;
    btn_Delete: TToolButton;
    gb_Rules: TGroupBox;
    procedure FormCreate(Sender: TObject);
    procedure lv_ComponentsColumnClick(Sender: TObject; Column: TListColumn);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btn_AddProfileClick(Sender: TObject);
    procedure tv_ProfilesEdited(Sender: TObject; Node: TTreeNode;
      var S: string);
    procedure tv_ProfilesEditing(Sender: TObject; Node: TTreeNode;
      var AllowEdit: Boolean);
    procedure tv_ProfilesKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure tv_ProfilesDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure btn_AddGroupClick(Sender: TObject);
    procedure btn_DeleteClick(Sender: TObject);
    procedure tv_ProfilesDragDrop(Sender, Source: TObject; X, Y: Integer);
  private
    function FindNodeBySetting(ASettingPath : TSettingName; out ANode : TTreeNode) : Boolean;
    function GetNodeData(ANode : TTreeNode) : PNodeData;
    function AddProfileNode(AProfilePath : TSettingName) : TTreeNode;
    function AddGroupNode(AProfileNode : TTreeNode; AGroupPath : TSettingName) : TTreeNode;
    function AddComponentNode(AGroupNode : TTreeNode; AComponentPath : TSettingName) : TTreeNode;

    procedure LoadComponents;
    procedure InitProfileTree;

    procedure FreeNodeData(ANode : TTreeNode);
  public
    class procedure Execute;
  end;


implementation

var
  FLastSortCol : Integer;
  FLastSortWasAsc : Boolean;

{$R *.dfm}

{ TCNA2ConfigDialog }

function CompareItems(Item1, Item2: TListItem;
  Data: Integer) : Integer; stdcall;
begin
  if Data = 0 then
    Result := CompareText(Item1.Caption, Item2.Caption)
  else
    Result := CompareText(Item1.SubItems[Data - 1], Item2.SubItems[Data - 1]);

  if FLastSortWasAsc then
    Result := -Result;
end;

function TCNA2ConfigDialog.AddComponentNode(AGroupNode: TTreeNode;
  AComponentPath: TSettingName): TTreeNode;
var
  NData : PNodeData;
begin
  if not FindNodeBySetting(AComponentPath, Result) then
  begin
    Result := tv_Profiles.Items.AddChild(AGroupNode, cna2Settings.GetValue(AComponentPath, ''));
    Result.ImageIndex := 2;
    Result.SelectedIndex := 2;
    NData := GetNodeData(Result);
    NData.NodeType := ntComponent;
    NData.Setting := AComponentPath;
  end;
end;

function TCNA2ConfigDialog.AddGroupNode(AProfileNode: TTreeNode;
  AGroupPath: TSettingName): TTreeNode;
var
  NData : PNodeData;
begin
  if not FindNodeBySetting(AGroupPath, Result) then
  begin
    Result := tv_Profiles.Items.AddChild(AProfileNode, cna2Settings.GetValue(AGroupPath, ''));
    Result.ImageIndex := 1;
    Result.SelectedIndex := 1;
    NData := GetNodeData(Result);
    NData.NodeType := ntGroup;
    NData.Setting := AGroupPath;
  end;
end;

function TCNA2ConfigDialog.AddProfileNode(
  AProfilePath: TSettingName): TTreeNode;
var
  NData : PNodeData;
begin
  if not FindNodeBySetting(AProfilePath, Result) then
  begin
    Result := tv_Profiles.Items.AddChild(nil, cna2Settings.GetValue(AProfilePath, ''));
    Result.ImageIndex := 0;
    Result.StateIndex := 0;
    NData := GetNodeData(Result);
    NData.NodeType := ntProfile;
    NData.Setting := AProfilePath;
  end;
end;

procedure TCNA2ConfigDialog.btn_AddGroupClick(Sender: TObject);
var
  NData : PNodeData;
  ProfileNode : TTreeNode;
begin
  ProfileNode := nil;

  if Assigned(tv_Profiles.Selected) then
  begin
    NData := GetNodeData(tv_Profiles.Selected);

    case NData.NodeType of
      ntProfile :
        ProfileNode := tv_Profiles.Selected;
      ntGroup :
        ProfileNode := tv_Profiles.Selected.Parent;
      ntComponent :
        ProfileNode := tv_Profiles.Selected.Parent.Parent;
    end;
  end
  else
    FindNodeBySetting(cna2Settings.CurrentProfile, ProfileNode);

  if not Assigned(ProfileNode) then
    ProfileNode := AddProfileNode(cna2Settings.AddProfile('NewProfile'));

  NData := GetNodeData(ProfileNode);
  
  tv_Profiles.Selected := AddGroupNode(ProfileNode, cna2Settings.AddGroup(Ndata.Setting, 'NewGroup'));
end;

procedure TCNA2ConfigDialog.btn_AddProfileClick(Sender: TObject);
begin
  tv_Profiles.Selected := AddProfileNode(cna2Settings.AddProfile('NewProfile'));
end;

procedure TCNA2ConfigDialog.btn_DeleteClick(Sender: TObject);
var
  NData : PNodeData;
begin
  if Assigned(tv_Profiles.Selected) then
  begin
    if (MessageBox(0, 'Do you really want to delete this item?', 'Delete item', MB_ICONWARNING or MB_YESNO) = ID_YES) then
    begin
      NData := GetNodeData(tv_Profiles.Selected);
      cna2Settings.Delete(NData.Setting);
      FreeNodeData(tv_Profiles.Selected);
      tv_Profiles.Items.Delete(tv_Profiles.Selected);
    end;

  end;
end;

class procedure TCNA2ConfigDialog.Execute;
var
  Form : TCNA2ConfigDialog;
begin
  Form := TCNA2ConfigDialog.Create(nil);
  try
    Form.ShowModal;
  finally
    Form.Free;
  end;
end;

function TCNA2ConfigDialog.FindNodeBySetting(ASettingPath: TSettingName;
  out ANode: TTreeNode): Boolean;
var
  idx : Integer;
begin
  Result := false;

  for idx := 0 to tv_Profiles.Items.Count - 1 do
  begin
    if WideSameText(GetNodeData(tv_Profiles.Items[idx]).Setting, ASettingPath) then
    begin
      Result := true;
      ANode := tv_Profiles.Items[idx];
      break;
    end;    
  end;
end;

procedure TCNA2ConfigDialog.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var
  idx : Integer;
begin
  CanClose := true;

  for idx := 0 to tv_Profiles.Items.Count - 1 do
  begin
    FreeNodeData(tv_Profiles.Items[idx]);
  end;
end;

procedure TCNA2ConfigDialog.FormCreate(Sender: TObject);
begin
  LoadComponents;
  InitProfileTree;

  FLastSortCol := -1;
  FLastSortWasAsc := false;
end;

procedure TCNA2ConfigDialog.FreeNodeData(ANode: TTreeNode);
begin
  Dispose(PNodeData(ANode.Data));
end;

function TCNA2ConfigDialog.GetNodeData(ANode: TTreeNode): PNodeData;
begin
  if not Assigned(ANode.Data) then
  begin
    New(Result);
    ANode.Data := Result;
  end
  else
    Result := PNodeData(ANode.Data);
end;

procedure TCNA2ConfigDialog.InitProfileTree;
var
  Profiles,
  Groups,
  Components : TSettingNames;
  nodeProfile,
  nodeGroup,
  nodeComponent : TTreeNode;
  idxProfile,
  idxGroup,
  idxComponent : Integer;
begin
  Profiles := cna2Settings.GetProfiles;

  for idxProfile := Low(Profiles) to High(Profiles) do
  begin
    nodeProfile := AddProfileNode(Profiles[idxProfile]);
    Groups := cna2Settings.GetGroups(Profiles[idxProfile]);

    for idxGroup := Low(Groups) to High(Groups) do
    begin
      nodeGroup := AddGroupNode(nodeProfile, Groups[idxGroup]);
      Components := cna2Settings.GetComponents(Groups[idxGroup]);

      for idxComponent := Low(Components) to High(Components) do
      begin
        nodeComponent := AddComponentNode(nodeGroup, Components[idxComponent]);
      end;

    end;

  end;

  if tv_Profiles.Items.Count > 0 then
    tv_Profiles.Selected := tv_Profiles.Items[0]
end;

procedure TCNA2ConfigDialog.LoadComponents;
var
  PS : IOTAPackageServices;
  idxPackage,
  idxComponent : Integer;
  Li : TListItem;
  ComponentClassName : String;
  ComponentClass : TPersistentClass;
begin
  Screen.Cursor := crHourGlass;
  try
    if Supports(BorlandIDEServices, IOTAPackageServices, PS) then
    begin
      for idxPackage := 0 to PS.PackageCount - 1 do
      begin
        for idxComponent := 0 to Ps.ComponentCount[idxPackage] - 1 do
        begin
          ComponentClassName := PS.ComponentNames[idxPackage, idxComponent];
          try
            ComponentClass := FindClass(ComponentClassName);
          except
            ComponentClass := nil;
          end;

          if Assigned(ComponentClass) then
          begin
            Li := lv_Components.Items.Add;
            Li.Caption := ComponentClassName;
            Li.SubItems.Add(rttihGetUnit(ComponentClass));
            Li.SubItems.Add(ps.PackageNames[idxPackage]);
            Li.Data := ComponentClass;
          end;
          
        end;
      end;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
  
end;

procedure TCNA2ConfigDialog.lv_ComponentsColumnClick(Sender: TObject;
  Column: TListColumn);
begin
  if FLastSortCol = Column.Index then
    FLastSortWasAsc := not FLastSortWasAsc;

  FLastSortCol := Column.Index;
  
  lv_Components.CustomSort(@CompareItems, Column.Index);
end;

procedure TCNA2ConfigDialog.tv_ProfilesDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  TargetNode : TTreeNode;
  NDataTarget: PNodeData;
begin
  TargetNode := tv_Profiles.GetNodeAt(X,Y);
  if Assigned(TargetNode) then
  begin
    NDataTarget := GetNodeData(TargetNode);

    if (Source = lv_Components) and (Assigned(lv_Components.Selected))then
    begin
      tv_Profiles.Selected := AddComponentNode(TargetNode, cna2Settings.AddComponent(NDataTarget.Setting, TClass(lv_Components.Selected.Data)))
    end;
  end;
end;

procedure TCNA2ConfigDialog.tv_ProfilesDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var
  TargetNode : TTreeNode;
  NDataTarget: PNodeData;
begin
  Accept := false;

  TargetNode := tv_Profiles.GetNodeAt(X,Y);
  if Assigned(TargetNode) then
  begin
    NDataTarget := GetNodeData(TargetNode);

    if (Source = lv_Components) and (Assigned(lv_Components.Selected))then
    begin
      Accept := NDataTarget.NodeType = ntGroup;
    end;
  end;
end;

procedure TCNA2ConfigDialog.tv_ProfilesEdited(Sender: TObject; Node: TTreeNode;
  var S: string);
var
  NData : PNodeData;
begin
  NData := GetNodeData(Node);

  cna2Settings.SetValue(NData.Setting, S);
  
end;

procedure TCNA2ConfigDialog.tv_ProfilesEditing(Sender: TObject; Node: TTreeNode;
  var AllowEdit: Boolean);
var
  NData : PNodeData;
begin
  NData := GetNodeData(Node);

  AllowEdit := NData.NodeType in [ntProfile, ntGroup];
end;

procedure TCNA2ConfigDialog.tv_ProfilesKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F2 then
    tv_Profiles.Selected.EditText;
end;

end.
