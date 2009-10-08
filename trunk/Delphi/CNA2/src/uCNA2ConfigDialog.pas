unit uCNA2ConfigDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls,
  ToolsAPI, ExtCtrls,
  uRTTIHelper,
  uCNA2Settings,
  uSettingsBase, ImgList,
  WideStrUtils, ToolWin,
  WideStrings,
  TypInfo,
  uCNA2Profiles, Menus,
  uCNA2Actions;

type
  TCNA2ConfigDialog = class(TForm)
    PC: TPageControl;
    ts_ProfilesActions: TTabSheet;
    gb_PGC: TGroupBox;
    tv_Profiles: TTreeView;
    Splitter1: TSplitter;
    iml_TreeProfiles: TImageList;
    TB: TToolBar;
    btn_AddProfile: TToolButton;
    iml_TB: TImageList;
    btn_AddGroup: TToolButton;
    btn_Delete: TToolButton;
    gb_Actions: TGroupBox;
    lv_Actions: TListView;
    pum_Profiles: TPopupMenu;
    mi_CurrentProfile: TMenuItem;
    rg_ActionClasses: TRadioGroup;
    gb_Components: TGroupBox;
    lv_Components: TListView;
    Splitter2: TSplitter;
    pan_ConfigAction: TPanel;
    btn_ConfigAction: TButton;
    pan_Buttons: TPanel;
    btn_OK: TButton;
    btn_Cancel: TButton;
    btn_MoveUp: TToolButton;
    btn_MoveDown: TToolButton;
    ToolButton3: TToolButton;
    ts_Misc: TTabSheet;
    cb_ExpertActive: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure lv_ComponentsColumnClick(Sender: TObject; Column: TListColumn);
    procedure tv_ProfilesKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure tv_ProfilesDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure tv_ProfilesDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure tv_ProfilesEditing(Sender: TObject; Node: TTreeNode;
      var AllowEdit: Boolean);
    procedure tv_ProfilesEdited(Sender: TObject; Node: TTreeNode;
      var S: string);
    procedure btn_DeleteClick(Sender: TObject);
    procedure btn_AddProfileClick(Sender: TObject);
    procedure btn_AddGroupClick(Sender: TObject);
    procedure mi_CurrentProfileClick(Sender: TObject);
    procedure pum_ProfilesPopup(Sender: TObject);
    procedure tv_ProfilesChange(Sender: TObject; Node: TTreeNode);
    procedure lv_ActionsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure rg_ActionClassesClick(Sender: TObject);
    procedure btn_ConfigActionClick(Sender: TObject);
    procedure btn_MoveUpClick(Sender: TObject);
    procedure btn_MoveDownClick(Sender: TObject);
    procedure cb_ExpertActiveClick(Sender: TObject);
  private
    procedure LoadComponents;
    procedure InitProfileTree;

    procedure UpdateCurrentMarker;

    function FindNode(AData : TObject; out ANode : TTreeNode) : Boolean;

    function AddProfileNode(AProfile : Tcna2Profile) : TTreeNode;
    function AddGroupNode(AGroup : Tcna2Group) : TTreeNode;
    function AddComponentNode(AComponent : Tcna2Component) : TTreeNode;

    procedure AddAll(AProfile : Tcna2Profile); overload;
    procedure AddAll(AGroup : Tcna2Group); overload;

    procedure GroupSelected(AGroup : Tcna2Group);
    procedure PropertySelected(AGroup : Tcna2Group; AProperty : TListItem);
    procedure ActionSelected(AGroup : Tcna2Group;
                             AProperty : TListItem;
                             AActionClass : Tcna2ActionClass);
    procedure UpdateActionString(AGroup : Tcna2Group; AProperty : TListItem);

    function GetCurrentGroup : Tcna2Group;

    procedure MoveCurrentGroup(AUp : Boolean);
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

procedure TCNA2ConfigDialog.AddAll(AProfile: Tcna2Profile);
var
  idxGroup : Integer;
  NodeGroup : TTreeNode;
begin
  for idxGroup := 0 to AProfile.Groups.Count - 1 do
  begin
    NodeGroup := AddGroupNode(AProfile.Groups[idxGroup]);

    AddAll(AProfile.Groups[idxGroup]);
  end;
end;


procedure TCNA2ConfigDialog.ActionSelected(AGroup: Tcna2Group;
  AProperty: TListItem; AActionClass: Tcna2ActionClass);
var
  Action : Tcna2Action;
begin
  if Assigned(AGroup) and
     Assigned(AProperty) and
     Assigned(AActionClass) then
  begin
    Action := nil;

    if AGroup.GetAction(AProperty.Caption, Action) and
      (Action.ClassType <> AActionClass) then
      AGroup.RemoveAction(AProperty.Caption);

    if not Assigned(Action) then
    begin
      AGroup.AddAction(AProperty.Caption, AActionClass);
    end;

    btn_ConfigAction.Enabled := AActionClass.HasConfigDialog;
    UpdateActionString(AGroup, AProperty);
  end
  else
  if Assigned(AGroup) and
     Assigned(AProperty) then
  begin
    AGroup.RemoveAction(AProperty.Caption);
    UpdateActionString(AGroup, AProperty);
    btn_ConfigAction.Enabled := false;
  end
  else
    btn_ConfigAction.Enabled := false;

end;

procedure TCNA2ConfigDialog.AddAll(AGroup: Tcna2Group);
var
  idxComponent: Integer;
begin
  for idxComponent := 0 to AGroup.Components.Count - 1 do
  begin
    AddComponentNode(AGroup.Components[idxComponent]);
  end;
end;

function TCNA2ConfigDialog.AddComponentNode(
  AComponent: Tcna2Component): TTreeNode;
begin
  if not FindNode(AComponent, Result) then
  begin
    Result := tv_Profiles.Items.AddChild(AddGroupNode(AComponent.Group), AComponent.ComponentClass.ClassName);
    Result.Data := AComponent;
    Result.ImageIndex := 3;
    Result.SelectedIndex := Result.ImageIndex;
  end;
end;

function TCNA2ConfigDialog.AddGroupNode(AGroup: Tcna2Group): TTreeNode;
begin
  if not FindNode(AGroup, Result) then
  begin
    Result := tv_Profiles.Items.AddChild(AddProfileNode(AGroup.Profile), AGroup.Name);
    Result.Data := AGroup;
    Result.ImageIndex := 2;
    Result.SelectedIndex := Result.ImageIndex;
  end;
end;

function TCNA2ConfigDialog.AddProfileNode(AProfile: Tcna2Profile): TTreeNode;
begin
  if not FindNode(AProfile, Result) then
  begin
    Result := tv_Profiles.Items.AddChild(nil, AProfile.Name);
    Result.Data := AProfile;
    Result.ImageIndex := 0;
    Result.SelectedIndex := Result.ImageIndex;
  end;

  UpdateCurrentMarker;
end;

procedure TCNA2ConfigDialog.btn_AddGroupClick(Sender: TObject);
var
  Sel : TObject;
  P : Tcna2Profile;
begin
  P := nil;

  if Assigned(tv_Profiles.Selected) then
  begin
    Sel := TObject(tv_Profiles.Selected.Data);

    if Sel is Tcna2Profile then
      P := Tcna2Profile(Sel)
    else
    if Sel is Tcna2Group then
      P := Tcna2Group(Sel).Profile
    else
    if Sel is Tcna2Component then
      P := Tcna2Component(Sel).Group.Profile
  end;

  if Assigned(P) then
    AddGroupNode(P.AddGroup('New Group')).EditText
  else
    MessageDlg('Please choose a Profile first!', mtWarning, [mbOK], 0);
end;

procedure TCNA2ConfigDialog.btn_AddProfileClick(Sender: TObject);
begin
  AddProfileNode(cna2Profiles.AddProfile('New profile')).EditText;
end;

procedure TCNA2ConfigDialog.btn_ConfigActionClick(Sender: TObject);
var
  Group : Tcna2Group;
  Prop : TListItem;
  Action : Tcna2Action;
begin
  Group := GetCurrentGroup;
  Prop := lv_Actions.Selected;

  if Assigned(Group) and
     Assigned(Prop) and
     Group.GetAction(Prop.Caption, Action) and
     Action.HasConfigDialog then
  begin
    Action.Configure();
    UpdateActionString(Group, Prop);
  end;
end;

procedure TCNA2ConfigDialog.btn_DeleteClick(Sender: TObject);
begin
  if Assigned(tv_Profiles.Selected) then
  begin
    if (MessageBox(0, 'Do you really want to delete this item?', 'Delete item', MB_ICONQUESTION or MB_YESNO) = idYes) then
    begin
      TObject(tv_Profiles.Selected.Data).Free;
      tv_Profiles.Items.Delete(tv_Profiles.Selected);
    end;
  end;

  UpdateCurrentMarker;
end;

procedure TCNA2ConfigDialog.btn_MoveDownClick(Sender: TObject);
begin
  MoveCurrentGroup(false);
end;

procedure TCNA2ConfigDialog.btn_MoveUpClick(Sender: TObject);
begin
  MoveCurrentGroup(true);
end;

procedure TCNA2ConfigDialog.cb_ExpertActiveClick(Sender: TObject);
begin
  cna2Settings.ExpertActive := cb_ExpertActive.Checked;
end;

class procedure TCNA2ConfigDialog.Execute;
var
  Form : TCNA2ConfigDialog;
begin
  Form := TCNA2ConfigDialog.Create(nil);
  try
    if Form.ShowModal = mrOk then
      cna2Profiles.Save
    else
      cna2Profiles.Load;
  finally
    Form.Free;
  end;
end;


function TCNA2ConfigDialog.FindNode(AData: TObject;
  out ANode: TTreeNode): Boolean;
var
  idx : Integer;
begin
  Result := false;

  for idx := 0 to tv_Profiles.Items.Count - 1 do
  begin
    if tv_Profiles.Items[idx].Data = AData then
    begin
      Result := true;
      ANode := tv_Profiles.Items[idx];
      break;
    end;
  end;
end;

procedure TCNA2ConfigDialog.FormCreate(Sender: TObject);
begin
  LoadComponents;
  InitProfileTree;

  FLastSortCol := -1;
  FLastSortWasAsc := false;

  cb_ExpertActive.Checked := cna2Settings.ExpertActive;
end;

function TCNA2ConfigDialog.GetCurrentGroup: Tcna2Group;
begin
  if Assigned(tv_Profiles.Selected) and
     (TObject(tv_Profiles.Selected.Data) is Tcna2Group) then
    Result := Tcna2Group(tv_Profiles.Selected.Data)
  else
    Result := nil;
end;

procedure TCNA2ConfigDialog.GroupSelected(AGroup: Tcna2Group);
var
  idx : Integer;
  Li : TListItem;
begin
  if not Assigned(AGroup) then
    lv_Actions.Clear
  else
  begin
    lv_Actions.Clear;

    for idx := 0 to AGroup.Properties.Count - 1 do
    begin
      Li := lv_Actions.Items.Add;
      Li.Caption := AGroup.Properties[idx];
      li.SubItems.Add(EmptyStr);
      Li.Data := AGroup.Properties.Objects[idx];

      UpdateActionString(AGroup, Li);
    end;

    if lv_Actions.Items.Count > 0 then
      lv_Actions.Selected := lv_Actions.Items[0];
  end;
end;

procedure TCNA2ConfigDialog.InitProfileTree;
var
  idxProfile,
  idxGroup,
  idxComponent : Integer;
  Profile : Tcna2Profile;
  Group : Tcna2Group;
  Compo : Tcna2Component;
begin
  for idxProfile := 0 to cna2Profiles.Profiles.Count - 1 do
  begin
    Profile := cna2Profiles.Profiles[idxProfile];
    AddProfileNode(Profile);

    for idxGroup := 0 to Profile.Groups.Count - 1 do
    begin
      Group := Profile.Groups[idxGroup];

      AddGroupNode(Group);
      for idxComponent := 0 to Group.Components.Count - 1 do
      begin
        Compo := Group.Components[idxComponent];
        AddComponentNode(Compo);
      end;
    end;
  end;
end;

procedure TCNA2ConfigDialog.LoadComponents;
var
  PS : IOTAPackageServices;
  idxPackage,
  idxComponent : Integer;
  ComponentClassName : String;
  ComponentClass : TPersistentClass;
  Li : TListItem;
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
          ComponentClass := GetClass(ComponentClassName);
          if Assigned(ComponentClass) then
          begin
            LI := lv_Components.Items.Add;
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


procedure TCNA2ConfigDialog.lv_ActionsSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  if Selected then
  begin
    if GetCurrentGroup <> nil then
    begin
      PropertySelected(GetCurrentGroup, Item);
    end;
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


procedure TCNA2ConfigDialog.mi_CurrentProfileClick(Sender: TObject);
begin
  cna2Profiles.CurrentProfile := Tcna2Profile(tv_Profiles.Selected.Data);
  UpdateCurrentMarker;
end;

procedure TCNA2ConfigDialog.MoveCurrentGroup(AUp: Boolean);
var
  Group : Tcna2Group;
  NewIndex,
  idx : Integer;
  Node : TTreeNode;
begin
  Group := GetCurrentGroup;
  idx := Group.Profile.Groups.IndexOf(Group);

  if AUp then
    NewIndex := idx - 1
  else
    NewIndex := idx + 1;

  if (AUp and (idx > 0)) or ((not AUp) and (idx < Group.Profile.Groups.Count - 1)) then
    Group.Profile.Groups.Move(idx, NewIndex);

  if FindNode(Group.Profile, Node) then
  begin
    Node.DeleteChildren;
    AddAll(Group.Profile);

    Node.Expanded := true;

    if FindNode(Group, Node) then
      tv_Profiles.Selected := Node;
  end;
end;

procedure TCNA2ConfigDialog.PropertySelected(AGroup: Tcna2Group;
  AProperty: TListItem);
var
  idxItem,
  idx : Integer;
  Action : Tcna2Action;
begin
  while rg_ActionClasses.Items.Count > 1 do
    rg_ActionClasses.Items.Delete(1);

  rg_ActionClasses.Enabled := Assigned(AGroup) and Assigned(AProperty);

  if Assigned(AGroup) and Assigned(AProperty) then
  begin
    for idx := 0 to cna2Actions.Count - 1 do
    begin
      if cna2Actions[idx].CanHandle(AProperty.Data) then
      begin
        idxItem := rg_ActionClasses.Items.AddObject(cna2Actions[idx].GetDisplayName, TObject(cna2Actions[idx]));

        if AGroup.GetAction(AProperty.Caption, Action) and
           (Action.ClassType = cna2Actions[idx]) then
        begin
          rg_ActionClasses.ItemIndex := idxItem;
          ActionSelected(AGroup, AProperty, cna2Actions[idx]);
        end;
      end;
    end;
  end;
end;

procedure TCNA2ConfigDialog.pum_ProfilesPopup(Sender: TObject);
begin
  mi_CurrentProfile.Enabled := Assigned(tv_Profiles.Selected) and (TObject(tv_Profiles.Selected.Data) is Tcna2Profile);
end;

procedure TCNA2ConfigDialog.rg_ActionClassesClick(Sender: TObject);
var
  Group : Tcna2Group;
begin
  if GetCurrentGroup <> nil then
  begin
    Group := GetCurrentGroup;

    if rg_ActionClasses.ItemIndex = 0 then
      ActionSelected(Group, lv_Actions.Selected, nil)
    else
      ActionSelected(Group, lv_Actions.Selected, Tcna2ActionClass(rg_ActionClasses.Items.Objects[rg_ActionClasses.ItemIndex]));
  end;
end;

procedure TCNA2ConfigDialog.tv_ProfilesChange(Sender: TObject; Node: TTreeNode);
begin
  btn_MoveUp.Enabled := Assigned(Node) and (TObject(Node.Data) is Tcna2Group);
  btn_MoveDown.Enabled := btn_MoveUp.Enabled;
  
  GroupSelected(GetCurrentGroup);
end;

procedure TCNA2ConfigDialog.tv_ProfilesDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  Node : TTreeNode;
  TargetObject : TObject;
  SourceObject : TObject;
  ComponentClass : TClass;
  P : Tcna2Profile;
  G : Tcna2Group;
  C : Tcna2Component;
  Li : TListItem;
begin
   Node := tv_Profiles.GetNodeAt(X,Y);
  if Assigned(Node) then
    TargetObject := TObject(Node.Data)
  else
    TargetObject := nil;

  if (Source = lv_Components) and Assigned(lv_Components.Selected) then
    SourceObject := lv_Components.Selected
  else
  if (Source = tv_Profiles) and Assigned(tv_Profiles.Selected) then
    SourceObject := TObject(tv_Profiles.Selected.Data)
  else
    SourceObject := nil;

  if SourceObject is TListItem then
  begin
    Li := TListItem(SourceObject);
    ComponentClass := TClass(Li.Data);

    if TargetObject is Tcna2Group then
    begin
      G := Tcna2Group(TargetObject);
      C := G.AddComponent(ComponentClass, Li.SubItems[1]);
      tv_Profiles.Selected := AddComponentNode(C)
    end
    else
    if TargetObject is Tcna2Profile then
    begin
      P := Tcna2Profile(TargetObject);
      G := P.AddGroup(Li.SubItems[0]);
      C := G.AddComponent(ComponentClass, Li.SubItems[1]);
      tv_Profiles.Selected := AddComponentNode(C);
    end
    else
    if TargetObject = nil then
    begin
      P := cna2Profiles.AddProfile(Li.SubItems[1]);
      G := P.AddGroup(Li.SubItems[0]);
      C := G.AddComponent(ComponentClass, Li.SubItems[1]);
      tv_Profiles.Selected := AddComponentNode(C);
    end;
  end
  else
  if SourceObject is Tcna2Profile then
  begin
    P :=cna2Profiles.AddProfile(Tcna2Profile(SourceObject).Name);
    P.CopyContent(Tcna2Profile(SourceObject));
    AddAll(P);
    tv_Profiles.Selected := AddProfileNode(P);
  end
  else
  if SourceObject is Tcna2Group then
  begin
    G := Tcna2Group(SourceObject);

    if TargetObject is Tcna2Profile then
      P := Tcna2Profile(TargetObject)
    else
    if TargetObject = nil then
      P := cna2Profiles.AddProfile(G.Profile.Name);

    G := P.AddGroup(Tcna2Group(SourceObject).Name);
    G.CopyContent(Tcna2Group(SourceObject));
    AddAll(G);
    tv_Profiles.Selected := AddGroupNode(G);

  end
  else
  if SourceObject is Tcna2Component then
  begin
    C := Tcna2Component(SourceObject);

    if TargetObject is Tcna2Group then
    begin
      G := Tcna2Group(TargetObject);
      C := G.AddComponent(C.ComponentClass, C.SourcePackage);
      tv_Profiles.Selected := AddComponentNode(C);
    end
    else
    if TargetObject is Tcna2Profile then
    begin
      P := Tcna2Profile(TargetObject);
      G := P.AddGroup(C.Group.Name);
      C := G.AddComponent(C.ComponentClass, C.SourcePackage);
      tv_Profiles.Selected := AddComponentNode(C);
    end
    else
    if TargetObject = nil then
    begin
      P := cna2Profiles.AddProfile('New Profile');
      G := P.AddGroup('New Group');
      C := G.AddComponent(C.ComponentClass, C.SourcePackage);
      tv_Profiles.Selected := AddComponentNode(C);
    end;
  end;
  
  
  
end;

procedure TCNA2ConfigDialog.tv_ProfilesDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var
  Node : TTreeNode;
  TargetObject : TObject;
  SourceObject : TObject;
begin
  Accept := false;

  Node := tv_Profiles.GetNodeAt(X,Y);
  if Assigned(Node) then
    TargetObject := TObject(Node.Data)
  else
    TargetObject := nil;

  if (Source = lv_Components) and Assigned(lv_Components.Selected) then
    SourceObject := lv_Components.Selected
  else
  if (Source = tv_Profiles) and Assigned(tv_Profiles.Selected) then
    SourceObject := TObject(tv_Profiles.Selected.Data)
  else
    SourceObject := nil;

  if SourceObject is TListItem then
  begin
    Accept := (TargetObject = nil) or
              (TargetObject is Tcna2Profile) or
              (TargetObject is Tcna2Group);
  end
  else
  if SourceObject is Tcna2Profile then
  begin
    Accept := TargetObject = nil;
  end
  else
  if SourceObject is Tcna2Group then
  begin
    Accept := (TargetObject = nil) or
              (TargetObject is Tcna2Profile);

  end
  else
  if SourceObject is Tcna2Component then
  begin
    Accept := (TargetObject = nil) or
              (TargetObject is Tcna2Profile) or
              (TargetObject is Tcna2Group);
  end;
  
  
  
end;

procedure TCNA2ConfigDialog.tv_ProfilesEdited(Sender: TObject; Node: TTreeNode;
  var S: string);
begin
  if TObject(Node.Data) is Tcna2Profile then
    Tcna2Profile(Node.Data).Name := s
  else
  if TObject(Node.Data) is Tcna2Group then
    Tcna2Group(Node.Data).Name := s;
end;

procedure TCNA2ConfigDialog.tv_ProfilesEditing(Sender: TObject; Node: TTreeNode;
  var AllowEdit: Boolean);
begin
  AllowEdit := (TObject(Node.Data) is Tcna2Profile) or
               (TObject(Node.Data) is Tcna2Group);
end;

procedure TCNA2ConfigDialog.tv_ProfilesKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F2 then
    tv_Profiles.Selected.EditText
  else
  if (Key = VK_DELETE) and (not tv_Profiles.IsEditing) then
  begin
    btn_Delete.Click;
  end;
end;

procedure TCNA2ConfigDialog.UpdateActionString(AGroup : Tcna2Group; AProperty: TListItem);
var
  Action : Tcna2Action;
begin
  if AGroup.GetAction(AProperty.Caption, Action) then
    AProperty.SubItems[0] := Action.AsString
  else
    AProperty.SubItems[0] := EmptyWideStr;
end;

procedure TCNA2ConfigDialog.UpdateCurrentMarker;
var
  P : Tcna2Profile;
  idx : Integer;
begin
  for idx := 0 to tv_Profiles.Items.Count - 1 do
  begin
    if TObject(tv_Profiles.Items[idx].Data) is Tcna2Profile then
    begin
      P := Tcna2Profile(tv_Profiles.Items[idx].Data);
      if P = cna2Profiles.CurrentProfile then
      begin
        tv_Profiles.Items[idx].ImageIndex := 1;
        tv_Profiles.Items[idx].SelectedIndex := tv_Profiles.Items[idx].ImageIndex;
      end
      else
      begin
        tv_Profiles.Items[idx].ImageIndex := 0;
        tv_Profiles.Items[idx].SelectedIndex := tv_Profiles.Items[idx].ImageIndex;
      end;
    end;
    
  end;
end;

end.
