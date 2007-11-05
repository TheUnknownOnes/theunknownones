//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit unitCNAConfig;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, Buttons, TypInfo, StrUtils,
  unitCNALangs,UnitCNATypes, Menus, ImgList;

type
  TformConfig = class(TForm)
    gbOptions: TGroupBox;
    panExpertActive: TPanel;
    cbExpertActive: TCheckBox;
    panUseNA: TPanel;
    cbUseNA: TCheckBox;
    panNAOptions: TPanel;
    cbNAPopIfDefined: TCheckBox;
    cbNACreateName: TCheckBox;
    lblNADelimiter: TLabel;
    edNADelimiter: TEdit;
    panUsePA: TPanel;
    cbUsePA: TCheckBox;
    gbSettings: TGroupBox;
    panFooter: TPanel;
    TV: TTreeView;
    panSettingsClient: TPanel;
    panComponents: TPanel;
    lblComponents: TLabel;
    comComponents: TComboBox;
    btnAddComponentGroup: TBitBtn;
    btnAddComponentNew: TBitBtn;
    btnOK: TBitBtn;
    pumTV: TPopupMenu;
    miAddProfile: TMenuItem;
    miAddGroup: TMenuItem;
    miDelComponent: TMenuItem;
    miDelGroup: TMenuItem;
    N1: TMenuItem;
    miDelProfile: TMenuItem;
    miRenameGroup: TMenuItem;
    miRenameProfile: TMenuItem;
    N2: TMenuItem;
    miDelete: TMenuItem;
    panPAProfile: TPanel;
    comPAProfile: TComboBox;
    lblProfile: TLabel;
    gbValues: TGroupBox;
    LV: TListView;
    panPAShowDLG: TPanel;
    cbPAShowDLG: TCheckBox;
    panCredits: TPanel;
    procedure LVAdvancedCustomDrawSubItem(Sender: TCustomListView;
      Item: TListItem; SubItem: Integer; State: TCustomDrawState;
      Stage: TCustomDrawStage; var DefaultDraw: Boolean);
    procedure TVKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure LVKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure comComponentsKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure LVDblClick(Sender: TObject);
    procedure LVCompare(Sender: TObject; Item1, Item2: TListItem; Data: Integer;
      var Compare: Integer);
    procedure LVAdvancedCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage;
      var DefaultDraw: Boolean);
    procedure comPAProfileClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure miDeleteClick(Sender: TObject);
    procedure TVDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure TVDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure miRenameProfileClick(Sender: TObject);
    procedure miRenameGroupClick(Sender: TObject);
    procedure btnAddComponentNewClick(Sender: TObject);
    procedure miDelProfileClick(Sender: TObject);
    procedure miDelGroupClick(Sender: TObject);
    procedure miDelComponentClick(Sender: TObject);
    procedure miAddGroupClick(Sender: TObject);
    procedure btnAddComponentGroupClick(Sender: TObject);
    procedure miAddProfileClick(Sender: TObject);
    procedure TVClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FSettings : TCNASettings;
    PSettings : PCNASettings;

    procedure MoveNode(FromN : TTreeNode; ToN : TTreeNode);
    procedure RefreshProfiles();
    function PropValueToString(Group : TCNAComponentGroup; PropSetI: Integer) : String;
  public
    procedure LoadSettings(Settings : PCNASettings);
  end;

var
  formConfig: TformConfig;

implementation

uses UnitCNAEditProperty, UnitCNAEditPropertyEnumeration,
  UnitCNAEditPropertySet, UnitCNAEditPropertyInteger, UnitCNAEditPropertyString,
  UnitCNAEditPropertyFloat, UnitCNAMain;

{$R *.dfm}

{.$region 'Form'}

procedure TformConfig.FormCreate(Sender: TObject);
{.$region 'GetStringsOfChild'}
  procedure GetStringsOfChildren(AParent: TComponent);
  var
    i : Integer;
  begin
    for i:=0 to AParent.ComponentCount-1 do
    begin
      if (not LangStringAvailable(AParent.Components[i],'Caption')) then
        Continue;
      if (AParent.Components[i] is TLabel) then
        TLabel(AParent.Components[i]).Caption:=GetLangString(AParent.Components[i],'Caption');
      if (AParent.Components[i] is TMenuItem) then
        TMenuItem(AParent.Components[i]).Caption:=GetLangString(AParent.Components[i],'Caption');
      if (not (AParent.Components[i] is TWincontrol)) then
        Continue;
      SetWindowText(TWinControl(AParent.Components[i]).Handle,
                      PChar(GetLangString(AParent.Components[i],'Caption')));
      if (AParent.Components[i].ComponentCount>0) then
        GetStringsOfChildren(AParent.Components[i]);
    end;
  end;

{.$endregion}
begin
  GetStringsOfChildren(Self);
  LV.Columns[0].Caption:=GetLangString('ListColNameCaption');
  LV.Columns[2].Caption:=GetLangString('ListColValueCaption');
end;

procedure TformConfig.btnOKClick(Sender: TObject);
begin
  if (comPAProfile.ItemIndex=-1) and (cbExpertActive.Checked) then
  begin
    MessageDlg(GetLangString('ChooseActiveProfile'), mtError, [mbOK], 0);
    exit;
  end;
  with TCNASettings(PSettings^) do
  begin
    ExpertActive:=cbExpertActive.Checked;
    UseNA:=cbUseNA.Checked;
    NAPopupIfAssigned:=cbNAPopIfDefined.Checked;
    NACreateName:=cbNACreateName.Checked;
    NADelimiter:=edNADelimiter.Text;
    UsePA:=cbUsePA.Checked;
    PAShowDLG:=cbPAShowDLG.Checked;
  end;
  Self.ModalResult:=mrOk;
end;

{.$endregion}

{.$region 'Helper'}

procedure TformConfig.MoveNode(FromN, ToN: TTreeNode);
var
  OldCompo, NewCompo : TCNAComponent;
  OldGroup, NewGroup : TCNAComponentGroup;
  OldProfile, NewProfile : TCNAProfile;
  Node, NGroup : TTreeNode;
  iGroup,iCompo : Integer;
  FoundCompo : Boolean;
begin
  // FromN has to be the source-node
  //ToN has to be the new parent for FromN

  if (TObject(FromN.Data) is TCNAComponent) then
  begin
    OldCompo:=TCNAComponent(FromN.Data);
    OldGroup:=TCNAComponentGroup(FromN.Parent.Data);
    NewGroup:=TCNAComponentGroup(ToN.Data);
    OldProfile:=TCNAProfile(FromN.Parent.Parent.Data);
    NewProfile:=TCNAProfile(ToN.Parent.Data);

    if (NewGroup.GetComponent(OldCompo.CNAClassName)<>nil) then
      exit;

    if (not NewGroup.AddComponent(OldCompo.CNAClassName)) then
    begin
      if (MessageBox(0, PAnsiChar(GetLangString('AddComponentBoxText')),
                        PAnsiChar(GetLangString('AddComponentBoxCaption')),
                        MB_ICONQUESTION or MB_YESNO) = idNo) then
        exit;
      NewGroup.AddComponent(OldCompo.CNAClassName,true);
    end;
    NewCompo:=NewGroup.GetComponent(OldCompo.CNAClassName);
    if (NewCompo<>nil) then
    begin
      Node:=TV.Items.AddChild(ToN,OldCompo.CNAClassName);
      Node.Data:=TObject(NewCompo);
      if (NewProfile=OldProfile) then
      begin
        OldGroup.DeleteComponent(OldCompo.CNAClassName);
        TV.Items.Delete(FromN);
      end;
    end
    else
      MessageDlg(GetLangString('AddComponentCouldNotAdd'), mtWarning, [mbOK], 0);
    exit;
  end;

  if (TObject(FromN.Data) is TCNAComponentGroup) then
  begin
    OldGroup:=TCNAComponentGroup(FromN.Data);
    OldProfile:=TCNAProfile(FromN.Parent.Data);
    NewProfile:=TCNAProfile(ToN.Data);

    if (NewProfile.GetGroup(OldGroup.GroupName)<>nil) then exit;

    NewProfile.AddGroup(OldGroup.GroupName);
    NewGroup:=NewProfile.GetGroup(OldGroup.GroupName);

    if (NewGroup<>nil) then
    begin
      NewGroup.PropertiesSet.Clear;
      NewGroup.PropertiesSet.AddStrings(OldGroup.PropertiesSet);
      NewGroup.Prefix:=OldGroup.Prefix;
      NewGroup.Suffix:=OldGroup.Suffix;

      NGroup:=Tv.Items.AddChild(ToN,OldGroup.GroupName);
      NGroup.Data:=TObject(NewGroup);
      for iCompo:=FromN.Count-1 downto 0 do
      begin
        OldCompo:=TCNAComponent(FromN.Item[iCompo].Data);
        FoundCompo:=false;
        for iGroup:=0 to NewProfile.GetGroupCount-1 do
        begin
          if (NewProfile.GetGroup(iGroup).GetComponent(OldCompo.CNAClassName)<>nil) then
          begin
            FoundCompo:=true;
            break;
          end;
        end;
        if (not FoundCompo) then
        begin
          NewGroup.AddComponent(OldCompo.CNAClassName,true);
          NewCompo:=NewGroup.GetComponent(OldCompo.CNAClassName);
          Node:=Tv.Items.AddChild(NGroup,NewCompo.CNAClassName);
          Node.Data:=TObject(NewCompo);
          if (NewProfile=OldProfile) then
          begin
            OldGroup.DeleteComponent(OldCompo.CNAClassName);
            Tv.Items.Delete(FromN.Item[iCompo]);
          end;
        end;
      end;
      if (NewProfile=OldProfile) then
      begin
        OldProfile.DeleteGroup(OldGroup.GroupName);
        TV.Items.Delete(FromN);
      end;
      Tv.Selected:=NGroup;
    end
    else
      MessageDlg(GetLangString('AddGroupFailed'), mtError, [mbOK], 0);
  end;
end;

procedure TformConfig.LoadSettings(Settings: PCNASettings);
var
  NProfile,
  NGroup,
  NClass : TTreeNode;
  iProfiles,
  iGroups,
  iClasses : Integer;
  cProfile : TCNAProfile;
  cGroup : TCNAComponentGroup;
  cComponent : TCNAComponent;
begin
  FSettings:=Settings^;
  PSettings:=Settings;
  cbExpertActive.Checked:=FSettings.ExpertActive;
  cbUseNA.Checked:=FSettings.UseNA;
  cbNAPopIfDefined.Checked:=FSettings.NAPopupIfAssigned;
  cbNACreateName.Checked:=FSettings.NACreateName;
  edNADelimiter.Text:=FSettings.NADelimiter;
  cbUsePA.Checked:=FSettings.UsePA;
  cbPAShowDLG.Checked:=FSettings.PAShowDLG;

  for iProfiles:=0 to FSettings.Profiles.GetProfileCount-1 do
  begin
    cProfile:=FSettings.Profiles.getProfile(iProfiles);
    NProfile:=TV.Items.AddChild(nil,cProfile.ProfileName);
    NProfile.Data:=TObject(cProfile);

    for iGroups:=0 to cProfile.GetGroupCount-1 do
    begin
      cGroup:=cProfile.GetGroup(iGroups);
      NGroup:=Tv.Items.AddChild(NProfile,cGroup.GroupName);
      NGroup.Data:=TObject(cGroup);
      for iClasses:=0 to cGroup.GetComponentCount-1 do
      begin
        cComponent:=cGroup.GetComponent(iClasses);
        NClass:=Tv.Items.AddChild(NGroup,cComponent.CNAClassName);
        NClass.Data:=TObject(cComponent);
      end;
    end;
    NProfile.Expand(False);
  end;

  RefreshProfiles;
end;

procedure TformConfig.RefreshProfiles();
var
  i : Integer;
begin
  comPAProfile.Clear;
  i:=-1;
  for i:=0 to FSettings.Profiles.GetProfileCount-1 do
    comPAProfile.Items.AddObject(FSettings.Profiles.GetProfile(i).ProfileName,
                                TObject(FSettings.Profiles.GetProfile(i)));
  i:=comPAProfile.Items.IndexOfObject(TObject(FSettings.curProfile));
  if (i>-1) then
    comPAProfile.ItemIndex:=i
  else if (comPAProfile.Items.Count>0) then
    comPAProfile.ItemIndex:=0
  else
    PSettings^.curProfile:=nil;

  comPAProfileClick(nil);
end;

function TformConfig.PropValueToString(Group : TCNAComponentGroup; PropSetI: Integer): String;
var
  Value : Integer;
  TypeK : TTypeKind;
  PropAvailI : Integer;
  tmpStr : TStrings;
  s      : String;
  i      : integer;
begin
  Result:='';
  tmpStr:=TStringList.Create;

  PropAvailI:=Group.PropertiesAvailable.IndexOfName(Group.PropertiesSet.Names[PropSetI]);
  if (PropAvailI>-1) then
  begin
    s:=Group.PropertiesAvailable.Values[Group.PropertiesAvailable.Names[PropAvailI]];
    if AnsiStartsStr('[',s) then
    begin
      tmpStr.CommaText:=copy(s,2,length(s)-2);
      for i:=0 to tmpStr.Count-1 do
        tmpStr.Objects[i]:=TObject(StrToInt(tmpStr.Values[tmpStr.Names[i]]));
    end;
    TypeK:=TTypeKind(Group.PropertiesAvailable.Objects[PropAvailI]);
    case TypeK of
      tkEnumeration:
        begin
          i:=tmpStr.IndexOfObject(TObject(StrToIntDef(Group.PropertiesSet.Values[Group.PropertiesSet.Names[PropSetI]],0)));
          if i<0 then
            i:=0;  
          Result:=tmpStr.Names[i];
        end;
      tkSet:
        begin
          Value:=StrToIntDef(Group.PropertiesSet.Values[Group.PropertiesSet.Names[PropSetI]],0);
          for i:=0 to tmpStr.Count-1 do
          begin
            if (Value and Integer(tmpStr.Objects[i])) = Integer(tmpStr.Objects[i]) then
              if Result='' then
                Result:=tmpStr.Names[i]
              else
                Result:=Result+','+tmpStr.Names[i];
          end;
        end;
    else
      Result:=Group.PropertiesSet.Values[Group.PropertiesSet.Names[PropSetI]];

    end;
  end;
  tmpStr.Free;
end;

{.$endregion}

{.$region 'Popmenu'}

procedure TformConfig.miAddProfileClick(Sender: TObject);
var
  ProfileName,
  Result        : String;
  Profile : TCNAProfile;
  Node : TTreeNode;
begin
  ProfileName:=GetLangString('NewProfileBoxDefault');
  Result:=Trim(InputBox(
                        GetLangString('NewProfileBoxCaption'),
                        GetLangString('NewProfileBoxPrompt'),
                        ProfileName));
  if (Result='') or (AnsiSameText(Result,ProfileName)) then exit;

  if (FSettings.Profiles.GetProfile(Result)<>nil) then
  begin
    MessageDlg(GetLangString('NewProfileExists'), mtError, [mbOK], 0);
    exit;
  end;

  FSettings.Profiles.AddProfile(Result);
  Profile:=FSettings.Profiles.GetProfile(Result);
  if (Profile<>nil) then
  begin
    Node:=Tv.Items.AddChild(nil,Result);
    Node.Data:=TObject(Profile);
    TV.Selected:=Node;
  end
  else
    MessageDlg(GetLangString('NewProfileCouldNotAdd'), mtError, [mbOK], 0);

  RefreshProfiles;
  TVClick(Sender);
end;

procedure TformConfig.miAddGroupClick(Sender: TObject);
var
  Profile : TCNAProfile;
  Group : TCNAComponentGroup;
  Node  : TTreeNode;
  GroupName,
  Result    : String;
begin
  if (Tv.Selected=nil) then
  begin
    MessageDlg(GetLangString('AddGroupSelectProfile'), mtWarning, [mbOK], 0);
    exit;
  end;

  Profile:=nil;

  if (TObject(Tv.Selected.Data) is TCNAProfile) then
    Profile:=TCNAProfile(Tv.Selected.Data);
  if (TObject(Tv.Selected.Data) is TCNAComponentGroup) then
  begin
    Profile:=TCNAProfile(Tv.Selected.Parent.Data);
    TV.Selected:=Tv.Selected.Parent;
  end;
  if (TObject(Tv.Selected.Data) is TCNAComponent) then
  begin
    Profile:=TCNAProfile(Tv.Selected.Parent.Parent.Data);
    TV.Selected:=Tv.Selected.Parent.Parent;
  end;

  GroupName:=GetLangString('AddGroupBoxDefault');
  Result:=Trim(InputBox(GetLangString('AddGroupBoxCaption'),
                        GetLangString('AddGroupBoxPrompt'),
                        GroupName));
  if (Result='') or (AnsiSameText(Result,GroupName)) then exit;

  if (Profile.GetGroup(Result)<>nil) then
  begin
    MessageDlg(GetLangString('AddGroupExists'), mtError, [mbOK], 0);
    exit;
  end;

  Profile.AddGroup(Result);
  Group:=Profile.GetGroup(Result);
  if (Group<>nil) then
  begin
    Node:=TV.Items.AddChild(TV.Selected,Result);
    Node.Data:=TObject(Group);
    Tv.Selected:=Node;
  end
  else
    MessageDlg(GetLangString('AddGroupFailed'), mtError, [mbOK], 0);

  TVClick(Sender);
end;

procedure TformConfig.miDelComponentClick(Sender: TObject);
var
  Group : TCNAComponentGroup;
  Compo : TCNAComponent;
  Node  : TTreeNode;
begin
  if (TV.Selected=nil) then exit;
  if not (TObject(Tv.Selected.Data) is TCNAComponent) then
  begin
    MessageDlg(GetLangString('DelComponentNoComponent'), mtWarning, [mbOK], 0);
    exit;
  end;

  Compo:=TCNAComponent(TV.Selected.Data);
  Group:=TCNAComponentGroup(TV.Selected.Parent.Data);
  Group.DeleteComponent(Compo.CNAClassName);
  Node:=TV.Selected;
  TV.Selected:=Node.getNextSibling;
  TV.Items.Delete(Node);

  TVClick(Sender);
end;

procedure TformConfig.miDelGroupClick(Sender: TObject);
var
  Group : TCNAComponentGroup;
  Profile : TCNAProfile;
  Node : TTreeNode;
begin
  if (Tv.Selected=nil) then exit;
  if (TObject(Tv.Selected.Data) is TCNAProfile) then
  begin
    MessageDlg(GetLangString('DelGroupSelectGroup'), mtWarning, [mbOK], 0);
    exit;
  end;

  if (TObject(TV.Selected.Data) is TCNAComponent) then
    Tv.Selected:=TV.Selected.Parent;

  Group:=TCNAComponentGroup(TV.Selected.Data);
  Profile:=TCNAProfile(TV.Selected.Parent.Data);
  if (Group.GetComponentCount>0) then
  begin
    if (MessageBox(0, PAnsiChar(GetLangString('DelGroupBoxText')),
                      PAnsiChar(GetLangString('DelGroupBoxCaption')),
                       MB_ICONQUESTION or MB_YESNO) = idNo) then
      exit;
  end;
  Profile.DeleteGroup(Group.GroupName);
  Node:=Tv.Selected;
  Tv.Selected:=Node.getNextSibling;
  TV.Items.Delete(Node);

  TVClick(Sender);
end;

procedure TformConfig.miDelProfileClick(Sender: TObject);
var
  Profile : TCNAProfile;
  Node : TTreeNode;
begin
  Profile:=nil;
  Node:=nil;

  if (TV.Selected=nil) then exit;
  if (TObject(Tv.Selected.Data) is TCNAProfile) then
  begin
    Profile:=TCNAProfile(TV.Selected.Data);
    Node:=TV.Selected;
  end;
  if (TObject(TV.Selected.Data) is TCNAComponentGroup) then
  begin
    Profile:=TCNAProfile(TV.Selected.Parent.Data);
    Node:=TV.Selected.Parent;
  end;
  if (TObject(TV.Selected.Data) is TCNAComponent) then
  begin
    Profile:=TCNAProfile(TV.Selected.Parent.Parent.Data);
    Node:=TV.Selected.Parent.Parent;
  end;

  if (Profile.GetGroupCount>0) then
  begin
    if (MessageBox(0, PAnsiChar(GetLangString('DelProfileBoxText')),
                      PAnsiChar(GetLangString('DelProfileBoxCaption')),
                      MB_ICONQUESTION or MB_YESNO) = idNo) then
      exit;
  end;

  FSettings.Profiles.DeleteProfile(Profile.ProfileName);
  TV.Selected:=Node.getNextSibling;
  TV.Items.Delete(Node);

  RefreshProfiles;
  TVClick(Sender);
end;

procedure TformConfig.miRenameGroupClick(Sender: TObject);
var
  Profile : TCNAProfile;
  Group : TCNAComponentGroup;
  NewName : String;
  OldName : String;
begin
  if (TV.Selected=nil) then exit;
  if (TObject(TV.Selected.Data) is TCNAProfile) then
  begin
    MessageDlg(GetLangString('RenameGroupNoGroup'), mtWarning, [mbOK], 0);
    exit;
  end;

  if (TObject(TV.Selected.Data) is TCNAComponent) then
    TV.Selected:=Tv.Selected.Parent;

  Profile:=TCNAProfile(TV.Selected.Parent.Data);
  Group:=TCNAComponentGroup(Tv.Selected.Data);

  OldName:=Group.GroupName;

  NewName:=Trim(InputBox(GetLangString('RenameGroupBoxCaption'),
                          GetLangString('RenameGroupBoxPrompt'),
                          OldName));
  if (not AnsiSameText(NewName,OldName)) and (NewName<>'') then
  begin
    if (Profile.GetGroup(NewName)<>nil) then
    begin
      MessageDlg(GetLangString('RenameGroupExists'), mtError, [mbOK], 0);
      exit;
    end;
    Profile.RenameGroup(OldName,NewName);
    Tv.Selected.Text:=NewName;
  end;

  TVClick(Sender);
end;

procedure TformConfig.miRenameProfileClick(Sender: TObject);
var
  Profile : TCNAProfile;
  NewName : String;
  OldName : String;
begin
  if (TV.Selected=nil) then exit;

  if (TObject(TV.Selected.Data) is TCNAComponent) then
    TV.Selected:=Tv.Selected.Parent.Parent;
  if (TObject(TV.Selected.Data) is TCNAComponentGroup) then
    TV.Selected:=Tv.Selected.Parent;

  Profile:=TCNAProfile(TV.Selected.Data);

  OldName:=Profile.ProfileName;

  NewName:=Trim(InputBox(GetLangString('RenameProfileBoxCaption'),
                          GetLangString('RenameProfileBoxPrompt'),
                          OldName));
  if (not AnsiSameText(NewName,OldName)) and (NewName<>'') then
  begin
    if (Profile.GetGroup(NewName)<>nil) then
    begin
      MessageDlg(GetLangString('RenameProfileExists'), mtError, [mbOK], 0);
      exit;
    end;
    FSettings.Profiles.RenameProfile(OldName,NewName);
    Tv.Selected.Text:=NewName;
  end;

  RefreshProfiles;
  TVClick(Sender);
end;

procedure TformConfig.miDeleteClick(Sender: TObject);
begin
  if (TV.Selected=nil) then exit;
  if (TObject(TV.Selected.Data) is TCNAComponent) then
  begin
    miDelComponentClick(Sender);
    exit;
  end;
  if (TObject(TV.Selected.Data) is TCNAComponentGroup) then
  begin
    miDelGroupClick(Sender);
    exit;
  end;
  if (TObject(TV.Selected.Data) is TCNAProfile) then
  begin
    miDelProfileClick(Sender);
    exit;
  end;

  TVClick(Sender);
end;

{.$endregion}

{.$region 'AddComponent-Buttons'}

procedure TformConfig.btnAddComponentGroupClick(Sender: TObject);
var
  Profile : TCNAProfile;
  Group : TCNAComponentGroup;
  Compo : TCNAComponent;
  Node  : TTreeNode;
  iGroup : Integer;
begin
  Group:=nil;

  if (comComponents.Items.IndexOf(comComponents.Text)=-1) then
  begin
    //MessageDlg(GetLangString('AddComponentValidComponent'), mtWarning, [mbOK], 0);
    //exit;
  end
  else
    comComponents.Text:=comComponents.Items[comComponents.Items.IndexOf(comComponents.Text)];

  if (Tv.Selected=nil) or
      (TObject(Tv.Selected.Data) is TCNAProfile) then
  begin
    MessageDlg(GetLangString('AddComponentSelectGroup'), mtWarning, [mbOK], 0);
    exit;
  end;

  if (TObject(Tv.Selected.Data) is TCNAComponentGroup) then
    Group:=TCNAComponentGroup(Tv.Selected.Data);

  if (TObject(Tv.Selected.Data) is TCNAComponent) then
  begin
    Group:=TCNAComponentGroup(Tv.Selected.Parent.Data);
    Tv.Selected:=Tv.Selected.Parent;
  end;
  Profile:=TCNAProfile(TV.Selected.Parent.Data);
  for iGroup:=0 to Profile.GetGroupCount-1 do
  begin
    if (Profile.GetGroup(iGroup).GetComponent(comComponents.Text)<>nil) then
    begin
      MessageDlg(GetLangString('AddComponentComponentExists'), mtError, [mbOK], 0);
      exit;
    end;
  end;

  if (not Group.AddComponent(comComponents.Text)) then
  begin
    if (MessageBox(0, PAnsiChar(GetLangString('AddComponentBoxText')),
                      PAnsiChar(GetLangString('AddComponentBoxCaption')),
                      MB_ICONQUESTION or MB_YESNO) = idNo) then
      exit;
    Group.AddComponent(comComponents.Text,true);
  end;
  Compo:=Group.GetComponent(comComponents.Text);
  if (Compo<>nil) then
  begin
    Node:=TV.Items.AddChild(Tv.Selected,comComponents.Text);
    Node.Data:=TObject(Compo);
  end
  else
  begin
    MessageDlg(GetLangString('AddComponentCouldNotAdd'), mtWarning, [mbOK], 0);
  end;
  TVClick(Sender);
end;

procedure TformConfig.btnAddComponentNewClick(Sender: TObject);
var
  Node : TTreeNode;
  Group : TCNAComponentGroup;
begin
  if (Tv.Selected=nil) then
  begin
    MessageDlg(GetLangString('AddComponentSelectProfile'), mtWarning, [mbOK], 0);
    exit;
  end;

  if (TObject(TV.Selected.Data) is TCNAComponent) then
    Tv.Selected:=Tv.Selected.Parent.Parent;
  if (TObject(TV.Selected.Data) is TCNAComponentGroup) then
    Tv.Selected:=Tv.Selected.Parent;

  TCNAProfile(TV.Selected.Data).AddGroup('NewGroup');
  Group:=TCNAProfile(TV.Selected.Data).GetGroup('NewGroup');
  if (Group<>nil) then
  begin
    Node:=TV.Items.AddChild(Tv.Selected,'NewGroup');
    Node.Data:=TObject(Group);
    TV.Selected:=Node;
    btnAddComponentGroupClick(Sender);
    if (Node.getFirstChild<>nil) then
    begin
      Node.Text:=Node.getFirstChild.Text;
      TCNAProfile(Node.Parent.Data).RenameGroup('NewGroup',Node.getFirstChild.Text);
    end
    else
    begin
      TCNAProfile(Node.Parent.Data).DeleteGroup('NewGroup');
      TV.Items.Delete(Node);
    end;
  end;
  TVClick(Sender);
end;

{.$endregion}

{.$region 'TreeView'}

procedure TformConfig.TVClick(Sender: TObject);
var
  Group : TCNAComponentGroup;
  LI : TListItem;
  iAvail, iSet : Integer;
begin
  if (Tv.Selected=nil) then
  begin
    gbValues.Visible:=false;
    exit;
  end;
  if (TObject(Tv.Selected.Data) is TCNAProfile) then
  begin
    gbValues.Visible:=false;
  end;

  if (TObject(Tv.Selected.Data) is TCNAComponentGroup) then
  begin
    Group:=TCNAComponentGroup(Tv.Selected.Data);
    gbValues.Visible:=true;
    lv.Clear;
    lv.Tag:=Integer(TObject(Group));
    LI:=Lv.Items.Add;
    Li.Caption:=GetLangString('Header_Fix');
    Li.Data:=Pointer(0);
    LI:=LV.Items.Add;
    Li.Data:=Pointer(1);
    Li.Caption:=GetLangString('Prefix');
    Li.SubItems.Add('');
    Li.SubItems.Add(Group.Prefix);
    LI:=LV.Items.Add;
    Li.Data:=Pointer(1);
    Li.Caption:=GetLangString('Suffix');
    Li.SubItems.Add('');
    Li.SubItems.Add(Group.Suffix);
    LI:=LV.Items.Add;
    Li.Data:=Pointer(2);
    Li.Caption:=GetLangString('Header_Properties');

    for iAvail:=0 to Group.PropertiesAvailable.Count-1 do
    begin
      LI:=LV.Items.Add;
      Li.Data:=Pointer(3);
      Li.Caption:=Group.PropertiesAvailable.Names[iAvail];
      iSet:=Group.PropertiesSet.IndexOfName(Li.Caption);
      if (iSet>-1) then
      begin
        if (Integer(Group.PropertiesSet.Objects[iSet]) and PROP_FLAGS_SHOW_INPUT_DLG) = PROP_FLAGS_SHOW_INPUT_DLG then
          Li.SubItems.Add('a')
        else
          Li.SubItems.Add('');
        Li.SubItems.Add(PropValueToString(Group,iSet))
      end
      else
      begin
        Li.SubItems.Add('');
        Li.SubItems.Add(GetLangString('PropertyNotSet'));
      end;
    end;
  end;

  if (TObject(Tv.Selected.Data) is TCNAComponent) then
  begin
    gbValues.Visible:=false;
  end;

end;

procedure TformConfig.TVDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  F,T : TTreeNode;
begin
  Accept:=false;
  if (Source<>Sender) then
    exit;

  F:=TV.Selected;
  T:=Tv.GetNodeAt(X,Y);

  if (F=nil) or (T=nil) or (F=T) or (F.Parent=T.Parent) or (F=T.Parent) or (F.Parent=T)then
    exit;

  if (TObject(F.Data) is TCNAComponent) then
  begin
    if (TObject(T.Data) is TCNAComponent) then
      Accept:=true;
    if (TObject(T.Data) is TCNAComponentGroup) then
      Accept:=true;
  end;

  if (TObject(F.Data) is TCNAComponentGroup) then
  begin
    if (TObject(T.Data) is TCNAProfile) then
      Accept:=true;
  end;
end;

procedure TformConfig.TVDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  F,T : TTreeNode;
begin
  if (Source<>Sender) then
    exit;

  F:=TV.Selected;
  T:=Tv.GetNodeAt(X,Y);

  if (F=nil) or (T=nil) then exit;

  if (TObject(F.Data) is TCNAComponent) then
  begin
    if (TObject(T.Data) is TCNAComponent) then
      T:=T.Parent;
    if (TObject(T.Data) is TCNAComponentGroup) then
    begin
      MoveNode(F,T);
      exit;
    end;
  end;

  if (TObject(F.Data) is TCNAComponentGroup) then
  begin
    if (TObject(T.Data) is TCNAComponentGroup) then
      T:=T.Parent;
    if (TObject(T.Data) is TCNAProfile) then
      MoveNode(F,T);
  end;

end;

procedure TformConfig.TVKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (TV.Selected=nil) then exit;
  if (Key=VK_F2) then
  begin
    if (TObject(Tv.Selected.Data) is TCNAComponentGroup) then
      miRenameGroupClick(Sender);
    if (TObject(Tv.Selected.Data) is TCNAProfile) then
      miRenameProfileClick(Sender);
  end;
end;

{.$endregion}

{.$region 'ComboBoxen'}

procedure TformConfig.comPAProfileClick(Sender: TObject);
var
  i : Integer;
begin
  i:=comPAProfile.ItemIndex;
  if (i>-1) then
  begin
    PSettings^.curProfile:=TCNAProfile(comPAProfile.Items.Objects[i]);
  end
  else
    PSettings^.curProfile:=nil;
end;

procedure TformConfig.comComponentsKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key=VK_RETURN) then
  begin
    btnAddComponentNew.SetFocus;
  end;
end;

{.$endregion}

{.$region 'ListView'}

procedure TformConfig.LVAdvancedCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage;
  var DefaultDraw: Boolean);
var
  i    : byte;
  PosY : Integer;
  HasProperties : Boolean;
  Group : TCNAComponentGroup;
begin
  if (Item.Data=Pointer(0)) or (Item.Data=Pointer(2)) then
  begin
    LV.Canvas.Brush.Color:=clWindow;
    LV.Canvas.FillRect(Item.DisplayRect(drBounds));

    LV.Canvas.TextOut(Item.DisplayRect(drLabel).Left,
                      Item.DisplayRect(drLabel).Top,
                      Item.Caption);


    PosY:=Item.DisplayRect(drBounds).Bottom-2;
    For i:=0 to 255 do
    begin
      LV.Canvas.Pixels[i,PosY]:=RGB(i,i,i);
    end;
    DefaultDraw:=False;
  end
  else
    DefaultDraw:=True;
end;

procedure TformConfig.LVAdvancedCustomDrawSubItem(Sender: TCustomListView;
  Item: TListItem; SubItem: Integer; State: TCustomDrawState;
  Stage: TCustomDrawStage; var DefaultDraw: Boolean);
begin
  if (Item.Data=Pointer(3)) and (SubItem=1) then
  begin
    LV.Canvas.Font.Name:='Marlett';
    LV.Canvas.Font.Size:=12;
  end;
end;

procedure TformConfig.LVCompare(Sender: TObject; Item1, Item2: TListItem;
  Data: Integer; var Compare: Integer);
begin
  Compare:=Integer(Item1.Data)-Integer(Item2.Data);
  if (Compare=0) then
    Compare:=AnsiCompareText(Item1.Caption,Item2.Caption);
end;

procedure TformConfig.LVKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_RETURN) then
    LVDblClick(Sender);
end;

procedure TformConfig.LVDblClick(Sender: TObject);
var
  Group : TCNAComponentGroup;

  myForm : TFormCNAPropEdBase;
  NewVal : String;
  i : Integer;
  ShowInputBox : Boolean;
begin
  Group:=TCNAComponentGroup(Lv.Tag);
  if (Group=nil) then exit;

  if (LV.Selected=nil) then exit;
  if (Integer(Lv.Selected.Data) in [0,2]) then exit; //Groupheader

  if (Integer(LV.Selected.Data)=1) then
  begin
    try
      myForm:=TFormCNAPropEdString.Create(nil);
      ShowInputBox:=false;
      NewVal:=LV.Selected.SubItems[1];
      myForm.GetNewValue(LV.Selected.SubItems[1],
                            GetLangString('Prefix'),
                            '',
                            false,
                            NewVal,
                            ShowInputBox);
      if (NewVal='<!--NILORNULL-->') then
      begin
        LV.Selected.SubItems[1]:='';
        if (LV.Selected.Caption=GetLangString('Prefix')) then
          Group.Prefix:='';
        if (LV.Selected.Caption=GetLangString('Suffix')) then
          Group.Suffix:='';
      end
      else if (NewVal<>'<!--CANCELOFDLG-->') then
      begin
        LV.Selected.SubItems[1]:=NewVal;
        if (LV.Selected.Caption=GetLangString('Prefix')) then
          Group.Prefix:=NewVal;
        if (LV.Selected.Caption=GetLangString('Suffix')) then
          Group.Suffix:=NewVal;
      end;
    finally
      myForm.Release;
      FreeAndNil(myForm);
    end;
    exit;
  end;

  i:=Group.PropertiesSet.IndexOfName(Lv.Selected.Caption);
  if (i>-1) then
    ShowInputBox:=(Integer(Group.PropertiesSet.Objects[i]) and PROP_FLAGS_SHOW_INPUT_DLG) = PROP_FLAGS_SHOW_INPUT_DLG
  else
    ShowInputBox:=false;

  try
    case TTypeKind(Group.PropertiesAvailable.Objects[Group.PropertiesAvailable.IndexOfName(Lv.Selected.Caption)]) of
      tkEnumeration:
        myForm:=TFormCNAPropEdEnum.Create(nil);
      tkSet:
        myForm:=TFormCNAPropEdSet.Create(nil);
      tkInteger:
      begin
        myForm:=TFormCNAPropEdInteger.Create(nil);
        myForm.Tag:=32;
      end;
      tkInt64:
      begin
        myForm:=TFormCNAPropEdInteger.Create(nil);
        myForm.Tag:=64;
      end;
      tkString, tkLString, tkWString:
        myForm:=TFormCNAPropEdString.Create(nil);
      tkFloat:
        myForm:=TFormCNAPropEdFloat.Create(nil);
      tkChar:
      begin
        myForm:=TFormCNAPropEdString.Create(nil);
        TFormCNAPropEdString(myForm).edNewValue.MaxLength:=1;
      end;
      else
      begin
        raise Exception.Create(GetLangString('E_NotSupported')+' (TypeKind='+IntToStr(Integer(Group.PropertiesAvailable.Objects[Group.PropertiesAvailable.IndexOfName(Lv.Selected.Caption)]))+')');
        exit;
      end;
    end;

    if Group.PropertiesSet.IndexOfName(Lv.Selected.Caption)<0 then
      NewVal:='<!--NILORNULL-->'
    else
      NewVal:=Group.PropertiesSet.Values[Lv.Selected.Caption];

    MyForm.GetNewValue(Group.PropertiesSet.Values[Lv.Selected.Caption],
                       ' '+Group.GroupName+'.'+Group.PropertiesAvailable.Names[Group.PropertiesAvailable.IndexOfName(Lv.Selected.Caption)],
                       Group.PropertiesAvailable.Values[Group.PropertiesAvailable.Names[Group.PropertiesAvailable.IndexOfName(Lv.Selected.Caption)]],
                       false,
                       NewVal,
                       ShowInputBox);

    i:=Group.PropertiesSet.IndexOfName(Lv.Selected.Caption);

    if (NewVal='<!--NILORNULL-->') then
    begin
      LV.Selected.SubItems[0]:='';
      LV.Selected.SubItems[1]:=GetLangString('PropertyNotSet');
      if (i>-1) then
        Group.PropertiesSet.Delete(i);
    end
    else if (NewVal<>'<!--CANCELOFDLG-->') then
    begin
      Group.PropertiesSet.Values[Lv.Selected.Caption]:=NewVal;
      i:=Group.PropertiesSet.IndexOfName(Lv.Selected.Caption);
      if (i=-1) then
        i:=Group.PropertiesSet.Add(Lv.Selected.Caption+'=');
      if (ShowInputBox=True) then
      begin
        Group.PropertiesSet.Objects[i]:=TObject(Integer(Group.PropertiesSet.Objects[i]) or PROP_FLAGS_SHOW_INPUT_DLG);
        LV.Selected.SubItems[0]:='a';
      end
      else
      begin
        LV.Selected.SubItems[0]:='';
        Group.PropertiesSet.Objects[i]:=TObject(Integer(Group.PropertiesSet.Objects[i]) and 0);
      end;
      LV.Selected.SubItems[1]:=PropValueToString(Group,i);
    end;

  finally
    if (Assigned(myForm)) then
      myForm.Release; 
    FreeAndNil(myForm);
  end;
end;

{.$endregion}

end.
