{-----------------------------------------------------------------------------
 Project: GroupListView_D100R
 Purpose: Wraps the Windows functions for groups in listviews
 Created: 22.05.2008 14:37:20
 
 (c) by TheUnknownOnes under dwywbdbu license - see http://theunknownones.googlecode.com/svn/ for the license
 see http://www.TheUnknownOnes.net
-----------------------------------------------------------------------------}

unit uGroupListView;

interface

uses
  Classes, Controls, Windows, ComCtrls, CommCtrl, SysUtils,  uGroupListViewHeader,
  Graphics, Messages;

type
  TListGroup = class;
  TListGroups = class;
  TCustomGroupListView = class;
  TGroupListView = class;
  TGroupMetrics = class;

  TListGroupAlign = (lgaLeft, lgaCenter, lgaRight);

  TListGroupState = (lgsNormal, lgsCollapsed, lgsHidden);

  TListGroupExOption = (lgeLABELTIP,
                        lgeBORDERSELECT,
                        lgeDOUBLEBUFFER,
                        lgeHIDELABELS,
                        lgeSINGLEROW,
                        lgeSNAPTOGRID,
                        lgeSIMPLESELECT,
                        lgeJUSTIFYCOLUMNS,
                        lgeTRANSPARENTBKGND,
                        lgeTRANSPARENTSHADOWTEXT,
                        lgeAUTOAUTOARRANGE,
                        lgeHEADERINALLVIEWS,
                        lgeAUTOCHECKSELECT,
                        lgeAUTOSIZECOLUMNS,
                        lgeCOLUMNSNAPPOINTS);
                        
  TListGroupExOptions = set of TListGroupExOption;

  TListGroup = class(TCollectionItem)
  private
    FGroupID : Integer;
    FHeader: WideString;
    FFooter: WideString;
    FHeaderAlign: TListGroupAlign;
    FFooterAlign: TListGroupAlign;
    FState: TListGroupState;
    FExtendedImage: Integer;
    FDescriptionBottom: WideString;
    FSubtitle: WideString;
    FDescriptionTop: WideString;
    FSubsetTitle: WideString;
    FTitleImage: Integer;
    FTask: WideString;
    procedure SetState(const Value: TListGroupState);
    procedure SetFooterAlign(const Value: TListGroupAlign);
    procedure SetHeaderAlign(const Value: TListGroupAlign);
    procedure SetFooter(const Value: WideString);
    procedure SetHeader(const Value: WideString);

    function GetOwnerListviewHandle : HWND;
    function SetGroupInfo(AGroupInfo : PLVGroup) : Boolean;
    function IDInUse(AID : Integer) : Boolean;
    function Designing() : Boolean;
    procedure SetDescriptionBottom(const Value: WideString);
    procedure SetDescriptionTop(const Value: WideString);
    procedure SetExtendedImage(const Value: Integer);
    procedure SetSubsetTitle(const Value: WideString);
    procedure SetSubtitle(const Value: WideString);
    procedure SetTask(const Value: WideString);
    procedure SetTitleImage(const Value: Integer);
  protected
    procedure SetIndex(Value: Integer); override;
  public
    constructor Create(ACollection : TCollection); override;
    procedure Assign(Source: TPersistent); override;
    function GetDisplayName():String; override;
  published
    property Header : WideString read FHeader write SetHeader;
    property Footer : WideString read FFooter write SetFooter;
    property Subtitle : WideString read FSubtitle write SetSubtitle;
    property Task : WideString read FTask write SetTask;
    property DescriptionTop : WideString read FDescriptionTop write SetDescriptionTop;
    property DescriptionBottom : WideString read FDescriptionBottom write SetDescriptionBottom;
    property TitleImage : Integer read FTitleImage write SetTitleImage default -1;
    property ExtendedImage : Integer read FExtendedImage write SetExtendedImage default -1;
    property SubsetTitle : WideString read FSubsetTitle write SetSubsetTitle;
    property GroupID : Integer read FGroupID;
    property HeaderAlign : TListGroupAlign read FHeaderAlign write SetHeaderAlign default lgaLeft;
    property FooterAlign : TListGroupAlign read FFooterAlign write SetFooterAlign default lgaLeft;
    property State : TListGroupState read FState write SetState default lgsNormal;

    procedure AddItem(AItemIndex : Integer);
  end;

  TListGroups = class(TCollection)
  private
    FOwner : TCustomGroupListView;
    function GetItem(Index: Integer): TListGroup;
    procedure SetItem(Index: Integer; const Value: TListGroup);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner : TCustomGroupListView);
    function Add: TListGroup;
    function AddItem(Item: TListGroup; Index: Integer): TListGroup;
    function Insert(Index: Integer): TListGroup;
    property Items[Index: Integer]: TListGroup read GetItem write SetItem; default;
  end;

  TGroupMetrics = class(TPersistent)
  private
    FOwner : TCustomGroupListView;
    FBorders : array[0..3] of Cardinal;
    FColors : array[0..5] of COLORREF;
    function GetColor(const Index: Integer): TColor;
    procedure SetColor(const Index: Integer; const Value: TColor);
    function GetBorder(const Index: Integer): Cardinal;
    procedure SetBorder(const Index: Integer; const Value: Cardinal);
  public
    constructor Create(AOwner : TCustomGroupListView);
    procedure Assign(Source: TPersistent); override;
  published
    property BorderLeft : Cardinal index 0 read GetBorder write SetBorder default 0;
    property BorderRight : Cardinal index 1 read GetBorder write SetBorder default 0;
    property BorderTop : Cardinal index 2 read GetBorder write SetBorder default 0;
    property BorderBottom : Cardinal index 3 read GetBorder write SetBorder default 0;
    property ColorBorderLeft : TColor index 0 read GetColor write SetColor default clNone;
    property ColorBorderRight : TColor index 1 read GetColor write SetColor default clNone;
    property ColorBorderTop : TColor index 2 read GetColor write SetColor default clNone;
    property ColorBorderBottom : TColor index 3 read GetColor write SetColor default clNone;
    property ColorHeader : TColor index 4 read GetColor write SetColor default clNone;
    property ColorFooter : TColor index 5 read GetColor write SetColor default clNone;
  end;

  TCustomGroupListView = class(TCustomListView)
  private
    FGroups: TListGroups;
    FGroupsEnabled: Boolean;
    FGroupMetrics: TGroupMetrics;
    FExOptions: TListGroupExOptions;
    procedure SetGroupMetrics(const Value: TGroupMetrics);
    procedure SetGroupsEnabled(const Value: Boolean);
    procedure SetGroups(const Value: TListGroups);
    procedure SetExOptions(const Value: TListGroupExOptions);
  protected
    procedure WM_SetExtendedListviewStyle(var AMessage : TMessage); message LVM_SETEXTENDEDLISTVIEWSTYLE;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    property Groups : TListGroups read FGroups write SetGroups;
    property GroupsEnabled : Boolean read FGroupsEnabled write SetGroupsEnabled default false;
    property GroupMetrics : TGroupMetrics read FGroupMetrics write SetGroupMetrics;

    property ExOptions : TListGroupExOptions read FExOptions write SetExOptions default [];
  end;

  TGroupListView = class(TCustomGroupListView)

  published
    {$REGION 'All of TListView'}
    property Action;
    property Align;
    property AllocBy;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind default bkNone;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property BorderWidth;
    property Checkboxes;
    property Color;
    property Columns;
    property ColumnClick;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property FlatScrollBars;
    property FullDrag;
    property GridLines;
    property HideSelection;
    property HotTrack;
    property HotTrackStyles;
    property HoverTime;
    property IconOptions;
    property Items;
    property LargeImages;
    property MultiSelect;
    property OwnerData;
    property OwnerDraw;
    property ReadOnly default False;
    property RowSelect;
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowColumnHeaders;
    property ShowWorkAreas;
    property ShowHint;
    property SmallImages;
    property SortType;
    property StateImages;
    property TabOrder;
    property TabStop default True;
    property ViewStyle;
    property Visible;
    property OnAdvancedCustomDraw;
    property OnAdvancedCustomDrawItem;
    property OnAdvancedCustomDrawSubItem;
    property OnChange;
    property OnChanging;
    property OnClick;
    property OnColumnClick;
    property OnColumnDragged;
    property OnColumnRightClick;
    property OnCompare;
    property OnContextPopup;
    property OnCustomDraw;
    property OnCustomDrawItem;
    property OnCustomDrawSubItem;
    property OnData;
    property OnDataFind;
    property OnDataHint;
    property OnDataStateChange;
    property OnDblClick;
    property OnDeletion;
    property OnDrawItem;
    property OnEdited;
    property OnEditing;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetImageIndex;
    property OnGetSubItemImage;
    property OnDragDrop;
    property OnDragOver;
    property OnInfoTip;
    property OnInsert;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnSelectItem;
    property OnStartDock;
    property OnStartDrag;
    {$ENDREGION}

    property Groups;
    property GroupsEnabled;
    property GroupMetrics;
    property ExOptions;
  end;

implementation


{ TListGroup }

procedure TListGroup.AddItem(AItemIndex: Integer);
var
  Item : PLVItem;
begin
  //ListView_MoveItemToGroup(GetOwnerListviewHandle,AItemIndex,GroupID); //doesnt work :(
  Item:=GetNewLVItem;
  Item^.mask:=LVIF_GROUPID;
  Item^.iGroupId:=GroupID;
  Item^.iItem:=AItemIndex;
  SendMessage(GetOwnerListviewHandle,LVM_SETITEMA,0,LParam(Item));
  Dispose(Item);
  Changed(true);
end;

procedure TListGroup.Assign(Source: TPersistent);
begin
  if Source is TListGroup then
  begin
    Header:=TListGroup(Source).Header;
    Footer:=TListGroup(Source).Footer;
    HeaderAlign:=TListGroup(Source).HeaderAlign;
    FooterAlign:=TListGroup(Source).FooterAlign;
    State:=TListGroup(Source).State;
  end
  else
    inherited;
end;

constructor TListGroup.Create(ACollection: TCollection);
var
  Group : PLVGroup;
  NewGroupID : Integer;
begin
  inherited;

  FHeaderAlign:=lgaLeft;
  FFooterAlign:=lgaLeft;
  FState:=lgsNormal;
  FTitleImage:=-1;
  FExtendedImage:=-1;

  FGroupID:=-1;

  NewGroupID:=0;
  while IDInUse(NewGroupID) do
    Inc(NewGroupID);
  FGroupID:=NewGroupID;

  Group:=GetNewLVGroup;
  Group^.mask:=LVGF_GROUPID;
  Group^.iGroupId:=GroupID;

  try
    if ListView_InsertGroup(GetOwnerListviewHandle,-1,Group)=-1 then
      raise Exception.Create('Can not create group');
  finally
    Dispose(Group);
  end;
end;

function TListGroup.Designing: Boolean;
begin
  Result:=csDesigning in TCustomGroupListView(Collection.Owner).ComponentState;
end;

function TListGroup.GetDisplayName: String;
begin
  Result:=FHeader;
end;

function TListGroup.GetOwnerListviewHandle: HWND;
begin
  Result:=TListView(Collection.Owner).Handle;
end;

function TListGroup.IDInUse(AID: Integer): Boolean;
var
  idx : Integer;
begin
  Result:=false;

  if Designing then
  begin
   for idx := 0 to Collection.Count - 1 do
   begin
     if TListGroup(Collection.Items[idx]).GroupID=AID then
     begin
       Result:=true;
       break;
     end;     
   end;
  end
  else
  begin
    Result:=ListView_HasGroup(GetOwnerListviewHandle,AID);
  end;
end;

procedure TListGroup.SetDescriptionBottom(const Value: WideString);
var
  Group : PLVGroup;
begin
  if Value<>FDescriptionBottom then
  begin
    Group:=GetNewLVGroup;
    Group^.mask:=LVGF_DESCRIPTIONBOTTOM;
    Group^.pszDescriptionBottom:=PWidechar(Value);
    Group^.cchDescriptionBottom:=Length(Value);

    if SetGroupInfo(Group) then
    begin
      FDescriptionBottom := Value;
      Changed(false);
    end;

    Dispose(Group);
  end;
end;

procedure TListGroup.SetDescriptionTop(const Value: WideString);
var
  Group : PLVGroup;
begin
  if Value<>FDescriptionTop then
  begin
    Group:=GetNewLVGroup;
    Group^.mask:=LVGF_DESCRIPTIONTOP;
    Group^.pszDescriptionTop:=PWidechar(Value);
    Group^.cchDescriptionTop:=Length(Value);

    if SetGroupInfo(Group) then
    begin
      FDescriptionTop := Value;
      Changed(false);
    end;

    Dispose(Group);
  end;
end;

procedure TListGroup.SetExtendedImage(const Value: Integer);
var
  Group : PLVGroup;
begin
  if Value<>FExtendedImage then
  begin
    Group:=GetNewLVGroup;
    Group^.mask:=LVGF_EXTENDEDIMAGE;
    Group^.iExtendedImage:=Value;

    if SetGroupInfo(Group) then
    begin
      FExtendedImage := Value;
      Changed(false);
    end;

    Dispose(Group);
  end;
end;
procedure TListGroup.SetFooter(const Value: WideString);
var
  Group : PLVGroup;
begin
  if Value<>FFooter then
  begin
    Group:=GetNewLVGroup;
    Group^.mask:=LVGF_FOOTER;
    Group^.pszFooter:=PWidechar(Value);
    Group^.cchFooter:=Length(Value);

    if SetGroupInfo(Group) then
    begin
      FFooter := Value;
      Changed(false);
    end;

    Dispose(Group);
  end;
end;

procedure TListGroup.SetFooterAlign(const Value: TListGroupAlign);
var
  Group : PLVGroup;
begin
  if Value<>FFooterAlign then
  begin
    Group:=GetNewLVGroup;
    Group^.mask:=LVGF_ALIGN;
    case Value of
      lgaLeft:
        Group^.uAlign:=LVGA_FOOTER_LEFT;
      lgaCenter:
        Group^.uAlign:=LVGA_FOOTER_CENTER;
      lgaRight:
        Group^.uAlign:=LVGA_FOOTER_RIGHT;
    end;

    if SetGroupInfo(Group) then
    begin
      FFooterAlign := Value;
      Changed(false);
    end;

    Dispose(Group);
  end;
end;

function TListGroup.SetGroupInfo(AGroupInfo: PLVGroup): Boolean;
const UniqueID = MaxInt-1;
begin
  //because of a bug in the implementation of the listview,
  //we have to change the GroupID twice
  AGroupInfo^.mask:=AGroupInfo^.mask or LVGF_GROUPID;
  AGroupInfo^.iGroupId:=UniqueID;
  ListView_SetGroupInfo(GetOwnerListviewHandle,GroupID,AGroupInfo);
  AGroupInfo^.iGroupId:=GroupID;
  Result:=ListView_SetGroupInfo(GetOwnerListviewHandle,UniqueID,AGroupInfo)<>-1;
end;

procedure TListGroup.SetHeader(const Value: WideString);
var
  Group : PLVGroup;
begin
  if Value<>FHeader then
  begin
    Group:=GetNewLVGroup;
    Group^.mask:=LVGF_HEADER;
    Group^.pszHeader:=PWideChar(Value);
    Group^.cchHeader:=Length(Value);

    if SetGroupInfo(Group) then
    begin
      FHeader := Value;
      Changed(false);
    end;

    Dispose(Group);
  end;
end;

procedure TListGroup.SetHeaderAlign(const Value: TListGroupAlign);
var
  Group : PLVGroup;
begin
  if Value<>FHeaderAlign then
  begin
    Group:=GetNewLVGroup;
    Group^.mask:=LVGF_ALIGN;
    case Value of
      lgaLeft:
        Group^.uAlign:=LVGA_HEADER_LEFT;
      lgaCenter:
        Group^.uAlign:=LVGA_HEADER_CENTER;
      lgaRight:
        Group^.uAlign:=LVGA_HEADER_RIGHT;
    end;

    if SetGroupInfo(Group) then
    begin
      FHeaderAlign := Value;
      Changed(false);
    end;

    Dispose(Group);
  end;
end;

procedure TListGroup.SetIndex(Value: Integer);
var
  RetValue : Integer;
begin
  //todo: make this working
  RetValue:=ListView_MoveGroup(GetOwnerListviewHandle,GroupID,Value);
  if (RetValue=Value) or ((Value=Collection.Count-1) and (RetValue=-1)) then
    inherited;
end;

procedure TListGroup.SetState(const Value: TListGroupState);
var
  Group : PLVGroup;
begin
  if Value<>FState then
  begin
    Group:=GetNewLVGroup;
    Group^.mask:=LVGF_STATE;
    case Value of
      lgsNormal:
        Group^.state:=LVGS_NORMAL;
      lgsCollapsed:
        Group^.state:=LVGS_COLLAPSED;
      lgsHidden:
        Group^.state:=LVGS_HIDDEN;
    end;


    if SetGroupInfo(Group) then
    begin
      FState := Value;
      Changed(false);
    end;

    Dispose(Group);
  end;
end;

procedure TListGroup.SetSubsetTitle(const Value: WideString);
var
  Group : PLVGroup;
begin
  if Value<>FSubsetTitle then
  begin
    Group:=GetNewLVGroup;
    Group^.mask:=LVGF_SUBSET;
    Group^.pszSubsetTitle:=PWidechar(Value);
    Group^.cchSubsetTitle:=Length(Value);

    if SetGroupInfo(Group) then
    begin
      FSubsetTitle := Value;
      Changed(false);
    end;

    Dispose(Group);
  end;
end;

procedure TListGroup.SetSubtitle(const Value: WideString);
var
  Group : PLVGroup;
begin
  if Value<>FSubtitle then
  begin
    Group:=GetNewLVGroup;
    Group^.mask:=LVGF_SUBTITLE;
    Group^.pszSubtitle:=PWidechar(Value);
    Group^.cchSubtitle:=Length(Value);

    if SetGroupInfo(Group) then
    begin
      FSubtitle := Value;
      Changed(false);
    end;

    Dispose(Group);
  end;
end;

procedure TListGroup.SetTask(const Value: WideString);
var
  Group : PLVGroup;
begin
  if Value<>FTask then
  begin
    Group:=GetNewLVGroup;
    Group^.mask:=LVGF_TASK;
    Group^.pszTask:=PWidechar(Value);
    Group^.cchTask:=Length(Value);

    if SetGroupInfo(Group) then
    begin
      FTask := Value;
      Changed(false);
    end;

    Dispose(Group);
  end;
end;

procedure TListGroup.SetTitleImage(const Value: Integer);
var
  Group : PLVGroup;
begin
  if Value<>FTitleImage then
  begin
    Group:=GetNewLVGroup;
    Group^.mask:=LVGF_TITLEIMAGE;
    Group^.iTitleImage:=Value;

    if SetGroupInfo(Group) then
    begin
      FTitleImage := Value;
      Changed(false);
    end;

    Dispose(Group);
  end;
end;

{ TCustomGroupListView }

constructor TCustomGroupListView.Create(AOwner: TComponent);
begin
  inherited;
  FGroups:=TListGroups.Create(Self);
  FGroupsEnabled:=false;
  FGroupMetrics:=TGroupMetrics.Create(Self);
end;

destructor TCustomGroupListView.Destroy;
begin
  FGroups.Free;
  FGroupMetrics.Free;

  inherited;
end;

procedure TCustomGroupListView.SetExOptions(
  const Value: TListGroupExOptions);
begin
  FExOptions:=Value;
  RowSelect:=not RowSelect;
  RowSelect:=not RowSelect;
end;

procedure TCustomGroupListView.SetGroupMetrics(const Value: TGroupMetrics);
begin
  FGroupMetrics.Assign(Value);
end;

procedure TCustomGroupListView.SetGroups(const Value: TListGroups);
begin
  FGroups.Assign(Value);
end;

procedure TCustomGroupListView.SetGroupsEnabled(const Value: Boolean);
begin
  if ListView_EnableGroupView(Handle, Value)<>-1 then
    FGroupsEnabled := Value;
end;

procedure TCustomGroupListView.WM_SetExtendedListviewStyle(
  var AMessage: TMessage);
var
  Opts : Cardinal;
begin
  Opts:=AMessage.LParam;

  if lgeLABELTIP in FExOptions then
    Opts:=Opts or LVS_EX_LABELTIP;

  if lgeBORDERSELECT in FExOptions then
    Opts:=Opts or LVS_EX_BORDERSELECT;

  if lgeDOUBLEBUFFER in FExOptions then
    Opts:=Opts or LVS_EX_DOUBLEBUFFER;

  if lgeHIDELABELS in FExOptions then
    Opts:=Opts or LVS_EX_HIDELABELS;

  if lgeSINGLEROW in FExOptions then
    Opts:=Opts or LVS_EX_SINGLEROW;

  if lgeSNAPTOGRID in FExOptions then
    Opts:=Opts or LVS_EX_SNAPTOGRID;

  if lgeSIMPLESELECT in FExOptions then
    Opts:=Opts or LVS_EX_SIMPLESELECT;

  if lgeJUSTIFYCOLUMNS in FExOptions then
    Opts:=Opts or LVS_EX_JUSTIFYCOLUMNS;

  if lgeTRANSPARENTBKGND in FExOptions then
    Opts:=Opts or LVS_EX_TRANSPARENTBKGND;

  if lgeTRANSPARENTSHADOWTEXT in FExOptions then
    Opts:=Opts or LVS_EX_TRANSPARENTSHADOWTEXT;

  if lgeAUTOAUTOARRANGE in FExOptions then
    Opts:=Opts or LVS_EX_AUTOAUTOARRANGE;

  if lgeHEADERINALLVIEWS in FExOptions then
    Opts:=Opts or LVS_EX_HEADERINALLVIEWS;

  if lgeAUTOCHECKSELECT in FExOptions then
    Opts:=Opts or LVS_EX_AUTOCHECKSELECT;

  if lgeAUTOSIZECOLUMNS in FExOptions then
    Opts:=Opts or LVS_EX_AUTOSIZECOLUMNS;

  if lgeCOLUMNSNAPPOINTS in FExOptions then
    Opts:=Opts or LVS_EX_COLUMNSNAPPOINTS;

  AMessage.LParam:=Opts;

  inherited;
end;

{ TListGroups }

function TListGroups.Add: TListGroup;
begin
  Result:=TListGroup(inherited Add);
end;

function TListGroups.AddItem(Item: TListGroup; Index: Integer): TListGroup;
begin
  if Item = nil then
    Result:=TListGroup.Create(Self)
  else
    Result := Item;
  if Assigned(Result) then
  begin
    if Index < 0 then
      Index := Count - 1;
    Result.Index := Index;
  end;
end;

constructor TListGroups.Create(AOwner: TCustomGroupListView);
begin
  inherited Create(TListGroup);
  FOwner:=AOwner;
end;

function TListGroups.GetItem(Index: Integer): TListGroup;
begin
  Result := TListGroup(inherited GetItem(Index));
end;

function TListGroups.GetOwner: TPersistent;
begin
  Result:=FOwner;
end;

function TListGroups.Insert(Index: Integer): TListGroup;
begin
  Result:=AddItem(nil,Index);
end;

procedure TListGroups.SetItem(Index: Integer; const Value: TListGroup);
begin
  inherited SetItem(Index, Value);
end;

procedure TListGroups.Update(Item: TCollectionItem);
begin
  //FOwner.Invalidate;
end;

{ TGroupMetrics }

procedure TGroupMetrics.Assign(Source: TPersistent);
begin
  if Source is TGroupMetrics then
  begin
    BorderLeft:=TGroupMetrics(Source).BorderLeft;
    BorderRight:=TGroupMetrics(Source).BorderRight;
    BorderTop:=TGroupMetrics(Source).BorderTop;
    BorderBottom:=TGroupMetrics(Source).BorderBottom;

    {FColors[0]:=TGroupMetrics(source).ColorBorderLeft;
    FColors[1]:=TGroupMetrics(source).ColorBorderRight;
    FColors[2]:=TGroupMetrics(source).ColorBorderTop;
    FColors[3]:=TGroupMetrics(source).ColorBorderBottom;}

    ColorHeader:=TGroupMetrics(Source).ColorHeader;
    ColorHeader:=TGroupMetrics(Source).ColorFooter;
  end
  else
    inherited;

end;

constructor TGroupMetrics.Create(AOwner : TCustomGroupListView);
begin
  FOwner:=AOwner;
  
  FBorders[0]:=0;
  FBorders[1]:=0;
  FBorders[2]:=0;
  FBorders[3]:=0;

  FColors[0]:=clNone;
  FColors[1]:=clNone;
  FColors[2]:=clNone;
  FColors[3]:=clNone;
  FColors[4]:=clNone;
  FColors[5]:=clNone;
end;

function TGroupMetrics.GetBorder(const Index: Integer): Cardinal;
begin
  Result:=FBorders[Index];
end;

function TGroupMetrics.GetColor(const Index: Integer): TColor;
begin
  Result:=FColors[Index];
end;

procedure TGroupMetrics.SetBorder(const Index: Integer; const Value: Cardinal);
var
  GroupMetrics : PLVGroupMetrics;
begin
  FBorders[Index]:=Value;

  GroupMetrics:=GetNewLVGroupMetrics;
  GroupMetrics^.mask:=LVGMF_BORDERSIZE;
  GroupMetrics^.Left:=FBorders[0];
  GroupMetrics^.Right:=FBorders[1];
  GroupMetrics^.Top:=FBorders[2];
  GroupMetrics^.Bottom:=FBorders[3];

  ListView_SetGroupMetrics(FOwner.Handle,GroupMetrics);

  Dispose(GroupMetrics);
end;

procedure TGroupMetrics.SetColor(const Index: Integer; const Value: TColor);
var
  GroupMetrics : PLVGroupMetrics;
begin
  FColors[Index]:=ColorToRGB(Value);

  GroupMetrics:=GetNewLVGroupMetrics;
  if Index in [0..3] then
  begin
    GroupMetrics^.mask:=LVGMF_BORDERSIZE;
    GroupMetrics^.crLeft:=FColors[0];
    GroupMetrics^.crRight:=FColors[1];
    GroupMetrics^.crTop:=FColors[2];
    GroupMetrics^.crBottom:=FColors[3];
  end
  else
  begin
    GroupMetrics^.mask:=LVGMF_TEXTCOLOR;
    GroupMetrics^.crHeader:=FColors[4];
    GroupMetrics^.crFooter:=FColors[5];
  end;

  ListView_SetGroupMetrics(FOwner.Handle,GroupMetrics);

  Dispose(GroupMetrics);
end;

end.
