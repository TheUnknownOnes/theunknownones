//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit uVirtualActionTree;

interface

uses
  SysUtils, Classes, Controls, VirtualTrees, ActnList, dialogs, gdipapi, gdipobj,
  gdiputil, Graphics, CommCtrl, Windows, Messages;

type
  TVirtualActionTree = class;

  TActionListsItem = class(TCollectionItem)
  private
    FActionList: TActionList;
    FDefaultCategory: String;
    procedure SetActionList(const Value: TActionList);
    procedure SetDefaultCategory(const Value: String);
  published
    destructor Destroy; override;
    property ActionList : TActionList read FActionList write SetActionList;
    property DefaultCategory : String read FDefaultCategory write SetDefaultCategory;
  end;

  TActionLists = class(TCollection)
  private
    FOwner : TVirtualActionTree;
    function GetItem(Index: Integer): TActionListsItem;
    procedure SetItem(Index: Integer; const Value: TActionListsItem);
  protected
    function GetOwner: TPersistent; override;
    procedure UnregisterActionList(AActionList : TActionList);
    procedure RegisterActionList(AActionList : TActionList; DefCategory: String);
  public
    constructor Create(AOwner : TVirtualActionTree);
    function Add: TActionListsItem;
    function AddItem(Item: TActionListsItem; Index: Integer): TActionListsItem;
    function Insert(Index: Integer): TActionListsItem;
    property Items[Index: Integer]: TActionListsItem read GetItem write SetItem; default;
  end;

  TVirtualActionTreeColors = class(TPersistent)
  private
    FHeaderGradientStart : TGPColor;
    FHeaderGradientEnd : TGPColor;
    FHottrackGradientStart : TGPColor;
    FHottrackGradientEnd : TGPColor;
    FHottrackFrame : TGPColor;
    FPressedGradientStart: TGPColor;
    FPressedFrame: TGPColor;
    FPressedGradientEnd: TGPColor;
  public
    procedure Assign(Source: TPersistent); override;
  published
    constructor Create; reintroduce;

    property HeaderGradientStart : TGPColor read FHeaderGradientStart write FHeaderGradientStart;
    property HeaderGradientEnd : TGPColor read FHeaderGradientEnd write FHeaderGradientEnd;
    property HottrackGradientStart : TGPColor read FHottrackGradientStart write FHottrackGradientStart;
    property HottrackGradientEnd : TGPColor read FHottrackGradientEnd write FHottrackGradientEnd;
    property HottrackFrame : TGPColor  read FHottrackFrame write FHottrackFrame;
    property PressedGradientStart : TGPColor read FPressedGradientStart write FPressedGradientStart;
    property PressedGradientEnd : TGPColor read FPressedGradientEnd write FPressedGradientEnd;
    property PressedFrame : TGPColor  read FPressedFrame write FPressedFrame;
  end;

  TVirtualActionTreeNodeType = (vatntNoNode, vatntAction, vatntActionGroup);

  TVirtualActionTree = class(TCustomVirtualDrawTree)
  private
    FItemIndent : Integer;
    FActionLists: TActionLists;
    FActionLinks: TList;
    FColors: TVirtualActionTreeColors;
    FLeftMousePressed : Boolean;
    function GetHint(Node: PVirtualNode): String;
    procedure SetColors(const Value: TVirtualActionTreeColors);
  protected
    function GetActionGroup(const ACaption : String; ACanCreate: Boolean): PVirtualNode;
    function GetNodeFromAction(const AAction: TBasicAction): PVirtualNode;
    function GetActionFromNode(const ANode: pVirtualNode; var Action: TBasicAction): TVirtualActionTreeNodeType;

    procedure InternalDrawHint(Sender: TBaseVirtualTree; HintCanvas: TCanvas; Node: PVirtualNode; R: TRect;
                               Column: TColumnIndex);

    procedure AddAction(const AAction: TBasicAction; const ACategory: String);
    procedure RemoveAction(const AAction: TBasicAction);
    procedure AddActionList(const AActionList : TActionList; const ADefCategory: String);
    procedure RemoveActionList(const AActionList : TActionList);
    
    {$REGION 'overridden'}
    procedure DoFreeNode(Node: PVirtualNode); override;
    procedure DoPaintNode(var PaintInfo: TVTPaintInfo); override;
    function DoCollapsing(Node: PVirtualNode): Boolean; override;
    function GetOptionsClass: TTreeOptionsClass; override;
    procedure DoGetHintSize(Node: PVirtualNode; Column: TColumnIndex; var R: TRect); override;

    procedure Click; override;
    procedure HandleMouseDown(var Message: TWMMouse; var HitInfo: THitInfo); override;
    procedure HandleMouseUp(var Message: TWMMouse; const HitInfo: THitInfo); override;
    function DoKeyAction(var CharCode: Word; var Shift: TShiftState): Boolean; override;
    procedure DoExit; override;
    {$ENDREGION}
  public
    procedure AfterConstruction; override;
    procedure SetHotAction(const AAction : TBasicAction);
  published
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property ActionLists : TActionLists read FActionLists write FActionLists;
    property DefaultNodeHeight;
    property Color;
    property OnDrawHint;
    property ShowHint;
    property ParentShowHint;
    property TreeOptions;
    property Align;
    property Font;
    property Ctl3D;
    property Colors : TVirtualActionTreeColors read FColors write SetColors;
    property TabStop;
    property TabOrder;
  end;

implementation

uses Types, ImgList;

type
  TVirtualActionGroup = class
  private
    FCaption: String;
  public
    property Caption : String read FCaption write FCaption;
  end;

  TVirtualActionLink = class(TActionLink)
  private
    FNode: PVirtualNode;
  protected
    procedure SetAutoCheck(Value: Boolean); override;
    procedure SetCaption(const Value: string); override;
    procedure SetChecked(Value: Boolean); override;
    procedure SetEnabled(Value: Boolean); override;
    procedure SetGroupIndex(Value: Integer); override;
    procedure SetHelpContext(Value: THelpContext); override;
    procedure SetHelpKeyword(const Value: string); override;
    procedure SetHelpType(Value: THelpType); override;
    procedure SetHint(const Value: string); override;
    procedure SetImageIndex(Value: Integer); override;
    procedure SetShortCut(Value: TShortCut); override;
    procedure SetVisible(Value: Boolean); override;
  public
    constructor Create(ANode: PVirtualNode); reintroduce;
    property Node : PVirtualNode read FNode write FNode;
  end;


{ TVirtualActionTree }

procedure TVirtualActionTree.AddAction(const AAction: TBasicAction; const ACategory: String);
var
  ActionLink : TVirtualActionLink;
  Node : PVirtualNode;
begin
  if not Assigned(GetNodeFromAction(AAction)) then
  begin
    ActionLink:=TVirtualActionLink.Create(Nil);
    Node:=Self.AddChild(GetActionGroup(ACategory, true), ActionLink);
    ActionLink.Node:=Node;
    AAction.RegisterChanges(ActionLink);
    Self.Expanded[Node.Parent]:=True;
    //DetermineDrawingDimensions;
  end;
end;

procedure TVirtualActionTree.AddActionList(const AActionList: TActionList; const ADefCategory: String);
var
  idx : integer;
  cat : String;
begin
  if not (csDesigning in Self.ComponentState) then
  begin
    for idx:=0 to AActionList.ActionCount-1 do
    begin
      cat:=AActionList.Actions[idx].Category;
      if cat=EmptyStr then
        cat:=ADefCategory;
      AddAction(AActionList.Actions[idx],cat);
    end;
  end;
end;

procedure TVirtualActionTree.AfterConstruction;
begin
  inherited;
  NodeDataSize:=SizeOf(TObject);
end;

procedure TVirtualActionTree.Click;
var
  mousePos : TPoint;
  Node     : PVirtualNode;
  NData    : TObject;
begin
  mousePos:=Mouse.CursorPos;
  mousePos:=Self.ScreenToClient(mousePos);

  Node:=Self.GetNodeAt(mousePos.X, mousePos.Y);
  if Assigned(Node) then
  begin
    NData:=TObject(self.GetNodeData(Node)^);
    if NData is TVirtualActionLink then
      TVirtualActionLink(NData).Execute(self);
  end;

  inherited;
end;

constructor TVirtualActionTree.Create(AOwner: TComponent);
var
  Opts : TVirtualTreeOptions;
begin
  inherited;
  FLeftMousePressed:=False;
  FColors:=TVirtualActionTreeColors.Create();
  FActionLists:=TActionLists.Create(self);
  FActionLinks:=TList.Create();
  Opts :=TVirtualTreeOptions.Create(self);
  Opts.PaintOptions:=[toThemeAware,toHotTrack];
  Opts.SelectionOptions:=[toFullRowSelect];
  Opts.AutoOptions:=[];
  Opts.AnimationOptions:=[];
  Self.TreeOptions.Assign(Opts);
  Opts.Free;
  HintMode:=hmHintAndDefault;
  OnDrawHint:=InternalDrawHint;
end;

destructor TVirtualActionTree.Destroy;
begin
  inherited;
  FActionLists.Free();
  while FActionLinks.Count>0 do
  begin
    TObject(FActionLists[0]).Free;
    FActionLists.Delete(0);
  end;
  FActionLinks.Free();
  FColors.Free();
end;

function TVirtualActionTree.DoCollapsing(Node: PVirtualNode): Boolean;
begin
  Result:=False;
end;

procedure TVirtualActionTree.DoExit;
begin
  inherited;
  SetHotAction(nil);
end;

procedure TVirtualActionTree.DoFreeNode(Node: PVirtualNode);
var
  NData : TObject;
begin
  inherited;
  NData:=TObject(Self.GetNodeData(Node)^);
  if Assigned(NData) then
    NData.Free;
end;

function TVirtualActionTree.GetHint(Node: PVirtualNode): String;
var
  NData : TObject;
begin
  NData:=TObject(self.GetNodeData(Node)^);
  if (NData is TVirtualActionLink) and (TVirtualActionLink(NData).Action is TCustomAction) then
  begin
    Result:=TCustomAction(TVirtualActionLink(NData).Action).Hint;

    if not TCustomAction(TVirtualActionLink(NData).Action).Enabled then
      Result:=Result+' (nicht aktiv)';
  end
  else
    Result:='';
end;

procedure TVirtualActionTree.DoGetHintSize(Node: PVirtualNode;
  Column: TColumnIndex; var R: TRect);
begin
  inherited;
  R.Left:=0;
  R.Top:=0;
  R.Right:=Canvas.TextWidth(GetHint(Node))+6;
  R.Bottom:=Canvas.TextHeight(GetHint(Node))+6;
end;

function TVirtualActionTree.DoKeyAction(var CharCode: Word;
  var Shift: TShiftState): Boolean;
var
  NData : TObject;
  Node : PVirtualNode;
  Action : TBasicAction;
begin
  if CharCode=VK_RETURN then
  begin
    if Assigned(HotNode) then
    begin
      NData:=TObject(self.GetNodeData(HotNode)^);
      if (NData is TVirtualActionLink) and (TVirtualActionLink(NData).Action is TCustomAction) then
      begin
        Result:=TCustomAction(TVirtualActionLink(NData).Action).Execute;
      end;
    end;
  end
  else
  if CharCode=VK_UP then
  begin
    if not Assigned(HotNode) then
      Node:=GetLast
    else
      Node := GetPrevious(HotNode);

    while (GetActionFromNode(Node, Action)=vatntActionGroup) or
          (Assigned(Action) and (not TAction(Action).Enabled)) do
    begin
      Node:=GetPrevious(Node);
    end;

    if Assigned(Action) then
      SetHotAction(Action);
  end
  else
  if CharCode=VK_DOWN then
  begin
    if not Assigned(HotNode) then
      Node:=GetFirst
    else
      Node := GetNext(HotNode);

    while (GetActionFromNode(Node, Action)=vatntActionGroup) or
          (Assigned(Action) and (not TAction(Action).Enabled)) do
    begin
      Node:=GetNext(Node);
    end;

    if Assigned(Action) then
      SetHotAction(Action);
  end
  else
    Result:=inherited DoKeyAction(CharCode, Shift);
end;

procedure TVirtualActionTree.DoPaintNode(var PaintInfo: TVTPaintInfo);
type
  TDrawRegion = (drHeader, drAction);
var
  graphics : TGPGraphics;
  background : TGPBrush;
  HotBackground : TGPBrush;
  HotFrame : TGPPen;
  DrawRegion : TDrawRegion;
  ItemRect : TGPRectF;
  TxtRect  : TGPRectF;
  NData : TObject;
  Font : TGPFont;
  FontBrush : TGPBrush;
  StrFormat : TGPStringFormat;
  txt : string;
  actEnabled : Boolean;
  icow, icoh: Integer;
  iml : TCustomImageList;
  imgidx : Integer;
  ds : TDrawingStyle;

  ColItem : Integer;

  procedure DrawRoundedRectangle(AGraphics : TGPGraphics; ARect: TGPRectF; AD: Integer; APen : TGPPen; ABrush : TGPBrush);
  var
    gp : TGPGraphicsPath;
  begin
    ARect.Height:=ARect.Height-1;

    gp:=TGPGraphicsPath.Create;
    gp.AddArc(ARect.X, ARect.Y, AD, AD, 180, 90);
    gp.AddArc(ARect.X+ARect.Width - AD, ARect.Y, AD, AD, 270, 90);
    gp.AddArc(ARect.X+ARect.Width-AD, ARect.Y+ARect.Height-AD, AD, AD, 0, 90);
    gp.AddArc(ARect.X, ARect.Y+ARect.Height-AD, AD, AD, 90, 90);
    gp.AddLine(ARect.X, ARect.Y+ARect.Height-AD, ARect.X, ARect.Y+AD/2);

    if Assigned(ABrush) then
      graphics.FillPath(ABrush, gp);

    if Assigned(APen) then
      graphics.DrawPath(APen,gp);
    gp.Free;
  end;

begin
  inherited;

  NData:=TObject(Self.GetNodeData(PaintInfo.Node)^);
  if NData is TVirtualActionGroup then
  begin
    txt:=TVirtualActionGroup(NData).Caption;
    DrawRegion:=drHeader;
    iml:=nil;
  end
  else
  if (NData is TVirtualActionLink) then
  begin
    DrawRegion:=drAction;
    if TVirtualActionLink(NData).Action is TCustomAction then
    begin
      if not TCustomAction(TVirtualActionLink(NData).Action).Visible then
      begin
        self.IsVisible[PaintInfo.Node]:=False;
        exit;
      end;

      txt:=TCustomAction(TVirtualActionLink(NData).Action).Caption;
      actEnabled:=TCustomAction(TVirtualActionLink(NData).Action).Enabled;

      if Assigned(TCustomAction(TVirtualActionLink(NData).Action).ActionList.Images) then
      begin
        icow:=TCustomAction(TVirtualActionLink(NData).Action).ActionList.Images.Width;
        if icow>FItemIndent then
          FItemIndent:=icow;
        icoh:=TCustomAction(TVirtualActionLink(NData).Action).ActionList.Images.Height;

        iml:=TCustomAction(TVirtualActionLink(NData).Action).ActionList.Images;
        imgidx:=TCustomAction(TVirtualActionLink(NData).Action).ImageIndex;
      end;
    end;
  end;

  ColItem:=MakeColor(GetRed(ColorToRGB(Color)),GetGreen(ColorToRGB(Color)),GetBlue(ColorToRGB(Color)));

  ItemRect:=MakeRect(1.0*PaintInfo.CellRect.Left,
                     PaintInfo.CellRect.Top,
                     PaintInfo.CellRect.Right-PaintInfo.CellRect.Left,
                     PaintInfo.CellRect.Bottom-PaintInfo.CellRect.Top);
  ItemRect.X:=ItemRect.X+4;
  ItemRect.Width:=ItemRect.Width-8;
  TxtRect:=ItemRect;

  PaintInfo.Canvas.Brush.Color:=Color;
  PaintInfo.Canvas.FillRect(PaintInfo.CellRect);

  graphics:=TGPGraphics.Create(PaintInfo.Canvas.Handle);
  graphics.SetSmoothingMode(SmoothingModeAntiAlias);

  case DrawRegion of
    drHeader: begin
                background:= TGPLinearGradientBrush.Create(ItemRect, Colors.HeaderGradientStart, Colors.HeaderGradientEnd, -90);
                Font:=TGPFont.Create(Self.Font.Name, Self.Font.Size, FontStyleBoldItalic);
                FontBrush:=TGPSolidBrush.Create(MakeColor(GetRed(ColorToRGB(self.Font.Color)),GetGreen(ColorToRGB(self.Font.Color)),GetBlue(ColorToRGB(self.Font.Color))));
                StrFormat:=TGPStringFormat.Create(0);
                StrFormat.SetLineAlignment(StringAlignmentCenter);
                TxtRect.X:=TxtRect.X+8;
                TxtRect.Width:=TxtRect.Width-8;
              end;
    drAction: begin
                background:= TGPSolidBrush.Create(COLITEM);
                Font:=TGPFont.Create(Self.Font.Name, Self.Font.Size);
                FontBrush:=TGPSolidBrush.Create(MakeColor(GetRed(ColorToRGB(self.Font.Color)),GetGreen(ColorToRGB(self.Font.Color)),GetBlue(ColorToRGB(self.Font.Color))));
                StrFormat:=TGPStringFormat.Create(0);
                StrFormat.SetLineAlignment(StringAlignmentCenter);
                TxtRect.X:=TxtRect.X+FItemIndent+8;
                TxtRect.Width:=TxtRect.Width-FItemIndent-8;
              end;
  end;

  DrawRoundedRectangle(Graphics, ItemRect, 10, nil, background);

  if (PaintInfo.Node=Hotnode) and (DrawRegion=drAction) and ActEnabled then
  begin
    if FLeftMousePressed then
    begin
      ds:=dsSelected;
      HotFrame:=TGPPen.Create(Colors.PressedFrame);
      HotBackground:=TGPLinearGradientBrush.Create(ItemRect,
                                                   Colors.PressedGradientStart,
                                                   Colors.PressedGradientEnd,
                                                   -90);
    end
    else
    begin
      ds:=dsFocus;
      HotFrame:=TGPPen.Create(Colors.HottrackFrame);
      HotBackground:=TGPLinearGradientBrush.Create(ItemRect,
                                                   Colors.HottrackGradientStart,
                                                   Colors.HottrackGradientEnd,
                                                   -90);
    end;
    DrawRoundedRectangle(Graphics, ItemRect, 10, HotFrame, Hotbackground);
    HotBackground.Free;
    HotFrame.Free;
  end
  else
    ds:=dsNormal;

  graphics.DrawString(txt,Length(txt),font,TxtRect,StrFormat, FontBrush);

  FontBrush.Free;
  font.free;
  StrFormat.Free;
  background.Free;
  graphics.Free;

  if Assigned(iml) then
  begin
    iml.Draw(PaintInfo.Canvas,
             round(itemRect.X+4),
             round(ItemRect.Y + ((ItemRect.Height - icoh) / 2)),
             imgidx,
             ds,
             itImage,
             actEnabled);
  end;
end;

function TVirtualActionTree.GetActionFromNode(const ANode: pVirtualNode;
  var Action: TBasicAction): TVirtualActionTreeNodeType;
var
  NData : TObject;
begin
  Action:=nil;

  if not Assigned(ANode) then
  begin
    Result:=vatntNoNode;
  end
  else
  begin
    NData:=TObject(Self.GetNodeData(ANode)^);

    if NData is TVirtualActionGroup then
      Result:=vatntActionGroup
    else
    if NData is TVirtualActionLink then
    begin
      Result:=vatntAction;
      Action:=TVirtualActionLink(Self.GetNodeData(ANode)^).Action;
    end;
  end;
end;

function TVirtualActionTree.GetActionGroup(const ACaption: String;
  ACanCreate: Boolean): PVirtualNode;
var
  Node : PVirtualNode;
  NData : TObject;
begin
  Result:=nil;

  Node:=Self.GetFirst;
  while Assigned(Node) do
  begin
    NData:=TObject(Self.GetNodeData(Node)^);
    if (NData is TVirtualActionGroup) and AnsiSameText(TVirtualActionGroup(NData).Caption,ACaption) then
    begin
      Result:=Node;
      break;
    end;
    Node:=Self.GetNextSibling(Node);
  end;

  if not Assigned(Result) and ACanCreate then
  begin
    NData:=TVirtualActionGroup.Create;
    TVirtualActionGroup(NData).Caption:=ACaption;
    Result:=Self.AddChild(nil,NData);
  end;
end;

function TVirtualActionTree.GetNodeFromAction(
  const AAction: TBasicAction): PVirtualNode;
var
  Node : PVirtualNode;
begin
  Result:=nil;
  Node:=Self.GetFirst;
  while (Assigned(Node)) do
  begin
    if (TObject(self.GetNodeData(Node)^) is TVirtualActionLink) and
       (TVirtualActionLink(Self.GetNodeData(Node)^).Action=AAction) then
    begin
      Result:=Node;
      break;
    end;
    Node:=Self.GetNext(Node);
  end;
end;

function TVirtualActionTree.GetOptionsClass: TTreeOptionsClass;
begin
  Result:=TVirtualTreeOptions;
end;

procedure TVirtualActionTree.HandleMouseDown(var Message: TWMMouse;
  var HitInfo: THitInfo);
begin
  inherited;
  FLeftMousePressed:=True;
  if Assigned(HitInfo.HitNode) then
    InvalidateNode(HitInfo.HitNode);
end;

procedure TVirtualActionTree.HandleMouseUp(var Message: TWMMouse;
  const HitInfo: THitInfo);
begin
  inherited;
  FLeftMousePressed:=False;
  if Assigned(HitInfo.HitNode) then
    InvalidateNode(HitInfo.HitNode);
end;

procedure TVirtualActionTree.InternalDrawHint(Sender: TBaseVirtualTree;
  HintCanvas: TCanvas; Node: PVirtualNode; R: TRect; Column: TColumnIndex);
begin
  HintCanvas.Font.Assign(Self.Font);
  HintCanvas.TextRect(R,R.Left+3,R.Top+3,GetHint(Node));
end;

procedure TVirtualActionTree.RemoveAction(const AAction: TBasicAction);
var
  Node : PVirtualNode;
  ActionLink : TVirtualActionLink;
  ParentNode : PVirtualNode;
begin
  Node:=GetNodeFromAction(AAction);
  if Assigned(Node) then
  begin
    ParentNode:=Node.Parent;
    ActionLink:=TVirtualActionLink(Self.GetNodeData(Node)^);
    AAction.UnRegisterChanges(ActionLink);
    self.DeleteNode(Node);
    if Assigned(ParentNode) and (self.ChildCount[ParentNode]=0) then
      self.DeleteNode(ParentNode);
  end;
end;

procedure TVirtualActionTree.RemoveActionList(const AActionList: TActionList);
var
  idx : integer;
begin
  try
    if not (csDesigning in Self.ComponentState) then
    begin
      for idx:=0 to AActionList.ActionCount-1 do
        RemoveAction(AActionList.Actions[idx]);
    end;
  except
  end;
end;

procedure TVirtualActionTree.SetColors(const Value: TVirtualActionTreeColors);
begin
  FColors.Assign(Value);
end;

procedure TVirtualActionTree.SetHotAction(const AAction: TBasicAction);
var
  Node : PVirtualNode;
  ARect : TRect;
begin
  HandleHotTrack(-1, -1);
  if Assigned(AAction) then
  begin
    Node:=GetNodeFromAction(AAction);
    if Assigned(Node) then
    begin
      ARect:=Self.GetDisplayRect(Node,-1,False);
      HandleHotTrack(ARect.Left, ARect.Top);
      Self.SetFocus;
      //Mouse.CursorPos:=Self.ClientToScreen(ARect.TopLeft);
    end;
  end;
end;

{ TVirtualActionLink }

constructor TVirtualActionLink.Create(ANode: PVirtualNode);
begin
  inherited Create(nil);
  FNode:=ANode;
end;

procedure TVirtualActionLink.SetAutoCheck(Value: Boolean);
begin
  TreeFromNode(FNode).InvalidateNode(FNode);
end;

procedure TVirtualActionLink.SetCaption(const Value: string);
begin
  TreeFromNode(FNode).InvalidateNode(FNode);
end;

procedure TVirtualActionLink.SetChecked(Value: Boolean);
begin
  TreeFromNode(FNode).InvalidateNode(FNode);
end;

procedure TVirtualActionLink.SetEnabled(Value: Boolean);
begin
  TreeFromNode(FNode).InvalidateNode(FNode);
end;

procedure TVirtualActionLink.SetGroupIndex(Value: Integer);
begin
  TreeFromNode(FNode).InvalidateNode(FNode);
end;

procedure TVirtualActionLink.SetHelpContext(Value: THelpContext);
begin
  TreeFromNode(FNode).InvalidateNode(FNode);
end;

procedure TVirtualActionLink.SetHelpKeyword(const Value: string);
begin
  TreeFromNode(FNode).InvalidateNode(FNode);
end;

procedure TVirtualActionLink.SetHelpType(Value: THelpType);
begin
  TreeFromNode(FNode).InvalidateNode(FNode);
end;

procedure TVirtualActionLink.SetHint(const Value: string);
begin
  TreeFromNode(FNode).InvalidateNode(FNode);
end;

procedure TVirtualActionLink.SetImageIndex(Value: Integer);
begin
  TreeFromNode(FNode).InvalidateNode(FNode);
end;

procedure TVirtualActionLink.SetShortCut(Value: TShortCut);
begin
  TreeFromNode(FNode).InvalidateNode(FNode);
end;

procedure TVirtualActionLink.SetVisible(Value: Boolean);
begin
  TreeFromNode(FNode).IsVisible[FNode]:=Value;
end;

{ TActionLists }

function TActionLists.Add: TActionListsItem;
begin
  Result:=TActionListsItem(Inherited Add);
end;

function TActionLists.AddItem(Item: TActionListsItem;
  Index: Integer): TActionListsItem;
begin
  if Item = nil then
    Result:=TActionListsItem.Create(Self)
  else
    Result := Item;
  if Assigned(Result) then
  begin
    if Index < 0 then
      Index := Count - 1;
    Result.Index := Index;
  end;
end;

constructor TActionLists.Create(AOwner: TVirtualActionTree);
begin
  inherited Create(TActionListsItem);
  FOwner:=AOwner;
end;

function TActionLists.GetItem(Index: Integer): TActionListsItem;
begin
  Result := TActionListsItem(inherited GetItem(Index));
end;

function TActionLists.GetOwner: TPersistent;
begin
  Result:=FOwner;
end;

function TActionLists.Insert(Index: Integer): TActionListsItem;
begin
  Result:=AddItem(nil, Index);
end;

procedure TActionLists.RegisterActionList(AActionList: TActionList; DefCategory: String);
begin
  FOwner.AddActionList(AActionList, DefCategory);
end;

procedure TActionLists.SetItem(Index: Integer; const Value: TActionListsItem);
begin
  inherited SetItem(Index, Value);
end;

procedure TActionLists.UnregisterActionList(AActionList: TActionList);
begin
  FOwner.RemoveActionList(AActionList);
end;

{ TActionListsItem }

destructor TActionListsItem.Destroy;
begin
  SetActionList(nil);
  inherited;
end;

procedure TActionListsItem.SetActionList(const Value: TActionList);
begin
  if Assigned(FActionList) then
    TActionLists(Collection).UnregisterActionList(FActionList);

  FActionList := Value;

  if Assigned(FActionList) then
    TActionLists(Collection).RegisterActionList(FActionList, FDefaultCategory);
end;

procedure TActionListsItem.SetDefaultCategory(const Value: String);
begin
  if Assigned(FActionList) then
    TActionLists(Collection).UnregisterActionList(FActionList);

  FDefaultCategory := Value;

  if Assigned(FActionList) then
    TActionLists(Collection).RegisterActionList(FActionList, FDefaultCategory);
end;

{ TVirtualActionTreeColors }

procedure TVirtualActionTreeColors.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TVirtualActionTreeColors then
  begin
    Self.FHeaderGradientStart:=TVirtualActionTreeColors(Source).FHeaderGradientStart;
    Self.FHeaderGradientEnd:=TVirtualActionTreeColors(Source).FHeaderGradientEnd;
    Self.FHottrackGradientStart:=TVirtualActionTreeColors(Source).FHottrackGradientStart;
    Self.FHottrackGradientEnd:=TVirtualActionTreeColors(Source).FHottrackGradientEnd;
    Self.FHottrackFrame:=TVirtualActionTreeColors(Source).FHottrackFrame;
    Self.FPressedGradientStart:=TVirtualActionTreeColors(Source).FPressedGradientStart;
    Self.FPressedGradientEnd:=TVirtualActionTreeColors(Source).FPressedGradientEnd;
    Self.FPressedFrame:=TVirtualActionTreeColors(Source).FPressedFrame;
  end; 
end;

constructor TVirtualActionTreeColors.Create;
begin
  inherited;
  FHeaderGradientStart := $FFB1D3DA;
  FHeaderGradientEnd := $FFD9E9EC;
  FHottrackGradientStart := $55AAAAFF;
  FHottrackGradientEnd := $55DDDDFF;
  FHottrackFrame := $FFAAAAFF;
  FPressedGradientStart := $55CCAB47;
  FPressedGradientEnd := $55FFD559;
  FPressedFrame := $FFFFD83D;
end;

end.
