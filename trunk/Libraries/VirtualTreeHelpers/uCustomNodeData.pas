unit uCustomNodeData;

interface

uses
  VirtualTrees;

type
  TCustomNodeData = class
  protected
    FNode : PVirtualNode;
    FTree : TBaseVirtualTree;
  public
    constructor Create(ATree : TBaseVirtualTree; AParent : PVirtualNode); virtual;
    destructor Destroy; override;

    property Node : PVirtualNode read FNode;
    property Tree : TBaseVirtualTree read FTree;

    class function GetNodeData(ANode : PVirtualNode; ATree : TBaseVirtualTree = Nil) : TCustomNodeData; overload;
    class function GetNodeData(ANode : PVirtualNode; ATree : TBaseVirtualTree; out ANodeData: TCustomNodeData) : Boolean; overload;
  end;

  PCustomNodeData = ^TCustomNodeData;

  TNodeDataEvent = (ndeFreeNode, ndeGetNodeDataSize);
  TNodeDataEvents = set of TNodeDataEvent;

  TNodeDataEventHandler = class
  protected
    procedure DoFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode); virtual;
    procedure DoGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer); virtual;
  public
    class procedure InitEvents(ATree: TVirtualStringTree; AEvents : TNodeDataEvents); overload; virtual;
    class procedure InitEvents(ATree: TVirtualDrawTree; AEvents : TNodeDataEvents); overload; virtual;
    class procedure InitEvents(ATree: TVirtualDrawTree); overload; virtual;
    class procedure InitEvents(ATree: TVirtualStringTree); overload; virtual;
  end;

var
  GlobalNodeDataEventHandler : TNodeDataEventHandler = nil;

implementation

{ TCustomNodeData }

constructor TCustomNodeData.Create(ATree: TBaseVirtualTree;
  AParent: PVirtualNode);
begin
  FTree := ATree;

  FNode := FTree.AddChild(AParent, Self);
end;

destructor TCustomNodeData.Destroy;
begin

  inherited;
end;

class function TCustomNodeData.GetNodeData(ANode: PVirtualNode;
  ATree: TBaseVirtualTree; out ANodeData: TCustomNodeData): Boolean;
begin
  Result:=False;
  if Assigned(ANode) then
  begin
    ANodeData := TCustomNodeData.GetNodeData(ANode, ATree);
    if Assigned(ANodeData) then
      Result:=True;
  end;
end;

class function TCustomNodeData.GetNodeData(
  ANode: PVirtualNode; ATree : TBaseVirtualTree): TCustomNodeData;
begin
  if not Assigned(ATree) then
    ATree:=TreeFromNode(ANode);

  Result := PCustomNodeData(ATree.GetNodeData(ANode))^;
end;

{ TNodeDataEventHandler }

procedure TNodeDataEventHandler.DoFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  TCustomNodeData.GetNodeData(Node, Sender).Free;
end;

procedure TNodeDataEventHandler.DoGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TCustomNodeData);
end;

class procedure TNodeDataEventHandler.InitEvents(ATree: TVirtualDrawTree);
var
  nde : TNodeDataEvent;
  ndes : TNodeDataEvents;
begin
  ndes := [];
  for nde := Low(TNodeDataEvent) to High(TNodeDataEvent) do
    Include(ndes, nde);

  InitEvents(ATree, ndes);
end;

class procedure TNodeDataEventHandler.InitEvents(ATree: TVirtualStringTree);
var
  nde : TNodeDataEvent;
  ndes : TNodeDataEvents;
begin
  ndes := [];
  for nde := Low(TNodeDataEvent) to High(TNodeDataEvent) do
    Include(ndes, nde);

  InitEvents(ATree, ndes);
end;

class procedure TNodeDataEventHandler.InitEvents(ATree: TVirtualStringTree;
  AEvents: TNodeDataEvents);
begin
  if ndeFreeNode in AEvents then
    ATree.OnFreeNode := GlobalNodeDataEventHandler.DoFreeNode;
  if ndeGetNodeDataSize in AEvents then
    ATree.OnGetNodeDataSize := GlobalNodeDataEventHandler.DoGetNodeDataSize;
end;

class procedure TNodeDataEventHandler.InitEvents(ATree: TVirtualDrawTree;
  AEvents: TNodeDataEvents);
begin
  if ndeFreeNode in AEvents then
    ATree.OnFreeNode := GlobalNodeDataEventHandler.DoFreeNode;
  if ndeGetNodeDataSize in AEvents then
    ATree.OnGetNodeDataSize := GlobalNodeDataEventHandler.DoGetNodeDataSize;
end;

initialization
  GlobalNodeDataEventHandler:=TNodeDataEventHandler.Create;

finalization
  GlobalNodeDataEventHandler.Free;

end.
