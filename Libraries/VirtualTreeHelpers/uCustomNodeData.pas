unit uCustomNodeData;

interface

uses
  VirtualTrees;

type
  TCustomNodeData = class
  protected
    FNode : PVirtualNode;
    FTree : TVirtualStringTree;

    procedure DoFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure DoGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
  public
    constructor Create(ATree : TVirtualStringTree; AParent : PVirtualNode); virtual;
    destructor Destroy; override;

    class function GetNodeData(ANode : PVirtualNode) : TCustomNodeData;
  end;

  PCustomNodeData = ^TCustomNodeData;

implementation

{ TCustomNodeData }

constructor TCustomNodeData.Create(ATree: TVirtualStringTree;
  AParent: PVirtualNode);
begin
  FTree := ATree;

  FTree.OnGetNodeDataSize := DoGetNodeDataSize;
  FTree.OnFreeNode := DoFreeNode;

  FNode := FTree.AddChild(AParent, Self);

end;

destructor TCustomNodeData.Destroy;
begin

  inherited;
end;

class function TCustomNodeData.GetNodeData(
  ANode: PVirtualNode): TCustomNodeData;
begin
  Result := PCustomNodeData(TreeFromNode(ANode).GetNodeData(ANode))^;
end;

procedure TCustomNodeData.DoFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  GetNodeData(Node).Free;
end;

procedure TCustomNodeData.DoGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TCustomNodeData);
end;

end.
