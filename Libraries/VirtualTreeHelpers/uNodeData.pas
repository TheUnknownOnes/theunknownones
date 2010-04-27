unit uNodeData;

interface

uses
  VirtualTrees;

type
  TTreeNodeData = class(TObject)
  private
    FTree: TBaseVirtualTree;
    FNode: PVirtualNode;
  protected
    function GetAsString: String; virtual; abstract;
  public
    constructor Create(ATree: TBaseVirtualTree; AParentNode: PVirtualNode); virtual;

    property Node : PVirtualNode read FNode;
    property Tree : TBaseVirtualTree read FTree;
    property AsString : String read GetAsString;
  end;

function GetNodeData(ATree: TBaseVirtualTree;
  ANode: PVirtualNode): TTreeNodeData; overload;

function GetNodeData(ATree: TBaseVirtualTree;
  ANode: PVirtualNode; var AData : TTreeNodeData) : Boolean; overload;

implementation

function GetNodeData(ATree: TBaseVirtualTree;
  ANode: PVirtualNode): TTreeNodeData;
begin
  Result:=nil;

  if Assigned(ANode) then
    Result:= TTreeNodeData(ATree.GetNodeData(ANode)^);
end;

function GetNodeData(ATree: TBaseVirtualTree;
  ANode: PVirtualNode; var AData : TTreeNodeData) : Boolean;
var
  Data : Pointer;
begin
  Result := Assigned(ANode);

  if Result then
  begin
    Data := ATree.GetNodeData(ANode);

    Result := Assigned(Data);

    if Result then
      AData := TTreeNodeData(Data^);
  end;
end;

{ TTreeNodeData }

constructor TTreeNodeData.Create(ATree: TBaseVirtualTree;
  AParentNode: PVirtualNode);
begin
  FTree:=ATree;

  FNode:=FTree.AddChild(AParentNode, self);

  FTree.CheckType[FNode]:=ctTriStateCheckBox;
end;

end.
