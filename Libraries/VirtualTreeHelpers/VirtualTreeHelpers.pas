unit VirtualTreeHelpers;

interface

uses
  VirtualTrees;

type
  TBaseVirtualTreeHelper = class helper for TBaseVirtualTree
  private
    function GetVisibleRecursive(Node: PVirtualNode): Boolean;
    procedure SetVisibleRecursive(Node: PVirtualNode; const Value: Boolean);
  public
    property VisibleRecursive[Node: PVirtualNode]: Boolean read GetVisibleRecursive write SetVisibleRecursive;
  end;


implementation

{ TBaseVirtualTreeHelper }

function TBaseVirtualTreeHelper.GetVisibleRecursive(
  Node: PVirtualNode): Boolean;

// Returns true if all parent nodes of Node are visible.

begin
  Assert(Assigned(Node) and (Node <> RootNode), 'Invalid parameters.');

  Result := (vsVisible in Node.States);

  while (Result) and (Node <> RootNode) do
  begin
    Node := Node.Parent;
    Result := (vsVisible in Node.States);
  end;
end;

procedure TBaseVirtualTreeHelper.SetVisibleRecursive(Node: PVirtualNode;
  const Value: Boolean);

// If Value is True then all parent nodes of Node are made visible.

begin
  Assert(Assigned(Node) and (Node <> RootNode), 'Invalid parameter.');

  repeat
    if not ((vsVisible in Node.States) = Value) then
      IsVisible[Node] := Value;

    Node := Node.Parent;
    if Node = RootNode then
      Break;
  until False;
end;

end.
