//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************

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

    function GetCheckedChildrenCountRecursive(Node: PVirtualNode; VisibleOnly : Boolean = false): Integer;
  end;


implementation

{ TBaseVirtualTreeHelper }

function TBaseVirtualTreeHelper.GetCheckedChildrenCountRecursive(
  Node: PVirtualNode; VisibleOnly : Boolean): Integer;

// Returns the number of children of "Node" which are checked

var
  Child : PVirtualNode;
begin
  Result := 0;

  Child := GetFirstChild(Node);
  while Assigned(Child) do
  begin
    if (CheckState[Child] = csCheckedNormal) then
    begin
      if (VisibleOnly and IsVisible[Child]) or
         (not VisibleOnly) then
        Inc(Result);
    end;

    Inc(Result, GetCheckedChildrenCountRecursive(Child, VisibleOnly));

    Child := GetNextSibling(Child);
  end;
end;

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
    if ((vsVisible in Node.States) <> Value) then
      IsVisible[Node] := Value;

    Node := Node.Parent;
    if Node = RootNode then
      Break;
  until False;
end;

end.
