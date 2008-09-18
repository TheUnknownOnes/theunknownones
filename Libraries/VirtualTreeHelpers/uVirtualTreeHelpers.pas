//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************

unit uVirtualTreeHelpers;

interface

uses
  VirtualTrees, Windows, SysUtils, SndKey32;

type
  TBaseVirtualTreeHelper = class helper for TBaseVirtualTree
  private
    function GetVisibleRecursive(Node: PVirtualNode): Boolean;
    procedure SetVisibleRecursive(Node: PVirtualNode; const Value: Boolean);


  public
    property VisibleRecursive[Node: PVirtualNode]: Boolean read GetVisibleRecursive write SetVisibleRecursive;

    function GetCheckedChildrenCountRecursive(Node: PVirtualNode; VisibleOnly : Boolean = false): Integer;

    procedure ScanEditorKeys(var AKey : Word);

    procedure ExpandAll;
    procedure CollapseAll;

    function GetFirstNodeByData(AData : Pointer; out ANode : PVirtualNode) : Boolean;
    function GetNextNodeByData(AStartAtNode : PVirtualNode; AData : Pointer; out ANode : PVirtualNode) : Boolean;
  end;


implementation

{ TBaseVirtualTreeHelper }

procedure TBaseVirtualTreeHelper.CollapseAll;
var
  Node : PVirtualNode;
begin
  Node:=GetFirst;
  while Assigned(Node) do
  begin
    Expanded[Node]:=False;
    Node:=GetNext(Node);
  end;
end;

procedure TBaseVirtualTreeHelper.ExpandAll;
var
  Node : PVirtualNode;
begin
  Node:=GetFirst;
  while Assigned(Node) do
  begin
    Expanded[Node]:=True;
    Node:=GetNext(Node);
  end;
end;

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

function TBaseVirtualTreeHelper.GetFirstNodeByData(AData: Pointer;
  out ANode: PVirtualNode): Boolean;
var
  Node : PVirtualNode;
  Data : Pointer;
begin
  Result := false;

  Node := GetFirst;
  while Assigned(Node) do
  begin
    Data := GetNodeData(Node);

    if Data = AData then
    begin
      Result := true;
      ANode := Node;
      break;
    end;

    Node := GetNext(Node);
  end;
end;

function TBaseVirtualTreeHelper.GetNextNodeByData(AStartAtNode: PVirtualNode;
  AData: Pointer; out ANode: PVirtualNode): Boolean;
var
  Node : PVirtualNode;
  Data : Pointer;
begin
  Result := false;

  Node := GetNext(AStartAtNode);
  while Assigned(Node) do
  begin
    Data := GetNodeData(Node);

    if Data = AData then
    begin
      Result := true;
      ANode := Node;
      break;
    end;

    Node := GetNext(Node);
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

procedure TBaseVirtualTreeHelper.ScanEditorKeys(var AKey: Word);
//Beim Tastendruck auf einem VST wird automatisch ein Editor geöffnet
// Aufruf im OnKeyDown Event
var
  InputStr : String;

  {$REGION 'VirtualKey in Char übersetzen'}
 
  function GetCharFromVirtualKey(Key: Word): string;
  var
     keyboardState: TKeyboardState;
     asciiResult: Integer;
  begin
     GetKeyboardState(keyboardState) ;

     SetLength(Result, 2) ;
     asciiResult := ToAscii(key, MapVirtualKey(key, 0), keyboardState, @Result[1], 0) ;
     case asciiResult of
       0: Result := '';
       1: SetLength(Result, 1) ;
       2:;
       else
         Result := '';
     end;
  end;
  {$ENDREGION}
begin
  if not IsEditing then
  begin
    if AKey=VK_RETURN then
    begin
      AKey:=0;
      EditNode(FocusedNode, FocusedColumn);
    end
    else
    begin
      InputStr:=GetCharFromVirtualKey(AKey);
      If Length(InputStr)>0 then
        if Copy(InputStr,1,1)[1]>=#32 then
        begin
          if EditNode(FocusedNode, FocusedColumn) and
             (Length(trim(InputStr))>0) then
            SendKeys(PChar(Copy(InputStr,1,1)), True);

          AKey:=0;
        end;
    end;
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
