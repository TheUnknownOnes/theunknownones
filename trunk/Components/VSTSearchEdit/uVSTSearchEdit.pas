//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit uVSTSearchEdit;

interface

{$I JEDI.inc}

uses
  SysUtils, Classes, VirtualTrees, StdCtrls, ComCtrls, StrUtils;

type
  TVSTSearchEdit = class;

  TOnAfterSearchEvent = procedure(ASender : TVSTSearchEdit; ASearchText : String) of object;

  TVSTSearchEdit = class(TComponent)
  private
    FTree: TVirtualStringTree;
    FProgress: TProgressBar;
    FEdit: TCustomEdit;
    FOldOnChangeProc : TNotifyEvent;
    FOnAfterSearch: TOnAfterSearchEvent;
    FMaxLevel: Integer;

    procedure DoSearch(const AWords : TStrings);
    function NodeMatches(const ANode : PVirtualNode; const AWords : TStrings) : Boolean;

    procedure OnEditChange(Sender : TObject);
    procedure SetEdit(const Value: TCustomEdit);
    procedure DetachFromEdit(const Value: TCustomEdit);
    procedure AttachToEdit(const Value: TCustomEdit);
  protected
    { Protected-Deklarationen }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;

    procedure RunSearch; overload;
    procedure RunSearch(AText : String); overload;
    function TestNode(const ANode: PVirtualNode): Boolean; overload;
    function TestNode(const ANode: PVirtualNode; AText : String): Boolean; overload;
  published
    property OnAfterSearch : TOnAfterSearchEvent read FOnAfterSearch write FOnAfterSearch;
    property VST : TVirtualStringTree read FTree write FTree;
    property Edit : TCustomEdit read FEdit write SetEdit;
    property Progress : TProgressBar read FProgress write FProgress;
    property MaxLevel : Integer read FMaxLevel write FMaxLevel default -1;
  end;


implementation

uses
  Forms, uVirtualTreeHelpers, Controls {$ifndef DELPHI12_UP}, TntStdCtrls{$endif},
  ExtCtrls;

{ TVSTSearchEdit }

constructor TVSTSearchEdit.Create(AOwner: TComponent);
begin
  inherited;
  FMaxLevel := -1;
end;

destructor TVSTSearchEdit.Destroy;
begin
  DetachFromEdit(FEdit);

  inherited;
end;

procedure TVSTSearchEdit.DoSearch(const AWords: TStrings);
var
  Node,
  NewNode : PVirtualNode;
  Matches : Boolean;
begin
  FTree.BeginUpdate;
  try
    if Assigned(FProgress) then
    begin
      FProgress.Position:=0;
      FProgress.Max:=FTree.ChildCount[FTree.RootNode];
    end;

    Node:=FTree.GetFirst;

    while Assigned(Node) do
    begin
      if Assigned(FProgress) then
        FProgress.Position:=FProgress.Position+1;

      if AWords.Count>0 then
        Matches := NodeMatches(Node, AWords)
      else
        Matches := true;

        
      FTree.FullyVisible[Node] := Matches;

      if not Matches then
      begin
        if FMaxLevel > -1 then
        begin
          if FTree.GetNodeLevel(Node) < Cardinal(FMaxLevel) then
            NewNode := FTree.GetFirstChild(Node)
          else
            NewNode := FTree.GetNextSibling(Node)
        end
        else
          NewNode := FTree.GetFirstChild(Node)
      end
      else
      begin
        FTree.VisibleRecursive[Node] := true;
        NewNode := FTree.GetNextSibling(Node);
      end;

      if not Assigned(NewNode) then
        NewNode := FTree.GetNext(Node);

      Node := NewNode;
    end;
  finally
    FTree.EndUpdate;
    if Assigned(FProgress) then
      FProgress.Position:=0;
  end;
end;

function TVSTSearchEdit.NodeMatches(const ANode: PVirtualNode;
  const AWords: TStrings): Boolean;
var
  CurWord : String;
  idxField : Integer;
  NodeStr : String;
begin
  Result:=true;
  NodeStr:='';

  if FTree.Header.Columns.Count>0 then
  begin
    for idxField:=0 to FTree.Header.Columns.Count-1 do
      NodeStr:=NodeStr+FTree.Text[ANode, idxField];
  end
  else
    NodeStr:=FTree.Text[ANode, -1];

  for CurWord in AWords do
  begin
    Result:=AnsiContainsText(NodeStr, CurWord);
    if not Result then exit;
  end;
end;

procedure TVSTSearchEdit.OnEditChange(Sender: TObject);
begin
  RunSearch;

  if Assigned(FOldOnChangeProc) then
    FOldOnChangeProc(Sender);
end;

procedure TVSTSearchEdit.RunSearch;
begin
  if Assigned(FEdit) then
    RunSearch(FEdit.Text);
end;

procedure TVSTSearchEdit.RunSearch(AText: String);
var
  Words : TStringList;
begin
  if not Assigned(FTree) then exit;
  
  Screen.Cursor:=crHourGlass;
  try
    Words:=TStringList.Create;
    Words.Delimiter:=#32;
    Words.DelimitedText:=AText;
    DoSearch(Words);
    Words.Free;
  finally
    Screen.Cursor:=crDefault;
  end;

  if Assigned(FOnAfterSearch) then
    FOnAfterSearch(Self, AText);
end;

procedure TVSTSearchEdit.DetachFromEdit(const Value: TCustomEdit);
begin
  if Assigned(Value) then
  begin
    if Value is TEdit then
      TEdit(Value).OnChange:=FOldOnChangeProc
    {$ifndef DELPHI12_UP}
    else
    if Value is TTntEdit then
      TTNTEdit(Value).OnChange:=FOldOnChangeProc;
    {$endif}
  end;
end;

procedure TVSTSearchEdit.AttachToEdit(const Value: TCustomEdit);
begin
  if Assigned(Value) then
  begin
    if Value is TEdit then
    begin
      FOldOnChangeProc:=TEdit(FEdit).OnChange;
      TEdit(FEdit).OnChange:=OnEditChange;
    end
    {$ifdef DELPHI12_UP}
    else
    if Value is TButtonedEdit then
    begin
      FOldOnChangeProc:=TButtonedEdit(FEdit).OnChange;
      TButtonedEdit(FEdit).OnChange:=OnEditChange;
    end;{$endif}
    {$ifndef DELPHI12_UP}else
    if Value is TTntEdit then
    begin
      FOldOnChangeProc:=TTNTEdit(FEdit).OnChange;
      TTNTEdit(FEdit).OnChange:=OnEditChange;
    end;{$endif}
  end;
end;

procedure TVSTSearchEdit.SetEdit(const Value: TCustomEdit);
begin
  if FEdit<>Value then
  begin
    if not (csDesigning in ComponentState) then
      DetachFromEdit(FEdit);

    FEdit := Value;

    if not (csDesigning in ComponentState) then
      AttachToEdit(FEdit);
  end;
end;

function TVSTSearchEdit.TestNode(const ANode: PVirtualNode): Boolean;
begin
  Result:=Assigned(FEdit) and TestNode(ANode, FEdit.Text);
end;

function TVSTSearchEdit.TestNode(const ANode: PVirtualNode;
  AText: String): Boolean;
var
  Words : TStringList;
begin
  Result:=False;
  if not Assigned(FTree) then exit;

  Words:=TStringList.Create;
  Words.Delimiter:=#32;
  Words.DelimitedText:=AText;
  Result:=NodeMatches(ANode, Words);
  Words.Free;
end;


end.
