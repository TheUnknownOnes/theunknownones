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
  TVSTSearchEdit = class(TComponent)
  private
    FTree: TVirtualStringTree;
    FProgress: TProgressBar;
    FEdit: TCustomEdit;
    FOldOnChangeProc : TNotifyEvent;

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

    procedure RunSearch(AText : String);
  published
    property VST : TVirtualStringTree read FTree write FTree;
    property Edit : TCustomEdit read FEdit write SetEdit;
    property Progress : TProgressBar read FProgress write FProgress;
  end;


implementation

uses
  Forms, Controls {$ifndef DELPHI12_UP}, TntStdCtrls{$endif};

{ TVSTSearchEdit }

constructor TVSTSearchEdit.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TVSTSearchEdit.Destroy;
begin
  DetachFromEdit(FEdit);

  inherited;
end;

procedure TVSTSearchEdit.DoSearch(const AWords: TStrings);
var
  Node : PVirtualNode;
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
        FTree.FullyVisible[Node]:=NodeMatches(Node, AWords)
      else
        FTree.IsVisible[Node]:=true;

      Node:=FTree.GetNext(Node);
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
  RunSearch(FEdit.Text);

  if Assigned(FOldOnChangeProc) then
    FOldOnChangeProc(Sender);
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

end.
