unit uPattExCommon;

interface

uses
  VirtualTrees,
  uCustomNodeData,
  SysUtils,
  StrUtils,
  RegExpr;

type
  TExpression = class(TCustomNodeData)
  private
    FRegEx : TRegExpr;
    FReturnSubstitution : String;
    FChildSubstitution: String;
    FFirstMatchOnly: Boolean;

    function GetExpression: String;
    procedure SetExpression(const Value: String);
    procedure SetReturnSubstitution(const Value: String);

    procedure Changed;
    function GetModifier(const Index: Integer): Boolean;
    procedure SetModifier(const Index: Integer; const Value: Boolean);
    procedure SetChildSubstitution(const Value: String);
    procedure SetFirstMatchOnly(const Value: Boolean);
  public
    ID : String;

    constructor Create(ATree : TBaseVirtualTree; AParent : PVirtualNode); override;
    destructor Destroy; override;

    property RegEx : TRegExpr read FRegEx;

    property Expression : String read GetExpression write SetExpression;
    property ReturnSubstitution : String read FReturnSubstitution write SetReturnSubstitution;
    property ChildSubstitution : String read FChildSubstitution write SetChildSubstitution;
    property FirstMatchOnly : Boolean read FFirstMatchOnly write SetFirstMatchOnly;

    property ModifierG : Boolean index 0 read GetModifier write SetModifier;
    property ModifierI : Boolean index 1 read GetModifier write SetModifier;
    property ModifierM : Boolean index 2 read GetModifier write SetModifier;
    property ModifierR : Boolean index 3 read GetModifier write SetModifier;
    property ModifierS : Boolean index 4 read GetModifier write SetModifier;
    property ModifierX : Boolean index 5 read GetModifier write SetModifier;
  end;

implementation

uses uFormMain;

{ TExpression }

procedure TExpression.Changed;
begin
  formMain.Changed;
end;

constructor TExpression.Create(ATree: TBaseVirtualTree; AParent: PVirtualNode);
var
  lguid : TGUID;
begin
  inherited;
  FRegEx := TRegExpr.Create;
  Expression := '.+';
  FReturnSubstitution := '$0';
  FFirstMatchOnly := false;

  CreateGUID(lguid);
  ID := GUIDToString(lguid);

  ATree.CheckType[FNode] := ctCheckBox;
  ATree.CheckState[FNode] := csCheckedNormal;
end;

destructor TExpression.Destroy;
begin
  FRegEx.Free;
  inherited;
end;

function TExpression.GetExpression: String;
begin
  Result := FRegEx.Expression;
end;

function TExpression.GetModifier(const Index: Integer): Boolean;
begin
  case Index of
    0: Result := FRegEx.ModifierG;
    1: Result := FRegEx.ModifierI;
    2: Result := FRegEx.ModifierM;
    3: Result := FRegEx.ModifierR;
    4: Result := FRegEx.ModifierS;
    5: Result := FRegEx.ModifierX;
  end;
end;

procedure TExpression.SetChildSubstitution(const Value: String);
begin
  FChildSubstitution := Value;
  Changed;
end;

procedure TExpression.SetExpression(const Value: String);
begin
  FRegEx.Expression := Value;
  Changed;
end;

procedure TExpression.SetFirstMatchOnly(const Value: Boolean);
begin
  FFirstMatchOnly := Value;
  Changed;
end;

procedure TExpression.SetModifier(const Index: Integer; const Value: Boolean);
begin
   case Index of
    0: FRegEx.ModifierG := Value;
    1: FRegEx.ModifierI := Value;
    2: FRegEx.ModifierM := Value;
    3: FRegEx.ModifierR := Value;
    4: FRegEx.ModifierS := Value;
    5: FRegEx.ModifierX := Value;
  end;

  Changed;
end;

procedure TExpression.SetReturnSubstitution(const Value: String);
begin
  FReturnSubstitution := Value;
  Changed;
end;

end.
