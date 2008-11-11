unit uCNA2ValueEditor;

interface

uses
  Classes,
  TypInfo,
  uCNA2FrameValueEditorBase;

type
  TCNA2ValueEditorFrameClassList = class(TList)
  private
    function Get(Index: Integer): TValueEditorFrameClass;
    procedure Put(Index: Integer; const Value: TValueEditorFrameClass);
  published
  public
    function Add(Item: TValueEditorFrameClass): Integer;
    function Extract(Item: TValueEditorFrameClass): TValueEditorFrameClass;
    function First: TValueEditorFrameClass;
    function IndexOf(Item: TValueEditorFrameClass): Integer;
    procedure Insert(Index: Integer; Item: TValueEditorFrameClass);
    function Last: TValueEditorFrameClass;
    function Remove(Item: TValueEditorFrameClass): Integer;
    property Items[Index: Integer]: TValueEditorFrameClass read Get write Put; default;
  end;

  TCNA2ValueEditor = class
  private
    FClasses : TCNA2ValueEditorFrameClassList;
  public
    constructor Create();
    destructor Destroy(); override;

    procedure RegisterEditorClass(AClass : TValueEditorFrameClass);
    procedure UnregisterEditorClass(AClass : TValueEditorFrameClass);

    function GetEditorClass(ATypeKind : TTypeKind; out AClass : TValueEditorFrameClass) : Boolean; 
  end;

var
  cna2ValueEditor : TCNA2ValueEditor;

implementation

uses uCNA2FrameValueEditorString;


{ TCNA2ValueEditorFrameClassList }

function TCNA2ValueEditorFrameClassList.Add(
  Item: TValueEditorFrameClass): Integer;
begin
  Result := inherited Add(Item);
end;

function TCNA2ValueEditorFrameClassList.Extract(
  Item: TValueEditorFrameClass): TValueEditorFrameClass;
begin
  Result := inherited Extract(Item);
end;

function TCNA2ValueEditorFrameClassList.First: TValueEditorFrameClass;
begin
  Result := inherited First;
end;

function TCNA2ValueEditorFrameClassList.Get(
  Index: Integer): TValueEditorFrameClass;
begin
  Result := inherited Get(Index);
end;

function TCNA2ValueEditorFrameClassList.IndexOf(
  Item: TValueEditorFrameClass): Integer;
begin
  Result := inherited IndexOf(Item);
end;

procedure TCNA2ValueEditorFrameClassList.Insert(Index: Integer;
  Item: TValueEditorFrameClass);
begin
  inherited Insert(Index, Item);
end;

function TCNA2ValueEditorFrameClassList.Last: TValueEditorFrameClass;
begin
  Result := inherited Last;
end;

procedure TCNA2ValueEditorFrameClassList.Put(Index: Integer;
  const Value: TValueEditorFrameClass);
begin
  inherited Put(Index, Value);
end;

function TCNA2ValueEditorFrameClassList.Remove(
  Item: TValueEditorFrameClass): Integer;
begin
  Result := inherited Remove(Item);
end;

{ TCNA2ValueEditor }

constructor TCNA2ValueEditor.Create;
begin
  FClasses := TCNA2ValueEditorFrameClassList.Create;

  RegisterEditorClass(Tframe_ValueEditorString);
end;

destructor TCNA2ValueEditor.Destroy;
begin
  UnregisterEditorClass(Tframe_ValueEditorString);

  FClasses.Free;

  inherited;
end;

function TCNA2ValueEditor.GetEditorClass(ATypeKind: TTypeKind;
  out AClass: TValueEditorFrameClass): Boolean;
var
  idx : Integer;
begin
  Result := false;

  for idx := 0 to FClasses.Count  - 1 do
  begin
    if FClasses[idx].Handles(ATypeKind) then
    begin
      AClass := FClasses[idx];
      Result := true;
      break;
    end;
  end;
end;

procedure TCNA2ValueEditor.RegisterEditorClass(AClass: TValueEditorFrameClass);
begin
  if FClasses.IndexOf(AClass) = -1 then
    FClasses.Add(AClass);
end;

procedure TCNA2ValueEditor.UnregisterEditorClass(
  AClass: TValueEditorFrameClass);
begin
  FClasses.Extract(AClass);
end;

initialization
  cna2ValueEditor := nil;

end.
