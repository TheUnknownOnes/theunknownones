//**********************************************************
// Developed by TheUnkownOnes.net
//
// for more information look at www.TheUnknownOnes.net
//**********************************************************

unit uHashList;

interface

uses
  Sysutils;

type
  PStringHashEntry = ^TStringHashEntry;
  TStringHashEntry = record
    Key : String;
    Value : String;
    Data : TObject;
    Next : PStringHashEntry;
  end;
  TStringHashEntryList = array of PStringHashEntry;

  TStringHashList = class
  private
    function GetValue(AKey: String): String;
    procedure SetValue(AKey: String; const Value: String);
    procedure SetCapacity(const Value: Integer);
    function GetExists(AKey: String): Boolean;
    function GetData(AKey: String): TObject;
    procedure SetData(AKey: String; const Value: TObject);
  protected
    FCount : Integer;
    FCapacity : Integer;
    FList : TStringHashEntryList;
    FMaxFillRatio : Single;
    FMaxFillCount : Integer;
    FMaxFillResizeFactor : Integer;

    function Hash(const AKey : String) : Integer;
    function Index(const AKey : String) : Integer;

    function CreateEntry(const AKey : String) : PStringHashEntry;
    function GetEntry(const AKey : String) : PStringHashEntry; overload;
    function GetEntry(const AKey : String; out AIndex : Integer) : PStringHashEntry; overload;
    procedure DeleteEntry(const AKey : String); overload;
    procedure DeleteEntry(AEntry : PStringHashEntry; AIndex : Integer; var AList : TStringHashEntryList); overload;
  public
    constructor Create(); virtual;
    destructor Destroy; override;

    procedure Clear;
    procedure Delete(AKey : String);

    property Count : Integer read FCount;

    property Capacity : Integer read FCapacity write SetCapacity;
    property Value[AKey : String] : String read GetValue write SetValue; default;
    property Data[AKey : String] : TObject read GetData write SetData;
    property Exists[AKey : String] : Boolean read GetExists;
    property MaxFillRatio : Single read FMaxFillRatio write FMaxFillRatio;
    property MaxFillResizeFactor : Integer read FMaxFillResizeFactor write FMaxFillResizeFactor;
  end;

implementation

{ THashList }

procedure TStringHashList.Clear;
var
  idx : Integer;
begin
  for idx := Low(FList) to High(FList) do
  begin
    while Assigned(FList[idx]) do
      DeleteEntry(FList[idx], idx, FList);
  end;

  FCount := 0;
end;

constructor TStringHashList.Create();
begin
  FCount := 0;
  FMaxFillRatio := 0.65;
  FMaxFillResizeFactor := 2;

  Capacity := 1000;
end;

function TStringHashList.CreateEntry(const AKey: String): PStringHashEntry;
var
  idx : Integer;
begin
  idx := Index(AKey);

  New(Result);
  Result^.Key := AKey;
  Result^.Next := FList[idx];
  Result^.Data := nil;
  FList[idx] := Result;
  Inc(FCount);

  if FCount > FMaxFillCount then
    Capacity := Capacity * FMaxFillResizeFactor;
end;

procedure TStringHashList.DeleteEntry(const AKey: String);
var
  idx : Integer;
  entry : PStringHashEntry;
begin
  entry := GetEntry(AKey, idx);
  if Assigned(entry) then
  begin
    DeleteEntry(entry, idx, FList);
    Dec(FCount);
  end;
end;

procedure TStringHashList.Delete(AKey: String);
begin
  DeleteEntry(AKey);
end;

procedure TStringHashList.DeleteEntry(AEntry: PStringHashEntry; AIndex : Integer; var AList : TStringHashEntryList);
var
  prev,
  entry,
  next : PStringHashEntry;
begin
  prev := nil;
  entry := AList[AIndex];

  while Assigned(entry) do
  begin
    next := entry.Next;

    if entry = AEntry then
    begin
      if Assigned(prev) then
        prev^.Next := next
      else
        AList[AIndex] := next;

      Dispose(entry);
      break;
    end
    else
    begin
      prev := entry;
      entry := next;
    end;

  end;
end;

destructor TStringHashList.Destroy;
begin
  Clear;
  inherited;
end;

function TStringHashList.GetEntry(const AKey: String): PStringHashEntry;
var
  idx : Integer;
begin
  Result := GetEntry(AKey, idx);
end;

function TStringHashList.GetData(AKey: String): TObject;
var
  entry : PStringHashEntry;
begin
  entry := GetEntry(AKey);
  if Assigned(entry) then
    Result := entry^.Data
  else
    Result := nil;
end;

function TStringHashList.GetEntry(const AKey: String;
  out AIndex: Integer): PStringHashEntry;
begin
  AIndex := Index(AKey);
  Result := FList[AIndex];

  while Assigned(Result) and
        (Result^.Key <> AKey) do
    Result := Result^.Next;
end;

function TStringHashList.GetExists(AKey: String): Boolean;
begin
  Result := Assigned(GetEntry(AKey));
end;

function TStringHashList.GetValue(AKey: String): String;
var
  entry : PStringHashEntry;
begin
  entry := GetEntry(AKey);
  if Assigned(entry) then
    Result := entry^.Value
  else
    Result := EmptyStr;
end;

function TStringHashList.Hash(const AKey : String): Integer;
var
  i, x: Integer;
begin
  Result := 0;
  for i := 1 to Length(AKey) do
  begin
    Result := (Result shl 4) + Ord(AKey[i]);
    x := Result and $F0000000;
    if (x <> 0) then
      Result := Result xor (x shr 24);
    Result := Result and (not x);
  end;
end;

function TStringHashList.Index(const AKey: String): Integer;
begin
  Result := Hash(AKey) mod FCapacity;
end;

procedure TStringHashList.SetCapacity(const Value: Integer);
var
  oldList : TStringHashEntryList;
  idx : Integer;
  entry,
  newentry : PStringHashEntry;
begin
  Assert(Value > 0, 'The capacity has to be greater then 0');

  oldList := FList;
  FCapacity := Value;
  SetLength(FList, 0); //set all entries to NIL
  SetLength(FList, FCapacity);
  FMaxFillCount := Trunc(FMaxFillRatio * FCapacity);
  FCount := 0;

  for idx := Low(oldList) to High(oldList) do
  begin
    while Assigned(oldList[idx]) do
    begin
      entry := oldList[idx];

      newentry := CreateEntry(entry^.Key);
      newentry^.Value := entry^.Value;
      newentry^.Data := entry^.Data;

      DeleteEntry(oldList[idx], idx, oldList);
    end;
  end;

end;


procedure TStringHashList.SetData(AKey: String; const Value: TObject);
var
  Entry : PStringHashEntry;
begin
  Entry := GetEntry(AKey);

  if Assigned(Entry) then
    Entry^.Data := Value
  else
    CreateEntry(AKey)^.Data := Value;
end;

procedure TStringHashList.SetValue(AKey: String; const Value: String);
var
  Entry : PStringHashEntry;
begin
  Entry := GetEntry(AKey);

  if Assigned(Entry) then
    Entry^.Value := Value
  else
    CreateEntry(AKey)^.Value := Value;
end;

end.
