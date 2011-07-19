//**********************************************************
// Developed by TheUnkownOnes.net
//
// for more information look at www.TheUnknownOnes.net
//**********************************************************

unit uHashList;

interface

uses
  Sysutils, Windows, Classes;

type
  PStringHashEntry = ^TStringHashEntry;
  TStringHashEntry = record
    Key : String;
    Value : String;
    _Object : TObject;
    Next : PStringHashEntry;
  end;
  TStringHashEntryList = array of PStringHashEntry;

  TStringHashList = class(TPersistent)
  protected
    FCount : Integer;
    FCapacity : Integer;
    FList : TStringHashEntryList;
    FMaxFillRatio : Single;
    FMaxFillCount : Integer;
    FMaxFillResizeFactor : Integer;
    FCaseSensitive: Boolean;

    function CaseKey(AKey : String) : String; inline;
    function Hash(AKey : String) : Cardinal;
    function Index(const AKey : String) : Cardinal;

    function CreateEntry(const AKey : String) : PStringHashEntry;
    function GetEntry(const AKey : String) : PStringHashEntry; overload;
    function GetEntry(AKey : String; out AIndex : Integer) : PStringHashEntry; overload;
    procedure DeleteEntry(const AKey : String); overload;
    procedure DeleteEntry(AEntry : PStringHashEntry; AIndex : Integer; var AList : TStringHashEntryList); overload;

    function GetValues(AKey: String): String;
    procedure SetValues(AKey: String; const Value: String);
    procedure SetCapacity(const Value: Integer);
    function GetExists(AKey: String): Boolean;
    function GetObjects(AKey: String): TObject;
    procedure SetObjects(AKey: String; const Value: TObject);

    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(); virtual;
    destructor Destroy; override;

    procedure Clear;
    procedure Delete(AKey : String);
    procedure Add(AKey, AValue : String);
    procedure AddObject(AKey : String; AObject : TObject);

    procedure Assign(Source: TPersistent); override;

    property Count : Integer read FCount;

    property CaseSensitive : Boolean read FCaseSensitive write FCaseSensitive;
    property Capacity : Integer read FCapacity write SetCapacity;
    property Values[AKey : String] : String read GetValues write SetValues; default;
    property Objects[AKey : String] : TObject read GetObjects write SetObjects;
    property Exists[AKey : String] : Boolean read GetExists;
    property MaxFillRatio : Single read FMaxFillRatio write FMaxFillRatio;
    property MaxFillResizeFactor : Integer read FMaxFillResizeFactor write FMaxFillResizeFactor;
  end;

implementation

{ THashList }

procedure TStringHashList.Add(AKey, AValue: String);
begin
  Self[AKey] := AValue;
end;

procedure TStringHashList.AddObject(AKey: String; AObject: TObject);
begin
  Self.Objects[AKey] := AObject;
end;

procedure TStringHashList.Assign(Source: TPersistent);
var
  s : TStrings;
  idx : Integer;
  entry : PStringHashEntry;
  key : String;
begin
  if Source is TStrings then
  begin
    s := TStrings(Source);
    Clear;
    Capacity := s.Count;

    for idx := 0 to s.Count - 1 do
    begin
      key := s.Names[idx];
      entry := GetEntry(key);

      if not Assigned(entry) then
        entry := CreateEntry(key);

      entry^.Value := s.ValueFromIndex[idx];
      entry^._Object := s.Objects[idx];
    end;
  end
  else
    inherited;
end;

procedure TStringHashList.AssignTo(Dest: TPersistent);
var
  s : TStrings;
  entry : PStringHashEntry;
  idx : Integer;
begin
  if Dest is TStrings then
  begin
    s := TStrings(Dest);
    s.BeginUpdate;
    try
      s.Clear;

      for idx := Low(FList) to High(FList) do
      begin
        entry := FList[idx];
        while Assigned(entry) do
        begin
          s.AddObject(entry^.Key + s.NameValueSeparator + entry^.Value, entry^._Object);
          entry := entry^.Next;
        end;
      end;

    finally
      s.EndUpdate;
    end;
  end
  else
    inherited;
end;

function TStringHashList.CaseKey(AKey: String): String;
begin
  if FCaseSensitive then
    Result := AKey
  else
    Result := LowerCase(AKey);
end;

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
  FCaseSensitive := true;

  Capacity := 1000;
end;

function TStringHashList.CreateEntry(const AKey: String): PStringHashEntry;
var
  idx : Integer;
begin
  idx := Index(AKey);

  New(Result);
  Result^.Key := CaseKey(AKey);
  Result^.Next := FList[idx];
  Result^._Object := nil;
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

function TStringHashList.GetObjects(AKey: String): TObject;
var
  entry : PStringHashEntry;
begin
  entry := GetEntry(AKey);
  if Assigned(entry) then
    Result := entry^._Object
  else
    Result := nil;
end;

function TStringHashList.GetEntry(AKey: String;
  out AIndex: Integer): PStringHashEntry;
begin
  AIndex := Index(AKey);
  Result := FList[AIndex];
  AKey := CaseKey(AKey);

  while Assigned(Result) and
        (Result^.Key <> AKey) do
    Result := Result^.Next;
end;

function TStringHashList.GetExists(AKey: String): Boolean;
begin
  Result := Assigned(GetEntry(AKey));
end;

function TStringHashList.GetValues(AKey: String): String;
var
  entry : PStringHashEntry;
begin
  entry := GetEntry(AKey);
  if Assigned(entry) then
    Result := entry^.Value
  else
    Result := EmptyStr;
end;

function TStringHashList.Hash(AKey : String): Cardinal;
var
  i, x: Integer;
begin
  AKey := CaseKey(AKey);

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

function TStringHashList.Index(const AKey: String): Cardinal;
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
      newentry^._Object := entry^._Object;

      DeleteEntry(oldList[idx], idx, oldList);
    end;
  end;

end;


procedure TStringHashList.SetObjects(AKey: String; const Value: TObject);
var
  Entry : PStringHashEntry;
begin
  Entry := GetEntry(AKey);

  if Assigned(Entry) then
    Entry^._Object := Value
  else
    CreateEntry(AKey)^._Object := Value;
end;

procedure TStringHashList.SetValues(AKey: String; const Value: String);
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
