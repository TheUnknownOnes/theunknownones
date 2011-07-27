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
    FResizing : Boolean;
    FCount : Integer;
    FCapacity : Integer;
    FList : TStringHashEntryList;
    FMaxFillRatio : Single;
    FMaxFillCount : Integer;
    FMaxFillResizeFactor : Integer;
    FOwnsObjects: Boolean;

    function Hash(const AKey : String) : Cardinal; inline;
    function Index(const AKey : String) : Cardinal; inline;

    function CreateEntry(const AKey : String) : PStringHashEntry;
    function GetEntry(const AKey : String) : PStringHashEntry; overload;
    function GetEntry(const AKey : String; out AIndex : Integer) : PStringHashEntry; overload;
    procedure DeleteEntry(const AKey : String); overload;
    procedure DeleteEntry(AEntry : PStringHashEntry; AIndex : Integer; var AList : TStringHashEntryList; AFreeObjects : Boolean); overload;

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

    procedure SaveToStream(AStream : TStream; AEncoding : TEncoding); overload; virtual;
    procedure SaveToStream(AStream : TStream); overload; virtual;
    procedure SaveToFile(const AFilename : String; AEncoding : TEncoding); overload; virtual;
    procedure SaveToFile(const AFilename : String); overload; virtual;

    procedure LoadFromStream(AStream : TStream; AEncoding : TEncoding); overload; virtual;
    procedure LoadFromStream(AStream : TStream); overload; virtual;
    procedure LoadFromFile(const AFilename : String; AEncoding : TEncoding); overload; virtual;
    procedure LoadFromFile(const AFilename : String); overload; virtual;

    property Count : Integer read FCount;

    property Capacity : Integer read FCapacity write SetCapacity;
    property Values[AKey : String] : String read GetValues write SetValues; default;
    property Objects[AKey : String] : TObject read GetObjects write SetObjects;
    property Exists[AKey : String] : Boolean read GetExists;
    property MaxFillRatio : Single read FMaxFillRatio write FMaxFillRatio;
    property MaxFillResizeFactor : Integer read FMaxFillResizeFactor write FMaxFillResizeFactor;
    property OwnsObjects : Boolean read FOwnsObjects write FOwnsObjects;
  end;

implementation

function IsPrime(Prim: Longint): Boolean;
var
  Z: Real;
  Max: LongInt;
  Divisor: LongInt;
begin
  Result := False;
  if (Prim and 1) = 0 then Exit;
  Z       := Sqrt(Prim);
  Max     := Trunc(Z) + 1;
  Divisor := 3;
  while Max > Divisor do
  begin
    if (Prim mod Divisor) = 0 then Exit;
    Inc(Divisor, 2);
    if (Prim mod Divisor) = 0 then Exit;
    Inc(Divisor, 4);
  end;
  Result := True;
end;

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
    Capacity := Round(s.Count * (1 + FMaxFillRatio));

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

procedure TStringHashList.Clear;
var
  idx : Integer;
begin
  for idx := Low(FList) to High(FList) do
  begin
    while Assigned(FList[idx]) do
      DeleteEntry(FList[idx], idx, FList, FOwnsObjects);
  end;

  FCount := 0;
end;

constructor TStringHashList.Create();
begin
  FCount := 0;
  FMaxFillRatio := 0.65;
  FMaxFillResizeFactor := 2;
  FResizing := false;

  FOwnsObjects := False;

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
    DeleteEntry(entry, idx, FList, FOwnsObjects);
    Dec(FCount);
  end;
end;

procedure TStringHashList.Delete(AKey: String);
begin
  DeleteEntry(AKey);
end;

procedure TStringHashList.DeleteEntry(AEntry: PStringHashEntry; AIndex : Integer; var AList : TStringHashEntryList; AFreeObjects : Boolean);
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

      if AFreeObjects and Assigned(entry._Object) then
        entry._Object.Free;

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

function TStringHashList.Hash(const AKey : String): Cardinal;
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

function TStringHashList.Index(const AKey: String): Cardinal;
begin
  Result := Hash(AKey) mod FCapacity;
end;

procedure TStringHashList.LoadFromStream(AStream: TStream;
  AEncoding: TEncoding);
var
  Buffer : TBytes;
  Size : Integer;
  len : Integer;
  key,
  value : String;
const
  MaxPreambleLength = 1024; //hopefully the preamble wont get longer :-)
begin
  Size := AStream.Size - AStream.Position;
  if Size > MaxPreambleLength then
    Size := MaxPreambleLength;
  SetLength(Buffer, Size);
  AStream.Read(Buffer[0], Size);
  Size := TEncoding.GetBufferEncoding(Buffer, AEncoding);
  AStream.Seek(-Length(Buffer) + Size, soFromCurrent);

  Clear;

  AStream.ReadBuffer(len, Sizeof(len));
  while len < MaxInt do
  begin
    SetLength(Buffer, len);
    AStream.ReadBuffer(buffer[0], len);
    key := AEncoding.GetString(Buffer);

    AStream.ReadBuffer(len, Sizeof(len));
    SetLength(Buffer, len);
    AStream.ReadBuffer(buffer[0], len);
    value := AEncoding.GetString(Buffer);

    Add(key, value);

    AStream.ReadBuffer(len, Sizeof(len));
  end;

end;

procedure TStringHashList.LoadFromFile(const AFilename: String;
  AEncoding: TEncoding);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream, AEncoding);
  finally
    Stream.Free;
  end;
end;

procedure TStringHashList.LoadFromFile(const AFilename: String);
begin
  LoadFromFile(AFilename, nil);
end;

procedure TStringHashList.LoadFromStream(AStream: TStream);
begin
  LoadFromStream(AStream, nil);
end;

procedure TStringHashList.SaveToStream(AStream: TStream; AEncoding: TEncoding);
var
  Buffer : TBytes;
  entry : PStringHashEntry;
  idx : Integer;

  procedure _WriteBuffer;
  var
    bufferlen : Integer;
  begin
    bufferlen := Length(Buffer);
    AStream.Write(bufferlen, sizeof(bufferlen));
    AStream.WriteBuffer(Buffer[0], Length(Buffer));
  end;

begin
  if not Assigned(AEncoding) then
    AEncoding := TEncoding.Default;

  Buffer := AEncoding.GetPreamble;
  AStream.WriteBuffer(Buffer[0], length(Buffer));

  for idx := Low(FList) to High(FList) do
  begin
    entry := FList[idx];

    while Assigned(entry) do
    begin
      Buffer := AEncoding.GetBytes(entry^.Key);
      _WriteBuffer;
      Buffer := AEncoding.GetBytes(entry^.Value);
      _WriteBuffer;

      entry := entry^.Next;
    end;
  end;

  idx := MaxInt;
  AStream.WriteBuffer(idx, SizeOf(idx)); //end-marker
end;

procedure TStringHashList.SaveToFile(const AFilename: String; AEncoding: TEncoding);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(Stream, AEncoding);
  finally
    Stream.Free;
  end;
end;

procedure TStringHashList.SaveToFile(const AFilename: String);
begin
  SaveToFile(AFilename, nil);
end;

procedure TStringHashList.SaveToStream(AStream: TStream);
begin
  SaveToStream(AStream, nil);
end;

procedure TStringHashList.SetCapacity(const Value: Integer);
var
  oldList : TStringHashEntryList;
  idx : Integer;
  entry,
  newentry : PStringHashEntry;
begin
  Assert(Value > 0, 'The capacity has to be greater then 0');

  if FResizing then
    exit;

  FResizing := true;
  try
    oldList := FList;

    for idx := Value downto 1 do
    begin
      if IsPrime(idx) then
      begin
        FCapacity := idx;
        break;
      end;
    end;

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

        DeleteEntry(oldList[idx], idx, oldList, False);
      end;
    end;
  finally
    FResizing := false;
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
