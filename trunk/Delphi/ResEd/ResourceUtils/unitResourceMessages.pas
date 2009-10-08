//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit unitResourceMessages;

interface

uses Windows, Classes, SysUtils, Contnrs, unitResourceElement;

type

//-------------------------------------------------------------------------
// TStringInfo

TStringInfo = class
private
  fId : Integer;
  fUnicode : boolean;
  fST : AnsiString;
public
  constructor Create (const ASt : AnsiString; AId : Integer; AUnicode : boolean);
end;

//-------------------------------------------------------------------------
// TTextResourceElement
//
// Base class for messages & Strings
TTextResourceSort = (trString, trID, trReverseString, trReverseID);
TTextResourceElement = class (TResourceElement)
private
  fStrings : TObjectList;
  fUpdating : boolean;
  fUnicode : Boolean;
  function GetCount: Integer;
  procedure GetStrings;
  function GetString(idx: Integer): AnsiString;
  function GetId(idx: Integer): Integer;
  procedure SetId(idx: Integer; const Value: Integer);
  procedure SetString(idx: Integer; const Value: AnsiString);
protected
  procedure DecodeStrings; virtual; abstract;
  procedure EncodeStrings; virtual; abstract;
public
  destructor Destroy; override;
  procedure ChangeData (newData : TMemoryStream); override;
  procedure BeginUpdate;
  procedure EndUpdate;
  procedure Delete (idx : Integer);
  procedure Sort (sortType : TTextResourceSort = trString);
  function IndexOfID (id : Integer) : Integer;

  property Count : Integer read GetCount;
  property Strings [idx : Integer] : AnsiString read GetString write SetString;
  property Ids [idx : Integer] : Integer read GetId write SetId;

end;

//-------------------------------------------------------------------------
// TMessageResourceElement

TMessageResourceElement = class (TTextResourceElement)
private
protected
  procedure DecodeStrings; override;
  procedure EncodeStrings; override;
  procedure InitNew; override;

public
  class function GetBaseType : AnsiString; override;
end;

//-------------------------------------------------------------------------
// TStringResourceElement

TStringResourceElement = class (TTextResourceElement)
protected
  procedure DecodeStrings; override;
  procedure EncodeStrings; override;
  procedure InitNew; override;
  procedure SetResourceName(const Value: AnsiString); override;

public
  class function GetBaseType : AnsiString; override;
end;

//-------------------------------------------------------------------------
// Global functions declarations

function ResIdToStringsId (const resourceName : AnsiString) : AnsiString;
function StringsIdToResId (const StringsId : AnsiString) : AnsiString;

implementation

type
  TMessageResourceBlock = record
    lowID : DWORD;
    highID : DWORD;
    entryOffset : DWORD   // Offset to entries from the start of the message resource
  end;
  PMessageResourceBlock = ^TMessageResourceBlock;

//-------------------------------------------------------------------------
// Global functions definitions

function ResIdToStringsId (const resourceName : AnsiString) : AnsiString;
begin
  Result := IntToStr ((StrToInt (resourceName) - 1) * 16)
end;

function StringsIdToResId (const StringsId : AnsiString) : AnsiString;
begin
  Result := IntToStr (StrToInt (stringsId) div 16 + 1)
end;
{ TStringResourceElement }

(*----------------------------------------------------------------------*
 | TStringResourceElement.DecodeStrings                                 |
 |                                                                      |
 | Extract Strings from String table into fStrings list                 |
 *----------------------------------------------------------------------*)
procedure TStringResourceElement.DecodeStrings;
var
  p : PWideChar;
  cnt, id : Integer;
  st : AnsiString;
begin
  p := PWideChar (Data.Memory);
  cnt := 0;
  
  while Cnt < 16 do
  begin
    id := (StrToInt (ResourceName) - 1) * 16 + cnt;
    st := ResourceWideCharToStr (p, CodePage);
    fStrings.Add (TStringInfo.Create (st, id, False));
    Inc (Cnt);
  end
end;

(*----------------------------------------------------------------------*
 | TStringResourceElement.EncodeStrings                                 |
 |                                                                      |
 | Encode Strings from fStrings list into String table                  |
 *----------------------------------------------------------------------*)
procedure TStringResourceElement.EncodeStrings;
var
  n, i : Integer;
  p : PWideChar;
begin
                                // Calculate total size of the 16 null-terminated
                                // wide Strings.
  n := 16 * sizeof (WideChar);
  for i := 0 to Count - 1 do
    if i < Count then
      Inc (n, (Length (Strings [i])) * sizeof (WideChar));

  Data.Size := n;
  p := PWideChar (data.Memory);
  ZeroMemory (p, n);

  for i := 0 to Count - 1 do
    ResourceStrToWideChar (Strings [i], p, CodePage);
end;

class function TStringResourceElement.GetBaseType: AnsiString;
begin
  result := IntToStr (Integer (RT_STRING));
end;

procedure TStringResourceElement.InitNew;
var
  i : Integer;
  wc : WideChar;
begin
  wc := #0;
  for i := 0 to 15 do
    data.Write (wc, SizeOf (wc))
end;

procedure TStringResourceElement.SetResourceName(const Value: AnsiString);
var
  i, id : Integer;

begin
  inherited;

  id := (StrToInt (Value) - 1) * 16;

  for i := 0 to Count - 1 do
    with TStringInfo (fStrings [i]) do
      fId := id + i
end;

{ TMessageResourceElement


zur Verwendung

var
  buf: AnsiString;
begin
FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER or
              FORMAT_MESSAGE_IGNORE_INSERTS or
              FORMAT_MESSAGE_FROM_SYSTEM or
              FORMAT_MESSAGE_FROM_HMODULE,
              NIL,1,LANG_NEUTRAL, PAnsiChar(@buf), 0, nil);
Memo1.Text:=buf;
end;  
}

procedure TMessageResourceElement.DecodeStrings;
var
  i, blockCount, id : Integer;
  block : PMessageResourceBlock;
  p : PAnsiChar;
  len, flags : word;
  gotUnicode : Boolean;
begin
  blockCount := PInteger (Data.Memory)^;

  block := PMessageResourceBlock (PAnsiChar (Data.Memory) + sizeof (Integer));

  gotUnicode := False;
  for i := 0 to blockCount - 1 do
  begin
    id := block^.lowID;
    p := data.memory;
    Inc (p, block^.entryOffset);              // 'p' points to the block's messages

    while id <= Integer (block^.highID) do
    begin
      len := PWORD (p)^;
      Inc (p, sizeof (WORD));
      Dec (len, sizeof (WORD));

      flags := PWORD (p)^;
      Inc (p, sizeof (WORD));
      Dec (len, sizeof (WORD));

      if not gotUnicode then
      begin
        fUnicode := (flags and 1) = 1;
        gotUnicode := True
      end;

      if (flags and 1) = 1 then
        fStrings.Add (TStringInfo.Create (PWideChar (p), id, True))
      else
        fStrings.Add (TStringInfo.Create (p, id, False));

      Inc (p, len);
      Inc (id)
    end;
    Inc (block)
  end
end;

procedure TMessageResourceElement.EncodeStrings;
var
  i, id, lastId, dataSize, blockCount, len : Integer;
  block : PMessageResourceBlock;
  offset : DWORD;
  ws : WideString;
  uniCode : word;
begin
  dataSize := sizeof (Integer);
  lastId := -2;
  blockCount := 0;
  for i := 0 to fStrings.Count - 1 do   // Count non-continuous blocks & calculate total size
  begin
    id := TStringInfo (fStrings [i]).fId;
    uniCode := Ord (TStringInfo (fStrings [i]).fUnicode);
    if id <> lastId + 1 then
    begin
      Inc (blockCount);
      Inc (dataSize, SizeOf (TMessageResourceBlock));
    end;

    lastId := id;

    len := Length (Strings [i]) + 1;
    if unicode <> 0 then
      len := len * 2;

    len := (len + 3) div 4 * 4;         // DWORD align

    Inc (dataSize, 2 * sizeof (word) + len)
  end;

  data.Size := dataSize;
  PInteger (data.Memory)^ := blockCount;
  offset := sizeof (Integer) + blockCount * sizeof (TMessageResourceBlock);
  blockCount := 0;
  block := Nil;

  lastId := -2;
  for i := 0 to fStrings.Count - 1 do
  begin
    id := TStringInfo (fStrings [i]).fId;
    uniCode := Ord (TStringInfo (fStrings [i]).fUnicode);
    if id <> lastId + 1 then
    begin
      if lastId <> -2 then
        block^.highID := lastId;
      block := PMessageResourceBlock (PAnsiChar (data.Memory) + sizeof (Integer));
      Inc (block, blockCount);
      block^.lowID := id;
      block^.entryOffset := offset;
      Inc (blockCount)
    end;

    lastId := id;
    len := Length (Strings [i]) + 1;
    if unicode <> 0 then
      len := len * 2;

    len := (len + 3) div 4 * 4;

    PWORD (PAnsiChar (data.Memory) + offset)^ := 2 * sizeof (word) + len;
    Inc (offset, sizeof (WORD));

    PWORD (PAnsiChar (data.Memory) + offset)^ := uniCode;
    Inc (offset, sizeof (WORD));

    ZeroMemory (PAnsiChar (data.Memory) + offset, len);
    if uniCode = 0 then
      lstrcpyA (PAnsiChar (data.Memory) + offset, PAnsiChar (Strings [i]))
    else
    begin
      ws := Strings [i];
      lstrcpyw (PWideChar (PAnsiChar (data.Memory) + offset), PWideChar (ws));
    end;
    Inc (offset, len)
  end;
  if lastId <> -2 then
    block^.highID := lastId;
end;

class function TMessageResourceElement.GetBaseType: AnsiString;
begin
  result := IntToStr (Integer (RT_MESSAGETABLE));
end;

procedure TMessageResourceElement.InitNew;
const
  zero : Integer = 0;
begin
  data.Write (zero, SizeOf (zero));
end;

{ TTextResourceElement }

procedure TTextResourceElement.BeginUpdate;
begin
  fUpdating := True;
end;

procedure TTextResourceElement.ChangeData(newData: TMemoryStream);
begin
  inherited;

  FreeAndNil (fStrings)
end;

procedure TTextResourceElement.Delete(idx: Integer);
begin
  if idx < fStrings.Count then
  begin
    fStrings.Delete(idx);

    if not fUpdating then
      EncodeStrings
  end
end;

destructor TTextResourceElement.Destroy;
begin
  fStrings.Free;
  inherited;
end;

procedure TTextResourceElement.EndUpdate;
begin
  if fUpdating then
  begin
    fUpdating := False;
    EncodeStrings
  end
end;

function TTextResourceElement.GetCount: Integer;
begin
  GetStrings;
  result := fStrings.Count
end;

function TTextResourceElement.GetId(idx: Integer): Integer;
begin
  GetStrings;
  result := TStringInfo (fStrings [idx]).fID;
end;

function TTextResourceElement.GetString(idx: Integer): AnsiString;
begin
  GetStrings;
  result := TStringInfo (fStrings [idx]).fST;
end;

procedure TTextResourceElement.GetStrings;
begin  
  if not Assigned (fStrings) then
  begin
    fStrings := TObjectList.Create;
    DecodeStrings;
  end;
end;


function TTextResourceElement.IndexOfID(id: Integer): Integer;
var
  i : Integer;
begin
  Result := -1;
  if Assigned (fStrings) then
    for i := 0 to fStrings.Count - 1 do
      if TStringInfo (fStrings [i]).fId = id then
      begin
        Result := i;
        break
      end
end;

procedure TTextResourceElement.SetId(idx: Integer; const Value: Integer);
begin
  with TStringInfo (fStrings [idx]) do
    fId := Value;

  if not fUpdating then
    EncodeStrings
end;

procedure TTextResourceElement.SetString(idx: Integer;
  const Value: AnsiString);
begin
  if idx = fStrings.Count then
    fStrings.Add (TStringInfo.Create (Value, idx, fUnicode))
  else
    TStringInfo (fStrings [idx]).fSt := Value;

  if not fUpdating then
    EncodeStrings
end;

function CompareStringInfo (p1, p2 : Pointer) : Integer;
begin
  Result := CompareText (TStringInfo (p1).fST, TStringInfo (p2).fST)
end;

function ReverseCompareStringInfo (p1, p2 : Pointer) : Integer;
begin
  Result := -CompareText (TStringInfo (p1).fST, TStringInfo (p2).fST)
end;

function CompareIDS (p1, p2 : Pointer) : Integer;
begin
  Result := TStringInfo (p1).fId - TStringInfo (p2).fId
end;

function ReverseCompareIDS (p1, p2 : Pointer) : Integer;
begin
  Result := TStringInfo (p2).fId - TStringInfo (p1).fId
end;

procedure TTextResourceElement.Sort (sortType : TTextResourceSort);
begin
  case sortType of
    trString        : fStrings.Sort(CompareStringInfo);
    trID            : fStrings.Sort(CompareIDS);
    trReverseString : fStrings.Sort(ReverseCompareStringInfo);
    trReverseID     : fStrings.Sort(ReverseCompareIDS)
  end
end;

{ TStringInfo }

constructor TStringInfo.Create(const ASt : AnsiString; AId: Integer; AUnicode : boolean);
begin
  fSt := ASt;
  fID := AId;
  fUnicode := AUnicode
end;

initialization
  RegisterResourceElement (TMessageResourceElement);
  RegisterResourceElement (TStringResourceElement);
finalization
  UnregisterResourceElement (TStringResourceElement);
  UnregisterResourceElement (TMessageResourceElement);
end.
