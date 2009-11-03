unit HVVMT;
// Written by Hallvard Vassbotn, 2006 - http://hallvards.blogspot.com/
// Currently assumes D7-D2006 (*probably* works in D5 and D6)
interface

type
  PObject = ^TObject;
  PClass = ^TClass;

  // TObject virtual methods' signatures
  PSafeCallException = function  (Self: TObject; ExceptObject: TObject;
                         ExceptAddr: Pointer): HResult;
  PAfterConstruction = procedure (Self: TObject);
  PBeforeDestruction = procedure (Self: TObject);
  PDispatch          = procedure (Self: TObject; var Message);
  PDefaultHandler    = procedure (Self: TObject; var Message);
  PNewInstance       = function  (Self: TClass) : TObject;
  PFreeInstance      = procedure (Self: TObject);
  PDestroy           = procedure (Self: TObject; OuterMost: ShortInt);

  // Dynamic methods table
  TDMTIndex   = Smallint;
  PDmtIndices = ^TDmtIndices;
  TDmtIndices = array[0..High(Word)-1] of TDMTIndex;
  PDmtMethods = ^TDmtMethods;
  TDmtMethods = array[0..High(Word)-1] of Pointer;
  PDmt = ^TDmt;
  TDmt = packed record
    Count: word;
    Indicies: TDmtIndices; // really [0..Count-1]
    Methods : TDmtMethods; // really [0..Count-1]
  end;


  // Published methods table
  PPublishedMethod = ^TPublishedMethod;
  TPublishedMethod = packed record
    Size: word;
    Address: Pointer;
    Name: {packed} ShortString;
  end;
  TPublishedMethods = packed array[0..High(Word)-1] of TPublishedMethod;
  PPmt = ^TPmt;
  TPmt = packed record
    Count: Word;
    Methods: TPublishedMethods; // really [0..Count-1]
  end;

  // Published fields table
  PPublishedField = ^TPublishedField;
  TPublishedField = packed record
    Offset: Integer;
    TypeIndex: word;  // Index into the FieldTypes array below
    Name: {packed} Shortstring; // really string[Length(Name)]
  end;
  PPublishedFieldTypes = ^TPublishedFieldTypes;
  TPublishedFieldTypes = packed record
    TypeCount: word;
    Types: array[0..High(Word)-1] of PClass; // really [0..TypeCount-1]
  end;
  TPublishedFields = packed array[0..High(Word)-1] of TPublishedField;
  PPft = ^TPft;
  TPft = packed record
    Count: Word;
    FieldTypes: PPublishedFieldTypes;
    Fields: TPublishedFields; // really [0..Count-1]
  end;

  // Virtual method table
  PVmt = ^TVmt;
  TVmt = packed record
    SelfPtr           : TClass;
    IntfTable         : Pointer; 
    AutoTable         : Pointer;
    InitTable         : Pointer;
    TypeInfo          : Pointer;
    FieldTable        : PPft;
    MethodTable       : PPmt;
    DynamicTable      : PDmt;
    ClassName         : PShortString;
    InstanceSize      : PLongint; 
    Parent            : PClass;
    SafeCallException : PSafeCallException;
    AfterConstruction : PAfterConstruction;
    BeforeDestruction : PBeforeDestruction;
    Dispatch          : PDispatch;
    DefaultHandler    : PDefaultHandler;
    NewInstance       : PNewInstance;
    FreeInstance      : PFreeInstance;
    Destroy           : PDestroy;
   {UserDefinedVirtuals: array[0..999] of procedure;}
  end;

// Virtual method table
function GetVmt(AClass: TClass): PVmt;
 
// Published methods 
function GetPmt(AClass: TClass): PPmt;
function GetPublishedMethodCount(AClass: TClass): integer;
function GetPublishedMethod(AClass: TClass; Index: integer): PPublishedMethod;
function GetFirstPublishedMethod(AClass: TClass): PPublishedMethod;
function GetNextPublishedMethod(AClass: TClass; PublishedMethod: PPublishedMethod): PPublishedMethod;
function FindPublishedMethodByName(AClass: TClass; const AName: ShortString): PPublishedMethod;
function FindPublishedMethodByAddr(AClass: TClass; AAddr: Pointer): PPublishedMethod;
function FindPublishedMethodAddr(AClass: TClass; const AName: ShortString): Pointer;
function FindPublishedMethodName(AClass: TClass; AAddr: Pointer): Shortstring;

// Published fields 
function GetPft(AClass: TClass): PPft;
function GetPublishedFieldCount(AClass: TClass): integer;
function GetNextPublishedField(AClass: TClass;
  PublishedField: PPublishedField): PPublishedField;
function GetPublishedField(AClass: TClass; TypeIndex: integer): PPublishedField;
function GetFirstPublishedField(AClass: TClass): PPublishedField;
function FindPublishedFieldByName(AClass: TClass; const AName: ShortString): PPublishedField;
function FindPublishedFieldByOffset(AClass: TClass; AOffset: Integer): PPublishedField;
function FindPublishedFieldByAddr(Instance: TObject; AAddr: Pointer): PPublishedField;
function FindPublishedFieldOffset(AClass: TClass; const AName: ShortString): integer;
function FindPublishedFieldAddr(Instance: TObject; const AName: ShortString): PObject;
function FindPublishedFieldName(AClass: TClass; AOffset: integer): Shortstring; overload;
function FindPublishedFieldName(Instance: TObject; AAddr: Pointer): Shortstring; overload;
function GetPublishedFieldType(AClass: TClass; Field: PPublishedField): TClass;
function GetPublishedFieldAddr(Instance: TObject; Field: PPublishedField): PObject;
function GetPublishedFieldValue(Instance: TObject; Field: PPublishedField): TObject;

implementation

uses
  Classes,
  SysUtils,
  TypInfo;
  
// Virtual method table

function GetVmt(AClass: TClass): PVmt;
begin
  Result := PVmt(AClass);
  Dec(Result);
end;
 
// Published methods 

function GetPmt(AClass: TClass): PPmt;
var
  Vmt: PVmt;
begin
  Vmt := GetVmt(AClass);
  if Assigned(Vmt)
  then Result := Vmt.MethodTable
  else Result := nil;
end;
 
function GetPublishedMethodCount(AClass: TClass): integer;
var
  Pmt: PPmt;
begin
  Pmt := GetPmt(AClass);
  if Assigned(Pmt)
  then Result := Pmt.Count
  else Result := 0;
end;
 
function GetPublishedMethod(AClass: TClass; Index: integer): PPublishedMethod;
var
  Pmt: PPmt;
begin
  Pmt := GetPmt(AClass);
  if Assigned(Pmt) and (Index < Pmt.Count) then
  begin
    Result := @Pmt.Methods[0];
    while Index > 0 do
    begin
      Inc(PChar(Result), Result.Size);
      Dec(Index);
    end;
  end
  else
    Result := nil;
end;
 
function GetFirstPublishedMethod(AClass: TClass): PPublishedMethod;
begin
  Result := GetPublishedMethod(AClass, 0);
end;
{.$DEFINE DEBUG}
function GetNextPublishedMethod(AClass: TClass; 
  PublishedMethod: PPublishedMethod): PPublishedMethod;
// Note: Caller is responsible for calling this the 
// correct number of times (using GetPublishedMethodCount)
{$IFDEF DEBUG}
var
  ExpectedSize: integer;
{$ENDIF}
begin
  Result := PublishedMethod;
{$IFDEF DEBUG}
  ExpectedSize :=   SizeOf(Result.Size) 
                  + SizeOf(Result.Address) 
                  + SizeOf(Result.Name[0]) 
                  + Length(Result.Name);
  if Result.Size <> ExpectedSize then
    raise Exception.CreateFmt(
'RTTI for the published method "%s" of class "%s" has %d extra bytes of unknown data!', 
[Result.Name, AClass.ClassName, Result.Size-ExpectedSize]);
{$ENDIF}
  if Assigned(Result) then
    Inc(PChar(Result), Result.Size);
end;
 
function FindPublishedMethodByName(AClass: TClass; const AName: ShortString): PPublishedMethod;
var
  i : integer;
begin
  while Assigned(AClass) do
  begin
    Result := GetFirstPublishedMethod(AClass);
    for i := 0 to GetPublishedMethodCount(AClass)-1 do
    begin
      // Note: Length(ShortString) expands to efficient inline code
      if (Length(Result.Name) = Length(AName)) and
         (StrLIComp(PAnsiChar(@Result.Name[1]), @AName[1], Length(AName)) = 0) then
        Exit;
      Result := GetNextPublishedMethod(AClass, Result);
    end;
    AClass := AClass.ClassParent;
  end;
  Result := nil;
end;
 
function FindPublishedMethodByAddr(AClass: TClass; AAddr: Pointer): PPublishedMethod;
var
  i : integer;
begin
  while Assigned(AClass) do
  begin
    Result := GetFirstPublishedMethod(AClass);
    for i := 0 to GetPublishedMethodCount(AClass)-1 do
    begin
      if Result.Address = AAddr then
        Exit;
      Result := GetNextPublishedMethod(AClass, Result);
    end;
    AClass := AClass.ClassParent;
  end;
  Result := nil;
end;
 
function FindPublishedMethodAddr(AClass: TClass; const AName: ShortString): Pointer;
var
  Method: PPublishedMethod;
begin
  Method := FindPublishedMethodByName(AClass, AName);
  if Assigned(Method)
  then Result := Method.Address
  else Result := nil;
end;
 
function FindPublishedMethodName(AClass: TClass; AAddr: Pointer): Shortstring;
var
  Method: PPublishedMethod;
begin
  Method := FindPublishedMethodByAddr(AClass, AAddr);
  if Assigned(Method)
  then Result := Method.Name
  else Result := '';
end;

// Published fields 

function GetPft(AClass: TClass): PPft;
var
  Vmt: PVmt;
begin
  Vmt := GetVmt(AClass);
  if Assigned(Vmt)
  then Result := Vmt.FieldTable
  else Result := nil;
end;
 
function GetPublishedFieldCount(AClass: TClass): integer;
var
  Pft: PPft;
begin
  Pft := GetPft(AClass);
  if Assigned(Pft)
  then Result := Pft.Count
  else Result := 0;
end;

function GetNextPublishedField(AClass: TClass;
  PublishedField: PPublishedField): PPublishedField;
// Note: Caller is responsible for calling this the
// correct number of times (using GetPublishedFieldCount)
begin
  Result := PublishedField;
  if Assigned(Result) then
    Inc(PChar(Result),   SizeOf(Result.Offset)
                       + SizeOf(Result.TypeIndex)
                       + SizeOf(Result.Name[0])
                       + Length(Result.Name));
end;

function GetPublishedField(AClass: TClass; TypeIndex: integer): PPublishedField;
var
  Pft: PPft;
begin
  Pft := GetPft(AClass);
  if Assigned(Pft) and (TypeIndex < Pft.Count) then
  begin
    Result := @Pft.Fields[0];
    while TypeIndex > 0 do
    begin
      Result := GetNextPublishedField(AClass, Result);
      Dec(TypeIndex);
    end;
  end
  else
    Result := nil;
end;

function GetFirstPublishedField(AClass: TClass): PPublishedField;
begin
  Result := GetPublishedField(AClass, 0);
end;

function FindPublishedFieldByName(AClass: TClass; const AName: ShortString): PPublishedField;
var
  i : integer;
begin
  while Assigned(AClass) do
  begin
    Result := GetFirstPublishedField(AClass);
    for i := 0 to GetPublishedFieldCount(AClass)-1 do
    begin
      // Note: Length(ShortString) expands to efficient inline code
      if (Length(Result.Name) = Length(AName)) and
         (StrLIComp(PAnsiChar(@Result.Name[1]), @AName[1], Length(AName)) = 0) then
        Exit;
      Result := GetNextPublishedField(AClass, Result);
    end;
    AClass := AClass.ClassParent;
  end;
  Result := nil;
end;

function FindPublishedFieldByOffset(AClass: TClass; AOffset: Integer): PPublishedField;
var
  i : integer;
begin
  while Assigned(AClass) do
  begin
    Result := GetFirstPublishedField(AClass);
    for i := 0 to GetPublishedFieldCount(AClass)-1 do
    begin
      if Result.Offset = AOffset then
        Exit;
      Result := GetNextPublishedField(AClass, Result);
    end;
    AClass := AClass.ClassParent;
  end;
  Result := nil;
end;

function FindPublishedFieldByAddr(Instance: TObject; AAddr: Pointer): PPublishedField;
begin
  Result := FindPublishedFieldByOffset(Instance.ClassType, PChar(AAddr) - PChar(Instance));
end;

function FindPublishedFieldOffset(AClass: TClass; const AName: ShortString): integer;
var
  Field: PPublishedField;
begin
  Field := FindPublishedFieldByName(AClass, AName);
  if Assigned(Field)
  then Result := Field.Offset
  else Result := -1;
end;

function FindPublishedFieldAddr(Instance: TObject; const AName: ShortString): PObject;
var
  Offset: integer;
begin
  Offset := FindPublishedFieldOffset(Instance.ClassType, AName);
  if Offset >= 0
  then Result := PObject(PChar(Instance) + Offset)
  else Result := nil;
end;

function FindPublishedFieldName(AClass: TClass; AOffset: integer): Shortstring; overload;
var
  Field: PPublishedField;
begin
  Field := FindPublishedFieldByOffset(AClass, AOffset);
  if Assigned(Field)
  then Result := Field.Name
  else Result := '';
end;

function FindPublishedFieldName(Instance: TObject; AAddr: Pointer): Shortstring; overload;
var
  Field: PPublishedField;
begin
  Field := FindPublishedFieldByAddr(Instance, AAddr);
  if Assigned(Field)
  then Result := Field.Name
  else Result := '';
end;

function GetPublishedFieldType(AClass: TClass; Field: PPublishedField): TClass;
var
  Pft: PPft;
begin
  Pft := GetPft(AClass);
  if Assigned(Pft) and Assigned(Field) and (Field.TypeIndex < Pft.FieldTypes.TypeCount)
  then Result := Pft.FieldTypes.Types[Field.TypeIndex]^
  else Result := nil;
end;

function GetPublishedFieldAddr(Instance: TObject; Field: PPublishedField): PObject;
begin
  if Assigned(Field)
  then Result := PObject(PChar(Instance) + Field.Offset)
  else Result := nil;
end;

function GetPublishedFieldValue(Instance: TObject; Field: PPublishedField): TObject;
var
  FieldAddr: PObject;
begin
  FieldAddr := GetPublishedFieldAddr(Instance, Field);
  if Assigned(FieldAddr)
  then Result := FieldAddr^
  else Result := nil;
end;

end.

