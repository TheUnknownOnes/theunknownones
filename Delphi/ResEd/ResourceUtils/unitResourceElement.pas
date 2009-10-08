(*======================================================================*
 | unitResourceElement                                                  |
 |                                                                      |
 | Classes to wrap resources and resource modules.                      |
 |                                                                      |
 | TResourceModule is an abstract base class for things that can        |
 | provide lists of resources - eg. .RES files, modules, etc.           |
 |                                                                      |
 | TResourceElement is a base class for resources.                      |
 |                                                                      |
 | Call the class function TResourceElement.CreateResourceElement to    |
 | create an instance of the appropriate registered TResourceElement    |
 | descendant                                                           | 
 |                                                                      |
 | Resource Utils are partly based on the work of Colin Wilson          |
 | It was thoroughly reworked, bugfixed and extended for dotNet support |
 |                                                                      |
 |                                                                      |
 |                                                                      |
 *======================================================================*)


unit unitResourceElement;

interface

uses Windows, Classes, SysUtils, Dialogs;

type

TResourceElement = class;
TResourceElementClass = class of TResourceElement;

{$region 'TResourceModule class'}
//======================================================================
// TResourceModule class

TResourceModule = class
private
  fDirty : Boolean;
  function GetDirty: Boolean;
protected
  function GetResourceCount: Integer; virtual; abstract;
  function GetResourceElement(idx: Integer): TResourceElement; virtual; abstract;
  procedure ClearDirty;

public
  procedure DeleteResource (idx : Integer); virtual;
  procedure InsertResource (idx : Integer; Element : TResourceElement); virtual;
  function AddResource (Element : TResourceElement) : Integer; virtual;
  function IndexOfResource (Element : TResourceElement) : Integer; virtual; abstract;
  function GetUniqueResourceName (const tp : AnsiString) : AnsiString;

  procedure SaveToStream (stream : TStream); virtual;
  procedure LoadFromStream (stream : TStream); virtual;

  procedure SaveToFile (const FileName : AnsiString); virtual;
  procedure LoadFromFile (const FileName : AnsiString); virtual;
  procedure SortResources; virtual;

  function FindResource (const tp, Name : AnsiString; ALanguage : Integer) : TResourceElement;

  property ResourceCount : Integer read GetResourceCount;
  property ResourceElement [idx : Integer] : TResourceElement read GetResourceElement;
  property Dirty : Boolean read GetDirty write fDirty;
end;

{$endregion}

{$region 'TResourceElement class'}
//======================================================================
// TResourceElement class

TResourceElement = class
private
  fParent : TResourceModule;
  fData : TMemoryStream;
  fCodePage : Integer;
  fResourceLanguage: LCID;
  fResourceName: AnsiString;
  fResourceType: AnsiString;

  fMemoryFlags : word;                    // Resource memory flags
  fDataVersion, fVersion : DWORD;         // Resource header version info
  fCharacteristics : DWORD;
  fDirty : Boolean;
  fTag: Integer;
                                         // Resource header characteristics

protected
  constructor Create (AParent : TResourceModule; ALanguage : Integer; const AName, AType : AnsiString; ASize : Integer; AData : pointer); virtual;
  procedure InitNew; virtual;
  procedure SetResourceName(const Value: AnsiString); virtual;
  class function SupportsRCData (const AName : AnsiString; Size : Integer; data : Pointer) : Boolean; virtual;
  class function SupportsData (Size : Integer; data : Pointer) : Boolean; virtual;
public
  class function CreateResourceElement (AParent : TResourceModule; ALanguage : Integer; const AName, AType : AnsiString; ASize : Integer; AData : pointer) : TResourceElement;
  class function GetBaseType : AnsiString; virtual;

  constructor CreateNew (AParent : TResourceModule; ALanguage : Integer; const AName : AnsiString); overload;
  constructor CreateNew (AParent : TResourceModule; ALanguage : Integer; const AName, AType : AnsiString); overload;
  destructor Destroy; override;
  procedure BeforeDelete; virtual;

  procedure ChangeData (newData : TMemoryStream); virtual;

  property Parent : TResourceModule read fParent;
  property Data : TMemoryStream read fData;
  property ResourceName : AnsiString read fResourceName write SetResourceName;
  property ResourceType : AnsiString read fResourceType;
  property ResourceLanguage : LCID read fResourceLanguage write fResourceLanguage;

  property CodePage : Integer read fCodePage write fCodePage;
  property Characteristics : DWORD read fCharacteristics write fCharacteristics;
  property Version : DWORD read fVersion write fDataVersion;
  property DataVersion : DWORD read fDataVersion write fDataVersion;
  property MemoryFlags : WORD read fMemoryFlags write fMemoryFlags;

  property Dirty : Boolean read fDirty write fDirty;
  property Tag : Integer read fTag write fTag;
end;
{$endregion}

{$region 'TAnsiResourceElement class'}
//======================================================================
// TAnsiResourceElement class

TAnsiResourceElement = class (TResourceElement)
private
  function GetText: AnsiString;
  procedure SetText(const Value: AnsiString);
protected
  procedure InitNew; override;
  class function SupportsData (Size : Integer; data : Pointer) : Boolean; override;
public
  property Text : AnsiString read GetText write SetText;
end;
{$endregion}

{$region 'TUnicodeResourceElement'}
//======================================================================
// TAnsiResourceElement class

TUnicodeResourceElement = class (TResourceElement)
private
  function GetText: WideString;
  procedure SetText(const Value: WideString);
protected
  procedure InitNew; override;
  class function SupportsData (Size : Integer; data : Pointer) : Boolean; override;
public
  property Text : WideString read GetText write SetText;
end;
{$endregion}

//======================================================================
// Global function definitions

procedure RegisterResourceElement (resourceClass : TResourceElementClass);
procedure UnRegisterResourceElement (resourceClass : TResourceElementClass);
function ResourceWideCharToStr(var wstr : PWideChar; codePage : Integer) : AnsiString;
procedure ResourceStrToWideChar (const s : AnsiString; var p : PWideChar; codePage : Integer);
function ResourceNameToInt (const s : AnsiString) : Integer;
function CompareElement (p1, p2 : Pointer) : Integer;

implementation

{$region 'Local Declarations and Functions'}
var
  registeredResourceElement : array of TResourceElementClass;
  registeredResourceElementCount : Integer = 0;

resourcestring
  rstNoBaseType = 'Can''t register resource Element class with no base type';
  rstNoStreaming = 'Module doesn''t support streaming';

(*----------------------------------------------------------------------*
 | procedure RegisterResourceElement                                    |
 |                                                                      |
 | Add a class, derived from TResourceElement, to the list of           |
 | registered resource Element classes                                  |
 *----------------------------------------------------------------------*)
procedure RegisterResourceElement (resourceClass : TResourceElementClass);
begin
  if Length (registeredResourceElement) = registeredResourceElementCount then
    SetLength (registeredResourceElement, Length (registeredResourceElement) + 10);

  registeredResourceElement [registeredResourceElementCount] := resourceClass;

  Inc (registeredResourceElementCount)
end;

(*----------------------------------------------------------------------*
 | procedure UnRegisterResourceElement                                  |
 |                                                                      |
 | Remove a class, derived from TResourceElement, from the list of      |
 | registered resource Element classes                                  |
 *----------------------------------------------------------------------*)
procedure UnRegisterResourceElement (resourceClass : TResourceElementClass);
var
  i : Integer;
begin
  i := 0;
  while i < registeredResourceElementCount do
    if registeredResourceElement [i] = resourceClass then
    begin
      if i < Length (registeredResourceElement) - 1 then
        Move (registeredResourceElement [i + 1], registeredResourceElement [i], (Length (registeredResourceElement) - i - 1) * sizeof (TResourceElementClass));

      Dec (registeredResourceElementCount)
    end
    else
      Inc (i)
end;

(*----------------------------------------------------------------------------*
 | procedure ResourceWideCharToStr ()                                         |
 |                                                                            |
 | Convert Pascal-style WideChar array to a AnsiString                            |
 |                                                                            |
 | Parameters:                                                                |
 |   WStr : PWChar             The characters                                 |
 *----------------------------------------------------------------------------*)
function ResourceWideCharToStr(var wstr : PWideChar; codePage : Integer) : AnsiString;
var
  len : word;
begin
  len := word (wstr^);
  SetLength (result, len);
  Inc (wstr);
  WideCharToMultiByte(codePage, 0, WStr, Len, PAnsiChar (Result), Len + 1, nil, nil);
  Inc (wstr, len);
  result := PAnsiChar (result);
end;

(*----------------------------------------------------------------------------*
 | procedure ResourceStrToWideChar ()                                         |
 |                                                                            |
 | Convert a AnsiString to a Pascal style Wide char array                         |
 |                                                                            |
 | Parameters:                                                                |
 |   s : AnsiString                The AnsiString                                     |
 |   var p : PWideChar         [in]  Points to the start of the receiving buf |
 |                             [out] Points after the characters.             |
 *----------------------------------------------------------------------------*)
procedure ResourceStrToWideChar (const s : AnsiString; var p : PWideChar; codePage : Integer);
var
  buffer : PWideChar;
  len, size : word;
begin
  len := Length (s);
  size := (Length (s) + 1) * sizeof (WideChar);
  GetMem (buffer, size);
  try
    MultiByteToWideChar (codePage, 0, PAnsiChar (s), -1, buffer, size);
    p^ := WideChar (len);
    Inc (p);
    Move (buffer^, p^, len * sizeof (WideChar));
    Inc (p, len)
  finally
    FreeMem (buffer)
  end
end;

(*----------------------------------------------------------------------*
 | procedure ResourceNameToInt                                          |
 |                                                                      |
 | Get integer value of resource name (or type).  Return -1 if it's     |
 | not numeric.                                                         |
 *----------------------------------------------------------------------*)
function ResourceNameToInt (const s : AnsiString) : Integer;
var
  isNumeric : Boolean;
  i : Integer;
begin
  isNumeric := Length (s) > 0;
  for i := 1 to Length (s) do
    if not (s [i] in ['0'..'9']) then
    begin
      isNumeric := False;
      break
    end;

  if isNumeric then
    Result := StrToInt (s)
  else
    Result := -1
end;

(*----------------------------------------------------------------------*
 | function CompareElement                                              |
 |                                                                      |
 | 'Compare' function used when sorting resources.  p1 and p2 must be   |
 | TResourceElement references.  Returns > 0 if Element at p1 are >     |
 | Element at p2.                                                       |
 |                                                                      |
 | *  Compare resource types.  If they match then compare names.        |
 | *  'Integer' ids or names must come *after* non integer ids or names.|
 *----------------------------------------------------------------------*)
function CompareElement (p1, p2 : Pointer) : Integer;
var
  d1 : TResourceElement;
  d2 : TResourceElement;
  i1, i2 : Integer;
begin
  d1 := TResourceElement (p1);
  d2 := TResourceElement (p2);

  i1 := ResourceNameToInt (d1.ResourceType);
  i2 := ResourceNameToInt (d2.ResourceType);

  if i1 >= 0 then
    if i2 >= 0 then
      Result := i1 - i2         // Compare two integer ids
    else
      Result := 1               // id1 is int, so it's greater than non-int id2
  else
    if i2 >= 0 then
      Result := -1              // id2 is int, so it's less than non-int id1
    else
                                // Compare two AnsiString resource ids
      Result := CompareText (d1.ResourceType, d2.ResourceType);

  if Result = 0 then            // If they match, do the same with the names
  begin
    i1 := ResourceNameToInt (d1.ResourceName);
    i2 := ResourceNameToInt (d2.ResourceName);

    if i1 >= 0 then
      if i2 >= 0 then
        Result := i1 - i2
      else
        Result := 1
    else
      if i2 >= 0 then
        Result := -1
      else
        Result := CompareText (d1.ResourceName, d2.ResourceName)
  end
end;

(*----------------------------------------------------------------------*
 | function LCIDTOCodePage                                              |
 |                                                                      |
 | Get the ANSI code page for a given language ID                       |
 *----------------------------------------------------------------------*)
function LCIDToCodePage(ALcid: LCID): Integer;
var
  Buffer: array [0..6] of Char;
begin
  GetLocaleInfo(ALcid, LOCALE_IDEFAULTANSICODEPAGE, Buffer, SizeOf(Buffer));
  Result:= StrToIntDef(Buffer, GetACP);
end;

{$endregion}

{$region 'TResourceElement implementation'}
{ TResourceElement }

(*----------------------------------------------------------------------*
 | TResourceElement.BeforeDelete                                        |
 |                                                                      |
 | Can override this to clear up before deleting.  Eg. deleting an      |
 | icon removes it from the icon group it's in.  Deleting an icon group |
 | removes the individual icon resources, etc.                          |
 *----------------------------------------------------------------------*)
procedure TResourceElement.BeforeDelete;
begin
  // Stub
end;

(*----------------------------------------------------------------------*
 | TResourceElement.ChangeData                                          |
 |                                                                      |
 | Change all the data.  Handy for implementing 'undo', etc.            |
 *----------------------------------------------------------------------*)
procedure TResourceElement.ChangeData(newData: TMemoryStream);
begin
  fData.Clear;
  fData.CopyFrom (newData, 0);
end;

(*----------------------------------------------------------------------*
 | TResourceElement.Create                                              |
 |                                                                      |
 | Raw - protected - constructor for resource Element.                  |
 *----------------------------------------------------------------------*)
constructor TResourceElement.Create(AParent: TResourceModule; ALanguage: Integer; const AName, AType: AnsiString; ASize: Integer;
  AData: pointer);
begin
  fParent := AParent;
  fResourceLanguage := ALanguage;
  fCodePage := LCIDToCodePage (fResourceLanguage);
  fResourceName := AName;
  fResourceType := AType;
  fData := TMemoryStream.Create;
  if AData <> Nil then
    fData.Write (AData^, ASize)
  else
    InitNew
end;

(*----------------------------------------------------------------------*
 | TResourceElement.CreateNew                                           |
 |                                                                      |
 | Constructor to be used when adding new resources to a module.        |
 *----------------------------------------------------------------------*)
constructor TResourceElement.CreateNew(AParent: TResourceModule;
  ALanguage: Integer; const aName : AnsiString);
begin
  CreateNew(AParent,ALanguage,AName,GetBaseType);

  {   fParent := AParent;
  fResourceLanguage := ALanguage;
  fCodePage := LCIDToCodePage (fResourceLanguage);
  fResourceName := AName;
  fResourceType := GetBaseType;
  if Assigned (AParent) then
    AParent.AddResource (Self);
  fData := TMemoryStream.Create;
  InitNew   }
end;

constructor TResourceElement.CreateNew(AParent: TResourceModule;
  ALanguage: Integer; const AName, AType: AnsiString);
begin
  fParent := AParent;
  fResourceLanguage := ALanguage;
  fCodePage := LCIDToCodePage (fResourceLanguage);
  fResourceName := AName;
  fResourceType := AType;
  if Assigned (AParent) then
    AParent.AddResource (Self);
  fData := TMemoryStream.Create;
  InitNew
end;

(*----------------------------------------------------------------------*
 | TResourceElement.CreateResourceElement                               |
 |                                                                      |
 | Create a class derived from TResourceDetals that reflects the 'Type' |
 | If no matching class is registered, create a base 'TResourceElement' |
 | class.    (Ha!  Try doing *that* in C++ ! )                          |
 *----------------------------------------------------------------------*)
class function TResourceElement.CreateResourceElement(
  AParent: TResourceModule; ALanguage: Integer; const AName,
  AType: AnsiString; ASize: Integer; AData: pointer): TResourceElement;
var
  i : Integer;
begin
  result := Nil;

  if (Length (AType) > 0) then
  try

  // Check for exact match

    for i := 0 to registeredResourceElementCount - 1 do
      if registeredResourceElement [i].GetBaseType = AType then
      begin
        if (AType <> IntToStr (Integer (RT_RCDATA))) or registeredResourceElement [i].SupportsRCData (AName, ASize, AData) then
        begin
          result := registeredResourceElement [i].Create (AParent, ALanguage, AName, AType, ASize, AData);
          break
        end
      end;
  except
  end;

  // If no exact match, check each clas to see if it supports the data
  if Result = nil then
  try
    for i := 0 to registeredResourceElementCount - 1 do
      if registeredResourceElement [i].SupportsData (ASize, AData) then
      begin
        result := registeredResourceElement [i].Create (AParent, ALanguage, AName, AType, ASize, AData);
        break
      end;
  except
  end;

  if result = Nil then
    if TAnsiResourceElement.SupportsData(ASize, AData) then
      result := TAnsiResourceElement.Create (AParent, ALanguage, AName, AType, ASize, AData)
    else
      if TUnicodeResourceElement.SupportsData(ASize, AData) then
        result := TUnicodeResourceElement.Create (AParent, ALanguage, AName, AType, ASize, AData)
      else
        result := TResourceElement.Create (AParent, ALanguage, AName, AType, ASize, AData)
end;

(*----------------------------------------------------------------------*
 | TResourceElement.Destroy                                             |
 *----------------------------------------------------------------------*)
destructor TResourceElement.Destroy;
begin
  fData.Free;
  inherited;
end;

(*----------------------------------------------------------------------*
 | TResourceElement.GetBaseType                                         |
 |                                                                      |
 | Return the base type for the resource Element.  This is overridden   |
 | in derived classes.                                                  |
 *----------------------------------------------------------------------*)
class function TResourceElement.GetBaseType: AnsiString;
begin
  Result := '0';
end;

(*----------------------------------------------------------------------*
 | TResourceElement.InitNew                                             |
 |                                                                      |
 | Override this to initialize a new resource being added to a module.  |
 *----------------------------------------------------------------------*)
procedure TResourceElement.InitNew;
begin
// Stub
end;

(*----------------------------------------------------------------------*
 | TResourceElement.SetResourceName                                     |
 |                                                                      |
 | Set the resource name.                                               |
 *----------------------------------------------------------------------*)
procedure TResourceElement.SetResourceName(const Value: AnsiString);
begin
  fResourceName := Value;
  fDirty := True
end;

(*----------------------------------------------------------------------*
 | TResourceElement.SupportsData                                        |
 |                                                                      |
 | Can be overridden to support a custom resource class, where you can  |
 | determine the custom class from the data - eg. RIFF data, etc.       |
 *----------------------------------------------------------------------*)
class function TResourceElement.SupportsData(Size: Integer;
  data: Pointer): Boolean;
begin
  Result := False; // stub
end;

(*----------------------------------------------------------------------*
 | TResourceElement.SupportsData                                        |
 |                                                                      |
 | Can be overridden to support RC data where you can determine the     |
 | type from the data and name - eg. the Delphi splash screen JPEG      |
 *----------------------------------------------------------------------*)
class function TResourceElement.SupportsRCData(const AName: AnsiString;
  Size: Integer; data: Pointer): Boolean;
begin
  Result := False; // stub
end;

{$endregion}

{$region 'TResourceModule implementation'}
{ TResourceModule }

function TResourceModule.AddResource(Element: TResourceElement): Integer;
begin
  result := -1
  // Stub
end;

procedure TResourceModule.ClearDirty;
var
  i : Integer;
begin
  fDirty := False;
  for i := 0 to ResourceCount - 1 do
    ResourceElement [i].Dirty := False
end;

(*----------------------------------------------------------------------*
 | TResourceModule.DeleteResource                                       |
 |                                                                      |
 | Must be overridden to remove the resource Element object from        |
 | wherever it's stored.  The overriding method must call               |
 | inherited                                                            |
 *----------------------------------------------------------------------*)
procedure TResourceModule.DeleteResource(idx: Integer);
begin
  fDirty := True;
  ResourceElement [idx].BeforeDelete;
end;

(*----------------------------------------------------------------------*
 | TResourceModule.FindResource                                         |
 |                                                                      |
 | Find a resource with a given type/name                               |
 *----------------------------------------------------------------------*)
function TResourceModule.FindResource(const tp,
  Name: AnsiString; ALanguage : Integer): TResourceElement;
var
  i : Integer;
begin
  Result := nil;
  for i := 0 to ResourceCount - 1 do
    if (ResourceElement [i].fResourceType = tp) and (ResourceElement [i].fResourceName = Name) and (Integer (ResourceElement [i].fResourceLanguage) = ALanguage) then
    begin
      Result := ResourceElement [i];
      break
    end;

  if not Assigned (result) then
    for i := 0 to ResourceCount - 1 do
      if (ResourceElement [i].fResourceType = tp) and (ResourceElement [i].fResourceName = Name) and (ResourceElement [i].fResourceLanguage = 0) then
      begin
        Result := ResourceElement [i];
        break
      end
end;

(*----------------------------------------------------------------------*
 | TResourceModule.GetDirty                                             |
 |                                                                      |
 | Returns true if the module or it's resources are 'dirty'             |
 |                                                                      |
 | nb. fDirty is only set if resources have been deleted.               |
 |     After adding a resource make sure the resource's Dirty is set to |
 |     true.                                                            |
 *----------------------------------------------------------------------*)
function TResourceModule.GetDirty: Boolean;
var
  i : Integer;
begin
  Result := fDirty;
  if not fDirty then
    for i := 0 to ResourceCount - 1 do
      if ResourceElement [i].Dirty then
      begin
        Result := True;
        break
      end
end;

(*----------------------------------------------------------------------*
 | TResourceModule.GetUniqueResourceName                                |
 |                                                                      |
 | Generate a unique resource name for a given type.  Names start at    |
 | 1 (though AnsiString lists downgrade that to '0')                        |
 *----------------------------------------------------------------------*)
function TResourceModule.GetUniqueResourceName(const tp: AnsiString): AnsiString;
var
  i : Integer;
  n, n1 : Integer;
  Element : TResourceElement;
begin
  n := 0;

  for i := 0 to ResourceCount - 1 do
  begin
    Element := ResourceElement [i];
    if Element.ResourceType = tp then
    begin
      n1 := ResourceNametoInt (Element.ResourceName);
      if n1 > n then
        n := n1
    end
  end;

  Result := IntToStr (n + 1);
end;

procedure TResourceModule.InsertResource(idx: Integer;
  Element: TResourceElement);
begin
// Stub
end;

(*----------------------------------------------------------------------*
 | TResourceModule.LoadFromFile                                         |
 |                                                                      |
 | Load from file.  This can be overriden but usually isn't as it       |
 | relies on LoadFromStream, which must be.                             |
 *----------------------------------------------------------------------*)
procedure TResourceModule.LoadFromFile(const FileName: AnsiString);
var
  s : TFileStream;
begin
  s := TFileStream.Create (FileName, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream (s);
  finally
    s.Free
  end
end;


procedure TResourceModule.LoadFromStream(stream: TStream);
begin
  raise Exception.Create (rstNoStreaming);
end;

(*----------------------------------------------------------------------*
 | TResourceModule.SaveToFile                                           |
 |                                                                      |
 | Save to file.  This can be overriden but usually isn't as it         |
 | relies on SaveToStream, which must be.                               |
 *----------------------------------------------------------------------*)
procedure TResourceModule.SaveToFile(const FileName: AnsiString);
var
  s : TFileStream;
  oldFileName, ext : AnsiString;
  p : PAnsiChar;
begin
// Rename old file to .~ext'
  oldFileName := FileName;
  UniqueString (oldFileName);
  p := StrRScan (PAnsiChar (oldFileName), '.');
  if p <> Nil then
  begin
    p^ := #0;
    Inc (p);
    ext := p;
    oldFileName := PAnsiChar (oldFileName);
  end
  else
    ext := '';
  ext := '~' + ext;
  oldFileName := oldFileName + '.' + ext;

  if FileExists (oldFileName) then
    DeleteFile (oldFileName);

  RenameFile (FileName, oldFileName);

  try
    s := TFileStream.Create (FileName, fmCreate);
    try
      SaveToStream (s);
      ClearDirty
    finally
      s.Free
    end
  except
// Failed.  Rename old file back.
    DeleteFile (FileName);
    RenameFile (oldFileName, FileName);
    raise
  end
end;

procedure TResourceModule.SaveToStream(stream: TStream);
begin
  raise Exception.Create (rstNoStreaming);
end;

procedure TResourceModule.SortResources;
begin
// Stub
end;
{$endregion}

{$region 'TAnsiResourceElement implementation'}
{ TAnsiResourceElement }

function TAnsiResourceElement.GetText: AnsiString;
begin
  data.Seek(0, soFromBeginning);
  SetString (result, PAnsiChar (data.Memory), data.Size);
end;

procedure TAnsiResourceElement.InitNew;
begin
  Data.Clear;
end;

procedure TAnsiResourceElement.SetText(const Value: AnsiString);
begin
  data.Clear;
  data.Write(Value [1], Length (Value))
end;

class function TAnsiResourceElement.SupportsData(Size: Integer;
  data: Pointer): Boolean;
var
  i, sample : Integer;
  pc : PAnsiChar;
begin
  result := Size > 0;
  sample := Size;
  if Sample > 1024 then
    Sample := 1024;
  pc := PAnsiChar (data);

  if result then
    for i := 0 to Sample - 1 do
    begin
      if (pc^ < ' ') or (pc^ > #127) then
        if not (pc^ in [#9, #10, #13]) then
        begin
          result := False;
          break
        end;

      Inc (pc)
    end
end;
{$endregion}

{$region 'TUnicodeResourceElement implementation'}
{ TUnicodeResourceElement }

function TUnicodeResourceElement.GetText: WideString;
begin
  SetLength (result, Data.Size div sizeof (WideChar));
  Move (Data.Memory^, result [1], data.Size);
end;

procedure TUnicodeResourceElement.InitNew;
begin
  Data.Clear;
end;

procedure TUnicodeResourceElement.SetText(const Value: WideString);
begin
  data.Write(Value [1], Length (Value) * sizeof (WideChar))
end;

class function TUnicodeResourceElement.SupportsData(Size: Integer;
  data: Pointer): Boolean;
var
  i, sample : Integer;
  pc : PAnsiChar;
begin
  result := Size > 5;
  sample := Size div 2;
  if Sample > 1024 then
    Sample := 1024
  else
    Dec (Sample);
  pc := PAnsiChar (data);

  if result then
    for i := 0 to Sample - 2 do
    begin
      if (pc^ < ' ') or (pc^ > #127) then
        if not (pc^ in [#9, #10, #13]) then
        begin
          result := False;
          break
        end;

      Inc (pc, 2)
    end
end;
{$endregion}

end.
