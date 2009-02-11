//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit unitResourceRCData;

interface

uses Windows, Classes, SysUtils, Contnrs, unitResourceElement;

type
TRCDataResourceElement = class (TResourceElement)
public
  class function GetBaseType : AnsiString; override;
end;

TRCDataDescriptionResourceElement = class (TRCDataResourceElement)
private
  function GetDescription: AnsiString;
  procedure SetDescription(const Value: AnsiString);
protected
  class function SupportsRCData (const AName : AnsiString; Size : Integer; data : Pointer) : Boolean; override;
public
  property Description : AnsiString read GetDescription write SetDescription;
end;

TRCDataFormResourceElement = class (TRCDataResourceElement)
  private
  function GetText: AnsiString;
  procedure SetText(const Value: AnsiString);
protected
  class function SupportsRCData (const AName : AnsiString; Size : Integer; data : Pointer) : Boolean; override;
public
  property Text : AnsiString read GetText write SetText;
end;

TPackageEnvironment = (pePreV4, peUndefine, peBCB, peDelphi);
TModuleType = (mtEXE, mtPackageDLL, mtLibraryDLL, mtUndefine);

TRCDataPackagesResourceElement = class (TRCDataResourceElement)
private
  fRequiresList : TStrings;
  fContainsList : TStrings;
  fFlags : DWORD;

  function GetRequiresCount: Integer;
  function GetRequires(idx : Integer): AnsiString;
  function GetContainsCount: Integer;
  function GetContains(idx: Integer): AnsiString;
  function GetContainsFlag(idx: Integer): Byte;

  procedure DecodeData;
  function GetCheckForDuplicates: Boolean;
  function GetDesignTimeOnly: Boolean;
  function GetEnvironment: TPackageEnvironment;
  function GetModuleType: TModuleType;
  function GetNeverBuild: Boolean;
  function GetRunTimeOnly: Boolean;
protected
  class function SupportsRCData (const AName : AnsiString; Size : Integer; data : Pointer) : Boolean; override;
public
  destructor Destroy; override;
  procedure ChangeData (newData : TMemoryStream); override;
  property RequiresCount : Integer read GetRequiresCount;
  property Requires [idx : Integer] : AnsiString read GetRequires;
  property ContainsCount : Integer read GetContainsCount;
  property Contains [idx : Integer] : AnsiString read GetContains;
  property ContainsFlag [idx : Integer] : Byte read GetContainsFlag;

  property NeverBuild : Boolean read GetNeverBuild;
  property DesignTimeOnly : Boolean read GetDesignTimeOnly;
  property RunTimeOnly : Boolean read GetRunTimeOnly;
  property CheckForDuplicates : Boolean read GetCheckForDuplicates;
  property Environment : TPackageEnvironment read GetEnvironment;
  property ModuleType : TModuleType read GetModuleType;


end;

implementation

type
  TPkgName = packed record
    HashCode : Byte;
    Name : array [0..255] of Char;
  end;
  PPkgName = ^TPkgName;

  { PackageUnitFlags:
    bit      meaning
    -----------------------------------------------------------------------------------------
    0      | main unit
    1      | package unit (dpk source)
    2      | $WEAKPACKAGEUNIT unit
    3      | original containment of $WEAKPACKAGEUNIT (package into which it was compiled)
    4      | implicitly imported
    5..7   | reserved
  }

  PUnitName = ^TUnitName;
  TUnitName = packed record
    Flags : Byte;
    HashCode: Byte;
    Name: array[0..255] of Char;
  end;

{ TRCDataResourceElement }

class function TRCDataResourceElement.GetBaseType: AnsiString;
begin
  result := IntToStr (Integer (RT_RCDATA));
end;

{ TRCDataDescriptionResourceElement }

function TRCDataDescriptionResourceElement.GetDescription: AnsiString;
begin
  Result := PWideChar (data.Memory);
end;

procedure TRCDataDescriptionResourceElement.SetDescription(
  const Value: AnsiString);
var
  ws : WideString;
begin
  data.Size := (Length (Value) + 1) * SizeOf (WideChar);
  ws := Value;
  Move (ws [1], data.memory^, (Length (Value) + 1) * SizeOf (WideChar))
end;

class function TRCDataDescriptionResourceElement.SupportsRCData(
  const AName: AnsiString; Size: Integer; data: Pointer): Boolean;
begin
  Result := CompareText (AName, 'DESCRIPTION') = 0;
end;

{ TRCDataPackagesResourceElement }

procedure TRCDataPackagesResourceElement.ChangeData(
  newData: TMemoryStream);
begin
  inherited;
  FreeAndNil (fRequiresList);
  FreeAndNil (fContainsList);
end;

procedure TRCDataPackagesResourceElement.DecodeData;
var
  p : PAnsiChar;
  i, Count : Integer;
  pkg : PPkgName;
  unt : PUnitName;
begin
  if not Assigned (fRequiresList) then
  begin
    fRequiresList := TStringList.Create;
    fContainsList := TStringList.Create;

    p := Data.Memory;
    fFlags := PDWORD (p)^;
    Inc (p, SizeOf (DWORD)); //  Flags

    Count := PInteger (p)^;
    Inc (p, SizeOf (Integer));

    for i := 0 to Count - 1 do
    begin
      pkg := PPkgName (p);


      fRequiresList.Add (pkg^.Name);
      Inc (p, 2 + lstrlen (pkg^.Name));
    end;

    Count := PInteger (p)^;
    Inc (p, SizeOf (Integer));

    for i := 0 to Count - 1 do
    begin
      unt := PUnitName (p);
      fContainsList.AddObject (unt^.Name, TObject (Integer (unt.Flags)));
      Inc (p, 3 + lstrlen (unt^.Name));
    end
  end
end;

destructor TRCDataPackagesResourceElement.Destroy;
begin
  fRequiresList.Free;
  fContainsList.Free;
  inherited;
end;

function TRCDataPackagesResourceElement.GetCheckForDuplicates: Boolean;
begin
  DecodeData;
  Result := (fFlags and 8) = 0
end;

function TRCDataPackagesResourceElement.GetContains(idx: Integer): AnsiString;
begin
  DecodeData;
  Result := fContainsList [idx]
end;

function TRCDataPackagesResourceElement.GetContainsCount: Integer;
begin
  DecodeData;
  Result := fContainsList.Count
end;

function TRCDataPackagesResourceElement.GetContainsFlag(
  idx: Integer): Byte;
begin
  DecodeData;
  Result := Integer (fContainsList.Objects [idx])
end;

function TRCDataPackagesResourceElement.GetDesignTimeOnly: Boolean;
begin
  DecodeData;
  Result := (fFlags and 2) <> 0
end;

function TRCDataPackagesResourceElement.GetEnvironment: TPackageEnvironment;
begin
  DecodeData;
  Result := TPackageEnvironment ((fFlags shr 26) and 3);
end;

function TRCDataPackagesResourceElement.GetModuleType: TModuleType;
begin
  DecodeData;
  Result := TModuleType (fFlags shr 30);
end;

function TRCDataPackagesResourceElement.GetNeverBuild: Boolean;
begin
  DecodeData;
  Result := (fFlags and 1) <> 0
end;

function TRCDataPackagesResourceElement.GetRequires(idx : Integer): AnsiString;
begin
  DecodeData;
  Result := fRequiresList [idx]
end;

function TRCDataPackagesResourceElement.GetRequiresCount: Integer;
begin
  DecodeData;
  Result := fRequiresList.Count
end;

function TRCDataPackagesResourceElement.GetRunTimeOnly: Boolean;
begin
  DecodeData;
  Result := (fFlags and 4) <> 0
end;

class function TRCDataPackagesResourceElement.SupportsRCData(
  const AName: AnsiString; Size: Integer; data: Pointer): Boolean;
begin
  Result := CompareText (AName, 'PACKAGEINFO') = 0;
end;

{ TRCDataFormResourceElement }

function TRCDataFormResourceElement.GetText: AnsiString;
var
  s : TStringStream;
begin
  s := TStringStream.Create ('');
  try
    data.Seek (0, soFromBeginning);
    ObjectBinaryToText (data, s);
    Result := s.DataString
  finally
    s.Free
  end
end;

procedure TRCDataFormResourceElement.SetText(const Value: AnsiString);
var
  s : TStringStream;
  m : TMemoryStream;
begin
  s := TStringStream.Create (Value);
  try
    m := TMemoryStream.Create;
    try
      s.Seek (0, soFromBeginning);
      ObjectTextToBinary (s, m);
      ChangeData (m);
    finally
      m.Free;
    end
  finally
    s.Free
  end
end;

class function TRCDataFormResourceElement.SupportsRCData(
  const AName: AnsiString; Size: Integer; data: Pointer): Boolean;
begin
  Result := (Size > 0) and (strlcomp (PAnsiChar (data), 'TPF0', 4) = 0);
end;

initialization
  RegisterResourceElement (TRCDataDescriptionResourceElement);
  RegisterResourceElement (TRCDataPackagesResourceElement);
  RegisterResourceElement (TRCDataFormResourceElement);
finalization
  UnregisterResourceElement (TRCDataDescriptionResourceElement);
  UnregisterResourceElement (TRCDataPackagesResourceElement);
  UnregisterResourceElement (TRCDataFormResourceElement);
end.
