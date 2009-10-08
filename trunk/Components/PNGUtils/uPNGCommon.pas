//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit uPNGCommon;

interface

uses
  Classes,
  SysUtils,
  PNGImage;

type
  {Forward declarations}
  TPNGPbjectListEnumerator = class;

  TPNGObjectList = class;

  TPNGObjectListChangeProc = procedure(Sender : TPNGObjectList; AIndex : Integer) of object;

  {TPNGObjectList
   - a list which holds TPNGObject's }
  TPNGObjectList = class(TList)
  private
    FOnChange: TPNGObjectListChangeProc;
    
    function Get(Index: Integer): TPNGObject;
    procedure Put(Index: Integer; const Value: TPNGObject);

    procedure DoChange(AIndex : Integer);
  public
    constructor Create(); virtual;
    destructor Destroy(); override;

    procedure Clear(); override;
    function Add(Item: TPNGObject): Integer;
    procedure Delete(Index: Integer);
    function Extract(Item: TPNGObject): TPNGObject;
    function First: TPNGObject;
    function GetEnumerator: TPNGPbjectListEnumerator;
    function IndexOf(Item: TPNGObject): Integer;
    procedure Insert(Index: Integer; Item: TPNGObject);
    function Last: TPNGObject;
    function Remove(Item: TPNGObject): Integer;
    property Items[Index: Integer]: TPNGObject read Get write Put; default;

    procedure Assign(Source : TObject);

    procedure LoadFromStream(AStream : TStream);
    procedure SaveToStream(AStream : TStream);

    property OnChange : TPNGObjectListChangeProc read FOnChange write FOnChange;
  end;


  {TPNGPbjectListEnumerator
   - the enumerator for TPNGObjectList
   - use it to walk through the list via:
       var
         PNG : TPNGObject;
       begin
         for PNG in List do
         begin
           ...
         end;}
  TPNGPbjectListEnumerator = class
  private
    FIndex: Integer;
    FList: TPNGObjectList;
  public
    constructor Create(APNGList : TPNGObjectList);
    function GetCurrent: TPNGObject;
    function MoveNext: Boolean;
    property Current: TPNGObject read GetCurrent;
  end;



implementation

{ TPNGPbjectListEnumerator }

constructor TPNGPbjectListEnumerator.Create(APNGList: TPNGObjectList);
begin
  FIndex:=-1;
  FList:=APNGList;
end;

function TPNGPbjectListEnumerator.GetCurrent: TPNGObject;
begin
  Result:=FList[FIndex];
end;

function TPNGPbjectListEnumerator.MoveNext: Boolean;
begin
  Result:=FIndex < FList.Count - 1;
  if Result then
    Inc(FIndex)
end;


{ TPNGObjectList }

function TPNGObjectList.Add(Item: TPNGObject): Integer;
begin
  Result:=Count;
  Insert(Result, Item);
  
  DoChange(Result);
end;

procedure TPNGObjectList.Assign(Source: TObject);
var
  ASource : TPNGObjectList;
  PNG : TPNGObject;
begin
  Clear;
  
  if Source is TPNGObjectList then
  begin
    ASource:=TPNGObjectList(Source);

    for PNG in ASource do
      Add(PNG);

  end;
end;

procedure TPNGObjectList.Clear;
begin
  while Count>0 do
    Delete(0);
    
  inherited;
end;

constructor TPNGObjectList.Create;
begin
  inherited;
end;

procedure TPNGObjectList.Delete(Index: Integer);
begin
  Items[Index].Free;
  inherited Delete(Index);
end;

destructor TPNGObjectList.Destroy;
begin
  inherited;
end;

procedure TPNGObjectList.DoChange(AIndex: Integer);
begin
  if Assigned(FOnChange) then
    FOnChange(Self, AIndex);
end;

function TPNGObjectList.Extract(Item: TPNGObject): TPNGObject;
begin
  Result:=inherited Extract(Item);
end;

function TPNGObjectList.First: TPNGObject;
begin
  Result:=inherited First;
end;

function TPNGObjectList.Get(Index: Integer): TPNGObject;
begin
  Result:=inherited Get(Index);
end;

function TPNGObjectList.GetEnumerator: TPNGPbjectListEnumerator;
begin
  Result:=TPNGPbjectListEnumerator.Create(Self);
end;

function TPNGObjectList.IndexOf(Item: TPNGObject): Integer;
begin
  Result:=inherited IndexOf(Item);
end;

procedure TPNGObjectList.Insert(Index: Integer; Item: TPNGObject);
var
  PNG : TPNGObject;
  idx : Integer;
begin
  PNG:=TPNGObject.Create();
  PNG.Assign(Item);
  inherited Insert(Index, PNG);

  for idx := Index to Count - 1 do
    DoChange(idx);
end;

function TPNGObjectList.Last: TPNGObject;
begin
  Result:=inherited Last;
end;

procedure TPNGObjectList.LoadFromStream(AStream: TStream);
var
  idx,
  Cnt : Integer;
  PNG : TPNGObject;
begin
  Clear;

  AStream.Read(Cnt, SizeOf(Cnt));

  PNG:=TPNGObject.Create;
  try
    for idx := 0 to Cnt - 1 do
    begin
      PNG.LoadFromStream(AStream);
      Add(PNG);
    end;
  finally
    PNG.Free;
  end;

end;

procedure TPNGObjectList.Put(Index: Integer; const Value: TPNGObject);
begin
  TPNGObject(inherited Get(Index)).Assign(Value);
end;

function TPNGObjectList.Remove(Item: TPNGObject): Integer;
begin
  Result:=inherited Remove(Item);
end;

procedure TPNGObjectList.SaveToStream(AStream: TStream);
var
  Cnt : Integer;
  PNG : TPNGObject;
begin
  Cnt:=Count;

  AStream.Write(Cnt, SizeOf(Cnt));

  for PNG in Self do
  begin
    PNG.SaveToStream(AStream);
  end;
end;

end.
