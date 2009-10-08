unit uEnumStringList;

interface

uses
 Classes, ActiveX;

type
  TEnumString = class(TInterfacedObject, IEnumString)
  private
    FStrings: TStrings;
    FIndex: Integer;
  protected
    {$region IEnumString }
    function Next(celt: Longint; out elt;
      pceltFetched: PLongint): HResult; stdcall;
    function Skip(celt: Longint): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out enm: IEnumString): HResult; stdcall;
    {$EndRegion}
  public
    constructor Create(Strings: TStrings);
  end;


  TEnumStringList = class(TStringList)
  private
    FEnumString : TEnumString;
    function GetDefaultInterface: IUnknown;
  public
    property DefaultInterface : IUnknown read GetDefaultInterface;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TEnumString }

constructor TEnumString.Create(Strings: TStrings);
begin
  inherited Create;
  FStrings := Strings;
end;

{ TEnumString.IEnumString }

function TEnumString.Next(celt: Longint;
  out elt; pceltFetched: PLongint): HResult;
var
  I: Integer;
begin
  I := 0;
  while (I < celt) and (FIndex < FStrings.Count) do
  begin
    TPointerList(elt)[I] := PWideChar(WideString(FStrings[FIndex]));
    Inc(I);
    Inc(FIndex);
  end;
  if pceltFetched <> nil then pceltFetched^ := I;
  if I = celt then Result := S_OK else Result := S_FALSE;
end;


function TEnumString.Skip(celt: Longint): HResult;
begin
  if (FIndex + celt) <= FStrings.Count then
  begin
    Inc(FIndex, celt);
    Result := S_OK;
  end
  else
  begin
    FIndex := FStrings.Count;
    Result := S_FALSE;
  end;
end;

function TEnumString.Reset: HResult;
begin
  FIndex := 0;
  Result := S_OK;
end;

function TEnumString.Clone(out enm: IEnumString): HResult;
begin
  try
    enm := TEnumString.Create(FStrings);
    Result := S_OK;
  except
    Result := E_UNEXPECTED;
  end;
end;

{ TEnumStringList }

constructor TEnumStringList.Create;
begin
  inherited;
  FEnumString:=TEnumString.Create(self);
end;

destructor TEnumStringList.Destroy;
begin
  inherited;
end;

function TEnumStringList.GetDefaultInterface: IUnknown;
begin
  result:=FEnumString as IUnknown;
end;

end.
